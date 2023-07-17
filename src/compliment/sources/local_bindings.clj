(ns compliment.sources.local-bindings
  "Completion source for local bindings introduced by defn, let and the like."
  (:require [compliment.sources :refer [defsource]]
            [compliment.sources.ns-mappings :refer [var-symbol? dash-matches?]]
            [compliment.utils :as utils]))

;; syntax-quoted variants are important for when we perform macroexpansion as part of our processing.
(def let-like-forms #{'let `let
                      'if-let `if-let
                      'when-let `when-let
                      'if-some `if-some
                      'when-some `when-some
                      'loop `loop
                      'with-open `with-open
                      'dotimes `dotimes
                      'with-local-vars `with-local-vars})

(def defn-like-forms #{'defn `defn
                       'defn- `defn-
                       'fn `fn
                       `fn*
                       'defmacro `defmacro
                       'defmethod `defmethod})

(def doseq-like-forms #{'doseq `doseq
                        'for `for})

(def letfn-like-forms #{'letfn `letfn})

(def destructuring-key-names #{"keys" "strs" "syms"})

(defn extract-tag-from-bound-to [ns bound-to]
  (when bound-to
    (when-let [found (or (-> bound-to meta :tag)
                         (utils/var->class ns bound-to)
                         (utils/invocation-form->class ns bound-to))]
      (if (class? found)
        (-> ^Class found .getName symbol)
        found))))

(defn parse-binding
  "Given a binding node returns the list of local bindings introduced by that
  node. Handles vector and map destructuring."
  [ns binding-node bound-to]
  (cond (vector? binding-node)
        (mapcat (fn [x]
                  (parse-binding ns x nil))
                binding-node)

        (map? binding-node)
        (let [normal-binds (->> (keys binding-node)
                                (remove keyword?)
                                (mapcat (fn [x]
                                          (parse-binding ns x nil))))
              keys-binds (->> binding-node
                              (mapcat (fn [[k v]]
                                        ;; This handles both plain and
                                        ;; namespaced destructuring keywords.
                                        (when (and (keyword? k)
                                                   (destructuring-key-names (name k)))
                                          v)))
                              (map #(if (keyword? %) (symbol (name %)) %)))
              as-bind (:as binding-node)]
          (cond-> (concat normal-binds keys-binds)
            as-bind (conj as-bind)))

        (not (#{'& '_} binding-node))
        (let [result binding-node
              candidate (delay (extract-tag-from-bound-to ns bound-to))]
          (if (-> result meta :tag)
            [result]
            (if-let [candidate (extract-tag-from-bound-to ns bound-to)]
              [(vary-meta result assoc :tag candidate)]
              [result])))))

(defn parse-fn-body
  "Extract function name and arglists from the function body, return list of all
  completable variables."
  [ns fn-body]
  (let [fn-name (when (symbol? (first fn-body))
                  (first fn-body))
        fn-body (if fn-name (rest fn-body) fn-body)]
    (cond->
        (mapcat (fn [x]
                  (parse-binding ns x nil))
                (loop [[c & r] fn-body, bnodes []]
                  (cond (nil? c) bnodes
                        (list? c) (recur r (conj bnodes (first c))) ;; multi-arity case
                        (vector? c) c                               ;; single-arity case
                        :else (recur r bnodes))))
      fn-name (conj fn-name))))

(defn extract-local-bindings
  "When given a form that has a binding vector traverses that binding vector and
  returns the list of all local bindings."
  [form ns]
  (when (seq? form)
    (let [sym (first form)
          locals-meta (when (symbol? sym)
                        (:completion/locals (meta (ns-resolve ns sym))))]
      (cond (or (let-like-forms sym) (= locals-meta :let))
            (mapcat (fn [[x y]]
                      (parse-binding ns x y))
                    (partition 2 (second form)))

            (or (defn-like-forms sym) (= locals-meta :defn))
            (parse-fn-body ns (rest form))

            (or (letfn-like-forms sym) (= locals-meta :letfn))
            (mapcat (fn [fn-body]
                      (parse-fn-body ns fn-body))
                    (second form))

            (or (doseq-like-forms sym) (= locals-meta :doseq))
            (->> (partition 2 (second form))
                 (mapcat (fn [[left right]]
                           (if (= left :let)
                             (take-nth 2 right) [left])))
                 (mapcat (fn [x]
                           (parse-binding ns x nil))))

            (= sym 'as->) [(nth form 2)]))))

(defn- distinct-preserve-tags
  "Like `distinct` but keeps symbols that have type tag with a higher priority."
  [coll]
  (->> coll
       (sort (fn [x y]
               (let [tx (:tag (meta x))
                     ty (:tag (meta y))]
                 (cond (and tx (not ty)) -1
                       (and (not tx) ty) 1
                       :else 0))))
       distinct))

(defn bindings-from-context
  "Returns all local bindings that are established inside the given context."
  [ctx ns]
  (try (->> (mapcat #(extract-local-bindings (:form %) ns) ctx)
            (filter symbol?)
            distinct-preserve-tags)
       (catch Exception ex ())))

(defn candidates
  "Returns a list of local bindings inside the context that match prefix."
  [prefix ns context]
  (when (var-symbol? prefix)
    (for [binding (bindings-from-context context ns)
          :let [binding (name binding)]
          :when (dash-matches? prefix binding)]
      {:candidate binding, :type :local})))

(defsource ::local-bindings
  :candidates #'candidates
  :doc (constantly nil))
