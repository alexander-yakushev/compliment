(ns compliment.sources.local-bindings
  "Completion source for local bindings introduced by defn, let and the like."
  (:require [compliment.sources :refer [defsource]]
            [compliment.sources.ns-mappings :refer [var-symbol? dash-matches?]]))

(def let-like-forms '#{let if-let when-let if-some when-some loop with-open
                       dotimes with-local-vars})

(def defn-like-forms '#{defn defn- fn defmacro defmethod})

(def doseq-like-forms '#{doseq for})

(def letfn-like-forms '#{letfn})

(def destructuring-keys #{:keys :strs :syms})

(def destructuring-key-names (into #{} (map name destructuring-keys)))

(defn parse-binding
  "Given a binding node returns the list of local bindings introduced by that
  node. Handles vector and map destructuring."
  [binding-node]
  (cond (vector? binding-node)
        (mapcat parse-binding binding-node)

        (map? binding-node)
        (let [normal-binds (->> (keys binding-node)
                                (remove keyword?)
                                (mapcat parse-binding))
              keys-binds (->> binding-node
                              (mapcat (fn [[k v]]
                                        (when (or (destructuring-keys k)
                                                  (and (keyword? k) (destructuring-key-names (name k))))
                                          v)))
                              (map #(if (keyword? %) (symbol (name %)) %)))
              as-bind (:as binding-node)]
          (cond-> (concat normal-binds keys-binds)
            as-bind (conj as-bind)))

        (not (#{'& '_} binding-node))
        [binding-node]))

(defn parse-fn-body
  "Extract function name and arglists from the function body, return list of all
  completable variables."
  [fn-body]
  (let [fn-name (when (symbol? (first fn-body))
                  (first fn-body))
        fn-body (if fn-name (rest fn-body) fn-body)]
    (cond->
        (mapcat parse-binding
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
  (when (list? form)
    (let [sym (first form)
          locals-meta (when (symbol? sym)
                        (:completion/locals (meta (ns-resolve ns sym))))]
      (cond (or (let-like-forms sym) (= locals-meta :let))
            (mapcat parse-binding (take-nth 2 (second form)))

            (or (defn-like-forms sym) (= locals-meta :defn))
            (parse-fn-body (rest form))

            (or (letfn-like-forms sym) (= locals-meta :letfn))
            (mapcat parse-fn-body (second form))

            (or (doseq-like-forms sym) (= locals-meta :doseq))
            (->> (partition 2 (second form))
                 (mapcat (fn [[left right]]
                           (if (= left :let)
                             (take-nth 2 right) [left])))
                 (mapcat parse-binding))

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
