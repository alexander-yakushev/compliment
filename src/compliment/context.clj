(ns compliment.context
  "Utilities for parsing and storing the current completion context."
  (:require [clojure.string :as str]
            [clojure.walk :as walk]))

(defn- walk-meta-preserving
  "Like `clojure.walk/walk`, but preserves meta. Redundant after
  https://clojure.atlassian.net/browse/CLJ-2568 is merged."
  [inner outer form]
  (let [restore-meta #(if-let [fm (meta form)]
                        (with-meta %
                          (merge fm (meta %)))
                        %)]
    (cond
      (list? form) (outer (restore-meta (apply list (map inner form))))
      (instance? clojure.lang.IMapEntry form)
      (outer (clojure.lang.MapEntry/create (inner (key form)) (inner (val form))))
      (seq? form) (outer (restore-meta (doall (map inner form))))
      (instance? clojure.lang.IRecord form)
      (outer (restore-meta (reduce (fn [r x] (conj r (inner x))) form form)))
      (coll? form) (outer (restore-meta (into (empty form) (map inner form))))
      :else (outer form))))

(defn- postwalk [f form]
  (walk-meta-preserving (partial postwalk f) identity (f form)))

(defn- restore-map-literals [context]
  (postwalk (fn [el]
              (if (and (sequential? el)
                       (= (first el) 'compliment-hashmap))
                (apply hash-map
                       (if (even? (count el))
                         (concat (rest el) [nil])
                         (rest el)))
                el)) context))

(defn- try-read-replacing-maps [s]
  (try (binding [*read-eval* false]
         (let [ns-aliases (ns-aliases *ns*)]
           (-> s
               ;; To avoid replacing '\{' and '\}' with '(compliment-hashmap ' and ')'
               (str/replace "\\{" "(char 123)")
               (str/replace "\\}" "(char 125)")
               (str/replace "{" "(compliment-hashmap ")
               (str/replace "}" ")")
               ;; The reader breaks on aliased keywords if the respective
               ;; namespace isn't imported into the current ns.
               (str/replace #"::([-!?+*_<>.\w]+)/"
                            (fn [[_ kw-ns]]
                              (str ":" (get ns-aliases (symbol kw-ns) kw-ns) "/")))
               read-string
               restore-map-literals)))
       (catch Exception ex)))

(defn- dumb-read-form
  "Take a presumably unfinished Clojure form and try to \"complete\" it so that it
  can be read. The algorithm is incredibly stupid, but is better than nothing."
  [unfinished-form-str]
  (let [open->close {\( \), \[ \], \{ \}},
        close->open {\) \(, \] \[, \} \{}]
    (loop [[c & r] (reverse (filter (set "([{}])") unfinished-form-str))
           to-append []]
      (if c
        (cond (open->close c)
              (recur r (conj to-append (open->close c)))

              (close->open c)
              (if (= c (open->close (first r)))
                (recur (rest r) to-append)
                ;; Everything is bad - just give up
                nil))
        (try-read-replacing-maps (apply str unfinished-form-str to-append))))))

#_(dumb-read-form "(let [a {:b 1}, c {__prefix__")

(defn- safe-read-context-string [^String context]
  (or (try-read-replacing-maps context)
      (dumb-read-form context)))

(def ^:private context-cache
  "Stores the last context string and parsed context."
  (atom nil))

(def prefix-placeholder
  "Special symbol which substitutes prefix in the context, so the former can be
  found unambiguously."
  '__prefix__)

(defn- macroexpand-form [form]
  (postwalk (fn [x]
              (let [call? (and (seq? x)
                               (-> x first symbol?))
                    resolved (when call?
                               (ns-resolve *ns* (first x)))]
                (cond
                  (and call?
                       (contains? #{#'clojure.core/->
                                    #'clojure.core/->>
                                    #'clojure.core/doto}
                                  resolved))
                  (macroexpand-1 x)

                  ;; The macroexpansion of some-> is trickier than that of ->,
                  ;; so we macroexpand -> instead,
                  ;; which is equivalent for our purposes:
                  (and call?
                       (= resolved #'clojure.core/some->))
                  (macroexpand-1 (cons `-> (rest x)))

                  (and call?
                       (= resolved #'clojure.core/some->>))
                  (macroexpand-1 (cons `->> (rest x)))

                  :else x)))
            form))

(defn parse-context
  "Takes a context which is a Lisp form and returns a transformed context.

  The result is a list of maps, each map represents a level of the context from
  inside to outside. Map has `:idx` and `:form` values, and `:map-role` if the
  level is a map. `:idx` defines the position of prefix (or the form containing
  prefix) on the current level (number for lists and vectors, key or value for
  maps).

  Example: `(dotimes [i 10] ({:foo {:baz __prefix__}, :bar 42} :quux))`

  Transformed it looks like:

  `({:idx :baz, :map-role :value, :form {:baz __prefix__}}
    {:idx :foo, :map-role :key, :form {:foo {:baz __prefix__}, :bar 42}}
    {:idx 0, :form ({:foo {:baz __prefix__}, :bar 42} :quux)}
    {:idx 2, :form (dotimes [i 10] ({:foo {:baz __prefix__}, :bar 42} :quux))})`."
  [context]
  (letfn [(parse [ctx]
            (cond
              (sequential? ctx)
              (when-let [[idx rest] (first (keep-indexed (fn [idx el]
                                                           (when-let [p (parse el)]
                                                             [idx p]))
                                                         ctx))]
                (cons {:idx idx :form ctx} rest))

              (map? ctx)
              (when-let [[idx role rest] (first (keep (fn [[k v]]
                                                        (if-let [p (parse v)]
                                                          [k :value p]
                                                          (when-let [p (parse k)]
                                                            [v :key p])))
                                                      ctx))]
                (cons {:idx idx :map-role role :form ctx} rest))

              (string? ctx)
              (let [idx (.indexOf ^String ctx (name prefix-placeholder))]
                (when (>= idx 0)
                  [{:idx idx :form ctx}]))

              (= ctx prefix-placeholder) ()))]
    (some-> (parse context) reverse)))

(defn cache-context
  "Parses the context, or returns one from cache if it was unchanged."
  [context-string]
  (let [[prev-ctx-string prev-ctx] @context-cache]
    (if (= context-string prev-ctx-string)
      prev-ctx
      (let [context (-> (safe-read-context-string context-string)
                        macroexpand-form
                        parse-context)]
        (reset! context-cache [context-string context])
        context))))
