(ns compliment.context
  "Utilities for parsing and storing the current completion context."
  (:require [clojure.string :as str]
            [clojure.walk :as walk]))

(defn- restore-map-literals [context]
  (walk/postwalk (fn [el]
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
               (str/replace "{" "(compliment-hashmap ")
               (str/replace "}" ")")
               (str/replace #"::([-\w]+)/" (fn [[_ kw-ns]]
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

(def ^{:doc "Stores the last completion context."
       :private true}
  previous-context (atom nil))

(def ^{:doc "Special symbol which substitutes prefix in the context,
  so the former can be found unambiguously."}
  prefix-placeholder '__prefix__)

(defn parse-context
  "Takes a context which is a Lisp form and returns a transformed context.

  The result is a list of maps, each map represents a level of the
  context from inside to outside. Map has `:idx` and `:form` values,
  and `:map-role` if the level is a map. `:idx` defines the position
  of prefix (or the form containing prefix) on the current
  level (number for lists and vectors, key or value for maps).

  Example: `(dotimes [i 10] ({:foo {:baz __prefix__}, :bar 42} :quux))`

  Transformed it looks like:

  `({:idx :baz, :map-role :value, :form {:baz __prefix__}}
    {:idx :foo, :map-role :key, :form {:foo {:baz __prefix__}, :bar 42}}
    {:idx 0, :form ({:foo {:baz __prefix__}, :bar 42} :quux)}
    {:idx 2, :form (dotimes [i 10] ({:foo {:baz __prefix__}, :bar 42} :quux))})`."
  [context]
  (let [parse (fn parse [ctx]
                (cond
                 (sequential? ctx)
                 (when-let [res (first (keep-indexed (fn [idx el]
                                                       (when-let [p (parse el)]
                                                         [idx p]))
                                                     ctx))]
                   (cons {:idx (first res) :form ctx} (second res)))

                 (map? ctx)
                 (when-let [res (first (keep (fn [[k v]]
                                               (if-let [p (parse v)]
                                                 [k :value p]
                                                 (when-let [p (parse k)]
                                                   [v :key p])))
                                             ctx))]
                   (cons {:idx (first res) :map-role (second res) :form ctx}
                         (nth res 2)))

                 (string? ctx)
                 (let [idx (.indexOf ^String ctx (name prefix-placeholder))]
                   (when (>= idx 0)
                     [{:idx idx :form ctx}]))

                 (= ctx prefix-placeholder) ()))
        parsed (parse context)]
    (when parsed
      (reverse parsed))))

(defn cache-context
  "Parses the context, or returns one from cache if it was unchanged."
  [context-string]
  (let [context (safe-read-context-string context-string)]
    (when-not (= context :same)
      (reset! previous-context (parse-context context))))
  @previous-context)
