(ns compliment.context
  "Utilities for parsing and storing the current completion context."
  (:require [clojure.walk :refer [walk]]))

(defn- restore-map-literals [context]
  (clojure.walk/postwalk (fn [el]
                           (if (and (sequential? el)
                                    (= (first el) 'compliment-hashmap))
                             (apply hash-map
                                    (if (even? (count el))
                                      (concat (rest el) [nil])
                                      (rest el)))
                             el)) context))

(defn- safe-read-context-string [^String context]
  (try (-> context
           (.replace "{" "(compliment-hashmap ")
           (.replace "}" ")")
           read-string
           restore-map-literals)
       (catch Exception ex nil)))

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
