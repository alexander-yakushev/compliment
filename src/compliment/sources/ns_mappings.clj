(ns compliment.sources.ns-mappings
  "Completion for vars and classes in the current namespace."
  (:require [compliment.sources :refer [defsource]]
            [compliment.utils :refer [fuzzy-matches? resolve-namespace
                                      *extra-metadata*]])
  (:import java.io.StringWriter))

(defn var-symbol?
  "Test if prefix resembles a var name."
  [x]
  (re-matches #"([^\/\:][^\.\/]*([^\/\:]*\/[^\.\/]*)?)?" x))

(defn dash-matches?
  "Tests if prefix partially matches a var name with dashes as
  separators."
  [prefix var]
  (fuzzy-matches? prefix var \-))

(defn get-scope-and-prefix
  "Tries to get take apart scope namespace and prefix in prefixes like
  `scope/var`."
  [^String s, ns]
  (let [[scope-name sym] (if (> (.indexOf s "/") -1)
                           (.split s "/") ())
        scope (when scope-name
                (resolve-namespace (symbol scope-name) ns))
        prefix (if scope
                 (or sym "") s)]
    [scope-name scope prefix]))

(defn try-get-ns-from-context
  "Tries to extract a namespace name if context is a `ns` definition."
  [context]
  (let [[var-list ns-def use-def top-form] context]
    (when (and (sequential? (:form var-list))
               (= (first (:form top-form)) 'ns)
               (or (and (= (first (:form use-def)) :use)
                        (= (second (:form ns-def)) :only))
                   (and (= (first (:form use-def)) :require)
                        (= (second (:form ns-def)) :refer))))
      (find-ns (first (:form ns-def))))))

(defn generate-docstring
  "Generates a docstring from a given var metadata. Copied from
  `clojure.repl` with some minor modifications."
  [m]
  (binding [*out* (StringWriter.)]
    (println (str (when-let [ns (:ns m)] (str (ns-name ns) "/")) (:name m)))
    (cond
      (:forms m) (doseq [f (:forms m)]
                   (print "  ")
                   (prn f))
      (:arglists m) (prn (:arglists m)))
    (if (:special-form m)
      (do
        (println "Special Form")
        (println " " (:doc m))
        (if (contains? m :url)
          (when (:url m)
            (println (str "\n  Please see http://clojure.org/" (:url m))))
          (println (str "\n  Please see http://clojure.org/special_forms#"
                        (:name m)))))
      (do
        (when (:macro m)
          (println "Macro"))
        (println " " (:doc m))))
    (str *out*)))

(defn candidates
  "Returns a list of namespace-bound candidates, with namespace being
  either the scope (if prefix is scoped), `ns` arg or the namespace
  extracted from context if inside `ns` declaration."
  [^String prefix, ns context]
  (when (var-symbol? prefix)
    (let [[scope-name scope ^String prefix] (get-scope-and-prefix prefix ns)
          ns-form-namespace (try-get-ns-from-context context)
          vars (cond
                 scope (ns-publics scope)
                 ns-form-namespace (ns-publics ns-form-namespace)
                 :else (ns-map ns))]
      (for [[var-sym var] vars
            :let [var-name (name var-sym)
                  {:keys [arglists doc] :as var-meta} (meta var)]
            :when (dash-matches? prefix var-name)]
        (if (= (type var) Class)
          {:candidate var-name, :type :class,
           :package (when-let [pkg (.getPackage ^Class var)]
                      ;; Some classes don't have a package
                      (.getName ^Package pkg))}

          (cond-> {:candidate (if scope
                                (str scope-name "/" var-name)
                                var-name)
                   :type (cond (:macro var-meta) :macro
                               arglists :function
                               :else :var)
                   :ns (str (or (:ns var-meta) ns))}
            (and arglists(:arglists *extra-metadata*))
            (assoc :arglists (apply list (map pr-str arglists)))

            (and doc (:doc *extra-metadata*))
            (assoc :doc (generate-docstring var-meta))))))))

(defn doc
  "Documentation function for this sources' completions."
  [symbol-str ns]
  (if (var-symbol? symbol-str)
    (when-let [var (ns-resolve ns (symbol symbol-str))]
      (when (meta var)
        (generate-docstring (meta var))))))

(defsource ::ns-mappings
  :candidates #'candidates
  :doc #'doc)
