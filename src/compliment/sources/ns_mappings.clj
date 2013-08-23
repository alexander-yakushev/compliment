(ns compliment.sources.ns-mappings
  "Completion for vars and classes in the current namespace."
  (:use [compliment.sources :only [defsource]]
        [compliment.utils :only [parts-match? split]])
  (:import java.io.StringWriter))

(defn var-symbol?
  "Test if prefix resembles a bar name."
  [x]
  (re-matches #"[^\.\/\:]+([^\/\:]+\/[^\.\/\:]*)?" x))

(defn dash-matches?
  "Tests if prefix partially matches a var name with dashes as
  separators."
  [^String prefix, ^String var]
  (parts-match? (split prefix #"-" true) (split var #"-")))

(defn get-scope-and-prefix
  "Tries to get take apart scope namespace and prefix in prefixes like
  `scope/var`."
  [^String s, ns]
  (let [[scope-name sym] (if (> (.indexOf s "/") -1)
                           (.split s "/") ())
        scope (when scope-name
                (or (find-ns (symbol scope-name))
                    ((ns-aliases ns) (symbol scope-name))))
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

(defn candidates
  "Returns a list of namespace-bound candidates, with namespace being
  either the scope (if prefix is scoped), `ns` arg or the namespace
  extracted from context if inside `ns` declaration."
  [^String prefix
   ns context]
  (if (var-symbol? prefix)
    (let [[scope-name scope ^String prefix] (get-scope-and-prefix prefix ns)
          ns-form-namespace (try-get-ns-from-context context)
          vars (cond
                scope (ns-publics scope)
                ns-form-namespace (ns-publics ns-form-namespace)
                :else (ns-map ns))
          has-dash (> (.indexOf prefix "-") -1)]
      (for [[var _] vars
            :let [^String var-name (name var)]
            :when (if has-dash
                    (dash-matches? prefix var-name)
                    (.startsWith var-name prefix))]
        (if scope
          (str scope-name "/" var-name)
          var-name)))))

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

(defn doc
  "Documentation function for this sources' completions."
  [symbol-str ns]
  (if (var-symbol? symbol-str)
    (when-let [var (ns-resolve ns (symbol symbol-str))]
      (generate-docstring (meta var)))))

(defsource ::ns-mappings
  :candidates #'candidates
  :doc #'doc)
