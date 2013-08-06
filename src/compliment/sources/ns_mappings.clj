(ns compliment.sources.ns-mappings
  (:use [compliment.sources :only [defsource]]
        [compliment.utils :only [parts-match?]]
        [clojure.string :only [split]])
  (:import java.io.StringWriter))

(defn var-symbol? [x]
  (re-matches #"[^\.\/\:]+([^\/\:]+\/[^\.\/\:]*)?" x))

(defn dash-matches? [^String prefix, ^String var]
  (parts-match? (split prefix #"-") (split var #"-")))

(defn get-scope-and-prefix [s ns]
  (let [[scope-name sym] (if (> (.indexOf s "/") -1)
                           (.split s "/") ())
        scope (when scope-name
                (or (find-ns (symbol scope-name))
                    ((ns-aliases ns) (symbol scope-name))))
        prefix (if scope
                 (or sym "") s)]
    [scope prefix]))

(defn try-get-ns-from-context [context]
  (let [[var-list ns-def use-def] context]
    (when (and (sequential? (:form var-list))
               (= (second (:form ns-def)) :only)
               (= (first (:form use-def)) :use)
               (= (first (:form (last context))) 'ns))
      (find-ns (first (:form ns-def))))))

(defn candidates [^String prefix ns context]
  (if (var-symbol? prefix)
    (let [[scope prefix] (get-scope-and-prefix prefix ns)
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
          (str scope "/" var-name)
          var-name)))))

(defn generate-docstring [m]
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

(defn doc [symbol-str ns]
  (if (var-symbol? symbol-str)
    (when-let [var (ns-resolve ns (symbol symbol-str))]
      (generate-docstring (meta var)))))

(defsource ::ns-mappings
  :candidates #'candidates
  :doc #'doc)
