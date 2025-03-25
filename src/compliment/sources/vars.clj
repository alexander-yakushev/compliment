(ns compliment.sources.vars
  "Completion for vars and classes in the current namespace."
  (:require [compliment.sources :refer [defsource]]
            [compliment.utils :refer [fuzzy-matches? resolve-namespace
                                      *extra-metadata* split-by-leading-literals]])
  (:import java.io.StringWriter))

(def ^:private base-priority 30)

(defn var-symbol?
  "Test if prefix resembles a var name."
  [x]
  (re-matches #"(?:([^\/\:][^\/]*)\/)?(|[^/:][^/]*)" x))

(defn dash-matches?
  "Tests if prefix partially matches a var name with dashes as
  separators."
  [prefix var]
  (fuzzy-matches? prefix var \-))

^{:lite nil}
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
      (let [namespace (first (:form ns-def))]
        (try (require namespace) (catch Exception _))
        (find-ns namespace)))))

^{:lite nil}
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

(defn ^{:lite 'vars-candidates} candidates
  "Returns a list of namespace-bound candidates, with namespace being
  either the scope (if prefix is scoped), `ns` arg or the namespace
  extracted from context if inside `ns` declaration."
  [^String prefix, ns context]
  (let [[literals prefix] (split-by-leading-literals prefix)]
    (when-let [[_ scope-name prefix] (var-symbol? prefix)]
      (let [scope (some-> scope-name symbol (resolve-namespace ns))
            ns-form-namespace ^{:lite '(get {} :nil)} (try-get-ns-from-context context)
            vars (cond
                   scope (if (and literals (re-find #"#'$" literals))
                           ;; If prefixed with #', suggest private vars too.
                           (ns-interns scope)
                           (ns-publics scope))
                   (and scope-name (nil? scope)) ()
                   ns-form-namespace (ns-publics ns-form-namespace)
                   :else (ns-map ns))]
        (for [[var-sym var] vars
              :let [var-name (name var-sym)]
              :when (and (var? var) (dash-matches? prefix var-name))
              :let [{:keys [arglists doc private deprecated] :as var-meta} (meta var)]
              :when (not (:completion/hidden var-meta))
              :let [var-ns (.ns ^clojure.lang.Var var)
                    this-ns? (= var-ns ns)
                    clojure-ns? (some-> var-ns ns-name name (.startsWith "clojure"))]]
          (cond-> {:candidate (str literals
                                   (if scope
                                     (str scope-name "/" var-name)
                                     var-name))
                   :type (cond (:macro var-meta) :macro
                               arglists :function
                               :else :var)
                   :ns (str (or (:ns var-meta) ns))
                   ;; Priority rule: vars from requested ns, then from Clojure
                   ;; namespaces, then the rest.
                   :priority (+ base-priority
                                (cond this-ns? 0, clojure-ns? 1, :else 2))}
            (and private (:private *extra-metadata*))
            (assoc :private (boolean private))

            (and deprecated (:deprecated *extra-metadata*))
            (assoc :deprecated (boolean deprecated))

            (and arglists (:arglists *extra-metadata*))
            (assoc :arglists (apply list (map pr-str arglists)))

            (and doc (:doc *extra-metadata*))
            (assoc :doc ^{:lite 'doc} (generate-docstring var-meta))))))))

^{:lite nil}
(defn- resolve-var [symbol-str ns]
  (let [strip-literals (comp second split-by-leading-literals)]
    (ns-resolve ns (symbol (strip-literals symbol-str)))))

^{:lite nil}
(defn doc
  "Documentation function for this sources' completions."
  [symbol-str ns]
  (when (var-symbol? symbol-str)
    (when-let [var (resolve-var symbol-str ns)]
      (when (meta var)
        (generate-docstring (meta var))))))

^{:lite '(defsource :compliment.lite/vars :candidates #'vars-candidates)}
(defsource ::vars
  :candidates #'candidates
  :doc #'doc)
