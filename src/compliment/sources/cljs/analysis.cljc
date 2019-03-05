(ns compliment.sources.cljs.analysis
  (:require [clojure.string :as str]
            [compliment.utils :as utils])
  (:refer-clojure :exclude [find-ns all-ns ns-aliases]))

(def NSES :cljs.analyzer/namespaces)

(defn all-ns
  [env]
  (->> (NSES env)
       ;; recent CLJS versions include data about macro namespaces in the
       ;; compiler env, but we should not include them in completions or pass
       ;; them to format-ns unless they're actually required
       (into {} (filter (fn [[_ ns]]
                          (not (and (contains? ns :macros)
                                    (= 1 (count ns)))))))))

(defn find-ns
  [env ns]
  (get (all-ns env) ns))

(defn add-ns-macros
  "Append $macros to the input symbol"
  [sym]
  (some-> sym
          (str "$macros")
          symbol))

(defn remove-macros
  "Remove $macros from the input symbol"
  [sym]
  (some-> sym
          str
          (str/replace #"\$macros" "")
          symbol))

;; Code adapted from clojure-complete (http://github.com/ninjudd/clojure-complete)

(defn imports
  "Returns a map of [import-name] to [ns-qualified-import-name] for all imports
  in the given namespace."
  [env ns]
  (:imports (find-ns env ns)))

(defn ns-aliases
  "Returns a map {ns-name-or-alias ns-name} for the given namespace."
  [env ns]
  (when-let [found (find-ns env ns)]
    (let [imports (:imports found)]
      (->> (:requires found)
           (filter #(not (contains? imports (key %))))
           (into {})))))

(defn macro-ns-aliases
  "Returns a map of [macro-ns-name-or-alias] to [macro-ns-name] for the given namespace."
  [env ns]
  (:require-macros (find-ns env ns)))

(defn- expand-refer-map
  [m]
  (into {} (for [[k v] m] [k (symbol (str v "/" k))])))

(defn referred-vars
  "Returns a map of [var-name] to [ns-qualified-var-name] for all referred vars
  in the given namespace."
  [env ns]
  (->> (find-ns env ns)
       :uses
       expand-refer-map))

(defn referred-macros
  "Returns a map of [macro-name] to [ns-qualified-macro-name] for all referred
  macros in the given namespace."
  [env ns]
  (->> (find-ns env ns)
       :use-macros
       expand-refer-map))

(defn ns-alias
  "If sym is an alias to, or the name of, a namespace referred to in ns, returns
  the name of the namespace; else returns nil."
  [env sym ns]
  (get (ns-aliases env ns) (utils/as-sym sym)))

(defn macro-ns-alias
  "If sym is an alias to, or the name of, a macro namespace referred to in ns,
  returns the name of the macro namespace; else returns nil."
  [env sym ns]
  (get (macro-ns-aliases env ns) (utils/as-sym sym)))

(defn- public?
  [[_ var]]
  (not (:private var)))

(defn- named?
  [[_ var]]
  (not (:anonymous var)))

(defn- foreign-protocol?
  [[_ var]]
  (and (:impls var)
       (not (:protocol-symbol var))))

(defn- macro?
  [[_ var]]
  (:macro (meta var)))

(defn ns-vars
  "Returns a list of the vars declared in the ns."
  [env ns]
  (->> (find-ns env ns)
       :defs
       (filter (every-pred named? (complement foreign-protocol?)))
       (into {})))

(defn public-vars
  "Returns a list of the public vars declared in the ns."
  [env ns]
  (->> (find-ns env ns)
       :defs
       (filter (every-pred named? public? (complement foreign-protocol?)))
       (into {})))

(defn public-macros
  "Given a namespace return all the public var analysis maps. Analagous to
  clojure.core/ns-publics but returns var analysis maps not vars.

  Inspired by the ns-publics in cljs.analyzer.api."
  [env ns]
  {:pre [(symbol? ns)]}
  #?(:clj (when (and ns (clojure.core/find-ns ns))
            (->> (ns-publics ns)
                 (filter macro?)
                 (into {})))
     :cljs (->> (merge
                 (get-in env [NSES ns :macros])
                 (get-in env [NSES ns :defs]))
                (remove (fn [[k v]] (:private v)))
                (into {}))))

(defn core-vars
  "Returns a list of cljs.core vars visible to the ns."
  [env ns]
  (let [vars (public-vars env 'cljs.core)
        excludes (:excludes (find-ns env ns))]
    (apply dissoc vars excludes)))

(defn core-macros
  "Returns a list of cljs.core macros visible to the ns."
  [env ns]
  (let [macros (public-macros env #?(:clj 'cljs.core :cljs 'cljs.core$macros))
        excludes (:excludes (find-ns env ns))]
    (apply dissoc macros excludes)))

(def ^:private language-keywords
  #{:require :require-macros :import
    :refer :refer-macros :include-macros
    :refer-clojure :exclude
    :keys :strs :syms
    :as :or
    :pre :post
    :let :when :while

    ;; reader conditionals
    :clj :cljs :default

    ;; common meta keywords
    :private :tag :static
    :doc :author :arglists
    :added :const

    ;; spec keywords
    :req :req-un :opt :opt-un
    :args :ret :fn

    ;; misc
    :keywordize-keys :else :gen-class})

(defn keyword-constants
  "Returns a list of both keyword constants in the environment and
  language specific ones."
  [env]
  ;; using namespace for backward compatibility with Clojure 1.8
  ;; use qualified-keyword? at some point in the future
  (concat language-keywords (filter namespace (keys (:cljs.analyzer/constant-table env)))))

;; grabbing directly from cljs.analyzer.api

(defn ns-interns-from-env
  "Given a namespace return all the var analysis maps. Analagous to
  clojure.core/ns-interns but returns var analysis maps not vars.

  Directly from cljs.analyzer.api."
  [env ns]
  {:pre [(symbol? ns)]}
  (merge
   (get-in env [NSES ns :macros])
   (get-in env [NSES ns :defs])))

(defn sanitize-ns
  "Add :ns from :name if missing."
  [m]
  (-> m
      (assoc :ns (or (:ns m) (:name m)))
      (update :ns utils/namespace-sym)
      (update :name utils/name-sym)))

(defn ns-obj?
  "Return true if n is a namespace object"
  [ns]
  (instance? #?(:clj clojure.lang.Namespace
                :cljs cljs.core/Namespace)
             ns))

(defn var-meta
  "Return meta for the var, we wrap it in order to support both JVM and
  self-host."
  [var]
  (cond-> {}
    (map? var) (merge var)
    (var? var) (-> (merge (meta var))
                   (update :ns #(cond-> % (ns-obj? %) ns-name)))
    true sanitize-ns
    #?@(:cljs [true (-> (update :ns remove-macros)
                        (update :name remove-macros))])))

(defn qualified-symbol-meta
  "Given a namespace-qualified var name, gets the analyzer metadata for
  that var."
  [env sym]
  {:pre [(symbol? sym)]}
  (let [ns (find-ns env (utils/namespace-sym sym))]
    (some-> (:defs ns)
            (get (utils/name-sym sym))
            var-meta)))

(defn ns-meta
  [ns]
  {:pre [(symbol? ns)]}
  (meta (clojure.core/find-ns ns)))

(defn macro-meta
  [env qualified-sym]
  #?(:clj (var-meta (find-var qualified-sym))
     :cljs (let [referred-ns (symbol (namespace qualified-sym))]
             (-> env
                 (ns-interns-from-env (add-ns-macros referred-ns))
                 (get refer)
                 var-meta))))
