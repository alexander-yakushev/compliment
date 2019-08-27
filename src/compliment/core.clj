;; ## Compliment - a completion library you deserve.
;; This library provides a fast and extensible way to complete symbols in your
;; editor. It is intended to be maximally editor-agnostic where
;; possible, to avoid duplicating implementation in different clients.

(ns compliment.core
  "Core namespace. Most interactions with Compliment should happen
  through functions defined here."
  (:require (compliment.sources ns-mappings
                                namespaces-and-classes
                                class-members
                                keywords
                                special-forms
                                local-bindings
                                resources)
            [compliment.sources :refer [all-sources]]
            [compliment.context :refer [cache-context]]
            [compliment.utils :refer [*extra-metadata*]]
            [clojure.string :refer [join]])
  (:import java.util.Comparator))

(def all-files
  "List of all Compliment files in an order they should be loaded. This is
  required by REPLy."
  (map (partial format "compliment/%s.clj")
       ["utils" "context" "sources" "sources/class_members"
        "sources/ns_mappings" "sources/namespaces_and_classes"
        "sources/keywords" "sources/special_forms" "sources/local_bindings"
        "sources/resources"
        "core"]))

(def ^:private by-length-comparator
  (reify Comparator
    (compare [_ s1 s2]
      (let [res (compare (count s1) (count s2))]
        (if (zero? res)
          (compare s1 s2)
          res)))))

(defn sort-by-length
  "Sorts list of strings by their length first, and then alphabetically if
  length is equal. Works for tagged and non-tagged results."
  [candidates]
  (sort-by :candidate by-length-comparator candidates))

(defn ensure-ns
  "Takes either a namespace object or a symbol and returns the corresponding
  namespace if it exists, otherwise returns `user` namespace."
  [nspc]
  (cond (instance? clojure.lang.Namespace nspc) nspc
        (symbol? nspc) (or (find-ns nspc) (find-ns 'user) *ns*)
        :else *ns*))

(defn completions
  "Returns a list of completions for the given prefix. Options map can contain
  the following options:
  - :ns - namespace where completion is initiated;
  - :context - code form around the prefix;
  - :sort-order (either :by-length or :by-name);
  - :plain-candidates - if true, returns plain strings instead of maps;
  - :extra-metadata - set of extra fields to add to the maps;
  - :sources - list of source keywords to use."
  ([prefix]
   (completions prefix {}))
  ([prefix options-map]
   (if (string? options-map)
     (completions prefix {:context options-map})
     (let [{:keys [context sort-order sources extra-metadata]
            :or {sort-order :by-length}} options-map
           nspc (ensure-ns (:ns options-map))
           options-map (assoc options-map :ns nspc)
           ctx (cache-context context)]
       (binding [*extra-metadata* extra-metadata]
         (let [candidate-fns (keep (fn [[_ src]]
                                     (when (:enabled src)
                                       (:candidates src)))
                                   (if sources
                                     (all-sources sources)
                                     (all-sources)))
               candidates (mapcat
                            (fn [f] (f prefix nspc ctx))
                            candidate-fns)
               sorted-cands (if (= sort-order :by-name)
                              (sort-by
                                :candidate
                                candidates)
                              (sort-by
                                :candidate by-length-comparator
                                candidates))
               cands (if (:plain-candidates options-map)
                       (map :candidate sorted-cands)
                       sorted-cands)]
           (doall cands)))))))

(defn documentation
  "Returns a documentation string that describes the given symbol."
  ([symbol-str]
   (documentation symbol-str *ns*))
  ([symbol-str ns]
   (if (empty? symbol-str)
     ""
     (->> (for [[_ {:keys [doc enabled]}] (all-sources)
                :when enabled
                :let [docstr (doc symbol-str (ensure-ns ns))]
                :when docstr]
            docstr)
          (interpose "\n\n")
          join))))

(def clj-sources
  "Source keywords for Clojure completions."
  [:compliment.sources.ns-mappings/ns-mappings
   :compliment.sources.namespaces-and-classes/namespaces-and-classes
   :compliment.sources.class-members/static-members
   :compliment.sources.keywords/keywords
   :compliment.sources.special-forms/literals
   :compliment.sources.local-bindings/local-bindings
   :compliment.sources.resources/resources])

(def cljs-sources
  "Source keywords for ClojureScript completions."
  [:compliment.sources.cljs/cljs-completions])
