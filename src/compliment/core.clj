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
                                local-bindings)
            [defprecated.core :as depr])
  (:use [compliment.sources :only [all-sources]]
        [compliment.context :only [cache-context]]
        [clojure.string :only [join]])
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
  [tag? candidates]
  (if tag?
    (sort-by :candidate by-length-comparator candidates)
    (sort by-length-comparator candidates)))

(defn ensure-ns
  "Takes either a namespace object or a symbol and returns the corresponding
  namespace if it exists, otherwise returns `user` namespace."
  [ns]
  (cond (instance? clojure.lang.Namespace ns) ns
        (symbol? ns) (or (find-ns ns) (find-ns 'user))
        :else (find-ns 'user)))

(defn- tag-candidates
  "Iterate over list of string candidates and return maps with each candidate
  having a type and possibly other metadata."
  [candidates tag-fn ns]
  (for [c candidates
        :let [cand-map {:candidate c}]]
    (if tag-fn
      (try (tag-fn cand-map ns)
           (catch Exception ex cand-map))
      cand-map)))

(depr/defn completions
  "Returns a list of completions for the given prefix. Options map can contain
  the following options:
  - :ns - namespace where completion is initiated;
  - :context - code form around the prefix;
  - :sort-order (either :by-length or :by-name);
  - :tag-candidates - if true, returns maps with extra data instead of strings;
  - :sources - list of source keywords to use."
  ([prefix]
   (completions prefix {}))
  ([prefix options-map]
   (if (string? options-map)
     (completions prefix {:context options-map})
     (let [{:keys [ns context sort-order sources]
            :or {sort-order :by-length}} options-map
            ns (ensure-ns (or ns *ns*))
            tag? (:tag-candidates options-map)
            ctx (cache-context context)
            sort-fn (if (= sort-order :by-name)
                      (if tag?
                        sort (partial sort-by :candidate))
                      (partial sort-by-length tag?))]
       (-> (for [[_ {:keys [candidates enabled tag-fn]}] (if sources
                                                           (all-sources sources)
                                                           (all-sources))
                 :when enabled
                 :let [cands (cond-> (candidates prefix ns ctx)
                                     tag? (tag-candidates tag-fn ns))]
                 :when cands]
             cands)
           flatten
           sort-fn))))
  (^:deprecated
   [prefix ns context-str]
   (completions prefix {:ns ns, :context context-str}))
  (^:deprecated
   [prefix ns context-str sort-order]
   (completions prefix {:ns ns, :context context-str, :sort-order sort-order})))

(defn documentation
  "Returns a documentation string that describes the given symbol."
  ([symbol-str]
   (documentation symbol-str *ns*))
  ([symbol-str ns]
   (->> (for [[_ {:keys [doc enabled]}] (all-sources)
              :when enabled
              :let [docstr (doc symbol-str (ensure-ns ns))]
              :when docstr]
          docstr)
        (interpose "\n\n")
        join)))
