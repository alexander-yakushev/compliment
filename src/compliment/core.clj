;; ## Compliment - a completion library you deserve.
;; This library provides a fast and extensible way to complete symbols in your
;; editor. It is intended to be maximally editor-agnostic where
;; possible, to avoid duplicating implementation in different clients.

(ns compliment.core
  "Core namespace. Most interactions with Compliment should happen
  through functions defined here."
  (:require (compliment.sources vars
                                namespaces
                                classes
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

(def ^:private by-length-comparator
  "Sorts list of strings by their length first, and then alphabetically if length
  is equal."
  (reify Comparator
    (compare [_ s1 s2]
      (let [res (Integer/compare (.length ^String s1) (.length ^String s2))]
        (if (zero? res)
          (.compareTo ^String s1 s2)
          res)))))

(defn ensure-ns
  "Takes either a namespace object or a symbol and returns the corresponding
  namespace if it exists, otherwise returns `user` namespace."
  [nspc]
  (cond (instance? clojure.lang.Namespace nspc) nspc
        (symbol? nspc) (or (find-ns nspc) (find-ns 'user) *ns*)
        :else *ns*))

(defn completions
  "Returns a list of completions for the given prefix.

  Options map can contain the following options:
   - :ns - namespace where completion is initiated;
   - :context - code form around the prefix;
   - :sort-order (either :by-length or :by-name);
   - :plain-candidates - if true, returns plain strings instead of maps;
   - :extra-metadata - set of extra fields to add to the maps;
   - :sources - list of source keywords to use."
  ([prefix]
   (completions prefix {}))
  ([prefix {:keys [ns context sort-order sources extra-metadata plain-candidates]
            :or {sort-order :by-length}}]
   (let [nspc (ensure-ns ns)
         ctx (binding [*ns* nspc]
               (cache-context context))]
     (binding [*extra-metadata* extra-metadata]
       (let [candidate-fns (keep (fn [[_ src]]
                                   (when (:enabled src)
                                     (:candidates src)))
                                 (if sources
                                   (all-sources sources)
                                   (all-sources)))
             candidates (mapcat (fn [f] (f prefix nspc ctx)) candidate-fns)
             sorted-cands (if (= sort-order :by-name)
                            (sort-by :candidate candidates)
                            (sort-by :candidate by-length-comparator candidates))
             cands (if plain-candidates
                     (map :candidate sorted-cands)
                     sorted-cands)]
         (doall cands))))))

(defn documentation
  "Returns a documentation string that describes the given symbol.

  Options map can contain the following options:
   - :sources - list of source keywords to use."
  ([symbol-str]
   (documentation symbol-str *ns*))
  ([symbol-str ns]
   (documentation symbol-str *ns* nil))
  ([symbol-str ns options-map]
   (let [{:keys [sources]} options-map]
     (if (empty? symbol-str)
       ""
       (->> (for [[_ {:keys [doc enabled]}] (if sources
                                              (all-sources sources)
                                              (all-sources))
                  :when enabled
                  :let [docstr (doc symbol-str (ensure-ns ns))]
                  :when docstr]
              docstr)
            (interpose "\n\n")
            join)))))
