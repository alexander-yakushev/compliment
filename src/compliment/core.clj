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

(defn ensure-ns
  "Takes either a namespace object or a symbol and returns the corresponding
  namespace if it exists, otherwise returns `user` namespace."
  [nspc]
  (or (and (instance? clojure.lang.Namespace nspc) nspc)
      (and (symbol? nspc) (find-ns nspc))
      (find-ns 'user)
      ;; In an environment that for some reason lacks 'user namespace, commit to
      ;; return any valid Namespace object.
      *ns*))

(defn completions
  "Returns a list of completions for the given prefix.

  Options map can contain the following options:
   - :ns - namespace where completion is initiated;
   - :context - code form around the prefix;
   - :sort-order (either :by-length or :by-name);
   - :extra-metadata - set of extra fields to add to the maps;
   - :sources - list of source keywords to use."
  ([prefix]
   (completions prefix {}))
  ([prefix ^{:lite {:keys [ns context sources]}}
    {:keys [ns context sort-order sources extra-metadata]
     :or {sort-order :by-length}}]
   (let [nspc (ensure-ns ns)
         ctx ^{:lite nil} (binding [*ns* nspc]
                            (cache-context context))]
     (binding ^{:lite []} [*extra-metadata* extra-metadata]
       (let [candidate-fns (keep (fn [[_ src]]
                                   (when (:enabled src)
                                     (:candidates src)))
                                 (if sources
                                   (all-sources sources)
                                   (all-sources)))
             candidates (into [] (comp (map (fn [f] (f prefix nspc ctx))) cat) candidate-fns)]
         ^{:lite '(sort-by :candidate candidates)}
         (compliment.sources/sort-candidates candidates sort-order))))))

^{:lite nil}
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
