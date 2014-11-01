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
                                local-bindings))
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
        "core"]))

(def ^:private by-length-comparator
  (reify Comparator
    (compare [_ s1 s2]
      (let [res (compare (count s1) (count s2))]
        (if (zero? res)
          (compare s1 s2)
          res)))))

(defn sort-by-length
  "Sorts list of strings by their length first, and then
  alphabetically if length is equal."
  [candidates]
  (sort by-length-comparator candidates))

(defn ensure-ns
  "Takes either a namespace object or a symbol and returns the corresponding
  namespace if it exists, otherwise returns `user` namespace."
  [ns]
  (cond (instance? clojure.lang.Namespace ns) ns
        (symbol? ns) (or (find-ns ns) (find-ns 'user))
        :else (find-ns 'user)))

(defn completions
  "Returns a list of completions for the given prefix. Optional context (can be
nil) should be a string with Lisp form from where the completion was initiated,
having prefix replaced with `__prefix__` symbol. Optional sort-order can be
either :by-length or :by-name."
  ([prefix context-str]
     (completions prefix *ns* context-str :by-length))
  ([prefix ns context-str]
     (completions prefix ns context-str :by-length))
  ([prefix ns context-str sort-order]
     (let [ctx (cache-context context-str)
           sort-fn (if (= sort-order :by-name)
                     sort sort-by-length)]
       (-> (for [[_ {:keys [candidates enabled]}] (all-sources)
                 :when enabled
                 :let [cands (candidates prefix (ensure-ns ns) ctx)]
                 :when cands]
             cands)
           flatten
           sort-fn))))

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
