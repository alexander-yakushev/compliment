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
                                special-forms))
  (:use [compliment.sources :only [all-sources]]
        [compliment.context :only [cache-context]]
        [clojure.string :only [join]])
  (:import java.util.Comparator))

(defn sort-by-length
  "Sorts list of strings by their length first, and then
  alphabetically if length is equal."
  [candidates]
  (sort (reify Comparator
          (compare [_ s1 s2]
            (let [res (compare (count s1) (count s2))]
              (if (zero? res)
                (compare s1 s2)
                res))))
        candidates))

(defn completions
  "Returns a list of completions for the given prefix. Optional
context (can be nil) should be a Lisp form from where the completion
was initiated, having prefix replaced with `__prefix__` symbol."
  ([prefix context-str]
     (completions prefix *ns* context-str))
  ([prefix ns context-str]
     (let [ctx (cache-context context-str)]
       (-> (for [[_ {:keys [candidates enabled]}] (all-sources)
                 :when enabled
                 :let [cands (candidates prefix ns ctx)]
                 :when cands]
             cands)
           flatten
           sort-by-length))))

(defn documentation
  "Returns a documentation string that describes the given symbol."
  ([symbol-str]
     (documentation symbol-str *ns*))
  ([symbol-str ns]
     (->> (for [[_ {:keys [doc enabled]}] (all-sources)
                :when enabled
                :let [docstr (doc symbol-str ns)]
                :when docstr]
            docstr)
          (interpose "\n\n")
          join)))
