(ns compliment.t-helpers
  (:require [midje.sweet :as midje]))

(defn candidate?
  "Returns a predicate that checks whether the given candidate map has the
  string `candidate`, and optionally the type `type`."
  ([name]
   #(= (:candidate %) name))
  ([name type]
   #(and (= (:candidate %) name)
         (= (:type %) type))))

(defn strip-tags
  "Returns a wrapper predicate that first removes tags from the list of
  candidates."
  [predicate]
  #(predicate (map :candidate %)))
