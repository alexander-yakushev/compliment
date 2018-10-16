(ns compliment.t-helpers)

(defn strip-tags
  "Returns a wrapper predicate that first removes tags from the list of
  candidates."
  [completions]
  (map :candidate completions))
