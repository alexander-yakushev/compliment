(ns compliment.sources.t-keywords
  (:require [midje.sweet :refer :all]
            [compliment.sources.keywords :as src]))

(facts "about keywords"
  (fact "keyword source completes keywords that were used at least once"
    (do (str :t-key-foo :t-key-bar :t-key-baz)
        (src/candidates ":t-key" *ns* nil))
    => (contains #{":t-key-foo" ":t-key-bar" ":t-key-baz"} :gaps-ok))

  (fact "namespace-qualified keywords work too"
    (do (str ::foo ::bar ::baz)
        (src/candidates ":compliment.sources.t-keywords" *ns* nil))
    => (contains #{":compliment.sources.t-keywords/bar"
                   ":compliment.sources.t-keywords/baz"
                   ":compliment.sources.t-keywords/foo"}
                 :gaps-ok)))
