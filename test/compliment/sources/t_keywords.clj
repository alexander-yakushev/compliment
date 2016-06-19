(ns compliment.sources.t-keywords
  (:require [midje.sweet :refer :all]
            [compliment.sources.keywords :as src]
            [compliment.t-helpers :refer :all]))

(facts "about keywords"
  (fact "keyword source completes keywords that were used at least once"
    (do (str :t-key-foo :t-key-bar :t-key-baz)
        (src/candidates ":t-key" *ns* nil))
    => (strip-tags (contains #{":t-key-foo" ":t-key-bar" ":t-key-baz"} :gaps-ok)))

  (fact "namespace-qualified keywords work too"
    (do (str ::foo ::bar ::baz)
        (src/candidates ":compliment.sources.t-keywords/b" *ns* nil))
    => (strip-tags (just #{":compliment.sources.t-keywords/bar"
                           ":compliment.sources.t-keywords/baz"})))

  (fact "namespace-qualified keywords can be completed in the same namespace"
    (do (str ::foo ::bar ::baz)
        (src/candidates "::ba" *ns* nil))
    => (strip-tags (just #{"::bar" "::baz"})))

  (fact "namespace-qualified keywords can be completed with an ns alias"
    (do (str :compliment.core/aliased-one :compliment.core/aliased-two)
        (require '[compliment.core :as core])
        (src/candidates "::core/ali" *ns* nil))
    => (strip-tags (just #{"::core/aliased-one" "::core/aliased-two"})))

  (fact "namespace aliases are completed when double colon"
    (src/candidates "::s" *ns* nil)
    => (strip-tags (just #{"::src"})))

  (fact "keyword candidates have a special tag"
    (do (str :deprecated)
        (src/candidates ":depr" *ns* nil))
    => [{:candidate ":deprecated", :type :keyword}]))
