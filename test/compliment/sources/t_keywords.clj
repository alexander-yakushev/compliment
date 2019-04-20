(ns compliment.sources.t-keywords
  (:require [fudje.sweet :refer :all]
            [clojure.test :refer :all]
            [compliment.sources.keywords :as src]
            [compliment.t-helpers :refer :all]))

(deftest keywords-test
  (fact "keyword source completes keywords that were used at least once"
    (do (str :t-key-foo :t-key-bar :t-key-baz)
        (strip-tags (src/candidates ":t-key" *ns* nil)))
    => (contains #{":t-key-foo" ":t-key-bar" ":t-key-baz"} :gaps-ok))

  (fact "namespace-qualified keywords work too"
    (do (str ::foo ::bar ::baz)
        (strip-tags (src/candidates ":compliment.sources.t-keywords/b" *ns* nil)))
    => (just [":compliment.sources.t-keywords/bar"
              ":compliment.sources.t-keywords/baz"] :in-any-order))

  (fact "namespace-qualified keywords can be completed in the same namespace"
    (do (str ::foo ::bar ::baz)
        (strip-tags (src/candidates "::ba" (find-ns 'compliment.sources.t-keywords) nil)))
    => (just ["::bar" "::baz"] :in-any-order))

  (fact "namespace-qualified keywords can be completed with an ns alias"
    (do (str :compliment.core/aliased-one :compliment.core/aliased-two)
        (in-ns 'compliment.sources.t-keywords)
        (require '[compliment.core :as core])
        (strip-tags (src/candidates "::core/ali" (find-ns 'compliment.sources.t-keywords) nil)))
    => (just ["::core/aliased-one" "::core/aliased-two"] :in-any-order))

  (fact "namespace aliases are completed when double colon"
    (strip-tags (src/candidates "::s" (find-ns 'compliment.sources.t-keywords) nil))
    => (just ["::src"]))

  (fact "keyword candidates have a special tag"
    (do (str :my-deprecated)
        (src/candidates ":my" *ns* nil))
    => (just [{:candidate ":my-deprecated" :type :keyword}]))

  (fact "namespace aliases without namespace are handled"
    (src/candidates "::/" *ns* nil)
    => nil))
