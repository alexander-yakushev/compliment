(ns compliment.sources.t-keywords
  (:require [clojure.test :refer :all]
            [compliment.sources.keywords :as src]
            [compliment.t-helpers :refer :all]
            [matcher-combinators.matchers :as mc]))

(deftest keywords-test
  (testing "keyword source completes keywords that were used at least once"
    (str :test-key-foo :test-key-bar :test-key-baz)
    (is? (mc/in-any-order [":test-key-foo" ":test-key-bar" ":test-key-baz"])
         (strip-tags (src/candidates ":test-key" *ns* nil))))

  (testing "keyword source completes keywords fuzzily"
    (str :my/foo-bar :my/foo-foo :my/foo-baz)
    (is? (mc/in-any-order [":my/foo-foo" ":my/foo-bar" ":my/foo-baz"])
         (strip-tags (src/candidates ":mfoo" *ns* nil)))

    (is? (mc/in-any-order [":my/foo-foo" ":my/foo-bar"  ":my/foo-baz"])
         (strip-tags (src/candidates ":my/foo" *ns* nil)))

    (is? (mc/in-any-order [":my/foo-bar" ":my/foo-baz"])
         (strip-tags (src/candidates ":my/foba" *ns* nil))))

  (testing "namespace-qualified keywords work too"
    (str ::foo ::bar ::baz)
    (is? (mc/in-any-order [":compliment.sources.t-keywords/bar"
                           ":compliment.sources.t-keywords/baz"])
         (strip-tags (src/candidates ":compliment.sources.t-keywords/b" *ns* nil))))

  (testing "namespace-qualified keywords can be completed in the same namespace"
    (str ::foo ::bar ::baz)
    (is? (mc/in-any-order ["::bar" "::baz"])
         (strip-tags (src/candidates "::ba" (find-ns 'compliment.sources.t-keywords) nil))))

  (testing "namespace-qualified keywords can be completed with an ns alias"
    (str :compliment.core/aliased-one :compliment.core/aliased-two)
    (in-ns 'compliment.sources.t-keywords)
    (require '[compliment.core :as core])

    (is? (mc/in-any-order ["::core/aliased-one" "::core/aliased-two"])
         (strip-tags (src/candidates "::core/ali" (find-ns 'compliment.sources.t-keywords) nil))))

  (testing "namespace aliases are completed when double colon"
    (is? ["::src"]
         (strip-tags (src/candidates "::s" (find-ns 'compliment.sources.t-keywords) nil))))

  (testing "keyword candidates have a special tag"
    (str :it-is-deprecated)
    (is? [{:candidate ":it-is-deprecated" :type :keyword}]
         (src/candidates ":it-is" *ns* nil)))

  (testing "namespace aliases without namespace are handled"
    (is? nil (src/candidates "::/" *ns* nil))))
