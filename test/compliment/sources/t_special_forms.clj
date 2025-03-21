(ns compliment.sources.t-special-forms
  (:require [clojure.test :refer :all]
            [compliment.context :as ctx]
            [compliment.sources.special-forms :as src]
            [compliment.t-helpers :refer :all]
            [matcher-combinators.matchers :as mc]))

(deftest special-forms-test
  (testing "special forms are matched when they are first item in the list"
    (is? [{:candidate "var", :type :special-form}]
         (src/candidates "va" *ns* (ctx/parse-context '(__prefix__ foo))))

    (is? ["catch"]
         (strip-tags (src/candidates "c" *ns* (ctx/parse-context '(__prefix__ Exception ex nil)))))

    (is? (mc/in-any-order ["monitor-enter" "monitor-exit"])
         (strip-tags (src/candidates "mo-e" *ns* (ctx/parse-context '(__prefix__)))))

    (is? nil (src/candidates "" *ns* (ctx/parse-context '(str __prefix__ 42))))

    (is? nil (src/candidates "" *ns* (ctx/parse-context '[__prefix__ 42]))))

  (testing "literals are completed"
    (is? [{:candidate "true", :type :special-form}] (src/literal-candidates "tr" *ns* nil))
    (is? [{:candidate "false", :type :special-form}] (src/literal-candidates "f" *ns* nil))
    (is? [{:candidate "nil", :type :special-form}] (src/literal-candidates "n"  *ns* nil)))

  (testing "there are docs for special forms too"
    (is? string? (src/doc "try" *ns*))
    (is? nil (src/doc "not-a-form" *ns*))))
