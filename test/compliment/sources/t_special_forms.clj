(ns compliment.sources.t-special-forms
  (:require [fudje.sweet :refer :all]
            [clojure.test :refer :all]
            [compliment.context :as ctx]
            [compliment.sources.special-forms :as src]
            [compliment.t-helpers :refer :all]))

(deftest special-forms-test
  (fact "special forms are matched when they are first item in the list"
    (src/candidates "va" *ns* (ctx/parse-context '(__prefix__ foo)))
    => [{:candidate "var", :type :special-form}]

    (strip-tags (src/candidates "c" *ns* (ctx/parse-context '(__prefix__ Exception ex nil))))
    => (just ["catch"])

    (strip-tags (src/candidates "mo-e" *ns* (ctx/parse-context '(__prefix__))))
    => (just ["monitor-enter" "monitor-exit"] :in-any-order)

    (src/candidates "" *ns* (ctx/parse-context '(str __prefix__ 42)))
    => nil

    (src/candidates "" *ns* (ctx/parse-context '[__prefix__ 42]))
    => nil)

  (fact "literals are completed"
    (src/literal-candidates "tr" *ns* nil) => [{:candidate "true", :type :special-form}]
    (src/literal-candidates "f" *ns* nil) => [{:candidate "false", :type :special-form}]
    (src/literal-candidates "n"  *ns* nil) => [{:candidate "nil", :type :special-form}])

  (fact "there are docs for special forms too"
    (src/doc "try" *ns*) => (checker string?)
    (src/doc "not-a-form" *ns*) => nil))
