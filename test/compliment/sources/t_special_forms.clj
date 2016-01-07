(ns compliment.sources.t-special-forms
  (:require [midje.sweet :refer :all]
            [compliment.context :as ctx]
            [compliment.sources.special-forms :as src]))

(facts "about special forms"
  (fact "special forms are matched when they are first item in the list"
    (src/candidates "va" *ns* (ctx/parse-context '(__prefix__ foo)))
    => ["var"]

    (src/candidates "c" *ns* (ctx/parse-context '(__prefix__ Exception ex nil)))
    => ["catch"]

    (src/candidates "mo-e" *ns* (ctx/parse-context '(__prefix__)))
    => ["monitor-enter" "monitor-exit"]

    (src/candidates "" *ns* (ctx/parse-context '(str __prefix__ 42)))
    => nil

    (src/candidates "" *ns* (ctx/parse-context '[__prefix__ 42]))
    => nil)

  (fact "literals are completed"
    (src/literal-candidates "tr" *ns* nil) => ["true"]
    (src/literal-candidates "f" *ns* nil) => ["false"]
    (src/literal-candidates "n"  *ns* nil) => ["nil"])

  (fact "there are docs for special forms too"
    (src/doc "try" *ns*) => string?
    (src/doc "not-a-form" *ns*) => nil))
