(ns compliment.sources.t-special-forms
  (:require [midje.sweet :refer :all]
            [compliment.sources.special-forms :as src]))

(facts "about special forms"
  (fact "special forms are matched everywhere and use dash as fuzzy separator"
    (src/candidates "va" *ns* nil)
    => ["var"]

    (src/candidates "c" *ns* nil)
    => ["catch"]

    (src/candidates "mo-e" *ns* nil)
    => ["monitor-enter" "monitor-exit"])

  (fact "there are docs for special forms too"
    (src/doc "try" *ns*) => string?
    (src/doc "not-a-form" => nil)))
