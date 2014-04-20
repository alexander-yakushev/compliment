(ns compliment.sources.t-class-members
  (:require [midje.sweet :refer :all]
            [compliment.sources.class-members :as src]
            [compliment.context :as ctx]))

(facts "about class members"
  (fact "fuzzy matching class members works with camelCase as separator"
    (src/camel-case-matches? ".getDeF" ".getDeclaredFields") => truthy
    (src/camel-case-matches? ".gT" ".getTextSize")           => truthy))

(facts "about class member completion"
  (fact "candidates are taken from all non-static members of classes
  imported into the current namespace"
    (src/members-candidates ".eq" *ns* nil)
    => (just #{".equals" ".equalsIgnoreCase"})

    (src/members-candidates ".getDeclF" *ns* nil)
    => (just #{".getDeclaredFields" ".getDeclaredField"})

    (src/members-candidates ".pu" *ns* nil)
    => () ; Because java.util.HashMap is not imported into current ns

    (import 'java.util.HashMap)
    (src/members-candidates ".pu" *ns* nil)
    => (just #{".put" ".putAll"}))

  (fact "if context is provided and the class of first arg can be
  resolved, select candidates only for that class (works only for vars)"
    (src/members-candidates ".st" *ns* nil)
    => (just #{".start" ".startsWith" ".stop" ".stripTrailingZeros"})

    (def a-str "a string")
    (src/members-candidates ".st" *ns* (ctx/parse-context '(__prefix__ a-str)))
    => [".startsWith"]))

(facts "about static members"
  (fact "static members can be matched by camelCase too"
    (src/camel-case-matches? "Thread/actC" "Thread/activeCount") => truthy)

  (fact "static members candidates are taken for the class in prefix"
    (src/static-members-candidates "String/" *ns* nil)
    => (just #{"String/CASE_INSENSITIVE_ORDER" "String/copyValueOf"
               "String/format" "String/valueOf"})

    ;; Don't have to import class to get static members for it.
    (src/static-members-candidates "java.io.File/sep" *ns* nil)
    => (just #{"java.io.File/separator" "java.io.File/separatorChar"})))
