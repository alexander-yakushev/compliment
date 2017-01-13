(ns compliment.sources.t-class-members
  (:require [midje.sweet :refer :all]
            [compliment.sources.class-members :as src]
            [compliment.context :as ctx]
            [compliment.t-helpers :refer :all]))

(facts "about class members"
  (fact "fuzzy matching class members works with camelCase as separator"
    (src/camel-case-matches? ".getDeF" ".getDeclaredFields") => truthy
    (src/camel-case-matches? ".gT" ".getTextSize")           => truthy)

  (fact "candidates are taken from all non-static members of classes
  imported into the current namespace"
    (src/members-candidates ".eq" *ns* nil)
    => (strip-tags (just #{".equals" ".equalsIgnoreCase"}))

    (src/members-candidates ".getDeclF" *ns* nil)
    => (strip-tags (just #{".getDeclaredFields" ".getDeclaredField"}))

    (src/members-candidates ".pu" *ns* nil)
    => () ; Because java.util.HashMap is not imported into current ns

    (import 'java.util.HashMap)
    (src/members-candidates ".pu" *ns* nil)
    => (contains #{{:candidate ".put", :type :method}
                   {:candidate ".putAll", :type :method}} :gaps-ok))

  (fact "if context is provided and the class of first arg can be
  resolved, select candidates only for that class (works only for vars)"
    (src/members-candidates ".st" *ns* nil)
    => (strip-tags (just #{".start" ".startsWith" ".stop" ".stripTrailingZeros"}))

    (def a-str "a string")
    (src/members-candidates ".st" *ns* (ctx/parse-context '(__prefix__ a-str)))
    => (strip-tags (just [".startsWith"])))

  (fact "completion should work with vars on different namespaces"
    (def an-object 1234)
    (create-ns 'another-ns)
    (intern 'another-ns 'an-object "foo")

    (let [context (ctx/parse-context '(__prefix__ an-object))]
      (src/members-candidates ".int" *ns* context)
      => (strip-tags (just [".intValue"]))

      (src/members-candidates ".toUpper" (find-ns 'another-ns) context)
      => (strip-tags (just [".toUpperCase"]))))

  (fact "class members have docs"
    (src/members-doc ".wait" *ns*) => string?))

(facts "about static members"
  (fact "static members can be matched by camelCase too"
    (src/camel-case-matches? "Thread/actC" "Thread/activeCount") => truthy)

  (fact "static members candidates are taken for the class in prefix"
    (src/static-members-candidates "String/" *ns* nil)
    => (strip-tags
        (contains #{"String/CASE_INSENSITIVE_ORDER" "String/copyValueOf"
                    "String/format" "String/valueOf"} :gaps-ok))

    ;; Don't have to import class to get static members for it.
    (src/static-members-candidates "java.io.File/sep" *ns* nil)
    => (just #{{:candidate "java.io.File/separatorChar", :type :static-field}
               {:candidate "java.io.File/separator", :type :static-field}})

    (src/static-members-candidates "java.io.File/cre" *ns* nil)
    => [{:candidate "java.io.File/createTempFile", :type :static-method}]

    ;; But for imported classes last name can be used.
    (do (import 'java.io.File)
        (src/static-members-candidates "File/sep" *ns* nil))
    => (strip-tags (just #{"File/separator" "File/separatorChar"})))

  (fact "single slash doesn't break the completion"
    (src/static-members-candidates "/" *ns* nil) => nil)

  (fact "static class members have docs"
    (src/static-member-doc "Integer/parseInt" *ns*) => string?))
