(ns compliment.sources.t-class-members
  (:require [fudje.sweet :refer :all]
            [clojure.test :refer :all]
            [compliment.sources.class-members :as src]
            [compliment.context :as ctx]
            [compliment.t-helpers :refer :all]))

(defn- -ns [] (find-ns 'compliment.sources.t-class-members))

(deftest class-members-test
  (in-ns 'compliment.sources.t-class-members)
  (fact "fuzzy matching class members works with camelCase as separator"
    (src/camel-case-matches? ".getDeF" ".getDeclaredFields") => truthy
    (src/camel-case-matches? ".gT" ".getTextSize")           => truthy)

  (fact "candidates are taken from all non-static members of classes
  imported into the current namespace"
    (strip-tags (src/members-candidates ".eq" (-ns) nil))
    => (just [".equals" ".equalsIgnoreCase"] :in-any-order)

    (strip-tags (src/members-candidates ".getDeclF" (-ns) nil))
    => (just [".getDeclaredFields" ".getDeclaredField"] :in-any-order)

    (src/members-candidates ".pu" (-ns) nil)
    => [] ;; Because java.util.HashMap is not imported into current ns

    (do (import 'java.util.HashMap)
        (src/members-candidates ".pu" (-ns) nil))
    => (contains #{{:candidate ".put", :type :method}
                   {:candidate ".putAll", :type :method}} :gaps-ok))


  (fact "if context is provided and the class of first arg can be resolved,
  select candidates only for that class (works for vars and tagged symbols)"
    (strip-tags (src/members-candidates ".sta" (-ns) nil))
    => (just [".start" ".startsWith"] :in-any-order)

    (do (def a-str "a string")
        (strip-tags (src/members-candidates ".sta" (-ns) (ctx/parse-context '(__prefix__ a-str)))))
    => (just [".startsWith"])

    (strip-tags (src/members-candidates ".sta" (-ns) (ctx/parse-context
                                                      '(__prefix__ ^String foo))))
    => (just [".startsWith"])

    (strip-tags (src/members-candidates ".in" (-ns) (ctx/parse-context
                                                     '(__prefix__ ^Thread foo))))
    => (just [".interrupt"])

    (strip-tags (src/members-candidates ".p" (-ns) (ctx/parse-context
                                                     '(__prefix__ ^java.util.Map foo))))
    => (just [".putAll" ".putIfAbsent" ".put"] :in-any-order))

  (fact "completes members of context object even if its class is not imported"
    (do (def a-bitset (java.util.BitSet.))
        (strip-tags (src/members-candidates ".inter" (-ns) (ctx/parse-context '(__prefix__ a-bitset)))))
    => (just [".intersects"]))

  (fact "completion should work with vars on different namespaces"
    (do (def an-object 1234)
        (strip-tags (src/members-candidates ".int" (-ns) (ctx/parse-context '(__prefix__ an-object)))))
    => (just [".intValue"])

    (do (create-ns 'another-ns)
        (intern 'another-ns 'an-object "foo")
        (strip-tags (src/members-candidates ".toUpper" (find-ns 'another-ns) (ctx/parse-context '(__prefix__ an-object)))))
    => (just [".toUpperCase"]))

  (fact "class members have docs"
    (src/members-doc ".wait" (-ns)) => (checker string?)))

(deftest static-members-test
  (in-ns 'compliment.sources.t-class-members)
  (fact "static members can be matched by camelCase too"
    (src/camel-case-matches? "Thread/actC" "Thread/activeCount") => truthy)

  (fact "static members candidates are taken for the class in prefix"
    (strip-tags (src/static-members-candidates "String/" (-ns) nil))
    => (contains #{"String/CASE_INSENSITIVE_ORDER" "String/copyValueOf"
                   "String/format" "String/valueOf"} :gaps-ok)

    ;; Don't have to import class to get static members for it.
    (src/static-members-candidates "java.io.File/sep" (-ns) nil)
    => (just [{:candidate "java.io.File/separatorChar", :type :static-field}
              {:candidate "java.io.File/separator", :type :static-field}])

    (src/static-members-candidates "java.io.File/cre" (-ns) nil)
    => [{:candidate "java.io.File/createTempFile", :type :static-method}]

    ;; But for imported classes last name can be used.
    (do (import 'java.io.File)
        (strip-tags (src/static-members-candidates "File/sep" (-ns) nil)))
    => (just ["File/separator" "File/separatorChar"] :in-any-order))

  (fact "single slash doesn't break the completion"
    (src/static-members-candidates "/" (-ns) nil) => nil)

  (fact "static class members have docs"
    (src/static-member-doc "Integer/parseInt" (-ns)) => (checker string?)))
