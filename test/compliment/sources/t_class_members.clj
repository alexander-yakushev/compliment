(ns compliment.sources.t-class-members
  (:require [fudje.sweet :refer :all]
            [clojure.test :refer :all]
            [compliment.sources.class-members :as src]
            [compliment.context :as ctx]
            [compliment.t-helpers :refer :all]))

(defn- -ns [] (-> ::_ namespace symbol find-ns))

(def blob (javax.sql.rowset.serial.SerialBlob. (byte-array 1)))

(deftest imports-don't-matter-for-package-qualified-classes-0
  (fact "SerialBlob is not imported into this ns, making the subsequent tests valid"
    (not-any? #{'SerialBlob} (ns-imports (-ns)))
    =>
    truthy))

(deftest imports-don't-matter-for-package-qualified-classes-1
  (fact "Whether a class has been imported does not matter for a fully-qualified class.
Reported as https://github.com/alexander-yakushev/compliment/issues/58"
    (src/members-candidates ".get" (-ns) (ctx/parse-context '(__prefix__ blob)))
    =>
    (just [{:candidate ".getBinaryStream", :type :method}
           {:candidate ".getBytes", :type :method}]
          :in-any-order)))

(deftest imports-don't-matter-for-package-qualified-classes-2
  (fact "Non-package qualified variant will result in less accurate reports"
    (> (count (src/members-candidates ".get" (-ns) (ctx/parse-context '(__prefix__ (SerialBlob. (byte-array 1))))))
       10)
    =>
    truthy))

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


  (fact "if context is provided and the first arg is a symbol with type tag
  (either immediate or anywhere in the local scope)"
    (strip-tags (src/members-candidates ".sta" (-ns) nil))
    => (just [".start" ".startsWith"] :in-any-order)

    (strip-tags (src/members-candidates ".sta" (-ns) (ctx/parse-context
                                                      '(__prefix__ ^String foo))))
    => (just [".startsWith"])

    (strip-tags (src/members-candidates ".in" (-ns) (ctx/parse-context
                                                     '(__prefix__ ^Thread foo))))
    => (just [".interrupt"])

    ;; read-string is used here because fudje seems to mess up metadata on forms
    (strip-tags (src/members-candidates ".m" (-ns) (ctx/parse-context
                                                     (read-string
                                                      "(__prefix__ ^java.io.File (foo))"))))
    => (just [".mkdirs" ".mkdir"] :in-any-order)

    (strip-tags (src/members-candidates ".in" (-ns) (ctx/parse-context
                                                     (read-string
                                                      "(__prefix__ ^Thread (foo))"))))
    => (just [".interrupt"])

    (strip-tags (src/members-candidates ".p" (-ns) (ctx/parse-context
                                                    '(__prefix__ ^java.util.Map foo))))
    => (just [".putAll" ".putIfAbsent" ".put"] :in-any-order)

    (strip-tags (src/members-candidates ".in" (-ns) (ctx/parse-context
                                                     '(let [^Thread foo ...
                                                            ... ...
                                                            _ (.start foo)]
                                                        (__prefix__ foo)))))
    => (just [".interrupt"])

    (strip-tags (src/members-candidates ".p" (-ns) (ctx/parse-context
                                                    '(defn bar [{:keys [^java.util.Map foo]}]
                                                       (__prefix__ foo)))))
    => (just [".putAll" ".putIfAbsent" ".put"] :in-any-order))

  (fact "if context is provided and the first arg is a global var with a
  resolvable class, use it to filter candidates"
    (do (def a-str "a string")
        (strip-tags (src/members-candidates ".sta" (-ns) (ctx/parse-context '(__prefix__ a-str)))))
    => (just [".startsWith"]))

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
