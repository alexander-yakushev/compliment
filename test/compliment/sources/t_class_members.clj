(ns compliment.sources.t-class-members
  (:require [clojure.test :refer :all]
            [compliment.context :as ctx]
            [compliment.sources.class-members :as src]
            [compliment.t-helpers :refer :all]
            [matcher-combinators.matchers :as mc]))

(defn- -ns [] (find-ns 'compliment.sources.t-class-members))

(def ^Thread thread (Thread.))

(deftest class-member-symbol-test
  (if (#'src/clojure-1-12+?)
    (testing "accepts qualified methods for Clojure 1.12+"
      (is? nil (src/class-member-symbol? "a"))
      (is? ["a.b" ""] (src/class-member-symbol? "a.b/"))
      (is? nil (src/class-member-symbol? "a.b/foo"))
      (is? ["a.b" "."] (src/class-member-symbol? "a.b/."))
      (is? ["a.b" ".foo"] (src/class-member-symbol? "a.b/.foo")))

    (testing "accepts only dot-methods for Clojure <1.12"
      (is? nil (src/class-member-symbol? "a"))
      (is? [nil "."] (src/class-member-symbol? "."))
      (is? [nil ".a"] (src/class-member-symbol? ".a")))))

(deftest thread-first-test
  (in-ns 'compliment.sources.t-class-members)
  (testing "`->` and `some->` work with Compliment"
    (is? [".interrupt"]
         (strip-tags (src/members-candidates ".int" (-ns) (ctx/cache-context
                                                           "(-> thread __prefix__)"))))

    (is? [".interrupt"]
         (strip-tags (src/members-candidates ".int" (-ns) (ctx/cache-context
                                                           "(some-> thread __prefix__)"))))

    (is? [".interrupt"]
         (strip-tags (src/members-candidates ".int" (-ns) (ctx/cache-context
                                                           "(-> thread __prefix__ FOO)"))))

    (is? [".subSequence" ".substring"]
         (strip-tags (src/members-candidates ".su" (-ns) (ctx/cache-context
                                                          "(-> \"\" clojure.string/trim __prefix__)"))))

    (is? [".subSequence" ".substring"]
         (strip-tags (src/members-candidates ".su" (-ns) (ctx/cache-context
                                                          "(-> \"\" clojure.string/trim __prefix__ FOO)"))))

    (is? [".subSequence" ".substring"]
         (strip-tags (src/members-candidates ".su" (-ns) (ctx/cache-context
                                                          "(-> [] (clojure.string/join) __prefix__)"))))

    (is? [".subSequence" ".substring"]
         (strip-tags (src/members-candidates ".su" (-ns) (ctx/cache-context
                                                          "(-> [] (clojure.string/join) __prefix__ FOO)"))))

    (is? [".interrupt"]
         (strip-tags (src/members-candidates ".int" (-ns) (ctx/cache-context
                                                           "(-> x ^Thread (anything) __prefix__)"))))

    (is? [".interrupt"]
         (strip-tags (src/members-candidates ".int" (-ns) (ctx/cache-context
                                                           "(-> x ^Thread (anything) __prefix__ FOO)"))))))

(deftest thread-last-test
  (testing "`->>` and `some->>` work with Compliment"
    (is? [".subSequence" ".substring"]
         (strip-tags (src/members-candidates ".su" (-ns) (ctx/cache-context
                                                          "(->> [] (clojure.string/join \"a\") __prefix__)"))))

    (is? [".subSequence" ".substring"]
         (strip-tags (src/members-candidates ".su" (-ns) (ctx/cache-context
                                                          "(some->> [] (clojure.string/join \"a\") __prefix__)"))))

    (is? [".subSequence" ".substring"]
         (strip-tags (src/members-candidates ".su" (-ns) (ctx/cache-context
                                                          "(->> [] (clojure.string/join \"a\") __prefix__ FOO)"))))

    (is? [".start"]
         (strip-tags (src/members-candidates ".sta" (-ns) (ctx/cache-context
                                                           "(->> [] ^Thread (anything) __prefix__ )"))))

    (is? [".start"]
         (strip-tags (src/members-candidates ".sta" (-ns) (ctx/cache-context
                                                           "(->> thread __prefix__)"))))))

(deftest doto-test
  (in-ns 'compliment.sources.t-class-members)
  (testing "`doto` works with Compliment"
    (is? [".interrupt"]
         (strip-tags (src/members-candidates ".int" (-ns) (ctx/cache-context
                                                           "(doto thread __prefix__)"))))

    (is? [".interrupt"]
         (strip-tags (src/members-candidates ".int" (-ns) (ctx/cache-context
                                                           "(doto thread (__prefix__))"))))

    (is? [".interrupt"]
         (strip-tags (src/members-candidates ".int" (-ns) (ctx/cache-context
                                                           "(doto thread .checkAccess (__prefix__))"))))

    (is? [".interrupt"]
         (strip-tags (src/members-candidates ".int" (-ns) (ctx/cache-context
                                                           "(doto thread (__prefix__) .checkAccess)"))))))

(deftype FooType [x-y x-z])
(definterface SomeIface (foo_bar [this]))

(deftest class-members-test
  (in-ns 'compliment.sources.t-class-members)
  (testing "fuzzy matching class members works with camelCase as separator"
    (is (src/camel-case-matches? ".getDeF" ".getDeclaredFields"))
    (is (src/camel-case-matches? ".gT" ".getTextSize")))

  (testing "candidates are taken from all non-static members of classes
  imported into the current namespace"
    (is? (mc/in-any-order [".equals" ".equalsIgnoreCase"])
         (strip-tags (src/members-candidates ".eq" (-ns) nil)))

    (is? (mc/in-any-order [".getDeclaredField" ".getDeclaredFields"])
         (strip-tags (src/members-candidates ".getDeclF" (-ns) nil)))

    (is? [] ;; Because java.util.HashMap is not imported into current ns
         (src/members-candidates ".pu" (-ns) nil))

    (import 'java.util.HashMap)
    (is? (mc/embeds [{:candidate ".put", :type :method}
                     {:candidate ".putAll", :type :method}])
         (src/members-candidates ".pu" (-ns) nil)))


  (when (#'src/clojure-1-12+?)
    (testing "candidates contain instance class members for Clojure 1.12+"
      (is? (mc/embeds ["Thread/.isAlive"])
           (strip-tags (src/members-candidates "Thread/" (-ns) nil)))))

  (testing "if context is provided and the first arg is a symbol with type tag
  (either immediate or anywhere in the local scope)"
    (is? (mc/in-any-order [".start" ".startsWith"])
         (strip-tags (src/members-candidates ".sta" (-ns) nil)))

    (is? [".startsWith"]
         (strip-tags (src/members-candidates ".sta" (-ns) (ctx/cache-context
                                                           "(__prefix__ ^String foo)"))))

    (is? [".interrupt"]
         (strip-tags (src/members-candidates ".in" (-ns) (ctx/cache-context
                                                          "(__prefix__ ^Thread foo)"))))

    (is? (mc/in-any-order [".mkdirs" ".mkdir"])
         (strip-tags (src/members-candidates ".m" (-ns) (ctx/cache-context
                                                         "(__prefix__ ^java.io.File (foo))"))))

    ;; Doesn't break if the class of the hint can't be resolved.
    (is? (mc/embeds [".matches"])
         (strip-tags (src/members-candidates ".m" (-ns) (ctx/cache-context
                                                         "(__prefix__ ^non.existing.Class (foo))"))))

    (is? [".interrupt"]
         (strip-tags (src/members-candidates ".in" (-ns) (ctx/cache-context
                                                          "(__prefix__ ^Thread (foo))"))))

    (is? (mc/in-any-order [".putAll" ".putIfAbsent" ".put"])
         (strip-tags (src/members-candidates ".p" (-ns) (ctx/cache-context
                                                         "(__prefix__ ^java.util.Map foo)"))))

    (is? [".interrupt"]
         (strip-tags (src/members-candidates ".in" (-ns) (ctx/cache-context
                                                          "(let [^Thread foo ...
                                                             ... ...
                                                             _ (.start foo)]
                                                         (__prefix__ foo))"))))

    (is? (mc/in-any-order [".putAll" ".putIfAbsent" ".put"])
         (strip-tags (src/members-candidates ".p" (-ns) (ctx/cache-context
                                                         "(defn bar [{:keys [^java.util.Map foo]}]
                                                        (__prefix__ foo))")))))

  (testing "if context is provided and the first arg is a global var with a
  resolvable class, use it to filter candidates"
    (def a-str "a string")
    (is? [".startsWith"]
         (strip-tags (src/members-candidates ".sta" (-ns) (ctx/cache-context
                                                           "(__prefix__ a-str)")))))

  (testing "completes members of context object even if its class is not imported"
    (def a-bitset (java.util.BitSet.))
    (is? [".intersects"]
         (strip-tags (src/members-candidates ".inter" (-ns) (ctx/cache-context
                                                             "(__prefix__ a-bitset)")))))

  (testing "given a subclass context, superclass members are suggested when not imported"
    (is? (mc/embeds [".getLineNumber" ".markSupported"])
         (strip-tags (src/members-candidates "." (-ns) (ctx/cache-context
                                                        "(__prefix__ ^java.io.LineNumberReader x")))))

  (testing "completion should work with vars on different namespaces"
    (def an-object 1234)
    (is? [".intValue"]
         (strip-tags (src/members-candidates ".int" (-ns) (ctx/cache-context
                                                           "(__prefix__ an-object)"))))

    (create-ns 'another-ns)
    (intern 'another-ns 'an-object "foo")
    (is? [".toUpperCase"]
         (strip-tags (src/members-candidates ".toUpper" (find-ns 'another-ns) (ctx/cache-context
                                                                               "(__prefix__ an-object)")))))

  (testing "deftype fields are demunged for better compatibility"
    (is? (mc/in-any-order [".x-z" ".x-y" ".xor"])
         (strip-tags (src/members-candidates ".x" (-ns) nil)))

    (is? [".foo_bar"]
         (strip-tags (src/members-candidates ".foo" (-ns) nil))))

  (when (#'src/clojure-1-12+?)
    (testing "instance class members have docs"
      (is? string? (src/members-doc "java.io.File/.isHidden" (-ns)))))

  (testing "class members have docs"
    (is? string? (src/members-doc ".wait" (-ns)))))

(deftest static-members-test
  (in-ns 'compliment.sources.t-class-members)
  (testing "static members can be matched by camelCase too"
    (is (src/camel-case-matches? "Thread/actC" "Thread/activeCount")))

  (testing "static members candidates are taken for the class in prefix"
    (is? (mc/embeds ["String/CASE_INSENSITIVE_ORDER" "String/copyValueOf"
                     "String/format" "String/valueOf"])
         (strip-tags (src/static-members-candidates "String/" (-ns) nil)))

    ;; Don't have to import class to get static members for it.
    (is? [{:candidate "java.io.File/separatorChar", :type :static-field}
          {:candidate "java.io.File/separator", :type :static-field}]
         (src/static-members-candidates "java.io.File/sep" (-ns) nil))

    (is? [{:candidate "java.io.File/createTempFile", :type :static-method}]
         (src/static-members-candidates "java.io.File/cre" (-ns) nil))

    ;; But for imported classes last name can be used.
    (import 'java.io.File)
    (is? (mc/embeds ["File/separator" "File/separatorChar"])
         (strip-tags (src/static-members-candidates "File/sep" (-ns) nil))))

  (testing "single slash doesn't break the completion"
    (is? nil (src/static-members-candidates "/" (-ns) nil)))

  (when (#'src/clojure-1-12+?)
    (testing "static members in 1.12+ include constructors"
      (is? (mc/in-any-order
            [{:candidate "String/valueOf", :type :static-method}
             {:candidate "String/CASE_INSENSITIVE_ORDER", :type :static-field}
             {:candidate "String/copyValueOf", :type :static-method}
             {:candidate "String/new", :type :constructor}
             {:candidate "String/join", :type :static-method}
             {:candidate "String/format", :type :static-method}])
           (src/static-members-candidates "String/" (-ns) nil)))

    (testing "static members candidates contain constructors for Clojure 1.12+"
      (is? ["java.io.File/new"]
           (strip-tags (src/static-members-candidates "java.io.File/n" (-ns) nil))))

    (testing "constructors have docs"
      (is? string? (src/static-member-doc "java.io.File/new" (-ns)))))

  (testing "static class members have docs"
    (is? string? (src/static-member-doc "Integer/parseInt" (-ns)))))

(deftest literals-inference-test
  (testing "Vector literals"
    (testing "Only returns members of clojure.lang.PersistentVector for the very short \".s\" query"
      (is? (mc/in-any-order [".seq" ".set" ".shift" ".size" ".sort" ".spliterator" ".stream" ".subList"])
           (strip-tags (src/members-candidates ".s" (-ns) (ctx/cache-context
                                                           "(__prefix__ [])"))))))

  (testing "String literals"
    (testing "Only returns members of String for the very short \".g\" query"
      (is? (mc/in-any-order [{:candidate ".getChars", :type :method}
                             {:candidate ".getBytes", :type :method}
                             {:candidate ".getClass", :type :method}])
           (src/members-candidates ".g" (-ns) (ctx/cache-context
                                               "(__prefix__ \"\")"))))

    (testing "A docstring is offered for the previous query"
      (is? string? (src/members-doc ".codePointBefore" (-ns))))))
