(ns compliment.lite-test
  (:require [clojure.test :refer :all]
            [compliment.lite :as lite :refer [completions]]
            [matcher-combinators.matchers :as mc]
            [matcher-combinators.test :refer [match?]]))

(def a-big-int 42M)

(defn strip-tags
  "Returns a wrapper predicate that first removes tags from the list of
  candidates."
  [completions]
  (map :candidate completions))

(defmacro is? [expected actual] `(is (~'match? ~expected ~actual)))

(deftest completions-test
  (testing "`completions` takes a prefix, and optional options-map."
    (is? (mc/embeds ["reduce" "reduce-kv" "reductions"])
         (strip-tags (completions "redu")))

    (is? (mc/embeds ["'reduce" "'reduce-kv" "'reductions"])
         (strip-tags (completions "'redu")))

    (is? (mc/embeds ["#'reduce" "#'reduce-kv" "#'reductions"])
         (strip-tags (completions "#'redu")))

    (is? (mc/in-any-order ["rename" "rename-keys"])
         (strip-tags (completions "rena" {:ns (find-ns 'clojure.set)})))

    (is? [] (strip-tags (completions "fac" {:ns (find-ns 'clojure.core)})))

    (is? ["compliment.lite/completions"]
         (strip-tags (completions "compliment.lite/comp")))

    (is? ["'compliment.lite/completions"]
         (strip-tags (completions "'compliment.lite/comp")))

    (is? ["#'compliment.lite/completions"]
         (strip-tags (completions "#'compliment.lite/comp")))

    (is? ["lite/completions"]
         (strip-tags (completions "lite/com" {:ns (find-ns 'compliment.lite-test)}))))

  (testing "check that all lite sources work"
    (is? (mc/embeds ["map" "map?" "mapv" "mapcat"])
         (strip-tags (completions "map")))

    (is? (mc/embeds ["clojure.core" "clojure.edn" "clojure.string"])
         (strip-tags (completions "clojure.")))

    (is? (mc/embeds [".subtract" ".substring" ".subSequence"])
         (strip-tags (completions ".sub")))

    (is? (mc/embeds ["Integer/MAX_VALUE" "Integer/MIN_VALUE"])
         (strip-tags (completions "Integer/")))

    (is? (mc/embeds [":require"]) (strip-tags (completions ":req")))

    (is? [":compliment.lite/keywords"]
         (strip-tags (completions ":compliment.lite/k")))

    (is? (mc/embeds ["catch"]) (strip-tags (completions "catc")))

    (is? (mc/embeds ["nil"]) (strip-tags (completions "ni"))))

  (testing "in case of non-existing namespace doesn't fail"
    (is (completions "redu" {:ns nil}))
    (is (completions "n-m" {:ns 'foo.bar.baz})))

  (testing "many sources allow some sort of fuzziness in prefixes"
    (is? (mc/in-any-order ["reset-meta!" "remove-method" "remove-all-methods"])
         (strip-tags (completions "re-me")))

    (is? (mc/in-any-order ["remove-method" "remove-all-methods"])
         (strip-tags (completions "remme")))

    (is? (mc/embeds ["clojure.core.protocols" "clojure.core.server"])
         (strip-tags (completions "cl.co.")))

    (is? (mc/embeds ["clojure.java.io"]) (strip-tags (completions "cji")))

    (is? (mc/in-any-order [".getSpecificationVendor" ".getSpecificationVersion"])
         (strip-tags (completions ".gSV"))))

  (testing "candidates are sorted by their length first, and then alphabetically"
    (is? (mc/embeds ["map" "map?" "mapv" "mapcat"])
         (strip-tags (completions "map")))

    (is? (mc/embeds ["all-ns" "alter-meta!" "alter-var-root"])
         (strip-tags (completions "al-"))))

  (testing "sorting directly by name can also be enabled"
    (is? (mc/embeds ["map-indexed" "map?" "mapcat" "mapv"])
         (strip-tags (completions "map" {:sort-order :by-name})))

    (is? (mc/embeds [{:ns "clojure.core", :type :function, :candidate "remove-method"}
                     {:ns "clojure.core", :type :function, :candidate "remove-ns"}
                     {:ns "clojure.core", :type :function, :candidate "remove-watch"}])
         (completions "remo" {:sort-order :by-name})))

  (testing ":sources list can filter the sources to be used during completion"
    (is? #(> (count %) 10) (completions "cl" {:ns 'compliment.lite-test}))

    (is? (mc/in-any-order ["class" "class?" "clojure-version" "clear-agent-errors"])
         (strip-tags (completions "cl" {:sources [:compliment.lite/vars]
                                        :ns 'compliment.lite-test}))))

  (testing "empty prefix returns a list of candidates"
    (is? not-empty (completions "")))

  (testing "different metadata is attached to candidates"
    (is? (mc/embeds [{:ns "clojure.core", :type :function, :candidate "bound-fn*"}
                     {:ns "clojure.core", :type :macro, :candidate "bound-fn"}])
         (completions "bound" {}))

    (is? (mc/embeds [{:candidate "deftest", :type :macro, :ns "clojure.test"}
                     {:candidate "deftest-", :type :macro, :ns "clojure.test"}])
         (completions "def" {:ns (find-ns 'clojure.test)}))

    (is? [{:ns "compliment.lite-test", :type :var, :candidate "a-big-int"}]
         (completions "a-big" {:ns 'compliment.lite-test}))

    (is? (mc/embeds [{:candidate "clojure.set", :type :namespace}])
         (completions "cl.se" {}))

    ;; Test for not required namespaces
    (is? [{:type :namespace, :candidate "clojure.test.tap" :file "clojure/test/tap.clj"}]
         (completions "cl.test.ta" {}))

    ;; Test for aliases
    (is? (mc/embeds [{:type :namespace, :candidate "lite/"}])
         (completions "lit" {:ns 'compliment.lite-test}))

    (is? [{:type :class, :candidate "clojure.lang.LispReader"}]
         (completions "clojure.lang.Lisp" {}))

    (is? (mc/embeds [{:type :class, :candidate "java.net.URLEncoder"}])
         (completions "java.net.URLE" {}))

    (is? (mc/embeds [{:package "java.lang", :type :class, :candidate "RuntimeException"}])
         (completions "RuntimeE" {}))

    (is? (mc/embeds [{:candidate ".getName", :type :method}
                     {:candidate ".getSimpleName", :type :method}])
         (completions ".getName" {}))

    (is? (mc/embeds [{:candidate ".getName", :type :method}])
         (completions ".getName" {:ns 'compliment.lite-test}))

    (is? [{:candidate "Thread/.interrupt", :type :method}]
         (completions "Thread/.int" {}))

    (is? [{:type :static-field, :candidate "Integer/SIZE"}]
         (completions "Integer/SI" {}))

    (is? (mc/embeds [{:candidate "Integer/compare", :type :static-method}])
         (completions "Integer/co" {}))

    (is? (mc/embeds [{:candidate "recur", :type :special-form}])
         (completions "recu" {}))

    (is? (mc/embeds [{:candidate "true", :type :special-form}])
         (completions "tru" {}))

    (is? (mc/embeds [{:candidate ":arglists", :type :keyword}])
         (completions ":argl" {}))))
