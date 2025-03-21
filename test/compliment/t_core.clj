(ns compliment.t-core
  (:require [clojure.test :refer :all]
            [compliment.core :as core]
            [compliment.t-helpers :refer :all]
            [matcher-combinators.matchers :as mc]))

;; Sanity check we run the Clojure version which we think we do.
(deftest version-sanity-check
  (is (let [v (System/getenv "CLOJURE_VERSION")]
        (println "Running on Clojure" (clojure-version))
        (or (nil? v) (.startsWith ^String (clojure-version) v)))))

;; This namespace contains only sanity checks for the public API. For
;; in-depth source testing see their respective test files.

(deftest completions-test
  (testing "`completions` takes a prefix, and optional options-map."
    (is? (mc/embeds ["reduce" "reduce-kv" "reductions"])
         (strip-tags (core/completions "redu")))

    (is? (mc/embeds ["'reduce" "'reduce-kv" "'reductions"])
         (strip-tags (core/completions "'redu")))

    (is? (mc/embeds ["#'reduce" "#'reduce-kv" "#'reductions"])
         (strip-tags (core/completions "#'redu")))

    (is? (mc/embeds ["run-tests" "run-all-tests"])
         (strip-tags (core/completions "run-" {:ns (find-ns 'clojure.test)})))

    (is? []
         (core/completions "fac" {:ns (find-ns 'clojure.core)}))

    (is? ["compliment.core/completions"]
         (strip-tags (core/completions "compliment.core/co")))

    (is? ["'compliment.core/completions"]
         (strip-tags (core/completions "'compliment.core/co")))

    (is? ["#'compliment.core/completions"]
         (strip-tags (core/completions "#'compliment.core/co")))

    (is? ["core/documentation"]
         (strip-tags (core/completions "core/doc" {:ns (find-ns 'compliment.t-core)})))

    (is? ["'core/documentation"]
         (strip-tags (core/completions "'core/doc" {:ns (find-ns 'compliment.t-core)})))

    (is? ["#'core/documentation"]
         (strip-tags (core/completions "#'core/doc" {:ns (find-ns 'compliment.t-core)}))))

  (testing "in case of non-existing namespace doesn't fail"
    (is (core/completions "redu" {:ns nil}))
    (is (core/completions "n-m" {:ns 'foo.bar.baz})))

  (testing "many sources allow some sort of fuzziness in prefixes"
    (is? ["reset-meta!" "remove-method" "remove-all-methods"]
         (strip-tags (core/completions "re-me")))

    (is? ["remove-method" "remove-all-methods"]
         (strip-tags (core/completions "remme")))

    (is? (mc/embeds ["clojure.core.server" "clojure.core.reducers"
                     "clojure.core.protocols" "clojure.core.specs.alpha"])
         (strip-tags (core/completions "cl.co.")))

    (is? (mc/embeds ["clojure.java.io"])
         (strip-tags (core/completions "cji")))

    (is? [".getSpecificationVendor" ".getSpecificationVersion"]
         (strip-tags (core/completions ".gSV"))))

  (testing "candidates are sorted by their length first, and then alphabetically"
    (is? (mc/prefix ["map" "map?" "mapv" "mapcat"])
         (strip-tags (core/completions "map")))

    (is? ["all-ns" "alter-meta!" "alter-var-root"]
         (strip-tags (core/completions "al-"))))

  (testing "sorting directly by name can also be enabled"
    (is? (mc/prefix ["map" "map-entry?" "map-indexed" "map?" "mapcat" "mapv"])
         (strip-tags (core/completions "map" {:sort-order :by-name})))

    (is? (mc/prefix ["remove" "remove-all-methods" "remove-method" "remove-ns"])
         (strip-tags (core/completions "remo" {:sort-order :by-name}))))

  (testing "context can help some sources to give better candidates list"
    (def a-str "a string")
    (is? [".substring" ".subSequence"]
         (strip-tags (core/completions ".sub" {:context "(__prefix__ a-str)"
                                               :ns 'compliment.t-core})))

    (def a-big-int 42M)
    (is? [".subtract"]
         (strip-tags (core/completions ".sub" {:context "(__prefix__ a-big-int)"
                                               :ns 'compliment.t-core})))

    ;; Aliased keywords don't break context parsing.
    (is? ["bar"]
         (strip-tags (core/completions "" {:context "(let [bar ::ex.data/id] __prefix__)"
                                           :sources [:compliment.sources.local-bindings/local-bindings]}))))

  (testing ":sources list can filter the sources to be used during completion"
    (is? #(> (count %) 20)
         (core/completions "cl" {:ns 'compliment.t-core}))

    (is? ["class" "class?" "clojure-version" "clear-agent-errors"]
         (strip-tags (core/completions "cl" {:sources [:compliment.sources.vars/vars]
                                             :ns 'compliment.t-core}))))

  (testing "empty prefix returns a list of candidates"
    (is (not-empty (core/completions ""))))

  (testing "different metadata is attached to candidates"
    (is? (mc/embeds [{:ns "clojure.core", :type :function, :candidate "bound-fn*"}
                     {:ns "clojure.core", :type :macro, :candidate "bound-fn"}])
         (core/completions "bound" {}))

    (is? (mc/embeds [{:candidate "deftest", :type :macro, :ns "clojure.test"}
                     {:candidate "deftest-", :type :macro, :ns "clojure.test"}])
         (core/completions "deft" {:ns (find-ns 'clojure.test)}))

    (is? [{:ns "compliment.t-core", :type :var, :candidate "a-big-int"}]
         (core/completions "a-big-" {:ns 'compliment.t-core}))

    (is? (mc/embeds [{:candidate "clojure.set", :type :namespace}])
         (core/completions "cl.se" {}))

    ;; Test for not required namespaces
    (is? [{:type :namespace, :candidate "clojure.test.tap" :file "clojure/test/tap.clj"}]
         (core/completions "cl.test.ta" {}))

    ;; Test for aliases
    (is? (mc/embeds [{:type :namespace, :candidate "core"}])
         (core/completions "cor" {:ns 'compliment.t-core}))

    (is? [{:type :class, :candidate "clojure.lang.LispReader"}]
         (core/completions "clojure.lang.Lisp" {}))

    (is? [{:type :class, :candidate "java.net.URLEncoder"}]
         (core/completions "java.net.URLE" {}))

    (is? (mc/embeds [{:package "java.lang", :type :class, :candidate "RuntimeException"}])
         (core/completions "RuntimeE" {}))

    (is? (mc/embeds [{:candidate ".getName", :type :method}
                     {:candidate ".getSimpleName", :type :method}])
         (core/completions ".getName" {:ns 'compliment.t-core}))

    (is? [{:type :static-field, :candidate "Integer/SIZE"}]
         (core/completions "Integer/SI" {}))

    (is? (mc/embeds [{:candidate "Integer/compare", :type :static-method}])
         (core/completions "Integer/co" {}))

    (is? (mc/embeds [{:candidate "recur", :type :special-form}])
         (core/completions "recu" {}))

    (is? (mc/embeds [{:candidate "true", :type :special-form}])
         (core/completions "tru" {}))

    (is? (mc/embeds [{:candidate "bar", :type :local}
                     {:candidate "baz", :type :local}])
         (core/completions "ba" {:context "(defn foo [bar baz] (+ 1 __prefix__))"}))

    (is? (mc/embeds [{:candidate ":arglists", :type :keyword}])
         (core/completions ":argl" {})))

  (testing "extra-metadata arglists"
    (is? (mc/embeds [{:ns "clojure.core", :type :function, :candidate "apply",
                      :arglists '("[f args]" "[f x args]" "[f x y args]" "[f x y z args]" "[f a b c d & args]")}])
         (core/completions "apply" {:extra-metadata #{:arglists}})))

  (testing "extra-metadata doc"
    (is? (mc/embeds [{:ns "clojure.core", :type :function, :candidate "bound-fn*"}
                     {:ns "clojure.core", :type :macro, :candidate "bound-fn"}])
         (core/completions "bound" {:extra-metadata #{:doc}}))

    (is? (mc/seq-of {:doc string?})
         (core/completions "bound" {:extra-metadata #{:doc}}))))

(deftest documentation-test
  (testing "`documentation` takes a symbol string which presumably can be
  resolved to something that has a docstring."
    (is? (mc/all-of string? not-empty) (core/documentation "reduce"))
    (is? (mc/all-of string? not-empty) (core/documentation ".start"))
    (is? "" (core/documentation "jus.t g:arbage"))
    (is? "" (core/documentation "reduce" *ns* {:sources []})))

  (testing "don't break on empty input"
    (is? "" (core/documentation ""))))
