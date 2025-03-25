(ns compliment.sources.t-vars
  (:require [clojure.test :refer :all]
            [compliment.context :as ctx]
            [compliment.sources.vars :as src]
            [compliment.t-helpers :refer :all]
            [compliment.utils :refer [*extra-metadata*]]
            [matcher-combinators.matchers :as mc]))

(defn- -ns [] (find-ns 'compliment.sources.t-vars))

(deftest vars-test
  (testing "var symbol is something looking like part of a var, possibly qualified"
    (is? some? (src/var-symbol? "redu"))
    (is? some? (src/var-symbol? "merge-wi"))
    (is? some? (src/var-symbol? "clojure.core/alter-m"))
    (is? some? (src/var-symbol? "src/var-sy"))
    (is? some? (src/var-symbol? ""))

    (is? nil (src/var-symbol? ":keyword"))
    (is? nil (src/var-symbol? "doesnt/make/sense"))

    ;; Edge cases
    ;; https://github.com/alexander-yakushev/compliment/pull/40
    (is? some? (src/var-symbol? "./foo")))

  (testing "fuzzy matching for vars allows writing only first letters after dashes"
    (is? true (src/dash-matches? "me-wi" "merge-with"))
    (is? true (src/dash-matches? "r-t-s" "release-the-sharks"))
    (is? true (src/dash-matches? "src/d-ma" "src/dash-matches?"))
    (is? true (src/dash-matches? "re-" "reduce-kv"))

    (is? false (src/dash-matches? "me-we" "merge-with"))
    (is? false (src/dash-matches? "upd-i-n" "update-in"))))

(deftest vars-completion-test
  (testing "unqualified vars are looked up in the given namespace"
    (is? (mc/embeds [{:candidate "reduce", :type :function, :ns "clojure.core"}
                     {:candidate "reductions", :type :function, :ns "clojure.core"}
                     {:candidate "reduce-kv", :type :function, :ns "clojure.core"}])
         (src/candidates "redu" (-ns) nil))

    (is? (mc/in-any-order ["re-matches" "re-matcher" "ref-max-history"])
         (strip-tags (src/candidates "re-ma" (-ns) nil)))

    (is? [{:candidate "binding", :type :macro, :ns "clojure.core"}]
         (src/candidates "bindi" (-ns) nil)))

  (testing "qualified vars are looked up in the namespace specified in the prefix"
    (is? (mc/in-any-order ["clojure.set/subset?" "clojure.set/superset?"])
         (strip-tags (src/candidates "clojure.set/su" (-ns) nil)))

    (in-ns 'compliment.sources.t-vars)
    (require '[clojure.string :as str])
    (is? (mc/embeds ["str/replace" "str/replace-first" "str/reverse"])
         (strip-tags (src/candidates "str/re" (-ns) nil)))

    (in-ns 'compliment.sources.t-vars)
    (require '[clojure.string :as s])
    (is? ["s/capitalize"] (strip-tags (src/candidates "s/cap" (-ns) nil)))

    (in-ns 'compliment.sources.t-vars)
    (require '[clojure.string :as c.str])
    (is? ["c.str/capitalize"] (strip-tags (src/candidates "c.str/cap" (-ns) nil))))

  (def some-atom (atom nil))

  (testing "var can be prefixed by ', #' or @"
    (is? (mc/embeds ["'reduce" "'reductions" "'reduce-kv"])
         (strip-tags (src/candidates "'redu" (-ns) nil)))

    (is? (mc/embeds ["#'reduce" "#'reductions" "#'reduce-kv"])
         (strip-tags (src/candidates "#'redu" (-ns) nil)))

    (is? (mc/in-any-order ["'clojure.string/includes?" "'clojure.string/index-of"])
         (strip-tags (src/candidates "'clojure.string/i" (-ns) nil)))

    (is? (mc/in-any-order ["#'clojure.string/includes?" "#'clojure.string/index-of"])
         (strip-tags (src/candidates "#'clojure.string/i" (-ns) nil)))

    (is? ["'s/capitalize"]
         (strip-tags (src/candidates "'s/cap" (-ns) nil)))

    (is? ["#'s/capitalize"]
         (strip-tags (src/candidates "#'s/cap" (-ns) nil)))

    (is? ["@some-atom"]
         (strip-tags (src/candidates "@some-a" (-ns) nil))))

  (testing "private vars will be suggested when prefixed with var quote"
    ;; no candidate for a non-public var
    (is? []
         (strip-tags (src/candidates "clojure.core/print-tagged-object" (-ns) nil)))
    ;; var quote works though
    (is? ["#'clojure.core/print-tagged-object"]
         (strip-tags (src/candidates "#'clojure.core/print-tagged-object" (-ns) nil)))
    (is? ["#'src/resolve-var"]
         (strip-tags (src/candidates "#'src/resolve-var" (-ns) nil))))

  (def foo:bar 1)
  (def foo:baz 2)
  (testing "handles vars with semicolons in them"
    (is? (mc/embeds ["foo:bar" "foo:baz"])
         (strip-tags (src/candidates "foo" (-ns) nil)))

    (is? (mc/embeds ["foo:bar" "foo:baz"])
         (strip-tags (src/candidates "foo:" (-ns) nil)))

    (is? (mc/embeds ["foo:bar" "foo:baz"])
         (strip-tags (src/candidates "foo:b" (-ns) nil))))

  (defn ^:deprecated ^:private some-deprecated-private-fn "Some doc" [])
  (testing "extra metadata can be requested from this completion source"
    (is? [{:candidate "some-deprecated-private-fn", :type :function, :ns "compliment.sources.t-vars"
           :private true, :deprecated true, :arglists ["[]"]
           :doc #"(?m)compliment.sources.t-vars/some-deprecated-private-fn\s+\(\[\]\)\s+Some doc"}]
         (binding [*extra-metadata* #{:doc :arglists :private :deprecated}]
           (vec (src/candidates "some-deprecated-private-fn" (-ns) nil)))))

  (defn should-appear [])
  (defn ^:completion/hidden shouldnt-appear [])
  (testing "when :completion/hidden true metadata is present, don't suggest"
    (is? ["should-appear"] (strip-tags (src/candidates "should" (-ns) nil))))

  (testing "inside (ns ...) vars are looked up only from :used namespace"
    (is? (mc/in-any-order ["insert-child" "insert-left" "insert-right"])
         (strip-tags
          (src/candidates "ins-" (-ns)
                          (ctx/parse-context '(ns foo.bar
                                                (:use [clojure.zip
                                                       :only [__prefix__]]))))))

    (is? (mc/in-any-order ["split" "split-lines"])
         (strip-tags
          (src/candidates "spl" (-ns)
                          (ctx/parse-context '(ns foo.bar
                                                (:require [clojure.string
                                                           :refer [__prefix__]])))))))

  (testing "priorities"
    (testing "first namespace's own vars, then clojure vars, then the rest"
      (refer 'compliment.context :only '[cache-context])
      (def catastrophic 1)
      (is? (mc/embeds [{:candidate "catastrophic", :ns "compliment.sources.t-vars", :priority 30}
                       {:candidate "cat", :ns "clojure.core", :priority 31}
                       {:candidate "cache-context", :ns "compliment.context", :priority 32}])
           (src/candidates "ca" (-ns) nil))))

  (testing "vars have documentation"
    (is? string? (src/doc "map" (-ns)))
    (is? string? (src/doc "clojure.core/map" (-ns)))
    (is? string? (src/doc "#'clojure.core/map" (-ns)))
    (is? string? (src/doc "#'src/var-symbol?" (-ns)))
    (is? nil (src/doc "bogus" (-ns)))
    (is? nil (src/doc "bo/gus" (-ns)))))
