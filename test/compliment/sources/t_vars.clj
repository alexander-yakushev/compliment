(ns compliment.sources.t-vars
  (:require [fudje.sweet :refer :all]
            [clojure.test :refer :all]
            [compliment.sources.vars :as src]
            [compliment.context :as ctx]
            [compliment.utils :refer [*extra-metadata*]]
            [compliment.t-helpers :refer :all]))

(defn- -ns [] (find-ns 'compliment.sources.t-vars))

(deftest vars-test
  (fact "var symbol is something looking like part of a var, possibly qualified"
    "redu"                 => (checker src/var-symbol?)
    "merge-wi"             => (checker src/var-symbol?)
    "clojure.core/alter-m" => (checker src/var-symbol?)
    "src/var-sy"           => (checker src/var-symbol?)
    ""                     => (checker src/var-symbol?)

    ":keyword"             => (checker (complement src/var-symbol?))
    "doesnt/make/sense"    => (checker (complement src/var-symbol?))

    ;; Edge cases
    ;; https://github.com/alexander-yakushev/compliment/pull/40
    "./foo"                => (checker src/var-symbol?))

  (fact "fuzzy matching for vars allows writing only first letters after dashes"
    (src/dash-matches? "me-wi" "merge-with")           => truthy
    (src/dash-matches? "r-t-s" "release-the-sharks")   => truthy
    (src/dash-matches? "src/d-ma" "src/dash-matches?") => truthy
    (src/dash-matches? "re-" "reduce-kv")              => truthy

    (src/dash-matches? "me-we" "merge-with")           => falsey
    (src/dash-matches? "upd-i-n" "update-in")          => falsey))

(deftest vars-completion-test
  (fact "unqualified vars are looked up in the given namespace"
    (src/candidates "redu" (-ns) nil)
    => (contains #{{:candidate "reduce", :type :function, :ns "clojure.core"}
                   {:candidate "reductions", :type :function, :ns "clojure.core"}
                   {:candidate "reduce-kv", :type :function, :ns "clojure.core"}} :gaps-ok)

    (strip-tags (src/candidates "re-ma" (-ns) nil))
    => (just ["re-matches" "re-matcher" "ref-max-history"] :in-any-order)

    (src/candidates "bindi" (-ns) nil)
    => [{:candidate "binding", :type :macro, :ns "clojure.core"}])

  (fact "qualified vars are looked up in the namespace specified in the prefix"
    (strip-tags (src/candidates "clojure.set/su" (-ns) nil))
    => (just ["clojure.set/subset?" "clojure.set/superset?"] :in-any-order)

    (do (in-ns 'compliment.sources.t-vars)
        (require '[clojure.string :as str])
        (strip-tags (src/candidates "str/re" (-ns) nil)))
    => (contains #{"str/replace" "str/replace-first" "str/reverse"} :gaps-ok)

    (do (in-ns 'compliment.sources.t-vars)
        (require '[clojure.string :as s])
        (strip-tags (src/candidates "s/cap" (-ns) nil)))
    => (just ["s/capitalize"])

    (do (in-ns 'compliment.sources.t-vars)
        (require '[clojure.string :as c.str])
        (strip-tags (src/candidates "c.str/cap" (-ns) nil)))
    => (just ["c.str/capitalize"]))

  (def some-atom (atom nil))

  (fact "var can be prefixed by ', #' or @"
    (strip-tags (src/candidates "'redu" (-ns) nil))
    => (contains ["'reduce" "'reductions" "'reduce-kv"] :gaps-ok)

    (strip-tags (src/candidates "#'redu" (-ns) nil))
    => (contains ["#'reduce" "#'reductions" "#'reduce-kv"] :gaps-ok)

    (strip-tags (src/candidates "'clojure.string/i" (-ns) nil))
    => (just ["'clojure.string/includes?" "'clojure.string/index-of"])

    (strip-tags (src/candidates "#'clojure.string/i" (-ns) nil))
    => (just ["#'clojure.string/includes?" "#'clojure.string/index-of"])

    (strip-tags (src/candidates "'s/cap" (-ns) nil))
    => (just ["'s/capitalize"])

    (strip-tags (src/candidates "#'s/cap" (-ns) nil))
    => (just ["#'s/capitalize"])

    (strip-tags (src/candidates "@some-a" (-ns) nil))
    => (just ["@some-atom"]))

  (fact "private vars will be suggested when prefixed with var quote"
    ;; no candidate for a non-public var
    (strip-tags (src/candidates "clojure.core/print-tagged-object" (-ns) nil))
    => (just [])
    ;; var quote works though
    (strip-tags (src/candidates "#'clojure.core/print-tagged-object" (-ns) nil))
    => (just [ "#'clojure.core/print-tagged-object"])
    (strip-tags (src/candidates "#'src/resolve-var" (-ns) nil))
    => (just ["#'src/resolve-var"]))

  (def foo:bar 1)
  (def foo:baz 2)
  (fact "handles vars with semicolons in them"
    (strip-tags (src/candidates "foo" (-ns) nil))
    => (contains #{"foo:bar" "foo:baz"} :gaps-ok)

    (strip-tags (src/candidates "foo:" (-ns) nil))
    => (contains #{"foo:bar" "foo:baz"} :gaps-ok)

    (strip-tags (src/candidates "foo:b" (-ns) nil))
    => (contains #{"foo:bar" "foo:baz"} :gaps-ok))

  (defn ^:deprecated ^:private some-deprecated-private-fn "Some doc" [])
  (fact "extra metadata can be requested from this completion source"
    (binding [*extra-metadata* #{:doc :arglists :private :deprecated}]
      (doall (src/candidates "some-deprecated-private-fn" (-ns) nil)))
    => [{:candidate "some-deprecated-private-fn", :type :function, :ns "compliment.sources.t-vars"
         :private true, :deprecated true, :arglists ["[]"]
         :doc (clojure.string/join
               (System/lineSeparator)
               ["compliment.sources.t-vars/some-deprecated-private-fn" "([])"
                "  Some doc" ""])}])

  (defn should-appear [])
  (defn ^:completion/hidden shouldnt-appear [])
  (fact "when :completion/hidden true metadata is present, don't suggest"
    (strip-tags (src/candidates "should" (-ns) nil))
    => (just ["should-appear"]))

  (fact "inside (ns ...) vars are looked up only from :used namespace"
    (strip-tags
     (src/candidates "ins-" (-ns)
                     (ctx/parse-context '(ns foo.bar
                                           (:use [clojure.zip
                                                  :only [__prefix__]])))))
    => (just ["insert-child" "insert-left" "insert-right"] :in-any-order)

    (strip-tags
     (src/candidates "spl" (-ns)
                     (ctx/parse-context '(ns foo.bar
                                           (:require [clojure.string
                                                      :refer [__prefix__]])))))
    => (just ["split" "split-lines"] :in-any-order))

  (fact "vars have documentation"
    (src/doc "map" (-ns)) => (checker string?)
    (src/doc "clojure.core/map" (-ns)) => (checker string?)
    (src/doc "#'clojure.core/map" (-ns)) => (checker string?)
    (src/doc "#'src/var-symbol?" (-ns)) => (checker string?)
    (src/doc "bogus" (-ns)) => nil
    (src/doc "bo/gus" (-ns)) => nil))
