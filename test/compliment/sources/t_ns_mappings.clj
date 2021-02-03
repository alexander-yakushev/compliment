(ns compliment.sources.t-ns-mappings
  (:require [fudje.sweet :refer :all]
            [clojure.test :refer :all]
            [compliment.sources.ns-mappings :as src]
            [compliment.context :as ctx]
            [compliment.utils :refer [*extra-metadata*]]
            [compliment.t-helpers :refer :all]))

(defn- -ns [] (find-ns 'compliment.sources.t-ns-mappings))

(deftest vars-test
  (fact "var symbol is something looking like part of a var, possibly qualified"
    "redu"                 => (checker src/var-symbol?)
    "merge-wi"             => (checker src/var-symbol?)
    "clojure.core/alter-m" => (checker src/var-symbol?)
    "src/var-sy"           => (checker src/var-symbol?)

    ":keyword"             => (checker (complement src/var-symbol?))
    "doesnt/make/sense"    => (checker (complement src/var-symbol?)))

  (fact "fuzzy matching for vars allows writing only first letters after dashes"
    (src/dash-matches? "me-wi" "merge-with")           => truthy
    (src/dash-matches? "r-t-s" "release-the-sharks")   => truthy
    (src/dash-matches? "src/d-ma" "src/dash-matches?") => truthy
    (src/dash-matches? "re-" "reduce-kv")              => truthy

    (src/dash-matches? "me-we" "merge-with")           => falsey
    (src/dash-matches? "upd-i-n" "update-in")          => falsey)

  (fact "if var symbol contains a / then first part is ns qualifier"
    (src/get-scope-and-prefix "clojure.core/red" (-ns))
    => ["clojure.core" (find-ns 'clojure.core) "red"]

    (src/get-scope-and-prefix "src/get-s" (-ns))
    => ["src" (find-ns 'compliment.sources.ns-mappings) "get-s"]

    (src/get-scope-and-prefix "no-qualifier" (-ns))
    => [nil nil "no-qualifier"]))

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

  (fact "imported classes are looked up in the given namespace"
    (src/candidates "Runt" (-ns) nil)
    => (just [{:candidate "Runtime", :type :class, :package "java.lang"}
              {:candidate "RuntimePermission", :type :class, :package "java.lang"}
              {:candidate "RuntimeException", :type :class, :package "java.lang"}]
             :in-any-order))

  (in-ns 'compliment.sources.t-ns-mappings)
  (defrecord Animal [name])
  (fact "defrecord produces classes that don't have a package"
    ;; Since JDK9, they inherit the namespace package
    (src/candidates "Anim" (-ns) nil)
    => [{:candidate "Animal", :type :class
         :package (if (try (resolve 'java.lang.Runtime$Version)
                           (catch Exception _))
                    "compliment.sources.t_ns_mappings"
                    nil)}])

  (fact "qualified vars are looked up in the namespace specified in the prefix"
    (strip-tags (src/candidates "clojure.set/su" (-ns) nil))
    => (just ["clojure.set/subset?" "clojure.set/superset?"] :in-any-order)

    (do (require '[clojure.string :as str])
        (strip-tags (src/candidates "str/re" (-ns) nil)))
    => (contains #{"str/replace" "str/replace-first" "str/reverse"} :gaps-ok)

    (do (require '[clojure.string :as s])
        (strip-tags (src/candidates "s/cap" (-ns) nil)))
    => (just ["s/capitalize"])

    (do (require '[clojure.string :as c.str])
        (strip-tags (src/candidates "c.str/cap" (-ns) nil)))
    => (just ["c.str/capitalize"]))

  (def foo:bar 1)
  (def foo:baz 2)
  (fact "handles vars with semicolons in them"
    (strip-tags (src/candidates "foo" (-ns) nil))
    => (contains #{"foo:bar" "foo:baz"} :gaps-ok)

    (strip-tags (src/candidates "foo:" (-ns) nil))
    => (contains #{"foo:bar" "foo:baz"} :gaps-ok)

    (strip-tags (src/candidates "foo:b" (-ns) nil))
    => (contains #{"foo:bar" "foo:baz"} :gaps-ok))

  (fact "extra metadata can be requested from this completion source"
    (binding [*extra-metadata* #{:doc :arglists}]
      (doall (src/candidates "freq" (-ns) nil)))
    => [{:candidate "frequencies", :type :function, :ns "clojure.core"
         :arglists ["[coll]"]
         :doc (clojure.string/join
               (System/lineSeparator)
               ["clojure.core/frequencies" "([coll])"
                "  Returns a map from distinct items in coll to the number of times
  they appear." ""])}])

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
    => (just ["split" "split-lines"] :in-any-order)))
