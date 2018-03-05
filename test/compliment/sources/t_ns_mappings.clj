(ns compliment.sources.t-ns-mappings
  (:require [midje.sweet :refer :all]
            [compliment.sources.ns-mappings :as src]
            [compliment.context :as ctx]
            [compliment.utils :refer [*extra-metadata*]]
            [compliment.t-helpers :refer :all]))

(facts "about vars"
  (fact "var symbol is something looking like part of a var, possibly qualified"
    "redu"                 => src/var-symbol?
    "merge-wi"             => src/var-symbol?
    "clojure.core/alter-m" => src/var-symbol?
    "src/var-sy"           => src/var-symbol?

    ":keyword"             =not=> src/var-symbol?
    "doesnt/make/sense"    =not=> src/var-symbol?)

  (fact "fuzzy matching for vars allows writing only first letters after dashes"
    (src/dash-matches? "me-wi" "merge-with")           => truthy
    (src/dash-matches? "r-t-s" "release-the-sharks")   => truthy
    (src/dash-matches? "src/d-ma" "src/dash-matches?") => truthy
    (src/dash-matches? "re-" "reduce-kv")              => truthy

    (src/dash-matches? "me-we" "merge-with")           => falsey
    (src/dash-matches? "upd-i-n" "update-in")          => falsey)

  (fact "if var symbol contains a / then first part is ns qualifier"
    (src/get-scope-and-prefix "clojure.core/red" *ns*)
    => ["clojure.core" (find-ns 'clojure.core) "red"]

    (src/get-scope-and-prefix "src/get-s" *ns*)
    => ["src" (find-ns 'compliment.sources.ns-mappings) "get-s"]

    (src/get-scope-and-prefix "no-qualifier" *ns*)
    => [nil nil "no-qualifier"]))

(facts "about vars completion"
  (fact "unqualified vars are looked up in the given namespace"
    (src/candidates "redu" *ns* nil)
    => (contains #{{:candidate "reduce", :type :function, :ns "clojure.core"}
                   {:candidate "reductions", :type :function, :ns "clojure.core"}
                   {:candidate "reduce-kv", :type :function, :ns "clojure.core"}} :gaps-ok)

    (src/candidates "re-ma" *ns* nil)
    => (strip-tags (just #{"re-matches" "re-matcher" "ref-max-history"}))

    (src/candidates "bindi" *ns* nil)
    => [{:candidate "binding", :type :macro, :ns "clojure.core"}])

  (fact "imported classes are looked up in the given namespace"
    (src/candidates "Runt" *ns* nil)
    => (just #{{:candidate "Runtime", :type :class, :package "java.lang"}
               {:candidate "RuntimePermission", :type :class, :package "java.lang"}
               {:candidate "RuntimeException", :type :class, :package "java.lang"}}))

  (fact "defrecord produces classes that don't have a package"
    ;; Since JDK9, they inherit the namespace package
    (defrecord Animal [name])
    (src/candidates "Anim" *ns* nil)
    => [{:candidate "Animal", :type :class
         :package (if (try (resolve 'java.lang.Runtime$Version)
                           (catch Exception _))
                    "compliment.sources.t_ns_mappings"
                    nil)}])

  (fact "qualified vars are looked up in the namespace specified in the prefix"
    (src/candidates "clojure.set/su" *ns* nil)
    => (strip-tags (just #{"clojure.set/subset?" "clojure.set/superset?"}))

    (src/candidates "str/re" ..some-ns.. nil)
    => (strip-tags
        (contains #{"str/replace" "str/replace-first" "str/reverse"} :gaps-ok))
    (provided (ns-aliases ..some-ns..) => {'str (find-ns 'clojure.string)})

    (src/candidates "s/cap" ..some-ns.. nil)
    => (strip-tags (just ["s/capitalize"]))
    (provided (ns-aliases ..some-ns..) => {'s (find-ns 'clojure.string)})

    (src/candidates "c.str/cap" ..some-ns.. nil)
    => (strip-tags (just ["c.str/capitalize"]))
    (provided (ns-aliases ..some-ns..) => {'c.str (find-ns 'clojure.string)}))

  (fact "handles vars with semicolons in them"
    (def foo:bar 1)
    (def foo:baz 2)

    (src/candidates "foo" *ns* nil)
    => (strip-tags (contains #{"foo:bar" "foo:baz"} :gaps-ok)) ;; passes

    (src/candidates "foo:" *ns* nil)
    => (strip-tags (contains #{"foo:bar" "foo:baz"} :gaps-ok)) ;; fails

    (src/candidates "foo:b" *ns* nil)
    => (strip-tags (contains #{"foo:bar" "foo:baz"} :gaps-ok))) ;; fails

  (fact "extra metadata can be requested from this completion source"
    (binding [*extra-metadata* #{:doc :arglists}]
      (doall (src/candidates "freq" *ns* nil)))
    => [{:candidate "frequencies", :type :function, :ns "clojure.core"
         :arglists ["[coll]"]
         :doc "clojure.core/frequencies\n([coll])
  Returns a map from distinct items in coll to the number of times
  they appear.\n"}])

  (fact "inside (ns ...) vars are looked up only from :used namespace"
    (src/candidates "ins-" *ns*
                    (ctx/parse-context '(ns foo.bar
                                          (:use [clojure.zip
                                                 :only [__prefix__]]))))
    => (strip-tags (just #{"insert-child" "insert-left" "insert-right"}))

    (src/candidates "spl" *ns*
                    (ctx/parse-context '(ns foo.bar
                                          (:require [clojure.string
                                                     :refer [__prefix__]]))))
    => (strip-tags (just #{"split" "split-lines"}))))
