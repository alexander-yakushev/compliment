(ns compliment.sources.t-ns-mappings
  (:require [midje.sweet :refer :all]
            [compliment.sources.ns-mappings :as src]
            [compliment.context :as ctx]))

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
    => (contains #{"reduce" "reduce-kv" "reductions"} :gaps-ok)

    (src/candidates "re-ma" *ns* nil)
    => (just #{"re-matches" "re-matcher" "ref-max-history"})

    ;; Imported classes also reside in namespace mapping, so they are
    ;; covered here.
    (src/candidates "Runt" *ns* nil)
    => (just #{"Runtime" "RuntimeException" "RuntimePermission"}))

  (fact "qualified vars are looked up in the namespace specified in the prefix"
    (src/candidates "clojure.set/su" *ns* nil)
    => (just #{"clojure.set/subset?" "clojure.set/superset?"})

    (src/candidates "str/re" ..some-ns.. nil)
    => (contains #{"str/replace" "str/replace-first" "str/reverse"} :gaps-ok)
    (provided (ns-aliases ..some-ns..) => {'str (find-ns 'clojure.string)})

    (src/candidates "s/cap" ..some-ns.. nil)
    => ["s/capitalize"]
    (provided (ns-aliases ..some-ns..) => {'s (find-ns 'clojure.string)}))

  (fact "inside (ns ...) vars are looked up only from :used namespace"
    (src/candidates "ins-" *ns*
                    (ctx/parse-context '(ns foo.bar
                                          (:use [clojure.zip
                                                 :only [__prefix__]]))))
    => (just #{"insert-child" "insert-left" "insert-right"})

    (src/candidates "s" *ns*
                    (ctx/parse-context '(ns foo.bar
                                          (:require [clojure.string
                                                     :refer [__prefix__]]))))
    => (just #{"split" "split-lines"})))
