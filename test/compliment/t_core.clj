(ns compliment.t-core
  (:require [midje.sweet :refer :all]
            [compliment.core :as core]
            [compliment.context :as ctx]
            [compliment.t-helpers :refer :all]))

;; This namespace contains only sanity checks for the public API. For
;; in-depth source testing see their respective test files.

(facts "about completion"
  (fact "`completions` takes a prefix, and optional options-map."
    (core/completions "redu")
    => (strip-tags (contains ["reduce" "reduce-kv" "reductions"] :gaps-ok))

    (core/completions "fac" {:ns (find-ns 'midje.sweet)})
    => (strip-tags (just #{"fact-group" "fact" "facts"}))

    (core/completions "fac" {:ns (find-ns 'clojure.core)})
    => ()

    (core/completions "compliment.core/co")
    => (strip-tags (just ["compliment.core/completions"]))

    (core/completions "core/doc")
    => (strip-tags (just ["core/documentation"])))

  (fact "in case of non-existing namespace doesn't fail"
    (core/completions "redu" {:ns nil}) => anything
    (core/completions "n-m" {:ns 'foo.bar.baz}) => anything)

  (fact "many sources allow some sort of fuzziness in prefixes"
    (core/completions "re-me")
    => (strip-tags (just #{"remove-method" "reset-meta!" "remove-all-methods"}))

    (core/completions "remme")
    => (strip-tags (just [#"remove-method" "remove-all-methods"]))

    (core/completions "cl.co.")
    => (strip-tags (contains #{"clojure.core.protocols" "clojure.core.unify"} :gaps-ok))

    (core/completions "cji")
    => (strip-tags (just ["clojure.java.io"]))

    (core/completions ".gSV")
    => (strip-tags (just #{".getSpecificationVersion" ".getSpecificationVendor"})))

  (fact "candidates are sorted by their length first, and then alphabetically"
    (core/completions "map")
    => (strip-tags (contains ["map" "map?" "mapv" "mapcat"]))

    (core/completions "al-")
    => (strip-tags (just ["all-ns" "alter-meta!" "alter-var-root"])))

  (fact "sorting directly by name can also be enabled"
    (core/completions "map" {:sort-order :by-name})
    => (strip-tags (contains ["map-indexed" "map?" "mapcat" "mapv"]))

    (core/completions "remo" {:sort-order :by-name})
    => (contains [{:ns "clojure.core", :type :function, :candidate "remove-method"}
                  {:ns "clojure.core", :type :function, :candidate "remove-ns"}
                  {:ns "clojure.core", :type :function, :candidate "remove-watch"}]))

  (fact "context can help some sources to give better candidates list"
    (def a-str "a string")
    (core/completions ".sub" {:context "(__prefix__ a-str)"})
    => (strip-tags (just #{".subSequence" ".substring"}))

    (def a-big-int 42M)
    (core/completions ".sub" {:context "(__prefix__ a-big-int)"})
    => (strip-tags (just [".subtract"])))

  (fact ":sources list can filter the sources to be used during completion"
    (core/completions "cl")
    => #(> (count %) 10)

    (core/completions "cl" {:sources [:compliment.sources.ns-mappings/ns-mappings]})
    => (strip-tags (just #{"class" "class?" "clojure-version" "clear-agent-errors"})))

  (fact "different metadata is attached to candidates"
    (core/completions "bound" {}) =>
    (contains #{{:ns "clojure.core", :type :function, :candidate "bound-fn*"}
                {:ns "clojure.core", :type :macro, :candidate "bound-fn"}} :gaps-ok)

    (core/completions "fac" {:ns (find-ns 'midje.sweet)})
    => (just [{:candidate "fact", :type :macro, :ns "midje.sweet"}
              {:candidate "facts", :type :macro, :ns "midje.sweet"}
              {:candidate "fact-group", :type :macro, :ns "midje.sweet"}])

    (core/completions "a-big-" {})
    => (just [{:ns "compliment.t-core", :type :var, :candidate "a-big-int"}])

    (core/completions "cl.se" {}) =>
    (contains [{:candidate "clojure.set", :type :namespace}])

    ;; Test for not required namespaces
    (core/completions "cl.test.ta" {}) =>
    (just [{:type :namespace, :candidate "clojure.test.tap"}])

    ;; Test for aliases
    (core/completions "cor" {})
    => (contains [{:type :namespace, :candidate "core"}])

    (core/completions "clojure.lang.Lisp" {}) =>
    (contains [{:type :class, :candidate "clojure.lang.LispReader"}])

    (core/completions "java.net.URLE" {}) =>
    (contains [{:type :class, :candidate "java.net.URLEncoder"}])

    (compliment.core/completions "RuntimeE" {})
    => (just [{:package "java.lang", :type :class, :candidate "RuntimeException"}])

    (core/completions ".getName" {}) =>
    (contains #{{:candidate ".getName", :type :method}
                {:candidate ".getSimpleName", :type :method}} :gaps-ok)

    (compliment.core/completions ".getName" {:ns 'compliment.t-core})
    => (contains [{:candidate ".getName", :type :method}])

    (core/completions "Integer/SI" {})
    => (just [{:type :static-field, :candidate "Integer/SIZE"}])

    (core/completions "Integer/co" {})
    => (contains [{:candidate "Integer/compare", :type :static-method}])

    (core/completions "recu" {})
    => (contains [{:candidate "recur", :type :special-form}])

    (core/completions "tru" {})
    => (contains [{:candidate "true", :type :special-form}])

    (core/completions "ba" {:context "(defn foo [bar baz] (+ 1 __prefix__))"})
    => (contains #{{:candidate "bar", :type :local} {:candidate "baz", :type :local}})

    (core/completions ":argl" {})
    => (contains [{:candidate ":arglists", :type :keyword}]))

  (fact ":plain-candidates true returns plain strings"
    (core/completions "bound" {:plain-candidates true})
    => (contains #{"bound?" "bound-fn" "bound-fn*"})

    (core/completions "fac" {:ns (find-ns 'midje.sweet) :plain-candidates true})
    => (just ["fact" "facts" "fact-group"]))

  (fact "extra-metadata arglists"
    (core/completions "apply" {:extra-metadata #{:arglists}}) =>
    (contains #{{:ns "clojure.core", :type :function, :candidate "apply", :arglists '("[f args]" "[f x args]" "[f x y args]" "[f x y z args]" "[f a b c d & args]")}}
              :gaps-ok))
  (fact "extra-metadata doc"
    (core/completions "bound" {:extra-metadata #{:doc}}) =>
    (contains #{(just {:ns "clojure.core", :type :function, :candidate "bound-fn*", :doc string?})
                (just {:ns "clojure.core", :type :macro, :candidate "bound-fn", :doc string?})} :gaps-ok)))

(facts "about documentation"
  (fact "`documentation` takes a symbol string which presumably can be
  resolved to something that has a docstring."
    (core/documentation "reduce")         => (every-pred string? not-empty)
    (core/documentation ".suspend")       => (every-pred string? not-empty)
    (core/documentation "jus.t g:arbage") => "")

  (fact "don't break on empty input"
    (core/documentation "") => ""))
