(ns compliment.t-core
  (:require [fudje.sweet :refer :all]
            [clojure.test :refer :all]
            [compliment.core :as core]
            [compliment.context :as ctx]
            [compliment.t-helpers :refer :all]))

;; This namespace contains only sanity checks for the public API. For
;; in-depth source testing see their respective test files.

(deftest completions-test
  (fact "`completions` takes a prefix, and optional options-map."
    (strip-tags (core/completions "redu"))
    => (contains ["reduce" "reduce-kv" "reductions"] :gaps-ok)

    (strip-tags (core/completions "fac" {:ns (find-ns 'fudje.sweet)}))
    => (just ["fact" "facts"] :in-any-order)

    (strip-tags (core/completions "fac" {:ns (find-ns 'clojure.core)}))
    => []

    (strip-tags (core/completions "compliment.core/co"))
    => (just ["compliment.core/completions"])

    (strip-tags (core/completions "core/doc" {:ns (find-ns 'compliment.t-core)}))
    => (just ["core/documentation"]))

  (fact "ClojureScript default candidates should come from cljs.core"
    (core/completions "" {:sources [:compliment.sources.cljs/clojurescript]})
    => (checker (fn [cs] (every? #(= "cljs.core" (:ns %)) cs))))

  (fact "in case of non-existing namespace doesn't fail"
    (core/completions "redu" {:ns nil}) => anything
    (core/completions "n-m" {:ns 'foo.bar.baz}) => anything)

  (fact "many sources allow some sort of fuzziness in prefixes"
    (strip-tags (core/completions "re-me"))
    => (just ["reset-meta!" "remove-method" "remove-all-methods"] :in-any-order)

    (strip-tags (core/completions "remme"))
    => (just ["remove-method" "remove-all-methods"] :in-any-order)

    (strip-tags (core/completions "cl.co."))
    => (contains #{"clojure.core.protocols" "clojure.core.server"} :gaps-ok)

    (strip-tags (core/completions "cji"))
    => (just ["clojure.java.io"])

    (strip-tags (core/completions ".gSV"))
    => (just [".getSpecificationVendor" ".getSpecificationVersion"] :in-any-order))

  (fact "candidates are sorted by their length first, and then alphabetically"
    (strip-tags (core/completions "map"))
    => (contains ["map" "map?" "mapv" "mapcat"] :gaps-ok)

    (strip-tags (core/completions "al-"))
    => (contains ["all-ns" "alter-meta!" "alter-var-root"] :gaps-ok))

  (fact "sorting directly by name can also be enabled"
    (strip-tags (core/completions "map" {:sort-order :by-name}))
    => (contains ["map-indexed" "map?" "mapcat" "mapv"] :gaps-ok)

    (core/completions "remo" {:sort-order :by-name})
    => (contains [{:ns "clojure.core", :type :function, :candidate "remove-method"}
                  {:ns "clojure.core", :type :function, :candidate "remove-ns"}
                  {:ns "clojure.core", :type :function, :candidate "remove-watch"}]
                 :gaps-ok))


  (fact "context can help some sources to give better candidates list"
    (do (def a-str "a string")
        (strip-tags (core/completions ".sub" {:context "(__prefix__ a-str)"
                                              :ns 'compliment.t-core})))
    => (just [".substring" ".subSequence"] :in-any-order)

    (do (def a-big-int 42M)
        (strip-tags (core/completions ".sub" {:context "(__prefix__ a-big-int)"
                                              :ns 'compliment.t-core})))
    => (just [".subtract"]))

  (fact ":sources list can filter the sources to be used during completion"
    (core/completions "cl")
    => (checker #(> (count %) 10))

    (strip-tags (core/completions "cl" {:sources [:compliment.sources.ns-mappings/ns-mappings]}))
    => (just ["class" "class?" "clojure-version" "clear-agent-errors"] :in-any-order))

  (fact "different metadata is attached to candidates"
    (core/completions "bound" {}) =>
    (contains #{{:ns "clojure.core", :type :function, :candidate "bound-fn*"}
                {:ns "clojure.core", :type :macro, :candidate "bound-fn"}} :gaps-ok)

    (core/completions "fac" {:ns (find-ns 'fudje.sweet)})
    => (just [{:candidate "fact", :type :macro, :ns "fudje.sweet"}
              {:candidate "facts", :type :macro, :ns "fudje.sweet"}]
             :in-any-order)

    (core/completions "a-big-" {:ns 'compliment.t-core})
    => (just [{:ns "compliment.t-core", :type :var, :candidate "a-big-int"}])

    (core/completions "cl.se" {}) =>
    (contains [{:candidate "clojure.set", :type :namespace}])

    ;; Test for not required namespaces
    (core/completions "cl.test.ta" {}) =>
    (just [{:type :namespace, :candidate "clojure.test.tap"}])

    ;; Test for aliases
    (core/completions "cor" {:ns 'compliment.t-core})
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

    (core/completions "fac" {:ns (find-ns 'fudje.sweet) :plain-candidates true})
    => (just ["fact" "facts"] :in-any-order))

  (fact "extra-metadata arglists"
    (core/completions "apply" {:extra-metadata #{:arglists}})
    => (contains #{{:ns "clojure.core", :type :function, :candidate "apply", :arglists '("[f args]" "[f x args]" "[f x y args]" "[f x y z args]" "[f a b c d & args]")}}
                 :gaps-ok))

  (fact "extra-metadata doc"
    (map #(select-keys % [:ns :type :candidate]) (core/completions "bound" {:extra-metadata #{:doc}}))
    => (contains #{{:ns "clojure.core", :type :function, :candidate "bound-fn*"}
                   {:ns "clojure.core", :type :macro, :candidate "bound-fn"}}
                 :gaps-ok)

    (map :doc (core/completions "bound" {:extra-metadata #{:doc}}))
    => (checker #(every? string? %))))

(deftest documentation-test
  (fact "`documentation` takes a symbol string which presumably can be
  resolved to something that has a docstring."
    (core/documentation "reduce")         => (checker (every-pred string? not-empty))
    (core/documentation ".suspend")       => (checker (every-pred string? not-empty))
    (core/documentation "jus.t g:arbage") => "")

  (fact "don't break on empty input"
    (core/documentation "") => ""))
