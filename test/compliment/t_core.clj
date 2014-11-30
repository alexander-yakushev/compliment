(ns compliment.t-core
  (:require [midje.sweet :refer :all]
            [compliment.core :as core]
            [compliment.context :as ctx]))

;; This namespace contains only sanity checks for the public API. For
;; in-depth source testing see their respective test files.

(facts "about completion"
  (fact "`completions` takes a prefix, and optional options-map."
    (core/completions "redu")
    => (contains ["reduce" "reduce-kv" "reductions"] :gaps-ok)

    (core/completions "fac" {:ns (find-ns 'midje.sweet)})
    => (just #{"fact-group" "fact" "facts"})

    (core/completions "fac" {:ns (find-ns 'clojure.core)})
    => ()

    (core/completions "compliment.core/co")
    => (just ["compliment.core/completions"])

    (core/completions "core/doc")
    => (just ["core/documentation"]))

  (fact "in case of non-existing namespace doesn't fail"
    (core/completions "redu" {:ns nil}) => anything
    (core/completions "n-m" {:ns 'foo.bar.baz}) => anything)

  (fact "many sources allow some sort of fuzziness in prefixes"
    (core/completions "re-me")
    => (just #{"remove-method" "reset-meta!" "remove-all-methods"})

    (core/completions "remme")
    => (just [#"remove-method" "remove-all-methods"])

    (core/completions "cl.co.")
    => (contains #{"clojure.core.protocols" "clojure.core.unify"} :gaps-ok)

    (core/completions "clcop")
    => ["clojure.core.protocols"]

    (core/completions ".gSV")
    => (just #{".getSpecificationVersion" ".getSpecificationVendor"}))

  (fact "candidates are sorted by their length first, and then alphabetically"
    (core/completions "map")
    => (contains ["map" "map?" "mapv" "mapcat" "map-indexed"])

    (core/completions "al-")
    => ["all-ns" "alter-meta!" "alter-var-root"])

  (fact "context can help some sources to give better candidates list"
    (def a-str "a string")
    (core/completions ".sub" {:context "(__prefix__ a-str)"})
    => (just #{".subSequence" ".substring"})

    (def a-big-int 42M)
    (core/completions ".sub" {:context "(__prefix__ a-big-int)"})
    => [".subtract"])

  (fact ":sources list can filter the sources to be used during completion"
    (core/completions "cl")
    => #(> (count %) 10)

    (core/completions "cl" {:sources [:compliment.sources.ns-mappings/ns-mappings]})
    => (just #{"class" "class?" "clojure-version" "clear-agent-errors"})))

(facts "about documentation"
  (fact "`documentation` takes a symbol string which presumably can be
  resolved to something that has a docstring.")
  (core/documentation "reduce")         => (every-pred string? not-empty)
  (core/documentation ".suspend")       => (every-pred string? not-empty)
  (core/documentation "jus.t g:arbage") => "")
