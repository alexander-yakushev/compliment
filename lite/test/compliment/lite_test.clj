(ns compliment.lite-test
  (:require [fudje.sweet :refer :all]
            [clojure.test :refer :all]
            [compliment.lite :as lite :refer [completions]]))

(def a-big-int 42M)

(defn strip-tags
  "Returns a wrapper predicate that first removes tags from the list of
  candidates."
  [completions]
  (map :candidate completions))

(deftest completions-test
  (fact "`completions` takes a prefix, and optional options-map."
    (strip-tags (completions "redu"))
    => (contains ["reduce" "reduce-kv" "reductions"] :gaps-ok)

    (strip-tags (completions "'redu"))
    => (contains ["'reduce" "'reduce-kv" "'reductions"] :gaps-ok)

    (strip-tags (completions "#'redu"))
    => (contains ["#'reduce" "#'reduce-kv" "#'reductions"] :gaps-ok)

    (strip-tags (completions "fac" {:ns (find-ns 'fudje.sweet)}))
    => (just ["fact" "facts"] :in-any-order)

    (strip-tags (completions "fac" {:ns (find-ns 'clojure.core)}))
    => []

    (strip-tags (completions "compliment.lite/comp"))
    => (just ["compliment.lite/completions"])

    (strip-tags (completions "'compliment.lite/comp"))
    => (just ["'compliment.lite/completions"])

    (strip-tags (completions "#'compliment.lite/comp"))
    => (just ["#'compliment.lite/completions"])

    (strip-tags (completions "lite/com" {:ns (find-ns 'compliment.lite-test)}))
    => (just ["lite/completions"]))

  (fact "check that all lite sources work"
    (strip-tags (completions "map"))
    => (contains ["map" "map?" "mapv" "mapcat"] :in-any-order)

    (strip-tags (completions "clojure."))
    => (contains ["clojure.core" "clojure.edn" "clojure.string"] :gaps-ok :in-any-order)

    (strip-tags (completions ".sub"))
    => (contains [".subtract" ".substring" ".subSequence"] :in-any-order)

    (strip-tags (completions "Integer/"))
    => (contains ["Integer/MAX_VALUE" "Integer/MIN_VALUE"] :in-any-order)

    (strip-tags (completions ":req"))
    => (contains [":require"])

    (strip-tags (completions ":compliment.lite/k"))
    => (just [":compliment.lite/keywords"])

    (strip-tags (completions "catc"))
    => (contains ["catch"])

    (strip-tags (completions "ni"))
    => (contains ["nil"]))

  (fact "in case of non-existing namespace doesn't fail"
    (completions "redu" {:ns nil}) => anything
    (completions "n-m" {:ns 'foo.bar.baz}) => anything)

  (fact "many sources allow some sort of fuzziness in prefixes"
    (strip-tags (completions "re-me"))
    => (just ["reset-meta!" "remove-method" "remove-all-methods"] :in-any-order)

    (strip-tags (completions "remme"))
    => (just ["remove-method" "remove-all-methods"] :in-any-order)

    (strip-tags (completions "cl.co."))
    => (contains #{"clojure.core.protocols" "clojure.core.server"} :gaps-ok)

    (strip-tags (completions "cji"))
    => (just ["clojure.java.io"])

    (strip-tags (completions ".gSV"))
    => (just [".getSpecificationVendor" ".getSpecificationVersion"] :in-any-order))

  (fact "candidates are sorted by their length first, and then alphabetically"
    (strip-tags (completions "map"))
    => (contains ["map" "map?" "mapv" "mapcat"] :gaps-ok)

    (strip-tags (completions "al-"))
    => (contains ["all-ns" "alter-meta!" "alter-var-root"] :gaps-ok))

  (fact "sorting directly by name can also be enabled"
    (strip-tags (completions "map" {:sort-order :by-name}))
    => (contains ["map-indexed" "map?" "mapcat" "mapv"] :gaps-ok)

    (completions "remo" {:sort-order :by-name})
    => (contains [{:ns "clojure.core", :type :function, :candidate "remove-method"}
                  {:ns "clojure.core", :type :function, :candidate "remove-ns"}
                  {:ns "clojure.core", :type :function, :candidate "remove-watch"}]
                 :gaps-ok))

  (fact ":sources list can filter the sources to be used during completion"
    (completions "cl")
    => (checker #(> (count %) 10) {:ns 'compliment.lite-test})

    (strip-tags (completions "cl" {:sources [:compliment.lite/vars]
                                        :ns 'compliment.lite-test}))
    => (just ["class" "class?" "clojure-version" "clear-agent-errors"] :in-any-order))

  (fact "empty prefix returns a list of candidates"
    (completions "") => (checker not-empty))

  (fact "different metadata is attached to candidates"
    (completions "bound" {}) =>
    (contains #{{:ns "clojure.core", :type :function, :candidate "bound-fn*"}
                {:ns "clojure.core", :type :macro, :candidate "bound-fn"}} :gaps-ok)

    (completions "fac" {:ns (find-ns 'fudje.sweet)})
    => (just [{:candidate "fact", :type :macro, :ns "fudje.sweet"}
              {:candidate "facts", :type :macro, :ns "fudje.sweet"}]
             :in-any-order)

    (completions "a-big" {:ns 'compliment.lite-test})
    => (just [{:ns "compliment.lite-test", :type :var, :candidate "a-big-int"}])

    (completions "cl.se" {}) =>
    (contains [{:candidate "clojure.set", :type :namespace}])

    ;; Test for not required namespaces
    (completions "cl.test.ta" {}) =>
    (just [{:type :namespace, :candidate "clojure.test.tap" :file "clojure/test/tap.clj"}])

    ;; Test for aliases
    (completions "lit" {:ns 'compliment.lite-test})
    => (contains [{:type :namespace, :candidate "lite"}])

    (completions "clojure.lang.Lisp" {}) =>
    (contains [{:type :class, :candidate "clojure.lang.LispReader"}])

    (completions "java.net.URLE" {}) =>
    (contains [{:type :class, :candidate "java.net.URLEncoder"}])

    (completions "RuntimeE" {})
    => (contains #{{:package "java.lang", :type :class, :candidate "RuntimeException"}})

    (completions ".getName" {}) =>
    (contains #{{:candidate ".getName", :type :method}
                {:candidate ".getSimpleName", :type :method}} :gaps-ok)

    (completions ".getName" {:ns 'compliment.lite-test})
    => (contains [{:candidate ".getName", :type :method}])

    (completions "Thread/.int" {})
    => (just [{:candidate "Thread/.interrupt", :type :method}])

    (completions "Integer/SI" {})
    => (just [{:type :static-field, :candidate "Integer/SIZE"}])

    (completions "Integer/co" {})
    => (contains [{:candidate "Integer/compare", :type :static-method}])

    (completions "recu" {})
    => (contains [{:candidate "recur", :type :special-form}])

    (completions "tru" {})
    => (contains [{:candidate "true", :type :special-form}])

    (completions ":argl" {})
    => (contains [{:candidate ":arglists", :type :keyword}]))

  (fact ":plain-candidates true returns plain strings"
    (completions "bound" {:plain-candidates true})
    => (contains #{"bound?" "bound-fn" "bound-fn*"})

    (completions "fac" {:ns (find-ns 'fudje.sweet) :plain-candidates true})
    => (just ["fact" "facts"] :in-any-order))

  (fact "extra-metadata arglists"
    (completions "apply" {:extra-metadata #{:arglists}})
    => (contains #{{:ns "clojure.core", :type :function, :candidate "apply", :arglists '("[f args]" "[f x args]" "[f x y args]" "[f x y z args]" "[f a b c d & args]")}}
                 :gaps-ok))

  (fact "extra-metadata doc"
    (map #(select-keys % [:ns :type :candidate]) (completions "bound" {:extra-metadata #{:doc}}))
    => (contains #{{:ns "clojure.core", :type :function, :candidate "bound-fn*"}
                   {:ns "clojure.core", :type :macro, :candidate "bound-fn"}}
                 :gaps-ok)

    (map :doc (completions "bound" {:extra-metadata #{:doc}}))
    => (checker #(every? string? %))))
