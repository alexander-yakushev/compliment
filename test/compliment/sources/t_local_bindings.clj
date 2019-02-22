(ns compliment.sources.t-local-bindings
  (:require [fudje.sweet :refer :all]
            [clojure.test :refer :all]
            [compliment.sources.local-bindings :as src]
            [compliment.context :as ctx]
            [compliment.t-helpers :refer :all]))

(deftest local-bindings
  (fact "local bindings are looked for in the context inside let-like forms"
    (src/candidates "" *ns* (ctx/parse-context '(let [a 1, b 2] __prefix__)))
    => (just [{:candidate "a", :type :local} {:candidate "b", :type :local}] :in-any-order)

    (strip-tags (src/candidates "f-ba" *ns* (ctx/parse-context
                                             '(when-let [foo-bar 10, foo-baz 20, bar-baz 30]
                                                __prefix__))))
    => (just ["foo-bar" "foo-baz"] :in-any-order)

    (strip-tags (src/candidates "it" *ns* (ctx/parse-context
                                           '(for [item (map inc items), part item]
                                              __prefix__))))
    => (just ["item"])

    (strip-tags (src/candidates "" *ns* (ctx/parse-context
                                         '(with-open [f (io/reader file)]
                                            __prefix__))))
    => (just ["f"])

    (strip-tags (src/candidates "" *ns* (ctx/parse-context
                                         '(dotimes [i 10]
                                            __prefix__))))
    => (just ["i"]))

  (fact "inside defn and defmacro forms the name and the arglist is returned"
    (strip-tags (src/candidates "" *ns* (ctx/parse-context
                                         '(defmacro amacro [bindings & body] __prefix__))))
    => (just ["amacro" "bindings" "body"] :in-any-order)

    (strip-tags (src/candidates "" *ns* (ctx/parse-context
                                         '(defn afunction "docstring" {:meta data}
                                            [foo bar & rest] __prefix__))))
    => (just ["afunction" "foo" "bar" "rest"] :in-any-order)

    (strip-tags (src/candidates "" *ns* (ctx/parse-context
                                         '(defn multiarg-fn "docstring"
                                            ([arg] (multiarg-fn arg nil))
                                            ([arg1 arg2] (do-stuff __prefix__))))))
    => (just ["multiarg-fn" "arg" "arg1" "arg2"] :in-any-order))

  (fact "letfn is supported"
    (strip-tags (src/candidates "" *ns* (ctx/parse-context
                                         '(letfn [(local-fn [foo bar & rest] __prefix__)
                                                  (f-2 ([[a b]] a) ([c] c))]))))
    => (just ["local-fn" "foo" "bar" "rest" "f-2" "a" "b" "c"] :in-any-order))

  (fact "as-> is supported"
    (strip-tags (src/candidates "" *ns* (ctx/parse-context
                                         '(as-> (+ 1 2) number
                                            (even? number) __prefix__))))
    => (just ["number"]))

  (fact "destructuring is also supported"
    (strip-tags (src/candidates "" *ns* (ctx/parse-context
                                         '(let [foo 42,
                                                [bar baz] lst
                                                {a :a, {b :b :as c} :b, [d] :d} m
                                                {:keys [key1 key2]} m2
                                                [_ rec {urs :ive :as total}] val]
                                            __prefix__))))
    => (just ["foo" "bar" "baz" "a" "b" "c" "d" "key1" "key2"
              "rec" "urs" "total"] :in-any-order))

  (fact "in doseq and for :let bindings are supported"
    (strip-tags (src/candidates "" *ns* (ctx/parse-context
                                         '(doseq [a b
                                                  :let [c (first a)]
                                                  {:keys [d]} e
                                                  :let [{g :g} f, [h i] j]]
                                            __prefix__))))
    => (just ["a" "c" "d" "g" "h" "i"] :in-any-order))

  (fact "bindings are scanned recursively"
    (strip-tags (src/candidates "" *ns* (ctx/parse-context
                                         '(defn afunction [arg1 arg2]
                                            (distinct (let [foo 13, arg2 14]
                                                        __prefix__))))))
    => (just ["afunction" "arg1" "arg2" "foo"] :in-any-order))

  (fact "bindings will be completed even if the form is unfinished"
    (strip-tags (src/candidates "" *ns* (ctx/parse-context
                                         (#'ctx/safe-read-context-string
                                          "(let [foo 42, [bar baz] 17, qux __prefix__"))))
    => (just ["foo" "bar" "baz" "qux"] :in-any-order))

  (fact "source silently fails if context is malformed"
    (src/candidates "" *ns* "(let __prefix__)") => []
    (src/candidates "" *ns* "(let [() 1]__prefix__)") => []
    (src/candidates "" *ns* "(defn [args] \"doc\" x (__prefix__))") => []
    (src/candidates "" *ns* "(defn resources
                               \"Build api functions for resources\"
                               [{:keys [resources] :as discovery-doc}]
                               (for [[_ {:keys __prefix__}] resources]
                                 (generate-schema s)))") => []))
