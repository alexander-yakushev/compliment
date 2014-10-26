(ns compliment.sources.t-local-bindings
  (:require [midje.sweet :refer :all]
            [compliment.sources.local-bindings :as src]
            [compliment.context :as ctx]))

(facts "about local bindings completion"
  (fact "local bindings are looked for in the context inside let-like forms"
    (src/candidates "" *ns* (ctx/parse-context '(let [a 1, b 2] __prefix__)))
    => (just #{"a" "b"})

    (src/candidates "f-ba" *ns* (ctx/parse-context
                                 '(when-let [foo-bar 10, foo-baz 20, bar-baz 30]
                                    __prefix__)))
    => (just #{"foo-bar" "foo-baz"})

    (src/candidates "it" *ns* (ctx/parse-context
                               '(for [item (map inc items), part item]
                                  __prefix__)))
    => (just #{"item"}))

  (fact "inside defn and defmacro forms the arglist is returned"
    (src/candidates "" *ns* (ctx/parse-context
                             '(defmacro amacro [bindings & body] __prefix__)))
    => (just #{"bindings" "body"})

    (src/candidates "" *ns* (ctx/parse-context
                             '(defn afunction "docstring" {:meta data}
                                [foo bar & rest] __prefix__)))
    => (just #{"foo" "bar" "rest"})

    (src/candidates "" *ns* (ctx/parse-context
                             '(defn multiarg-fn "docstring"
                                ([arg] (multiarg-fn arg nil))
                                ([arg1 arg2] (do-stuff __prefix__)))))
    => (just #{"arg" "arg1" "arg2"}))

  (fact "destructuring is also supported"
    (src/candidates "" *ns* (ctx/parse-context
                             '(let [foo 42,
                                    [bar baz] lst
                                    {a :a, {b :b :as c} :b, [d] :d} m
                                    {:keys [key1 key2]} m2
                                    [_ rec {urs :ive :as total}] val]
                                __prefix__)))
    => (just #{"foo" "bar" "baz" "a" "b" "c" "d" "key1" "key2" "rec" "urs" "total"}))

  (fact "bindings are scanned recursively"
    (src/candidates "" *ns* (ctx/parse-context
                             '(defn afunction [arg1 arg2]
                                (distinct (let [foo 13, arg2 14]
                                            __prefix__)))))
    => (just #{"arg1" "arg2" "foo"}))

  (fact "source silently fails if context is malformed"
    (src/candidates "" *ns* "(let __prefix__)") => empty?
    (src/candidates "" *ns* "(defn [args] \"doc\" x (__prefix__))") => empty?
    (src/candidates "" *ns* "(defn resources
                               \"Build api functions for resources\"
                               [{:keys [resources] :as discovery-doc}]
                               (for [[_ {:keys __prefix__}] resources]
                                 (generate-schema s)))") => empty?))
