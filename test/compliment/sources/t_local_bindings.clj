(ns compliment.sources.t-local-bindings
  (:require [clojure.string :as string]
            [clojure.test :refer :all]
            [compliment.context :as ctx]
            [compliment.sources.local-bindings :as src]
            [compliment.t-helpers :refer :all]
            [fudje.sweet :refer :all]))

(defn- -ns [] (find-ns 'compliment.sources.t-local-bindings))

(def thread1
  "A thread identified as such by its class"
  (Thread.))

(def ^Thread thread2
  "A thread identified as such by its :tag"
  nil)

(deftest bindings-from-context
  (fact "The class of a given binding can be identified by the class of its bound value"
    (map (comp :tag meta)
         (src/bindings-from-context (ctx/parse-context '(let [a thread1] __prefix__))
                                    (-ns)))
    => (just [`Thread]))

  (fact "The class of a given binding can be identified by the :tag of its bound value"
    (map (comp :tag meta)
         (src/bindings-from-context (ctx/parse-context '(let [a thread2] __prefix__))
                                    (-ns)))
    => (just [`Thread]))

  (fact "The class of a given binding can be identified by the :tag of the var from an invocation"
    (map (comp :tag meta)
         (src/bindings-from-context (ctx/parse-context '(let [a (string/trim "a")] __prefix__))
                                    (-ns)))
    => (just [`String]))

  (fact "The class of a given binding can be identified by the class of a string literal"
    (map (comp :tag meta)
         (src/bindings-from-context (ctx/parse-context '(let [a ""] __prefix__))
                                    (-ns)))
    => (just [`String]))

  (fact "The class of a given binding can be identified by the class of a vector literal"
    (map (comp :tag meta)
         (src/bindings-from-context (ctx/parse-context '(let [a []] __prefix__))
                                    (-ns)))
    => (just ['clojure.lang.PersistentVector])))

(deftest local-bindings
  (defmacro ^{:completion/locals :let} like-let [& _])
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
    => (just ["i"])

    (strip-tags (src/candidates "" (-ns) (ctx/parse-context
                                          '(like-let [i 10]
                                                     __prefix__))))
    => (just ["i"]))

  (defmacro ^{:completion/locals :defn} like-defn [& _])
  (fact "inside defn and defmacro forms the name and the arglist is returned"
    (strip-tags (src/candidates "" *ns* (ctx/parse-context
                                         '(defmacro amacro [bindings & body] __prefix__))))
    => (just ["amacro" "bindings" "body"] :in-any-order)

    (strip-tags (src/candidates "" (-ns) (ctx/parse-context
                                          '(like-defn amacro [bindings & body] __prefix__))))
    => (just ["amacro" "bindings" "body"] :in-any-order)

    (strip-tags (src/candidates "" *ns* (ctx/parse-context
                                         '(defn afunction "docstring" {:meta data}
                                            [foo bar & rest] __prefix__))))
    => (just ["afunction" "foo" "bar" "rest"] :in-any-order)

    (strip-tags (src/candidates "" *ns* (ctx/parse-context
                                         '(defn multiarg-fn "docstring"
                                            ([arg] (multiarg-fn arg nil))
                                            ([arg1 arg2] (do-stuff __prefix__))))))
    => (just ["multiarg-fn" "arg" "arg1" "arg2"] :in-any-order)

    (strip-tags (src/candidates "" *ns* (ctx/parse-context
                                         '(defmethod multimethod "dispatch-val"
                                            [foo bar & rest] __prefix__))))
    => (just ["multimethod" "foo" "bar" "rest"]))

  (defmacro ^{:completion/locals :letfn} like-letfn [& _])
  (fact "letfn is supported"
    (strip-tags (src/candidates "" *ns* (ctx/parse-context
                                         '(letfn [(local-fn [foo bar & rest] __prefix__)
                                                  (f-2 ([[a b]] a) ([c] c))]))))
    => (just ["local-fn" "foo" "bar" "rest" "f-2" "a" "b" "c"] :in-any-order)

    (strip-tags (src/candidates "" (-ns) (ctx/parse-context
                                          '(like-letfn [(local-fn [foo bar & rest] __prefix__)
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
                                                {:keys [key1 key2] :strs [key3] :syms [key4]} m2
                                                {:keys [:a/key5 :b/key6 ::src/key7 ::src/key8]} m3
                                                {:c/keys [key9 key10] :c/syms [key11]} m4
                                                {::src/keys [key12 key13] ::src/syms [key14]} m5
                                                [_ rec {urs :ive :as total}] val]
                                            __prefix__))))
    => (just ["foo" "bar" "baz" "a" "b" "c" "d" "key1" "key2" "key3" "key4" "key5"
              "key6" "key7" "key8" "key9" "key10" "key11" "key12" "key13" "key14"
              "rec" "urs" "total"] :in-any-order))

  (defmacro ^{:completion/locals :doseq} like-doseq [& _])
  (fact "in doseq and for :let bindings are supported"
    (strip-tags (src/candidates "" *ns* (ctx/parse-context
                                         '(doseq [a b
                                                  :let [c (first a)]
                                                  {:keys [d]} e
                                                  :let [{g :g} f, [h i] j]]
                                            __prefix__))))
    => (just ["a" "c" "d" "g" "h" "i"] :in-any-order)

    (strip-tags (src/candidates "" (-ns) (ctx/parse-context
                                          '(like-doseq [a b
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
