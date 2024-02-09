(ns compliment.t-context
  (:require [fudje.sweet :refer :all]
            [clojure.test :refer :all]
            [compliment.context :as ctx]))

;; This namespace tests only context parsing. For testing usage of
;; context see sources test files.

(deftest context-reading
  (testing "safe-read-context-string takes clojure code in a string and
  reads it into clojure data structures"
    (is (= '(__prefix__ foo [bar] :baz "with strings")
           (#'ctx/safe-read-context-string "(__prefix__ foo [bar] :baz \"with strings\")"))))

  (testing "aliased namespace keyword is replaced to non-aliased namespace keyword"
    (is (= '(__prefix__ foo [bar] :my-ns/baz "with strings")
           (#'ctx/safe-read-context-string "(__prefix__ foo [bar] ::my-ns/baz \"with strings\")")))
    (is (= '(__prefix__ foo [bar] :my.full.namespace/baz "with strings")
           (do
             (alias 'my-ns (create-ns 'my.full.namespace))
             (#'ctx/safe-read-context-string "(__prefix__ foo [bar] ::my-ns/baz \"with strings\")")))))

  (testing "'{' and '}' is replaced to '(char 123)' and '(char 125)' respectively"
    (is (= '(__prefix__ foo [bar] (or (= bar (char 123)) (= bar (char 125))))
           (#'ctx/safe-read-context-string "(__prefix__ foo [bar] (or (= bar \\{) (= bar \\}))))"))))

  (testing "maps with odd number of elements are also handled"
    (is (= '{:foo bar, __prefix__ nil}
           (#'ctx/safe-read-context-string "{:foo bar __prefix__}"))))
  
  (testing "reader conditionals are parsable"
    (is (= '(defn a [] ^{:reader-conditional true} (:cljs [1 2 3 __prefix__]))
           (#'ctx/safe-read-context-string "(defn a [] #?(:cljs [1 2 3 __prefix__]))")))
    
    (is (= '(defn a [] ^{:splicing-reader-conditional true} (:cljs [1 2 3 __prefix__]))
           (#'ctx/safe-read-context-string "(defn a [] #?@(:cljs [1 2 3 __prefix__]))"))))

(deftest context-parsing
  (fact "prefix placeholder in a context represents the location in
  the code form from where the completion was initiated"
    ctx/prefix-placeholder => '__prefix__)

  (fact "`parse-context` takes a clojure code and turns it inside out
  respective to the __prefix__"
    (ctx/parse-context '(dotimes [i 10] (__prefix__ foo i)))
    => '({:idx 0, :form (__prefix__ foo i)}
         {:idx 2, :form (dotimes [i 10] (__prefix__ foo i))})

    (ctx/parse-context '(ns test
                          (:import java.io.File)
                          (:use [clojure.string :only [reverse __prefix__]])))
    => '({:idx 1, :form [reverse __prefix__]}
         {:idx 2, :form [clojure.string :only [reverse __prefix__]]}
         {:idx 1, :form (:use [clojure.string :only [reverse __prefix__]])}
         {:idx 3, :form (ns test
                          (:import java.io.File)
                          (:use [clojure.string :only [reverse __prefix__]]))}))

  (fact "on each level in lists and vectors :idx field shows the
  position in the outer form of either __prefix__ itself or the form
  that contains __prefix__"
    (ctx/parse-context '(dotimes [i 10] ((foo __prefix__ bar) i)))
    => (checker #(and (= (:idx (first %)) 1)    ; in (foo __prefix__ bar)
                      (= (:idx (second %)) 0)   ; in ((foo __prefix__ bar) i)
                      (= (:idx (nth % 2)) 2)))  ; in top-level form
    )

  (fact "broken map literals are fixed before they get to parse-context"
    (ctx/parse-context (#'ctx/safe-read-context-string "{:foo __prefix__ :bar}"))
    => '({:idx :foo, :map-role :value, :form {:foo __prefix__, :bar nil}}))

  (fact "failing to parse an unfinished form will try to recover by completing it"
    (ctx/parse-context (#'ctx/safe-read-context-string "(let [a {:b 1}, c {__prefix__"))
    => '({:idx nil, :map-role :key, :form {__prefix__ nil}}
         {:idx 3, :form [a {:b 1} c {__prefix__ nil}]}
         {:idx 1, :form (let [a {:b 1} c {__prefix__ nil}])}))

  (fact "in maps :map-role shows which role in key-value pair the
  __prefix__ (or the form with it) has, and :idx shows the opposite
  element of its key-value pair."
    (ctx/parse-context '{:akey {__prefix__ 42}})
    => (checker #(and (= (:map-role (first %)) :key) ; in {__prefix__ 42}
                      (= (:idx (first %)) 42)

                      (= (:map-role (second %)) :value) ; in top-level form
                      (= (:idx (second %)) :akey)))))
