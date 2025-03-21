(ns compliment.t-context
  (:require [clojure.test :refer :all]
            [clojure.walk :as walk]
            [compliment.context :as ctx]
            [compliment.t-helpers :refer :all]))

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
           (#'ctx/safe-read-context-string "(defn a [] #?@(:cljs [1 2 3 __prefix__]))")))))

(deftest context-parsing
  (testing "prefix placeholder in a context represents the location in
  the code form from where the completion was initiated"
    (is (= '__prefix__ ctx/prefix-placeholder)))

  (testing "`parse-context` takes a clojure code and turns it inside out
  respective to the __prefix__"
    (is? '({:idx 0, :form (__prefix__ foo i)}
           {:idx 2, :form (dotimes [i 10] (__prefix__ foo i))})
         (ctx/parse-context '(dotimes [i 10] (__prefix__ foo i))))

    (is? '({:idx 1, :form [reverse __prefix__]}
          {:idx 2, :form [clojure.string :only [reverse __prefix__]]}
          {:idx 1, :form (:use [clojure.string :only [reverse __prefix__]])}
          {:idx 3, :form (ns test
                           (:import java.io.File)
                           (:use [clojure.string :only [reverse __prefix__]]))})
         (ctx/parse-context '(ns test
                               (:import java.io.File)
                               (:use [clojure.string :only [reverse __prefix__]])))))

  (testing "on each level in lists and vectors :idx field shows the
  position in the outer form of either __prefix__ itself or the form
  that contains __prefix__"
    (is? '[{:idx 1, :form (foo __prefix__ bar)}
           {:idx 0, :form ((foo __prefix__ bar) i)}
           {:idx 2, :form (dotimes [i 10] ((foo __prefix__ bar) i))}]
         (ctx/parse-context '(dotimes [i 10] ((foo __prefix__ bar) i)))))

  (testing "broken map literals are fixed before they get to parse-context"
    (is? '({:idx :foo, :map-role :value, :form {:foo __prefix__, :bar nil}})
         (ctx/parse-context (#'ctx/safe-read-context-string "{:foo __prefix__ :bar}"))))

  (testing "hashset literals are handled correctly"
    ;; https://github.com/alexander-yakushev/compliment/issues/118
    (is? '({:idx 2, :form (remove (fn* [arg1]
                                      (some (compliment-hashset (:value arg1))))
                                 __prefix__)})
         (->> (ctx/parse-context
               (#'ctx/safe-read-context-string "(remove #(some #{(:value %)}) __prefix__)"))
              (walk/postwalk #(if (and (symbol? %) (.endsWith (name %) "#"))
                                'arg1
                                %)))))

  (testing "failing to parse an unfinished form will try to recover by completing it"
    (is? '({:idx nil, :map-role :key, :form {__prefix__ nil}}
           {:idx 3, :form [a {:b 1} c {__prefix__ nil}]}
           {:idx 1, :form (let [a {:b 1} c {__prefix__ nil}])})
         (ctx/parse-context (#'ctx/safe-read-context-string "(let [a {:b 1}, c {__prefix__"))))

  (testing "in maps :map-role shows which role in key-value pair the
  __prefix__ (or the form with it) has, and :idx shows the opposite
  element of its key-value pair."
    (is? '[{:idx 42, :map-role :key, :form {__prefix__ 42}}
           {:idx :akey, :map-role :value, :form {:akey {__prefix__ 42}}}]
         (ctx/parse-context '{:akey {__prefix__ 42}}))))
