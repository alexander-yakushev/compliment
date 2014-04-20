(ns compliment.t-context
  (:require [midje.sweet :refer :all]
            [compliment.context :as ctx]
            ))

;; This namespace test only context parsing. For testing usage of
;; context see sources test files.

(facts "about context parsing"
  (fact "prefix placeholder in a context represents the location in
  the code form from where the completion was initiated"
    ctx/prefix-placeholder => '__prefix__)

  (fact "`parse-context` takes a clojure code and turns it inside out
  respective to the __prefix__"
    (ctx/parse-context '(dotimes [i 10] (__prefix__ foo i)))
    => '({:idx 0, :form (__prefix__ foo i)}
         {:idx 2, :form (dotimes [i 10] (__prefix__ foo i))})

    (ctx/parse-context '(ns (:import java.io.File)
                          (:use [clojure.string :only [reverse __prefix__]])))
    => '({:idx 1, :form [reverse __prefix__]}
         {:idx 2, :form [clojure.string :only [reverse __prefix__]]}
         {:idx 1, :form (:use [clojure.string :only [reverse __prefix__]])}
         {:idx 2, :form (ns (:import java.io.File)
                          (:use [clojure.string :only [reverse __prefix__]]))}))

  (fact "on each level in lists and vectors :idx field shows the
  position in the outer form of either __prefix__ itself or the form
  that contains __prefix__"
    (ctx/parse-context '(dotimes [i 10] ((foo __prefix__ bar) i)))
    => #(and (= (:idx (first %)) 1)  ; in (foo __prefix__ bar)
             (= (:idx (second %)) 0) ; in ((foo __prefix__ bar) i)
             (= (:idx (nth % 2)) 2)) ; in top-level form
    )

  (fact "map literals are replaced by (compliment-hashmap ...) lists
  by the client and replaced back by Compliment. This is done because
  Clojure reader would fail if map contained odd number of elements.
  If it is the case, nil is appended to make the elements even."
    (ctx/parse-context '(compliment-hashmap :foo __prefix__ :bar))
    => '({:idx :foo, :map-role :value, :form {:foo __prefix__, :bar nil}}))

  (fact "in maps :map-role shows which position the __prefix__ (or
  form with it) is in the map, and :idx shows the opposite element of
  its key-value pair."
    (ctx/parse-context '(compliment-hashmap :akey (compliment-hashmap __prefix__ 42)))
    => #(and (= (:map-role (first %)) :key) ; in {__prefix__ 42}
             (= (:idx (first %)) 42)

             (= (:map-role (second %)) :value) ; in top-level form
             (= (:idx (second %)) :akey))))
