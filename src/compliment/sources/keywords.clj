(ns compliment.sources.keywords
  "Completion for keywords interned globally across the application"
  (:require [compliment.sources :refer [defsource]]
            [compliment.utils :refer [defmemoized]])
  (:import java.lang.reflect.Field))

(defmemoized ^:private keywords-table
  []
  (let [^Field field (.getDeclaredField clojure.lang.Keyword "table")]
    (.setAccessible field true)
    (.get field nil)))

(defn candidates
  [prefix _ _]
  (when (= (first prefix) \:)
    (for [[kw _] (keywords-table)
          :when (.startsWith (str kw) (subs prefix 1))]
      (str ":" kw))))

(defsource ::keywords
  :candidates #'candidates
  :doc (constantly nil))
