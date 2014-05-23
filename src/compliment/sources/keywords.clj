(ns compliment.sources.keywords
  "Completion for keywords interned globally across the application"
  (:use [compliment.sources :only [defsource]])
  (:import java.lang.reflect.Field))

(def ^{:private true}
  keywords-table
  (memoize
   (fn []
     (let [^Field field (.getDeclaredField clojure.lang.Keyword "table")]
       (.setAccessible field true)
       (.get field nil)))))

(defn candidates
  [prefix _ _]
  (when (= (first prefix) \:)
    (for [[kw _] (keywords-table)
          :when (.startsWith (str kw) (subs prefix 1))]
      (str ":" kw))))

(defsource ::keywords
  :candidates #'candidates
  :doc (constantly nil))
