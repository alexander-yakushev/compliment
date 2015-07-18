(ns compliment.sources.keywords
  "Completion for keywords interned globally across the application"
  (:require [compliment.sources :refer [defsource]]
            [compliment.utils :refer [defmemoized resolve-namespace]])
  (:import java.lang.reflect.Field))

(defmemoized ^:private keywords-table
  []
  (let [^Field field (.getDeclaredField clojure.lang.Keyword "table")]
    (.setAccessible field true)
    (.get field nil)))

(defn qualified-candidates
  "Returns a list of namespace-qualified double-colon keywords (like ::foo)
  resolved for the given namespace."
  [prefix ns]
  (let [prefix (subs prefix 2)
        ns-name (str ns)]
    (for [[kw _] (keywords-table)
          :when (= (namespace kw) ns-name)
          :when (.startsWith (name kw) prefix)]
      (str "::" (name kw)))))

(defn aliased-candidates
  "Returns a list of alias-qualified double-colon keywords (like ::str/foo),
  where alias has to be registered in the given namespace."
  [prefix ns]
  (let [[_ alias prefix] (re-matches #"::([^/]+)/(.*)" prefix)
        alias-ns-name (str (resolve-namespace (symbol alias) ns))]
    (for [[kw _] (keywords-table)
          :when (= (namespace kw) alias-ns-name)
          :when (.startsWith (name kw) prefix)]
      (str "::" alias "/" (name kw)))))

(defn candidates
  [^String prefix, ns _]
  (let [single-colon? (.startsWith prefix ":")
        double-colon? (.startsWith prefix "::")
        has-slash? (> (.indexOf prefix "/") -1)]
    (cond (and double-colon? has-slash?) (aliased-candidates prefix ns)
          double-colon? (qualified-candidates prefix ns)
          single-colon? (for [[kw _] (keywords-table)
                              :when (.startsWith (str kw) (subs prefix 1))]
                          (str ":" kw)))))

(defsource ::keywords
  :candidates #'candidates
  :doc (constantly nil)
  :tag-fn (fn [m _] (assoc m :type :keyword)))
