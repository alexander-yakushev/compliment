(ns compliment.sources.keywords
  "Completion for keywords interned globally across the application"
  (:require [compliment.sources :refer [defsource]]
            [compliment.utils :refer [fuzzy-matches-multi? resolve-namespace]])
  (:import java.lang.reflect.Field))

(def ^:private keywords-table
  (delay
    (let [^Field field (.getDeclaredField clojure.lang.Keyword "table")]
      (.setAccessible field true)
      (.get field nil))))

(defn- tagged-candidate [c]
  {:candidate c, :type :keyword})

(defn fuzzy-kw-matches?
  "Tests if prefix partially matches a keyword."
  [prefix kw]
  (fuzzy-matches-multi? prefix kw #{\/ \-}))

(defn qualified-candidates
  "Returns a list of namespace-qualified double-colon keywords (like ::foo)
  resolved for the given namespace."
  [prefix ns]
  (let [prefix (subs prefix 2)
        ns-name (str ns)]
    (for [[kw _] @keywords-table
          :when (= (namespace kw) ns-name)
          :when (.startsWith (name kw) prefix)]
      (tagged-candidate (str "::" (name kw))))))

(defn namespace-alias-candidates
  "Returns a list of namespace aliases prefixed by double colon required in the
  given namespace."
  [prefix ns]
  (let [prefix (subs prefix 2)]
    (for [[alias _] (ns-aliases ns)
          :let [aname (name alias)]
          :when (.startsWith aname prefix)]
      (tagged-candidate (str "::" aname)))))

(defn aliased-candidates
  "Returns a list of alias-qualified double-colon keywords (like ::str/foo),
  where alias has to be registered in the given namespace."
  [prefix ns]
  (when-let [[_ alias prefix] (re-matches #"::([^/]+)/(.*)" prefix)]
    (let [alias-ns-name (str (resolve-namespace (symbol alias) ns))]
      (for [[kw _] @keywords-table
            :when (= (namespace kw) alias-ns-name)
            :when (.startsWith (name kw) prefix)]
        (tagged-candidate (str "::" alias "/" (name kw)))))))

(defn ^{:lite 'keyword-candidates} candidates
  [^String prefix, ns _]
  (let [single-colon? (.startsWith prefix ":")
        double-colon? (.startsWith prefix "::")
        has-slash? (> (.indexOf prefix "/") -1)]
    (cond (and double-colon? has-slash?) (aliased-candidates prefix ns)
          double-colon? (concat (qualified-candidates prefix ns)
                                (namespace-alias-candidates prefix ns))
          single-colon? (for [[kw _] @keywords-table
                              :when (fuzzy-kw-matches? (subs prefix 1) (str kw))]
                          (tagged-candidate (str ":" kw))))))

^{:lite '(defsource :compliment.lite/keywords :candidates #'keyword-candidates)}
(defsource ::keywords
  :candidates #'candidates
  :doc (constantly nil))
