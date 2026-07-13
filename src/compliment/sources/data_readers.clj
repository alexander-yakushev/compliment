(ns compliment.sources.data-readers
  "Completion for data reader tags (like #inst and #uuid)."
  (:require [compliment.sources :refer [defsource]]))

(defn data-reader-candidates
  [^String prefix, _ _]
  (when-let [[_ prefix] (re-matches #"#(.*)" prefix)]
    (for [dr (concat (keys default-data-readers) (keys *data-readers*))
          :let [drn (name dr)]
          :when (.startsWith drn prefix)]
      {:candidate (str "#" drn), :type :data-reader})))

^{:lite '(defsource :compliment.lite/data-readers :candidates #'data-reader-candidates)}
(defsource ::data-readers
  :candidates #'data-reader-candidates
  :doc (constantly nil))
