(ns compliment.sources)

(def ^:private sources (atom nil))

(defn all-sources []
  @sources)

(defn defsource [name & {:as kw-args}]
  {:pre [(every? kw-args [:candidates :doc])]}
  (swap! sources assoc name (assoc kw-args :enabled true)))
