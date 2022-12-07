(ns compliment.sources
  "Tools for defining sources for the completion.")

(def ^{:doc "Stores defined sources."
       :private true}
  sources (atom nil))

(defn ^:private filter-vals [pred a-map]
  (into {}
        (map (fn [[k v]] (when (pred v)
                           [k v])))
        a-map))

(defn all-sources
  "Returns the list of all completion sources, or the selected once specified by
  `source-selector`.

  `source-selector` is either a list of source keywords or a predicate function
  operating over source map"
  ([] @sources)
  ([source-selector]
   (if (fn? source-selector)
     (filter-vals source-selector @sources)
     (select-keys @sources source-selector))))

(defn defsource
  "Defines a source with the given name and argument map. Map must
  contain two keys - `:candidates` and `:doc`.

  Value of `:candidates`should be a function of prefix, namespace and
  context.

  Value of `:doc` latter should be a function of symbol name and
  namespace."
  [name & {:as kw-args}]
  {:pre [(every? kw-args [:candidates :doc])]}
  (swap! sources assoc name (assoc kw-args :name name :enabled true)))
