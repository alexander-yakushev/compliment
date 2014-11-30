(ns compliment.sources
  "Tools for defining sources for the completion.")

(def ^{:doc "Stores defined sources."
       :private true}
  sources (atom nil))

(defn all-sources
  "Returns the list of all completion sources, or the selected once specified by
  `source-kws`."
  ([] @sources)
  ([source-kws]
   (select-keys @sources source-kws)))

(defn defsource
  "Defines a source with the given name and argument map. Map must
  contain two keys - `:candidates` and `:doc`.

  Value of `:candidates`should be a function of prefix, namespace and
  context.

  Value of `:doc` latter should be a function of symbol name and
  namespace."
  [name & {:as kw-args}]
  {:pre [(every? kw-args [:candidates :doc])]}
  (swap! sources assoc name (assoc kw-args :enabled true)))
