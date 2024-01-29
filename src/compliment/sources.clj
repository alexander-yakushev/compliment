(ns compliment.sources
  "Tools for defining sources for the completion.")

(def ^:private sources "Stores defined sources." (atom {}))

(defn all-sources
  "Returns the list of all completion sources, or the selected once specified by
  `source-kws`."
  ([] @sources)
  ([source-kws]
   (select-keys @sources source-kws)))

(defn defsource
  "Define a source with the given name and completion functions:
  `:candidates` - a function of prefix, namespace and context;
  `:doc` - a function of symbol name and namespace."
  [name & {:keys [candidates doc] :as kw-args}]
  {:pre [^{:lite 'candidates} (and candidates doc)]}
  (swap! sources assoc name (assoc kw-args :enabled true)))
