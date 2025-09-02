(ns compliment.sources
  "Tools for defining sources for the completion."
  (:import java.util.Comparator))

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

;;;; Candidate sorting

^{:lite nil}
(defmacro ^:private first-non-zero [v & rst]
  (if (seq rst)
    `(let [v# ~v]
       (if (zero? v#) (first-non-zero ~@rst) v#))
    v))

^{:lite nil}
(def ^:private by-name-comparator
  "Sorts list of candidates by their priority first and then alphabetically."
  (reify Comparator
    (compare [_ c1 c2]
      (let [^String s1 (:candidate c1)
            ^String s2 (:candidate c2)]
        ;; Treat nil priority as very high - we want nils at the end of the list.
        (first-non-zero (Integer/compare (:priority c1 999) (:priority c2 999))
                        (.compareTo ^String s1 s2))))))

^{:lite nil}
(def ^:private by-length-comparator
  "Sorts list of candidates by their priority first, then by length of candidate,
  and then alphabetically."
  (reify Comparator
    (compare [_ c1 c2]
      (let [^String s1 (:candidate c1)
            ^String s2 (:candidate c2)]
        ;; Treat nil priority as very high - we want nils at the end of the list.
        (first-non-zero (Integer/compare (:priority c1 999) (:priority c2 999))
                        (Integer/compare (.length s1) (.length s2))
                        (.compareTo ^String s1 s2))))))

^{:lite nil}
(defn sort-candidates
  "Sort `candidates` according to the provided `sort-order` keyword."
  [candidates sort-order]
  (sort (case sort-order
          :by-name by-name-comparator
          :by-length by-length-comparator)
        candidates))
