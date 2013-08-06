(ns compliment.sources.namespaces-and-classes
  (:use [compliment.sources :only [defsource]]
        [compliment.utils :only [parts-match?]]
        [clojure.string :only [split]]))

(defn nscl-symbol? [x]
  (re-matches #"[^\/\:\.][^\/\:]+" x))

(defn nscl-matches? [^String prefix, ^String namespace]
  (parts-match? (split prefix #"\.") (split namespace #"\.")))

(defn imported-classes [ns]
  (for [[_ ^Class val] (ns-map ns) :when (class? val)]
    (.getName val)))

(defn candidates
  "Returns a list of potential namespace and class completions for a
  given namespace."
  [^String prefix, ns context]
  (when (nscl-symbol? prefix)
    (let [has-dot (> (.indexOf prefix ".") -1)]
      (for [^String ns-str (concat (map (comp name ns-name) (all-ns))
                                   (imported-classes ns)
                                   (when (not has-dot)
                                     (map name (keys (ns-aliases ns)))))
            :when (if has-dot
                    (nscl-matches? prefix ns-str)
                    (.startsWith ns-str prefix))]
        ns-str))))

(defsource ::namespaces-and-classes
  :candidates #'candidates
  :doc (constantly nil))
