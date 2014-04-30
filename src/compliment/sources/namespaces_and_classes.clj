(ns compliment.sources.namespaces-and-classes
  "Completion for namespace and class names."
  (:use [compliment.sources :only [defsource]]
        [compliment.utils :only [parts-match? split]]
        [compliment.sources.class-members :only [classname-doc]]))

(defn nscl-symbol?
  "Tests if prefix looks like a namespace or classname."
  [x]
  (re-matches #"[^\/\:\.][^\/\:]+" x))

(defn nscl-matches?
  "Tests if prefix partially matches a var name with periods as
  separators."
  [^String prefix, ^String namespace]
  (parts-match? (split prefix #"\." true) (split namespace #"\.")))

(defn imported-classes
  "Returns names of all classes imported into a given namespace."
  [ns]
  (for [[_ ^Class val] (ns-map ns) :when (class? val)]
    (.getName val)))

(defn candidates
  "Returns a list of namespace and classname completions."
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

(defn doc [ns-or-class-str curr-ns]
  (when (nscl-symbol? ns-or-class-str)
    (if-let [ns (find-ns (symbol ns-or-class-str))]
      (str ns "\n" (:doc (meta ns)) "\n")
      (when-let [class (ns-resolve curr-ns (symbol ns-or-class-str))]
        (when (= (type class) Class)
          (classname-doc class))))))

(defsource ::namespaces-and-classes
  :candidates #'candidates
  :doc #'doc)
