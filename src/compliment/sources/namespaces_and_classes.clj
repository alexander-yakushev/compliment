(ns compliment.sources.namespaces-and-classes
  "Completion for namespace and class names."
  (:require [compliment.sources :refer [defsource]]
            [compliment.utils :refer [fuzzy-matches? defmemoized] :as utils]
            [compliment.sources.class-members :refer [classname-doc]])
  (:import java.io.File))

(defn nscl-symbol?
  "Tests if prefix looks like a namespace or classname."
  [x]
  (re-matches #"[^\/\:\.][^\/\:]+" x))

(defn nscl-matches?
  "Tests if prefix partially matches a var name with periods as
  separators."
  [prefix namespace]
  (fuzzy-matches? prefix namespace \.))

;;; Obtaining the list of classes

(defn imported-classes
  "Returns names of all classes imported into a given namespace."
  [ns]
  (for [[_ ^Class val] (ns-map ns) :when (class? val)]
    (.getName val)))

(defmemoized all-classes-short-names
  "Returns a map where short classnames are matched with vectors with
  package-qualified classnames."
  []
  (group-by #(-> (re-matches #"([^\.]+\.)*([^\.]+)" %)
                 (nth 2))
            (apply concat (vals (utils/classes-on-classpath)))))

(defn- analyze-import-context
  "Checks if the completion is called from ns import declaration. If so, and the
  prefix is inside import vector, return that package name, otherwise return
  `:root`. If not inside :import, return nil."
  [ctx]
  (let [ns-decl (:form (last ctx))
        import-list (:form (last (butlast ctx)))
        prefix-form (:form (first ctx))]
    (when (and (sequential? ns-decl)
               (= (first ns-decl) 'ns)
               (sequential? import-list)
               (= (first import-list) :import))
      (if (= prefix-form import-list)
        :root
        (str (first prefix-form))))))

(defn- get-all-full-names
  "Returns a list of package-qualified classnames given a short classname."
  [prefix]
  (reduce-kv (fn [l, ^String short-name, full-names]
               (if (.startsWith short-name prefix)
                 (concat l full-names)
                 l))
             ()
             (all-classes-short-names)))

(defn- get-classes-by-package-name
  "Returns simple classnames that match the `prefix` and belong to `pkg-name`."
  [prefix pkg-name]
  (reduce-kv (fn [l, ^String short-name, full-names]
               (if (and (.startsWith short-name prefix)
                        (some #(.startsWith ^String % pkg-name) full-names))
                 (conj l short-name)
                 l))
             ()
             (all-classes-short-names)))

(defn candidates
  "Returns a list of namespace and classname completions."
  [^String prefix, ns context]
  (when (nscl-symbol? prefix)
    (let [has-dot (> (.indexOf prefix ".") -1)
          import-ctx (analyze-import-context context)]
      (cond (= import-ctx :root)
            (get-all-full-names prefix)

            import-ctx
            (get-classes-by-package-name prefix import-ctx)

            :else
            ((comp distinct concat)
             (for [ns-str (concat (map (comp name ns-name) (all-ns))
                                  (imported-classes ns)
                                  (when-not has-dot
                                    (map name (keys (ns-aliases ns)))))
                   :when (nscl-matches? prefix ns-str)]
               ns-str)
             ;; Fuzziness is too slow for all classes, so just startsWith.
             ;; Also have to do clever tricks to keep the performance high.
             (if has-dot
               (concat (for [[root-pkg classes] (utils/classes-on-classpath)
                             :when (.startsWith prefix root-pkg)
                             ^String cl-str classes
                             :when (.startsWith cl-str prefix)]
                         cl-str)
                       (for [ns-str (utils/namespaces-on-classpath)
                             :when (nscl-matches? prefix ns-str)]
                         ns-str))
               (concat (for [[^String root-pkg _] (utils/classes-on-classpath)
                             :when (.startsWith root-pkg prefix)]
                         (str root-pkg "."))
                       (for [^String ns-str (utils/namespaces-on-classpath)
                             :when (.startsWith ns-str prefix)]
                         ns-str))))))))

(defn doc [ns-or-class-str curr-ns]
  (when (nscl-symbol? ns-or-class-str)
    (if-let [ns (find-ns (symbol ns-or-class-str))]
      (str ns "\n" (:doc (meta ns)) "\n")
      (when-let [class (try (ns-resolve curr-ns (symbol ns-or-class-str))
                            (catch Exception ex nil))]
        (when (= (type class) Class)
          (classname-doc class))))))

(defsource ::namespaces-and-classes
  :candidates #'candidates
  :doc #'doc
  :tag-fn (fn [m ns]
            (let [c (:candidate m)]
              (assoc m :type (if (or (find-ns (symbol c))
                                     ((ns-aliases ns) (symbol c))
                                     ((utils/namespaces-on-classpath) c))
                               :namespace :class)))))
