(ns compliment.sources.classes
  "Completion for class names."
  (:require [compliment.sources :refer [defsource]]
            [compliment.utils :refer [fuzzy-matches?] :as utils]
            [compliment.sources.class-members :refer [classname-doc]]
            [compliment.sources.namespaces :refer [nscl-symbol? nscl-matches?]]))

;;; Obtaining the list of classes

(defn imported-classes
  "Returns names of all classes imported into a given namespace."
  [ns]
  (for [[_ ^Class val] (ns-map ns) :when (class? val)]
    (.getName val)))

(defn- all-classes-short-names
  "Returns a map where short classnames are matched with vectors with
  package-qualified classnames."
  []
  (let [all-classes (utils/classes-on-classpath)]
    (utils/cache-last-result :all-classes-short-names all-classes
      (group-by (fn [^String s] (.substring s (inc (.lastIndexOf s "."))))
                (apply concat (vals all-classes))))))

(defn- analyze-import-context
  "Checks if the completion is called from ns import declaration. If so, and the
  prefix is inside import list, return that package name, otherwise return nil."
  [ctx]
  (let [ns-decl (:form (last ctx))
        import-list (:form (last (butlast ctx)))
        prefix-form (:form (first ctx))]
    (when (and (sequential? ns-decl)
               (= (first ns-decl) 'ns)
               (sequential? import-list)
               (= (first import-list) :import)
               (not= prefix-form import-list))
      (str (first prefix-form)))))

(defn- get-all-full-names
  "Returns a list of package-qualified classnames given a short classname."
  [prefix]
  (reduce-kv (fn [l, ^String short-name, full-names]
               (if (.startsWith short-name prefix)
                 (into l (map (fn [c] {:candidate c, :type :class})) full-names)
                 l))
             []
             (all-classes-short-names)))

(defn- get-classes-by-package-name
  "Returns simple classnames that match the `prefix` and belong to `pkg-name`."
  [prefix pkg-name]
  (reduce-kv (fn [l, ^String short-name, full-names]
               (if (and (.startsWith short-name prefix)
                        (some #(.startsWith ^String % pkg-name) full-names))
                 (conj l {:candidate short-name, :type :class})
                 l))
             []
             (all-classes-short-names)))

(defn candidates
  "Returns a list of classname completions."
  [^String prefix, ns context]
  (when (nscl-symbol? prefix)
    (let [has-dot (> (.indexOf prefix ".") -1)]
      (if-let [import-ctx (analyze-import-context context)]
        (get-classes-by-package-name prefix import-ctx)

        (into []
              (comp cat (distinct))
              [(for [class-str (imported-classes ns)
                     :when (nscl-matches? prefix class-str)]
                 {:candidate class-str, :type :class})

               (for [[_ ^Class val] (ns-map ns) :when (class? val)
                     :let [sname (.getSimpleName val)]
                     :when (nscl-matches? prefix sname)]
                 {:candidate sname, :type :class,
                  :package (when-let [pkg (.getPackage ^Class val)]
                             ;; Some classes don't have a package
                             (.getName ^Package pkg))})

               ;; For capitalized prefix, complete class FQN from a short name.
               (when (Character/isUpperCase (.charAt prefix 0))
                 (get-all-full-names prefix))

               ;; Fuzziness is too slow for all classes, so only startsWith. Also, if no
               ;; period in prefix, only complete root package names to maintain good
               ;; performance and not produce too many candidates.
               (let [all-classes (utils/classes-on-classpath)]
                 (if (or has-dot (contains? all-classes prefix))
                   (for [[root-pkg classes] all-classes
                         :when (.startsWith prefix root-pkg)
                         ^String cl-str classes
                         :when (.startsWith cl-str prefix)]
                     {:candidate cl-str, :type :class})
                   (for [[^String root-pkg _] all-classes
                         :when (.startsWith root-pkg prefix)]
                     {:candidate (str root-pkg "."), :type :class})))])))))

(defn doc [class-str curr-ns]
  (when (nscl-symbol? class-str)
    (some-> (utils/resolve-class curr-ns class-str) classname-doc)))

(defsource ::classes
  :candidates #'candidates
  :doc #'doc)
