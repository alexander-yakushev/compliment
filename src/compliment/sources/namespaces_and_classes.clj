(ns compliment.sources.namespaces-and-classes
  "Completion for namespace and class names."
  (:require [compliment.sources :refer [defsource]]
            [compliment.utils :refer [fuzzy-matches?] :as utils]
            [compliment.sources.class-members :refer [classname-doc]]))

(defn nscl-symbol?
  "Tests if prefix looks like a namespace or classname."
  [^String x]
  (and (re-matches #"[^\/\:]+" x)
       (not (= (.charAt x 0) \.))))

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

(defn all-classes-short-names
  "Returns a map where short classnames are matched with vectors with
  package-qualified classnames."
  []
  (let [all-classes (utils/classes-on-classpath)]
    (utils/cache-last-result ::all-classes-short-names all-classes
      (group-by (fn [^String s] (.substring s (inc (.lastIndexOf s "."))))
                (apply concat (vals all-classes))))))

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
  "Returns a list of namespace and classname completions."
  [^String prefix, ns context]
  (when (nscl-symbol? prefix)
    (let [has-dot (> (.indexOf prefix ".") -1)
          import-ctx (analyze-import-context context)]
      (into []
            (comp cat (distinct))
            [(for [ns-str (concat (map (comp name ns-name) (all-ns))
                                  (map name (keys (ns-aliases ns))))
                   :let [[literals prefix] (utils/split-by-leading-literals prefix)]
                   :when (nscl-matches? prefix ns-str)]
               {:candidate (str literals ns-str), :type :namespace})
             (for [class-str (imported-classes ns)
                   :when (nscl-matches? prefix class-str)]
               {:candidate class-str, :type :class})
             (cond (= import-ctx :root) (get-all-full-names prefix)
                   import-ctx (get-classes-by-package-name prefix import-ctx))
             ;; For capitalized prefixes, try to complete class FQN from a short name.
             (when (and (Character/isUpperCase (.charAt prefix 0))
                        (not import-ctx))
               (get-all-full-names prefix))
             ;; If prefix doesn't contain a period, using fuziness produces too many
             ;; irrelevant candidates.
             (for [{:keys [^String ns-str, ^String file]} (utils/namespaces&files-on-classpath)
                   :let [[literals prefix] (utils/split-by-leading-literals prefix)]
                   :when (and (re-find #"\.cljc?$" file)
                              (if has-dot
                                (nscl-matches? prefix ns-str)
                                (.startsWith ns-str prefix)))]
               {:candidate (str literals ns-str), :type :namespace, :file file})
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
                   {:candidate (str root-pkg "."), :type :class})))]))))

(defn doc [ns-or-class-str curr-ns]
  (when (nscl-symbol? ns-or-class-str)
    (let [strip-literals (comp second utils/split-by-leading-literals)
          ns-or-class-sym (symbol (strip-literals ns-or-class-str))]
      (if-let [ns (or (find-ns ns-or-class-sym)
                      (get (ns-aliases curr-ns) ns-or-class-sym))]
        (str ns "\n" (:doc (meta ns)) "\n")
        (when-let [class (try (ns-resolve curr-ns ns-or-class-sym)
                              (catch Exception ex nil))]
          (when (= (type class) Class)
            (classname-doc class)))))))

(defsource ::namespaces-and-classes
  :candidates #'candidates
  :doc #'doc)
