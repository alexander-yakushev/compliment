(ns compliment.sources.classes
  "Completion for class names."
  (:require [compliment.sources :refer [defsource]]
            [compliment.utils :as utils]
            [compliment.sources.class-members :refer [classname-doc]]
            [compliment.sources.namespaces :refer [nscl-symbol? nscl-matches?]])
  (:import java.util.HashSet))

^{:lite nil}
(def ^:private base-priority 60)

(defn- all-classes-short-names
  "Returns a map where short classnames are matched with vectors with
  package-qualified classnames."
  []
  (utils/with-classpath-cache :all-classes-short-names
    (group-by (fn [^String s] (.substring s (inc (.lastIndexOf s "."))))
              (utils/classes-on-classpath))))

^{:lite nil}
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

(defn- get-classes-by-package-name
  "Returns simple classnames that match the `prefix` and belong to `pkg-name`."
  [prefix pkg-name]
  (reduce-kv (fn [l, ^String short-name, full-names]
               (if (and (.startsWith short-name prefix)
                        (some (fn [^String fn] (.startsWith fn pkg-name))
                              full-names))
                 (conj l {:candidate short-name, :type :class})
                 l))
             [] (all-classes-short-names)))

^{:lite nil}
(defn- priority-by-name [^String full-name]
  (if (or (.startsWith full-name "clojure")
          (.startsWith full-name "java")
          (.startsWith full-name "jdk"))
    0 1))

;; The function below is quite ugly for performance and efficiency reasons. The
;; total number of classes can go up to 7 digits, and we must still return the
;; result reasonably quickly in such scenarios.

(defn ^{:lite 'classes-candidates} candidates
  "Returns a list of classname completions."
  [^String prefix, ns context]
  (when (nscl-symbol? prefix)
    (if-let [import-ctx ^{:lite '(get {} :nil)} (analyze-import-context context)]
      (get-classes-by-package-name prefix import-ctx)

      (let [has-dot (.contains prefix ".")
            seen (HashSet.)
            include? (fn ([class-str remember?]
                          (when-not (.contains seen class-str)
                            (when remember? (.add seen class-str))
                            true)))
            str->cand (fn [s fqname]
                        {:candidate s, :type :class
                         :priority ^{:lite 0} (+ base-priority (priority-by-name fqname) 1)})
            all-classes (utils/classes-on-classpath)
            it (.iterator ^Iterable all-classes)
            roots (utils/root-packages-on-classpath)]

        (as-> (transient []) result
          ;; For classes imported into the namespace, suggest them both as
          ;; short names and fully-qualified names.
          (reduce-kv
           (fn [result _ ^Class v]
             (if (class? v)
               (let [fqname (.getName v)
                     sname (.getSimpleName v)]
                 (cond-> result
                   (and (nscl-matches? prefix fqname) (include? fqname true))
                   (conj! (str->cand fqname fqname))

                   (and (nscl-matches? prefix sname) (include? sname true))
                   (conj! {:candidate sname, :type :class,
                           :package (some-> (.getPackage v) .getName)
                           :priority ^{:lite 0} (+ base-priority (priority-by-name fqname))})))
               result))
           result (ns-map ns))

          ;; For capitalized prefix, complete class FQN from a short name.
          (if (Character/isUpperCase (.charAt prefix 0))
            (reduce-kv (fn [result, ^String short-name, full-names]
                         (if (.startsWith short-name prefix)
                           (reduce (fn [result cl]
                                     (cond-> result
                                       (include? cl true) (conj! (str->cand cl cl))))
                                   result full-names)
                           result))
                       result (all-classes-short-names))
            result)

          ;; Fuzziness is too slow for all classes, so only startsWith. Also, if
          ;; no period in prefix, only complete root package names to maintain
          ;; good performance and not produce too many candidates.
          (if (or has-dot (contains? roots prefix))
            (loop [result result]
              (if (.hasNext it)
                (let [^String cl (.next it)]
                  (recur (cond-> result
                           (and (.startsWith cl prefix) (include? cl false))
                           (conj! (str->cand cl cl)))))
                result))

            (reduce conj! result
                    (for [^String root-pkg roots
                          :when (and (.startsWith root-pkg prefix)
                                     (include? root-pkg false))]
                      (str->cand (str root-pkg ".") ""))))

          (persistent! result))))))

^{:lite nil}
(defn doc [class-str curr-ns]
  (when (nscl-symbol? class-str)
    (some-> (utils/resolve-class curr-ns (symbol class-str)) classname-doc)))

^{:lite '(defsource :compliment.lite/classes :candidates #'classes-candidates)}
(defsource ::classes
  :candidates #'candidates
  :doc #'doc)
