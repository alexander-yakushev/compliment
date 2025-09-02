(ns compliment.sources.namespaces
  "Completion for namespaces."
  (:require [compliment.sources :refer [defsource]]
            [compliment.utils :refer [fuzzy-matches?] :as utils]))

^{:lite nil}
(def ^:private base-priority 50)

(defn nscl-symbol?
  "Tests if prefix looks like a namespace or classname."
  [^String x]
  (and (re-matches #"[^\/\:]+" x)
       (not (= (.charAt x 0) \.))))

(defn nscl-matches?
  "Tests if prefix partially matches a namespace or classname with periods as
  separators."
  [prefix namespace]
  (fuzzy-matches? prefix namespace \.))

^{:lite nil}
(defn- namespace-priority [^String ns-name]
  (+ base-priority (if (.startsWith ns-name "clojure.") 0 1)))

(defn ^{:lite 'namespaces-candidates} candidates
  "Return a list of namespace candidates."
  [^String prefix, ns context]
  (when (nscl-symbol? prefix)
    (let [has-dot (> (.indexOf prefix ".") -1)
          [literals prefix] (utils/split-by-leading-literals prefix)

          cands-from-cp
          (for [{:keys [^String ns-str, file]} (utils/namespaces&files-on-classpath)
                :when (and (re-find #"\.cljc?$" file)
                           ;; If prefix doesn't contain a period, using fuziness
                           ;; produces too many irrelevant candidates.
                           (if has-dot
                             (nscl-matches? prefix ns-str)
                             (.startsWith ns-str prefix)))]
            {:candidate (str literals ns-str), :type :namespace, :file file
             :priority ^{:lite 0} (namespace-priority ns-str)})

          ns-names (set (map :candidate cands-from-cp))
          ns-sym->cand #(let [ns-str (name %1)]
                          (when (and (nscl-matches? prefix ns-str)
                                     (not (ns-names ns-str)))
                            {:candidate (str literals ns-str %2)
                             :type :namespace
                             :priority ^{:lite 0} (namespace-priority ns-str)}))]
      ;; Add aliases and runtime namespaces not found on the classpath.
      (-> cands-from-cp
          (into (keep #(ns-sym->cand % "/")) (keys (ns-aliases ns)))
          (into (comp (map ns-name)
                      (keep #(ns-sym->cand % nil)))
                (all-ns))))))

^{:lite nil}
(defn doc [ns-str curr-ns]
  (when (nscl-symbol? ns-str)
    (let [ns-sym (symbol (second (utils/split-by-leading-literals ns-str)))]
      (when-let [ns (or (find-ns ns-sym)
                        (get (ns-aliases curr-ns) ns-sym))]
        (str ns "\n" (:doc (meta ns)) "\n")))))

^{:lite '(defsource :compliment.lite/namespaces :candidates #'namespaces-candidates)}
(defsource ::namespaces
  :candidates #'candidates
  :doc #'doc)
