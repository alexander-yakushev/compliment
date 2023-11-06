(ns compliment.sources.namespaces
  "Completion for namespaces."
  (:require [compliment.sources :refer [defsource]]
            [compliment.utils :refer [fuzzy-matches?] :as utils]))

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

(defn candidates
  "Returns a list of namespace and classname completions."
  [^String prefix, ns context]
  (when (nscl-symbol? prefix)
    (let [has-dot (> (.indexOf prefix ".") -1)
          [literals prefix] (utils/split-by-leading-literals prefix)

          cands-from-classpath
          (for [{:keys [^String ns-str, file]} (utils/namespaces&files-on-classpath)
                :when (and (re-find #"\.cljc?$" file)
                           ;; If prefix doesn't contain a period, using fuziness
                           ;; produces too many irrelevant candidates.
                           (if has-dot
                             (nscl-matches? prefix ns-str)
                             (.startsWith ns-str prefix)))]
            {:candidate (str literals ns-str), :type :namespace, :file file})

          ns-names (set (map :candidate cands-from-classpath))
          ns-sym->cand #(let [ns-str (name %)]
                          (when (and (nscl-matches? prefix ns-str)
                                     (not (ns-names ns-str)))
                            {:candidate (str literals ns-str), :type :namespace}))]
      ;; Add aliases and runtime namespaces not found on the classpath.
      (-> cands-from-classpath
          (into (keep ns-sym->cand) (keys (ns-aliases ns)))
          (into (comp (map ns-name)
                      (keep ns-sym->cand))
                (all-ns))))))

(defn doc [ns-str curr-ns]
  (when (nscl-symbol? ns-str)
    (let [ns-sym (symbol (second (utils/split-by-leading-literals ns-str)))]
      (when-let [ns (or (find-ns ns-sym)
                        (get (ns-aliases curr-ns) ns-sym))]
        (str ns "\n" (:doc (meta ns)) "\n")))))

(defsource ::namespaces
  :candidates #'candidates
  :doc #'doc)
