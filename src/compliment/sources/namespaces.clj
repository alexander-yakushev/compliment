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
    (let [has-dot (> (.indexOf prefix ".") -1)]
      (into []
            (comp cat (distinct))
            [;; If prefix doesn't contain a period, using fuziness produces too
             ;; many irrelevant candidates.
             (for [{:keys [^String ns-str, ^String file]} (utils/namespaces&files-on-classpath)
                   :let [[literals prefix] (utils/split-by-leading-literals prefix)]
                   :when (and (re-find #"\.cljc?$" file)
                              (if has-dot
                                (nscl-matches? prefix ns-str)
                                (.startsWith ns-str prefix)))]
               {:candidate (str literals ns-str), :type :namespace, :file file})

             (for [ns-str (concat (map (comp name ns-name) (all-ns))
                                  (map name (keys (ns-aliases ns))))
                   :let [[literals prefix] (utils/split-by-leading-literals prefix)]
                   :when (nscl-matches? prefix ns-str)]
               {:candidate (str literals ns-str), :type :namespace})]))))

(defn doc [ns-str curr-ns]
  (when (nscl-symbol? ns-str)
    (let [ns-sym (symbol (second (utils/split-by-leading-literals ns-str)))]
      (when-let [ns (or (find-ns ns-sym)
                        (get (ns-aliases curr-ns) ns-sym))]
        (str ns "\n" (:doc (meta ns)) "\n")))))

(defsource ::namespaces
  :candidates #'candidates
  :doc #'doc)
