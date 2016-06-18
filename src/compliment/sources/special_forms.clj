(ns compliment.sources.special-forms
  "Completion for Clojure's special forms."
  (:require [clojure.repl :as repl]
            [compliment.sources :refer [defsource]]
            [compliment.sources.ns-mappings :as vars]))

(def ^:private special-forms
  (set (map name '[def if do quote var recur throw try catch
                   monitor-enter monitor-exit new set!])))

(defn first-item-in-list?
  "If context is not nil, check if prefix is the first item in a list form."
  [ctx]
  (if ctx
    (when-let [expr (first ctx)]
      (and (list? (:form expr)) (= (:idx expr) 0)))
    true))

(defn candidates
  "Returns list of completions for special forms."
  [prefix _ context]
  (when (and (vars/var-symbol? prefix) (first-item-in-list? context))
    (for [form special-forms
          :when (vars/dash-matches? prefix form)]
      {:candidate form
       :type :special-form})))

(defn doc
  "Documentation function for special forms."
  [symbol-str _]
  (when (and (vars/var-symbol? symbol-str) (special-forms symbol-str))
    (vars/generate-docstring (#'repl/special-doc (symbol symbol-str)))))

(defsource ::special-forms
  :candidates #'candidates
  :doc #'doc)

(defn literal-candidates
  "We define `true`, `false`, and `nil` in a separate source because they are
  not context-dependent (don't have to be first items in the list)."
  [prefix _ __]
  (->> ["true" "false" "nil"]
       (filter #(.startsWith ^String % prefix))
       (map (fn [c] {:candidate c, :type :special-form}))))

(defsource ::literals
  :candidates #'literal-candidates
  :doc (constantly nil))
