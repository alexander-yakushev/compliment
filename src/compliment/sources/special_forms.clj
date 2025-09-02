(ns compliment.sources.special-forms
  "Completion for Clojure's special forms."
  (:require [clojure.repl :as repl]
            [compliment.sources :refer [defsource]]
            [compliment.sources.vars :as vars]))

(def ^:private special-forms
  (set (map name '[def if do quote var recur throw try catch
                   monitor-enter monitor-exit new set!])))

^{:lite nil}
(defn first-item-in-list?
  "If context is not nil, check if prefix is the first item in a list form."
  [ctx]
  (if ctx
    (when-let [expr (first ctx)]
      (and (list? (:form expr)) (= (:idx expr) 0)))
    true))

(defn ^{:lite 'special-form-candidates} candidates
  "Returns list of completions for special forms."
  [prefix _ context]
  (when (vars/var-symbol? prefix)
    (for [form (concat ^{:lite special-forms} (when (first-item-in-list? context) special-forms)
                       ["true" "false" "nil"])
          :when (vars/dash-matches? prefix form)]
      {:candidate form
       :type :special-form})))

^{:lite nil}
(defn doc
  "Documentation function for special forms."
  [symbol-str _]
  (when (and (vars/var-symbol? symbol-str) (special-forms symbol-str))
    (vars/generate-docstring (#'repl/special-doc (symbol symbol-str)))))

^{:lite '(defsource :compliment.lite/special-forms :candidates #'special-form-candidates)}
(defsource ::special-forms
  :candidates #'candidates
  :doc #'doc)
