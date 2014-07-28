(ns compliment.sources.special-forms
  "Completion for Clojure's special forms."
  (:require [clojure.repl :as repl]
            [compliment.sources :refer [defsource]]
            [compliment.sources.ns-mappings :as vars]))

(def ^:private special-forms
  (set (map name '[def if do let quote var fn loop recur throw try catch
                   monitor-enter monitor-exit doto new set!])))

(defn first-item-in-list? [ctx]
  "If context is not nil, check if prefix is the first item in a list form."
  (if ctx
    (if-let [expr (first ctx)]
      (and (list? (:form expr)) (= (:idx expr) 0)))
    true))

(defn candidates
  "Returns list of completions for special forms."
  [prefix _ context]
  (when (and (vars/var-symbol? prefix) (first-item-in-list? context))
    (for [form special-forms
          :when (vars/dash-matches? prefix form)]
      form)))

(defn doc
  "Documentation function for special forms."
  [symbol-str _]
  (when (and (vars/var-symbol? symbol-str) (special-forms symbol-str))
    (vars/generate-docstring (#'repl/special-doc (symbol symbol-str)))))

(defsource ::special-forms
  :candidates #'candidates
  :doc doc)
