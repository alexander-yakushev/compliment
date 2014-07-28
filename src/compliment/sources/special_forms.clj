(ns compliment.sources.special-forms
  "Completion for Clojure's special forms."
  (:require [clojure.repl :as repl]
            [compliment.sources :refer [defsource]]
            [compliment.sources.ns-mappings :as vars]))

(def ^:private special-forms
  (set (map name '[def if do let quote var fn loop recur throw try catch
                   monitor-enter monitor-exit doto new set!])))

(defn candidates
  "Returns list of completions for special forms."
  [prefix _ _]
  (when (vars/var-symbol? prefix)
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
