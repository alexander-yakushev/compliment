(ns compliment.t-benchmark
  (:require [midje.sweet :refer :all]
            [compliment.core :refer [completions]]
            [compliment.sources.namespaces-and-classes :refer [all-classes]]
            [criterium.core :as crit]))

(def ^:private ctx
  (str '(let [foo 13, [bar {:keys [baz] :as qux}] 42]
          (ns compliment.t-benchmark
            (:require [criterium.core :as crit])
            (:require [compliment.core :refer [__prefix__]])))))

(defn execute-completions []
  (do (completions "cji" nil)
      (completions "c.j.i" nil)
      (completions "rek" nil)
      (completions "java" nil)
      (completions "java." nil)
      (completions "compl" ctx)
      (completions "ba" ctx)
      (completions "seageseexs" nil)
      (completions ".geIV" nil)
      (completions ":req" nil)))

(facts "about performance" :bench
  (let [;; Don't include initialization into benchmark.
        _ (all-classes)
        res (crit/quick-benchmark (execute-completions)
                                  {:supress-jvm-option-warnings true})]
    (fact "simple benchmark suite shouldn't take longer than specified limit"
      (first (:mean res)) => (partial > 100))

    (crit/report-result res)))

(fact "this is a full benchmark" :fullbench
  (all-classes)
  (crit/bench (execute-completions) :supress-jvm-option-warnings true))
