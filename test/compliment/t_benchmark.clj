(ns compliment.t-benchmark
  (:require [midje.sweet :refer :all]
            [compliment.core :refer [completions]]
            [criterium.core :as crit]))

(def ^:private ctx
  (str '(let [foo 13, [bar {:keys [baz] :as qux}] 42]
          (ns compliment.t-benchmark
            (:require [criterium.core :as crit])
            (:require [compliment.core :refer [__prefix__]])))))

(defn execute-completions []
  (do (completions "cji" nil)
      (completions "rek" nil)
      (completions "compl" ctx)
      (completions "ba" ctx)
      (completions "seageseexs" nil)
      (completions ".geIV" nil)
      (completions ":req" nil)))

(facts "about performance"
  (let [res (crit/quick-benchmark (execute-completions)
                                  {:supress-jvm-option-warnings true})]
    (fact "simple benchmark suite shouldn't take longer than specified limit"
      (first (:mean res)) => (partial > 15))

    (crit/report-result res)))
