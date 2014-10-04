(ns compliment.t-benchmark
  (:require [criterium.core :as crit])
  (:require [compliment.core :refer [completions]]))

(def ctx
  (str '(let [foo 13, [bar {:keys [baz] :as qux}] 42]
          (ns compliment.t-benchmark
            (:require [criterium.core :as crit])
            (:require [compliment.core :refer [__prefix__]])))))

(defn -main
  "Very simple benchmark to ensure that completion works reasonably fast."
  [& args]
  (println "Stable mean time:" 4.281811 "ms")
  (>bench (do (completions "cji" "")
              (completions "rek" "")
              (completions "compl" ctx)
              (completions "ba" ctx)
              (completions "seageseexs" "")
              (completions ".geIV" "")
              (completions ":req" ""))))










