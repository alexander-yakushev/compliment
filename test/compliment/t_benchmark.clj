(ns compliment.t-benchmark
  (:require [midje.sweet :refer :all]
            [compliment.core :refer [completions]]
            [compliment.utils :as utils]
            [criterium.core :as crit]))

(def ^:private ctx
  (str '(let [foo 13, [bar {:keys [baz] :as qux}] 42]
          (ns compliment.t-benchmark
            (:require [criterium.core :as crit])
            (:require [compliment.core :refer [__prefix__]])))))

(def ^:private ctx2
  (str '(jio/resource "__prefix__")))

(defn execute-completions []
  (do (completions "cji")
      (completions "c.j.i")
      (completions "rek")
      (completions "java")
      (completions "java.")
      (completions "compl" {:context ctx})
      (completions "ba" {:context ctx})
      (completions "seageseexs")
      (completions ".geIV")
      (completions ":req")
      (completions "META" {:context ctx2})))

(defn benchmark [quick?]
  (let [;; Don't include initialization into benchmark.
        _ (do (utils/classes-on-classpath)
              (utils/namespaces-on-classpath)
              (utils/project-resources))]
    (if quick?
      (crit/quick-benchmark (execute-completions)
                            {:supress-jvm-option-warnings true})
      (crit/bench (execute-completions)
                  :supress-jvm-option-warnings true))))

(facts "about performance" :bench :quickbench
  (let [res (benchmark true)]
    (fact "simple benchmark suite shouldn't take longer than specified limit"
      (first (:mean res)) => (partial > 100))

    (crit/report-result res)))

(fact "this is a full benchmark" :bench :fullbench
  (benchmark false))
