(ns compliment.t-benchmark
  (:require [clojure.test :refer :all]
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
  (reduce into []
          [(completions "cji")
           (completions "c.j.i")
           (completions "rek")
           (completions "java")
           (completions "java.")
           (completions "compl" {:context ctx})
           (completions "ba" {:context ctx})
           (completions "seageseexs")
           (completions ".geIV")
           (completions ":req")
           (completions "META" {:context ctx2})]))

(defn benchmark [quick?]
  ;; Don't include initialization into benchmark, time it separately.
  (println "Initialization:")
  (time (do (utils/classes-on-classpath)
            (utils/namespaces&files-on-classpath)
            (utils/project-resources)))
  (if quick?
    (crit/quick-benchmark (execute-completions)
                          {:supress-jvm-option-warnings true})
    (crit/bench (execute-completions)
                :supress-jvm-option-warnings true)))

(deftest ^:benchmark quick-benchmark
  (let [res (benchmark true)]
    (testing "simple benchmark suite shouldn't take longer than specified limit"
      (is (< (first (:mean res)) 0.1)))

    (crit/report-result res)))

(defn -main [& [quick?]]
  (crit/report-result (benchmark (= quick? "true"))))
