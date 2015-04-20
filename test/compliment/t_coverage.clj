(ns compliment.t-coverage
  (:require [cloverage.coverage :as coverage]
            [bultitude.core :as blt]))

(defn ns-names-for-dirs [dirs]
  (map name (mapcat blt/namespaces-in-dir dirs)))

(defn -main
  [& args]
  (let [source-namespaces (ns-names-for-dirs ["src"])
        test-namespace (->> (ns-names-for-dirs ["test"])
                            (remove #{"compliment.t-benchmark"
                                      "compliment.t-coverage"}))]
    (apply #'coverage/-main (concat (mapcat #(list "-x" %) test-namespace)
                                    args
                                    source-namespaces))))
