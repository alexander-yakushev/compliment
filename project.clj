(defproject compliment "0.3.10-SNAPSHOT"
  :description "The Clojure completion library you deserve"
  :url "https://github.com/alexander-yakushev/compliment"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :profiles {:dev {:dependencies [[org.clojure/clojure "1.10.1"]
                                  [criterium "0.4.4"]
                                  [cloverage "1.0.13"]
                                  [fudje "0.9.7"]]
                   :plugins [[jonase/eastwood "0.3.5"]
                             [lein-shell "0.5.0"]]
                   :eastwood {:namespaces [:source-paths]}

                   :aliases {"test" ["do" ["check"] ["test"]]
                             "bench" ["run" "-m" "compliment.t-benchmark" "true"]
                             "fullbench" ["run" "-m" "compliment.t-benchmark"]
                             "coverage" ["do" ["run" "-m" "compliment.t-coverage" "--coveralls"]
                                         ["shell" "curl" "-F"
                                          "json_file=@target/coverage/coveralls.json"
                                          "https://coveralls.io/api/v1/jobs"]]}}
             :1.8 {:dependencies [[org.clojure/clojure "1.8.0"]]}
             :1.9 {:dependencies [[org.clojure/clojure "1.9.0"]]}})
