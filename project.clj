(defproject compliment "0.3.7-SNAPSHOT"
  :description "The Clojure completion library you deserve"
  :url "https://github.com/alexander-yakushev/compliment"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :profiles {:dev {:dependencies [[org.clojure/clojure "1.9.0"]
                                  [midje "1.9.1"]
                                  [criterium "0.4.4"]
                                  [cloverage "1.0.10"]]
                   :plugins [[lein-midje "3.2.1"]
                             [jonase/eastwood "0.2.5"]
                             [lein-shell "0.5.0"]]
                   :eastwood {:namespaces [:source-paths]}

                   :aliases {"test" ["do" ["check"] ["midje" ":filters" "-bench"]]
                             "test-all" ["do" ["check"] ["midje" ":filters" "-fullbench"]]
                             "bench" ["midje" ":filters" "quickbench"]
                             "fullbench" ["midje" ":filters" "fullbench"]
                             "coverage" ["do" ["run" "-m" "compliment.t-coverage" "--coveralls"]
                                         ["shell" "curl" "-F"
                                          "json_file=@target/coverage/coveralls.json"
                                          "https://coveralls.io/api/v1/jobs"]]}}
             :1.7 {:dependencies [[org.clojure/clojure "1.7.0"]]}
             :1.8 {:dependencies [[org.clojure/clojure "1.8.0"]]}})
