(defproject compliment "0.2.2-SNAPSHOT"
  :description "The Clojure completion library you deserve"
  :url "https://github.com/alexander-yakushev/compliment"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [defprecated "0.1.2"]]
  :profiles {:default [:dev :user]

             :dev {:dependencies [[midje "1.6.3"]
                                  [criterium "0.4.3"]
                                  [cloverage "1.0.4"]]
                   :plugins [[lein-midje "3.1.3"]
                             [jonase/eastwood "0.2.1"]
                             [lein-cloverage "1.0.2"]
                             [lein-shell "0.4.0"]]
                   :eastwood {:namespaces [:source-paths]}

                   :aliases {"test" ["do" ["check"] ["midje" ":filters" "-bench"]]
                             "test-all" ["do" ["check"] ["midje" ":filters" "-fullbench"]]
                             "bench" ["midje" ":filters" "quickbench"]
                             "fullbench" ["midje" ":filters" "fullbench"]
                             "coverage" ["do" ["run" "-m" "compliment.t-coverage" "--coveralls"]
                                         ["shell" "curl" "-F"
                                          "json_file=@target/coverage/coveralls.json"
                                          "https://coveralls.io/api/v1/jobs"]]}}
             :1.6 {:dependencies [[org.clojure/clojure "1.6.0"]]}
             :1.7 {:dependencies [[org.clojure/clojure "1.7.0-beta1"]]}})
