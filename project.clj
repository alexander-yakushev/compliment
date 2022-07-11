(defproject compliment "0.3.14"
  :description "The Clojure completion library you deserve"
  :url "https://github.com/alexander-yakushev/compliment"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :deploy-repositories [["clojars" {:url "https://clojars.org/repo"
                                    :username :env/clojars_username
                                    :password :env/clojars_password
                                    :sign-releases false}]]

  :profiles {:dev {:dependencies [[org.clojure/clojure "1.10.1"]
                                  [criterium "0.4.4"]
                                  [cloverage "1.0.13"]
                                  [fudje "0.9.7"]]
                   :plugins [[jonase/eastwood "1.2.3"]
                             [lein-shell "0.5.0"]]
                   :eastwood {:ignored-faults {:reflection {compliment.utils true}
                                               :bad-arglists {compliment.sources.t-local-bindings true}
                                               :def-in-def {compliment.sources.t-class-members true
                                                            compliment.sources.t-ns-mappings true
                                                            compliment.t-core true
                                                            compliment.sources.t-local-bindings true}}}

                   :aliases {"test" ["do" ["check"] ["test"]]
                             "bench" ["run" "-m" "compliment.t-benchmark" "true"]
                             "fullbench" ["run" "-m" "compliment.t-benchmark"]
                             "coverage" ["do" ["run" "-m" "compliment.t-coverage" "--coveralls"]
                                         ["shell" "curl" "-F"
                                          "json_file=@target/coverage/coveralls.json"
                                          "https://coveralls.io/api/v1/jobs"]]}}
             :1.8 {:dependencies [[org.clojure/clojure "1.8.0"]]}
             :1.9 {:dependencies [[org.clojure/clojure "1.9.0"]]}
             :1.10 {:dependencies [[org.clojure/clojure "1.10.1"]]}
             :1.11 {:dependencies [[org.clojure/clojure "1.11.0"]]}})
