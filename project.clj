(defproject compliment "0.3.15-SNAPSHOT"
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
                                  [fudje "0.9.7"]]
                   :plugins [[jonase/eastwood "1.2.3"]
                             [lein-cloverage "1.2.4"]]
                   :eastwood {:ignored-faults {:reflection {compliment.utils true}
                                               :bad-arglists {compliment.sources.t-local-bindings true}
                                               :def-in-def {compliment.sources.t-class-members true
                                                            compliment.sources.t-ns-mappings true
                                                            compliment.t-core true
                                                            compliment.sources.t-local-bindings true}}}
                   :test-selectors {:no-bench (complement :benchmark)}
                   :aliases {"test-all" ["do" ["check"] ["test"]]
                             "test-no-bench" ["do" ["check"] ["test" ":no-bench"]]
                             "bench" ["run" "-m" "compliment.t-benchmark" "true"]
                             "fullbench" ["run" "-m" "compliment.t-benchmark"]
                             "coverage" ["cloverage" "--test-ns-regex" ".+\\.t-(?!benchmark)[^.]+$"
                                         "--codecov"]}}
             :1.8 {:dependencies [[org.clojure/clojure "1.8.0"]]}
             :1.9 {:dependencies [[org.clojure/clojure "1.9.0"]]}
             :1.10 {:dependencies [[org.clojure/clojure "1.10.1"]]}
             :1.11 {:dependencies [[org.clojure/clojure "1.11.0"]]}})
