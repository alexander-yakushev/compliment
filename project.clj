(defproject compliment "0.5.0-SNAPSHOT"
  :description "The Clojure completion library you deserve"
  :url "https://github.com/alexander-yakushev/compliment"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :deploy-repositories [["clojars" {:url "https://clojars.org/repo"
                                    :username :env/clojars_username
                                    :password :env/clojars_password
                                    :sign-releases false}]]

  :profiles {:dev {:dependencies [[org.clojure/clojure "1.11.1"]
                                  [criterium "0.4.6"]
                                  [fudje "0.9.7"]]
                   :plugins [[jonase/eastwood "1.4.0"]
                             [lein-cloverage "1.2.4"]]
                   :eastwood {:ignored-faults {:bad-arglists {compliment.sources.t-local-bindings true}
                                               :unused-meta-on-macro {compliment.utils true
                                                                      compliment.sources.classes true
                                                                      compliment.sources.class-members true
                                                                      compliment.sources.special-forms true
                                                                      compliment.sources.namespaces true
                                                                      compliment.sources.vars true
                                                                      compliment.core true}
                                               :def-in-def {compliment.sources.t-class-members true
                                                            compliment.sources.t-vars true
                                                            compliment.t-core true
                                                            compliment.sources.t-local-bindings true}}}
                   :test-selectors {:no-bench (complement :benchmark)}
                   :aliases {"test-all" ["do" ["check"] ["test"]]
                             "test-no-bench" ["do" ["check"] ["test" ":no-bench"]]
                             "bench" ["run" "-m" "compliment.t-benchmark" "true"]
                             "fullbench" ["run" "-m" "compliment.t-benchmark"]
                             "coverage" ["cloverage" "--test-ns-regex" ".+\\.t-(?!benchmark)[^.]+$"
                                         "--codecov"]}}
             :1.10 {:dependencies [[org.clojure/clojure "1.10.1"]]}
             :1.12 {:dependencies [[org.clojure/clojure "1.12.0-alpha4"]]}})
