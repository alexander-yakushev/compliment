(defproject compliment "0.2.1-SNAPSHOT"
  :description "The Clojure completion library you deserve"
  :url "https://github.com/alexander-yakushev/compliment"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]]
  :profiles {:dev {:dependencies [[midje "1.6.3"]
                                  [criterium "0.4.3"]]
                   :plugins [[lein-midje "3.1.3"]]}
             :1.6 {:dependencies [[org.clojure/clojure "1.6.0"]]}
             :1.7 {:dependencies [[org.clojure/clojure "1.7.0-alpha1"]]}}
  :aliases {"test-all" ["do" ["check"] ["midje" ":filters" "-fullbench"]]
            "bench" ["midje" ":filters" "quickbench"]
            "fullbench" ["midje" ":filters" "fullbench"]})
