(ns compliment.sources.t-resources
  (:require [clojure.java.io :as io]
            [compliment.context :as ctx]
            [compliment.sources.resources :as src]
            [compliment.t-helpers :refer :all]
            [midje.sweet :refer :all]))

(facts "about project resources"
  (fact "completion works when started from a string in a resource call"
    (src/candidates "META" *ns* (ctx/parse-context '(resource "__prefix__")))
    => [{:candidate "META-INF/maven/compliment/compliment/pom.properties"
         :type :resource}]

    (src/candidates "META" *ns* (ctx/parse-context '(io/resource "__prefix__")))
    => (strip-tags (contains ["META-INF/maven/compliment/compliment/pom.properties"]))

    (src/candidates "META" *ns* nil) => nil)

  (fact "there are docs for resources too"
    (src/doc "META-INF/maven/compliment/compliment/pom.properties" *ns*)
    => #(.startsWith % "File type: application/unknown, size:")

    (src/doc "not-a-resource" *ns*) => nil))
