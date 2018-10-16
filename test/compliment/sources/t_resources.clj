(ns compliment.sources.t-resources
  (:require [clojure.java.io :as io]
            [compliment.context :as ctx]
            [compliment.sources.resources :as src]
            [compliment.t-helpers :refer :all]
            [fudje.sweet :refer :all]
            [clojure.test :refer :all]))

(deftest resources-test
  (fact "completion works when started from a string in a resource call"
    (src/candidates "META" *ns* (ctx/parse-context '(resource "__prefix__")))
    => [{:candidate "META-INF/maven/compliment/compliment/pom.properties"
         :type :resource}]

    (src/candidates "META" *ns* (ctx/parse-context '(io/resource "__prefix__")))
    => [{:candidate "META-INF/maven/compliment/compliment/pom.properties"
         :type :resource}]

    (src/candidates "META" *ns* nil) => nil)

  (fact "there are docs for resources too"
    (src/doc "META-INF/maven/compliment/compliment/pom.properties" *ns*)
    => (checker #(.startsWith % "File type: application/unknown, size:"))

    (src/doc "not-a-resource" *ns*) => nil))
