(ns compliment.sources.t-resources
  (:require [clojure.java.io :as io]
            [compliment.context :as ctx]
            [compliment.sources.resources :as src]
            [compliment.t-helpers :refer :all]
            [fudje.sweet :refer :all]
            [clojure.test :refer :all]))

(deftest resources-test
  (fact "completion works when started from a string in a resource call"
    (src/candidates "comp" *ns* (ctx/parse-context '(resource "__prefix__")))
    => [{:candidate "compliment/dummy_resource.txt"
         :type :resource}]

    (src/candidates "comp" *ns* (ctx/parse-context '(io/resource "__prefix__")))
    => [{:candidate "compliment/dummy_resource.txt"
         :type :resource}]

    (src/candidates "comp" *ns* nil) => nil)

  (fact "there are docs for resources too"
    (src/doc "compliment/dummy_resource.txt" *ns*)
    => (checker #(.startsWith ^String % "File type: text/plain, size: 43 bytes"))

    (src/doc "not-a-resource" *ns*) => nil))
