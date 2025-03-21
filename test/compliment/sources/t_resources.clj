(ns compliment.sources.t-resources
  (:require [clojure.java.io :as io]
            [clojure.test :refer :all]
            [compliment.context :as ctx]
            [compliment.sources.resources :as src]
            [compliment.t-helpers :refer :all]))

(deftest resources-test
  (testing "completion works when started from a string in a resource call"
    (is? [{:candidate "compliment/dummy_resource.txt" :type :resource}]
         (src/candidates "comp" *ns* (ctx/parse-context '(resource "__prefix__"))))

    (is? [{:candidate "compliment/dummy_resource.txt" :type :resource}]
         (src/candidates "comp" *ns* (ctx/parse-context '(io/resource "__prefix__"))))

    (is? nil (src/candidates "comp" *ns* nil)))

  (testing "there are docs for resources too"
    (is? #"File type: text/plain, size: 43 bytes" (src/doc "compliment/dummy_resource.txt" *ns*))
    (is? nil (src/doc "not-a-resource" *ns*))))
