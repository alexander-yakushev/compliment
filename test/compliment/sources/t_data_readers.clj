(ns compliment.sources.t-data-readers
  (:require [clojure.test :refer :all]
            [compliment.sources.data-readers :as src]
            [compliment.t-helpers :refer :all]
            [matcher-combinators.matchers :as mc]))

(deftest data-readers-test
  (is? [{:candidate "#inst", :type :data-reader}]
       (src/data-reader-candidates "#in" *ns* nil))
  (is? [{:candidate "#uuid", :type :data-reader}]
       (src/data-reader-candidates "#u" *ns* nil))

  (testing "completes all data readers with #"
    (is? (mc/embeds
          [{:candidate "#uuid", :type :data-reader}
           {:candidate "#inst", :type :data-reader}])
        (src/data-reader-candidates "#" *ns* nil))))
