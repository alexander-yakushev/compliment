(ns compliment.t-utils
  (:require [fudje.sweet :refer :all]
            [clojure.test :refer :all]
            [compliment.utils :refer :all]))

(deftest fuzzy-matching
  (fact "fuzzy matching works with or without separators provided"
    (fuzzy-matches? "ge-la-me-da" "get-last-message-date" \-) => truthy
    (fuzzy-matches? "gelameda" "get-last-message-date" \-) => truthy)

  (let [symbol "get-last-message-date"]
    (fact "cases where fuzzy matching should or shouldn't work"
      (fuzzy-matches? "ge" symbol \-)      => truthy
      (fuzzy-matches? "ge-" symbol \-)     => truthy
      (fuzzy-matches? "ge-la" symbol \-)   => truthy
      (fuzzy-matches? "gela" symbol \-)    => truthy
      (fuzzy-matches? "ge-last" symbol \-) => truthy
      (fuzzy-matches? "gelast-" symbol \-) => truthy
      (fuzzy-matches? "ge-me" symbol \-)   => truthy
      (fuzzy-matches? "geme" symbol \-)    => truthy

      (fuzzy-matches? "et-la" symbol \-)   => falsey
      (fuzzy-matches? "-get" symbol \-)    => falsey
      (fuzzy-matches? "geast" symbol \-)   => falsey
      (fuzzy-matches? "get-lat" symbol \-) => falsey))

  (let [symbol "getImplementationVendor"
        pred #(Character/isUpperCase ^char %)]
    (fact "rules for camel-case matching"
      (fuzzy-matches-no-skip? "gIV" symbol pred) => truthy
      (fuzzy-matches-no-skip? "getImVendor" symbol pred) => truthy
      (fuzzy-matches-no-skip? "getVen" symbol pred) => truthy

      (fuzzy-matches-no-skip? "ImpVen" symbol pred) => falsey
      (fuzzy-matches-no-skip? "getmple" symbol pred) => falsey)))

(deftest classpath-test
  (testing "if System/getProperty returns nil, Compliment won't fail"
    (is (= () (#'compliment.utils/list-files "" true)))))

(deftest namespace-resolving-test
  (require '[compliment.context :as user])
  (fact "can resolve a namespace aliased as user"
    (resolve-namespace 'user *ns*) => (find-ns 'compliment.context)))

(deftest classes-on-classpath-test
  (let [all-classes (->> (classes-on-classpath)
                         (vals)
                         (reduce into)
                         (set))]
    (is (< 3000 (count all-classes)))
    (is (contains? all-classes "java.lang.Thread"))
    (is (contains? all-classes "java.io.File"))
    (is (contains? all-classes "java.nio.channels.FileChannel"))))
