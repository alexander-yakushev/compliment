(ns compliment.t-utils
  (:require [clojure.test :refer :all]
            [compliment.t-helpers :refer :all]
            [compliment.utils :refer :all]
            [matcher-combinators.matchers :as mc]))

(deftest fuzzy-matching
  (testing "fuzzy matching works with or without separators provided"
    (is (fuzzy-matches? "ge-la-me-da" "get-last-message-date" \-))
    (is (fuzzy-matches? "gelameda" "get-last-message-date" \-)))

  (let [symbol "get-last-message-date"]
    (testing "cases where fuzzy matching should or shouldn't work"
      (is? true (fuzzy-matches? "" symbol \-))
      (is? true (fuzzy-matches? "ge" symbol \-))
      (is? true (fuzzy-matches? "ge-" symbol \-))
      (is? true (fuzzy-matches? "ge-la" symbol \-))
      (is? true (fuzzy-matches? "gela" symbol \-))
      (is? true (fuzzy-matches? "ge-last" symbol \-))
      (is? true (fuzzy-matches? "gelast-" symbol \-))
      (is? true (fuzzy-matches? "ge-me" symbol \-))
      (is? true (fuzzy-matches? "geme" symbol \-))
      (is? true (fuzzy-matches? "getme" symbol \-))
      (is? true (fuzzy-matches? "get-me" symbol \-))

      (is? false (fuzzy-matches? "et-la" symbol \-))
      (is? false (fuzzy-matches? "-get" symbol \-))
      (is? false (fuzzy-matches? "geast" symbol \-))
      (is? false (fuzzy-matches? "getm-e" symbol \-))
      (is? false (fuzzy-matches? "get-lat" symbol \-))))

  (let [symbol "getImplementationVendor"
        pred #(Character/isUpperCase ^char %)]
    (testing "rules for camel-case matching"
      (is? true (fuzzy-matches-no-skip? "" symbol pred))
      (is? true (fuzzy-matches-no-skip? "gIV" symbol pred))
      (is? true (fuzzy-matches-no-skip? "getImVendor" symbol pred))
      (is? true (fuzzy-matches-no-skip? "getVen" symbol pred))

      (is? false (fuzzy-matches-no-skip? "ImpVen" symbol pred))
      (is? false (fuzzy-matches-no-skip? "getmple" symbol pred)))))

(deftest split-by-leading-literals-test
  (testing "separates quote/var/deref qualifiers from a var name"
    (is? ["@" "some-atom"] (split-by-leading-literals "@some-atom"))
    (is? ["@#'" "a"] (split-by-leading-literals "@#'a"))
    (is? ["#'" "ns/var"] (split-by-leading-literals "#'ns/var"))
    (is? [nil "nothing"] (split-by-leading-literals "nothing"))))

(deftest classpath-test
  (testing "if System/getProperty returns nil, Compliment won't fail"
    (is? [] (#'compliment.utils/list-files "" true))))

(deftest namespace-resolving-test
  (require '[compliment.context :as user])
  (testing "can resolve a namespace aliased as user"
    (is? (find-ns 'compliment.context) (resolve-namespace 'user *ns*))))

(deftest classes-on-classpath-test
  (is? (mc/all-of #(> (count %) 3000)
                  (mc/embeds ["java.lang.Thread" "java.io.File" "java.nio.channels.FileChannel"]))
       (classes-on-classpath)))

(deftest namespaces&files-on-classpath-test
  (is? (mc/embeds [{:ns-str "compliment.t-utils" :file "compliment/t_utils.clj"}])
       (namespaces&files-on-classpath))
  (is? (mc/embeds [{:ns-str "dummy" :file "dummy.cljs"}])
       (namespaces&files-on-classpath)))
