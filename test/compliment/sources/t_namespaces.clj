(ns compliment.sources.t-namespaces
  (:require [clojure.test :refer :all]
            [compliment.sources.namespaces :as src]
            [compliment.t-helpers :refer :all]
            [compliment.utils :as utils]
            [matcher-combinators.matchers :as mc]))

(defn- -ns [] (find-ns 'compliment.sources.t-namespaces))

(deftest namespaces-and-classes-prefixes
  (testing "ns/class symbol is something that looks like part of either"
    (is (src/nscl-symbol? "clojure"))
    (is (src/nscl-symbol? "clojure.z"))
    (is (src/nscl-symbol? "java.lang.Run")))

  (testing "fuzzy matching nses/classes works with period as separator"
    (is (src/nscl-matches? "c.str" "clojure.string"))
    (is (src/nscl-matches? "c.j.i" "clojure.java.io"))
    (is (src/nscl-matches? "j.l.Run" "java.lang.Runtime")))

  (testing "separator in prefix can be omitted"
    (is (src/nscl-matches? "cstr" "clojure.string"))
    (is (src/nscl-matches? "cji" "clojure.java.io"))
    (is (src/nscl-matches? "jlRun" "java.lang.Runtime"))))

(deftest ns-completion
  (in-ns 'compliment.sources.t-namespaces)

  (testing "they are completed either according to the mapping in the given
  namespace, or by classpath scanning results"
    (is? (mc/embeds ["clojure.java.shell" "clojure.set"
                     "clojure.stacktrace" "clojure.string"])
         (strip-tags (src/candidates "cl.s" (-ns) nil)))

    (is? ["src"]
         (strip-tags (src/candidates "src" (-ns) nil)))

    (is? (mc/embeds [{:candidate "clojure.java.browse", :type :namespace, :file "clojure/java/browse.clj"}
                     {:candidate "clojure.java.shell", :type :namespace, :file "clojure/java/shell.clj"}])
         (src/candidates "clojure.java." (-ns) nil))

    (is? (mc/embeds ["#'clojure.core"])
         (strip-tags (src/candidates "#'clojure.co" (-ns) nil))))

  (testing "aliases are completed by this source too"
    (require '[clojure.string :as str])
    (is? ["str"] (strip-tags (src/candidates "st" (-ns) nil)))

    (require '[clojure.string :as c.str])
    (is? (mc/embeds ["c.str"])
         (strip-tags (src/candidates "c.st" (-ns) nil))))

  (testing "namespaces have documentation"
    (is? string? (src/doc "clojure.core" (-ns)))
    (is? string? (src/doc "utils" (-ns)))
    (is? string? (src/doc "#'utils" (-ns)))))
