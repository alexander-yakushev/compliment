(ns compliment.sources.t-namespaces
  (:require [fudje.sweet :refer :all]
            [clojure.test :refer :all]
            [compliment.sources.namespaces :as src]
            [compliment.utils :as utils]
            [compliment.t-helpers :refer :all]))

(defn- -ns [] (find-ns 'compliment.sources.t-namespaces))

(deftest namespaces-and-classes-prefixes
  (fact "ns/class symbol is something that looks like part of either"
    "clojure"       => (checker src/nscl-symbol?)
    "clojure.z"     => (checker src/nscl-symbol?)
    "java.lang.Run" => (checker src/nscl-symbol?))

  (fact "fuzzy matching nses/classes works with period as separator"
    (src/nscl-matches? "c.str" "clojure.string")      => truthy
    (src/nscl-matches? "c.j.i" "clojure.java.io")     => truthy
    (src/nscl-matches? "j.l.Run" "java.lang.Runtime") => truthy)

  (fact "separator in prefix can be omitted"
    (src/nscl-matches? "cstr" "clojure.string")     => truthy
    (src/nscl-matches? "cji" "clojure.java.io")     => truthy
    (src/nscl-matches? "jlRun" "java.lang.Runtime") => truthy))

(deftest ns-completion
  (in-ns 'compliment.sources.t-namespaces)

  (fact "they are completed either according to the mapping in the given
  namespace, or by classpath scanning results"
    (strip-tags (src/candidates "cl.s" (-ns) nil))
    => (contains #{"clojure.java.shell" "clojure.set"
                   "clojure.stacktrace" "clojure.string"} :gaps-ok)

    (strip-tags (src/candidates "src" (-ns) nil))
    => (just ["src"])

    (src/candidates "clojure.java." (-ns) nil)
    => (contains #{{:candidate "clojure.java.browse", :type :namespace, :file "clojure/java/browse.clj"}
                   {:candidate "clojure.java.shell", :type :namespace, :file "clojure/java/shell.clj"}} :gaps-ok)

    (strip-tags (src/candidates "#'clojure.co" (-ns) nil))
    => (contains ["#'clojure.core"]))

  (fact "aliases are completed by this source too"
    (do (require '[clojure.string :as str])
        (strip-tags (src/candidates "st" (-ns) nil)))
    => (contains ["str"])

    (do (require '[clojure.string :as c.str])
        (strip-tags (src/candidates "c.st" (-ns) nil)))
    => (contains ["c.str"]))

    (fact "namespaces have documentation"
    (src/doc "clojure.core" (-ns)) => (checker string?)
    (src/doc "utils" (-ns)) => (checker string?)
    (src/doc "#'utils" (-ns)) => (checker string?)))
