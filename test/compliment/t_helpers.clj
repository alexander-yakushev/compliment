(ns compliment.t-helpers
  (:require [clojure.test :refer [is]]
            [compliment.utils :as utils]
            [matcher-combinators.test :refer [match?]]))

(defn strip-tags
  "Returns a wrapper predicate that first removes tags from the list of
  candidates."
  [completions]
  (map :candidate completions))

(defmacro is? [expected actual]
  `(is (~'match? ~expected ~actual)))

(def jdk11+? (try (resolve 'java.lang.Runtime$Version)
                  (catch Exception _)))

(def bb? (some? (System/getProperty "babashka.version")))

(defmacro when-not-bb [& body]
  (when-not bb?
    `(do ~@body)))
