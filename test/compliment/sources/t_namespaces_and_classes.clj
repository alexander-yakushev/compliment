(ns compliment.sources.t-namespaces-and-classes
  (:require [midje.sweet :refer :all]
            [compliment.sources.namespaces-and-classes :as src]
            [compliment.context :as ctx]
            [compliment.utils :as utils]))

(facts "about namespaces/classes"
  (fact "ns/class symbol is something that looks like part of either"
    "clojure"       => src/nscl-symbol?
    "clojure.z"     => src/nscl-symbol?
    "java.lang.Run" => src/nscl-symbol?)

  (fact "fuzzy matching nses/classes works with period as separator"
    (src/nscl-matches? "c.str" "clojure.string")      => truthy
    (src/nscl-matches? "c.j.i" "clojure.java.io")     => truthy
    (src/nscl-matches? "j.l.Run" "java.lang.Runtime") => truthy))

(facts "about ns/class completion"
  (fact "they are completed according to the mapping in the given namespace"
    (src/candidates "cl.s" *ns* nil)
    => (just #{"clojure.set" "clojure.stacktrace" "clojure.string"})

    (src/candidates "ct" *ns* nil)
    => ["ctx"]

    (src/candidates "j.i.F" *ns* nil)
    => () ; Because java.io.File is not imported into the current ns

    (src/candidates "j.i.F" ..some-ns.. nil)
    => ["java.io.File"]
    (provided (ns-map ..some-ns..) => {'File (utils/resolve-class 'java.io.File)})

    ;; Imported classes are covered by ns-mappings source, see
    ;; respective test file.
    ))
