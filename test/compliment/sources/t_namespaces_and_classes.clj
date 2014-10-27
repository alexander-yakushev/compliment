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
    (src/nscl-matches? "j.l.Run" "java.lang.Runtime") => truthy)

  (fact "separator in prefix can be omitted"
    (src/nscl-matches? "cstr" "clojure.string")     => truthy
    (src/nscl-matches? "cji" "clojure.java.io")     => truthy
    (src/nscl-matches? "jlRun" "java.lang.Runtime") => truthy)

  (fact "if System/getProperty returns nil, Compliment won't fail"
    (#'src/classfiles-from-path "") => ()))

(facts "about ns/class completion"
  (fact "they are completed either according to the mapping in the given
  namespace, or by classpath scanning results"
    (src/candidates "cl.s" *ns* nil)
    => (contains #{"clojure.java.shell" "clojure.set"
                   "clojure.stacktrace" "clojure.string"} :gaps-ok)

    (src/candidates "sr" *ns* nil)
    => ["src"]

    (src/candidates "clojure.java." *ns* nil)
    => (contains #{ "clojure.java.browse" "clojure.java.shell"} :gaps-ok)

    (src/candidates "java.io.Stri" *ns* nil)
    => (contains #{"java.io.StringWriter" "java.io.StringReader"} :gaps-ok)

    (src/candidates "j.i.F" *ns* nil)
    => () ; Because fuzziness works only for classes imported into current ns

    (src/candidates "j.i.F" ..some-ns.. nil)
    => ["java.io.File"]
    (provided (ns-map ..some-ns..) => {'File (utils/resolve-class 'java.io.File)})

    ;; Imported classes without package qualifiers are covered by ns-mappings
    ;; source, see respective test file.
    ))
