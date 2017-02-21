(ns compliment.sources.t-namespaces-and-classes
  (:require [midje.sweet :refer :all]
            [compliment.sources.namespaces-and-classes :as src]
            [compliment.context :as ctx]
            [compliment.utils :as utils]
            [compliment.t-helpers :refer :all]))

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
    (src/nscl-matches? "jlRun" "java.lang.Runtime") => truthy))

(facts "about ns/class completion"
  (fact "they are completed either according to the mapping in the given
  namespace, or by classpath scanning results"
    (src/candidates "cl.s" *ns* nil)
    => (strip-tags (contains #{"clojure.java.shell" "clojure.set"
                               "clojure.stacktrace" "clojure.string"} :gaps-ok))

    (src/candidates "src" *ns* nil)
    => (strip-tags (just ["src"]))

    (src/candidates "clojure.java." *ns* nil)
    => (contains #{{:candidate "clojure.java.browse", :type :namespace}
                   {:candidate "clojure.java.shell", :type :namespace}} :gaps-ok)

    (src/candidates "java.io.Stri" *ns* nil)
    => (contains #{{:candidate "java.io.StringReader", :type :class}
                   {:candidate "java.io.StringWriter", :type :class}} :gaps-ok)

    (src/candidates "j.i.F" *ns* nil)
    => () ; Because fuzziness works only for classes imported into current ns

    (import 'java.io.File)
    (src/candidates "j.i.F" *ns* nil)
    => (contains [{:candidate "java.io.File", :type :class}])

    ;; Imported classes without package qualifiers are covered by ns-mappings
    ;; source, see respective test file.
    )

  (fact "aliases are completed by this source too"
    (require '[clojure.string :as str])
    (src/candidates "st" *ns* nil) => (strip-tags (contains ["str"]))

    (require '[clojure.string :as c.str])
    (src/candidates "c.st" *ns* nil) => (strip-tags (contains ["c.str"])))

  (fact "anonymous and inner classes are not suggested"
    (src/candidates "java.util.ArrayDeq" *ns* nil)
    => (strip-tags (just ["java.util.ArrayDeque"])))

  (fact "inside :import block additional rules apply"
    (src/candidates "Handler" *ns* (ctx/parse-context '(ns (:require stuff)
                                                         (:import __prefix__))))
    => (contains #{{:candidate "clojure.asm.Handler", :type :class}
                   {:candidate "java.util.logging.Handler", :type :class}} :gaps-ok)

    (src/candidates "Lis" 'clojure.core (ctx/parse-context '(ns (:import [clojure.lang __prefix__]))))
    => [{:candidate "LispReader", :type :class}]

    (src/candidates "java.util" *ns* (ctx/parse-context '(ns (:require stuff)
                                                           (:import __prefix__))))
    => (strip-tags
        (contains #{"java.util.Map" "java.util.Set" "java.util.Date"} :gaps-ok)))

  (fact "namespaces and classes have documentation"
    (src/doc "clojure.core" *ns*) => string?
    (src/doc "java.lang.Runnable" *ns*) => string?))
