(ns compliment.sources.t-namespaces-and-classes
  (:require [fudje.sweet :refer :all]
            [clojure.test :refer :all]
            [compliment.sources.namespaces-and-classes :as src]
            [compliment.context :as ctx]
            [compliment.utils :as utils]
            [compliment.t-helpers :refer :all]))

(defn- -ns [] (find-ns 'compliment.sources.t-namespaces-and-classes))

(deftest namespaces-and-classes
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

(deftest ns-class-completion
  (in-ns 'compliment.sources.t-namespaces-and-classes)

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

    (src/candidates "java.io.Stri" (-ns) nil)
    => (contains #{{:candidate "java.io.StringReader", :type :class}
                   {:candidate "java.io.StringWriter", :type :class}} :gaps-ok)

    (src/candidates "j.i.F" (-ns) nil)
    => [] ;; Because fuzziness works only for classes imported into current ns

    (do (import 'java.io.File)
        (src/candidates "j.i.F" (-ns) nil))
    => (contains [{:candidate "java.io.File", :type :class}])

    ;; Imported classes without package qualifiers are covered by ns-mappings
    ;; source, see respective test file.
    )

  (fact "aliases are completed by this source too"
    (strip-tags (src/candidates "#'clojure.co" (-ns) nil)) => (contains ["#'clojure.core"])

    (do (require '[clojure.string :as str])
        (strip-tags (src/candidates "st" (-ns) nil)))
    => (contains ["str"])

    (do (require '[clojure.string :as c.str])
        (strip-tags (src/candidates "c.st" (-ns) nil)))
    => (contains ["c.str"]))

  (fact "anonymous and inner classes are not suggested"
    (strip-tags (src/candidates "java.util.ArrayDeq" (-ns) nil))
    => (just ["java.util.ArrayDeque"]))

  (fact "for prefixes without a period only root package names are suggested"
    (strip-tags (src/candidates "jd" (-ns) nil))
    => (just ["jdk."])

    ;; But if the prefix is a full root package name, then suggest classes.
    (src/candidates "jdk" (-ns) nil)
    => (checker #(> (count %) 100)))

  (fact "capitalized prefixes are treated as short classnames for completing FQN"
    (strip-tags (src/candidates "BiPred" (-ns) nil))
    => (just ["java.util.function.BiPredicate"])

    (sort (strip-tags (src/candidates "Array" (-ns) nil)))
    => (contains #{"java.util.ArrayList" "java.lang.reflect.Array"} :gaps-ok))

  (fact "inside :import block additional rules apply"
    (src/candidates "Handler" (-ns) (ctx/parse-context '(ns (:require stuff)
                                                          (:import __prefix__))))
    => (contains #{{:candidate "clojure.asm.Handler", :type :class}
                   {:candidate "java.util.logging.Handler", :type :class}} :gaps-ok)

    (src/candidates "Lis" 'clojure.core (ctx/parse-context '(ns (:import [clojure.lang __prefix__]))))
    => [{:candidate "LispReader", :type :class}]

    (strip-tags
     (src/candidates "java.util" (-ns) (ctx/parse-context '(ns (:require stuff)
                                                             (:import __prefix__)))))
    => (contains #{"java.util.Map" "java.util.Set" "java.util.Date"} :gaps-ok))

  (fact "namespaces and classes have documentation"
    (src/doc "clojure.core" (-ns)) => (checker string?)
    (src/doc "java.lang.Runnable" (-ns)) => (checker string?)
    ;; aliases
    (src/doc "utils" (-ns)) => (checker string?)
    ;; literals
    (src/doc "'utils" (-ns)) => (checker string?)
    (src/doc "#'utils" (-ns)) => (checker string?)
    (src/doc "@#'clojure.core" (-ns)) => (checker string?)
    (src/doc "@@#'utils" (-ns)) => (checker string?)))
