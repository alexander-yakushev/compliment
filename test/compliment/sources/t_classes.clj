(ns compliment.sources.t-classes
  (:require [fudje.sweet :refer :all]
            [clojure.test :refer :all]
            [compliment.sources.classes :as src]
            [compliment.context :as ctx]
            [compliment.t-helpers :refer :all])
  (:import java.io.File))

(defn- -ns [] (find-ns 'compliment.sources.t-classes))

(defrecord Animal [name])

(deftest class-completion
  (fact "they are completed either according to the mapping in the given
  namespace, or by classpath scanning results"
    (src/candidates "java.io.Stri" (-ns) nil)
    => (contains #{{:candidate "java.io.StringReader", :type :class}
                   {:candidate "java.io.StringWriter", :type :class}} :gaps-ok)

    (src/candidates "j.i.I" (-ns) nil)
    => [] ;; Because fuzziness works only for classes imported into current ns

    (src/candidates "j.i.F" (-ns) nil)
    => (contains [{:candidate "java.io.File", :type :class}]))

  (fact "local directories are correctly scanned for classes"
    (src/candidates "compliment.E" (-ns) nil)
    => [{:candidate "compliment.Example", :type :class}])

  (fact "imported classes are looked up in the given namespace"
    (src/candidates "Runt" (-ns) nil)
    => (contains [{:candidate "Runtime", :type :class, :package "java.lang"}
                  {:candidate "RuntimePermission", :type :class, :package "java.lang"}
                  {:candidate "RuntimeException", :type :class, :package "java.lang"}]
         :gaps-ok))

  (fact "defrecord produces classes that don't have a package"
        ;; Since JDK9, they inherit the namespace package
        (src/candidates "Anim" (-ns) nil)
        => (contains [{:candidate "Animal", :type :class
                       :package (if (try (resolve 'java.lang.Runtime$Version)
                                         (catch Exception _))
                                  "compliment.sources.t_classes"
                                  nil)}]))

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

  (fact "classes have documentation"
    (src/doc "java.lang.Runnable" (-ns)) => (checker string?)))
