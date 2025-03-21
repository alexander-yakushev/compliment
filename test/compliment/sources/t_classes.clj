(ns compliment.sources.t-classes
  (:require [clojure.test :refer :all]
            [compliment.context :as ctx]
            [compliment.sources.classes :as src]
            [compliment.t-helpers :refer :all]
            [matcher-combinators.matchers :as mc])
  (:import java.io.File))

(defn- -ns [] (find-ns 'compliment.sources.t-classes))

(defrecord Animal [name])

(deftest class-completion
  (testing "they are completed either according to the mapping in the given
  namespace, or by classpath scanning results"
    (is? (mc/in-any-order [{:candidate "java.io.StringBufferInputStream", :type :class}
                           {:candidate "java.io.StringReader", :type :class}
                           {:candidate "java.io.StringWriter", :type :class}])
         (src/candidates "java.io.Stri" (-ns) nil))

    ;; Because fuzziness works only for classes imported into current ns
    (is? [] (src/candidates "j.i.I" (-ns) nil)) 

    (is? (mc/embeds [{:candidate "java.io.File", :type :class}])
         (src/candidates "j.i.F" (-ns) nil)))

  (testing "local directories are correctly scanned for classes"
    (is? [{:candidate "compliment.Example", :type :class}]
         (src/candidates "compliment.E" (-ns) nil)))

  (testing "imported classes are looked up in the given namespace"
    (is? (mc/embeds [{:candidate "Runtime", :type :class, :package "java.lang"}
                     {:candidate "RuntimePermission", :type :class, :package "java.lang"}
                     {:candidate "RuntimeException", :type :class, :package "java.lang"}])
         (src/candidates "Runt" (-ns) nil)))

  (testing "defrecord produces classes that don't have a package"
    ;; Since JDK9, they inherit the namespace package
    (is? (mc/embeds [{:candidate "Animal", :type :class
                      :package (if (try (resolve 'java.lang.Runtime$Version)
                                        (catch Exception _))
                                 "compliment.sources.t_classes"
                                 nil)}])
         (src/candidates "Anim" (-ns) nil)))

  (testing "anonymous and inner classes are not suggested"
    (is? ["java.util.ArrayDeque"]
         (strip-tags (src/candidates "java.util.ArrayDeq" (-ns) nil))))

  (testing "for prefixes without a period only root package names are suggested"
    (is? ["jdk."]
         (strip-tags (src/candidates "jd" (-ns) nil)))

    ;; But if the prefix is a full root package name, then suggest classes.
    (is? #(> (count %) 100)
         (src/candidates "jdk" (-ns) nil)))

  (testing "capitalized prefixes are treated as short classnames for completing FQN"
    (is? ["java.util.function.BiPredicate"]
         (strip-tags (src/candidates "BiPred" (-ns) nil)))

    (is? (mc/embeds ["java.util.ArrayList" "java.lang.reflect.Array"])
         (strip-tags (src/candidates "Array" (-ns) nil))))

  (testing "inside :import block additional rules apply"
    (is? (mc/embeds [{:candidate "clojure.asm.Handler", :type :class}
                     {:candidate "java.util.logging.Handler", :type :class}])
         (src/candidates "Handler" (-ns) (ctx/parse-context '(ns (:require stuff)
                                                               (:import __prefix__)))))

    (is? [{:candidate "LispReader", :type :class}]
         (src/candidates "Lis" 'clojure.core (ctx/parse-context '(ns (:import [clojure.lang __prefix__])))))

    (is? (mc/embeds ["java.util.Map" "java.util.Set" "java.util.Date"])
         (strip-tags
          (src/candidates "java.util" (-ns) (ctx/parse-context '(ns (:require stuff)
                                                                  (:import __prefix__)))))))

  (testing "classes have documentation"
    (is string? (src/doc "java.lang.Runnable" (-ns)))))
