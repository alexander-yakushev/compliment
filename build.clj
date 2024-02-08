(ns build
  (:refer-clojure :exclude [test])
  (:require [clojure.string :as str]
            [clojure.tools.build.api :as b]
            clojure.tools.build.tasks.write-pom
            [org.corfield.build :as bb]))

(defmacro opts+ []
  `(let [url# "https://github.com/alexander-yakushev/compliment"
         version# "0.5.2-SNAPSHOT"]
     (-> {:lib 'compliment
          :version version#
          :tag version#
          :scm {:url url#}
          :pom-data [[:description "Clojure completion library that you deserve"]
                     [:url url#]
                     [:licenses
                      [:license
                       [:name "Eclipse Public License"]
                       [:url "http://www.eclipse.org/legal/epl-v10.html"]]]]}
         (merge ~'opts))))

;; Hack to propagate scope into pom.
(alter-var-root
 #'clojure.tools.build.tasks.write-pom/to-dep
 (fn [f]
   (fn [[_ {:keys [mvn/scope]} :as arg]]
     (let [res (f arg)
           alias (some-> res first namespace)]
       (cond-> res
         (and alias scope) (conj [(keyword alias "scope") scope]))))))

(defn jar
  "Compile and package the JAR."
  [opts]
  (bb/clean opts)
  (let [{:keys [class-dir src+dirs] :as opts} (#'bb/jar-opts (opts+))]
    (b/write-pom opts)
    (b/copy-dir {:src-dirs   src+dirs
                 :target-dir class-dir
                 :include    "**"
                 :ignores    [#".+\.java"]})
    (println "Building jar...")
    (b/jar opts)))

(defn deploy "Deploy the JAR to Clojars." [opts]
  (bb/deploy (opts+)))
