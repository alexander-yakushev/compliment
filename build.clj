(ns build
  (:refer-clojure :exclude [test])
  (:require [clojure.tools.build.api :as b]
            [clojure.tools.build.tasks.write-pom]
            [deps-deploy.deps-deploy :as dd]))

(def default-opts
  (let [url "https://github.com/alexander-yakushev/compliment"
        version "0.5.4-SNAPSHOT"]
    {;; Pom section
     :lib 'compliment/compliment
     :version version
     :scm {:url url, :tag version}
     :pom-data [[:description "Clojure completion library that you deserve"]
                [:url url]
                [:licenses
                 [:license
                  [:name "Eclipse Public License"]
                  [:url "http://www.eclipse.org/legal/epl-v10.html"]]]]

     ;; Build section
     :basis (b/create-basis {})
     :target "target"
     :class-dir "target/classes"}))

(defmacro opts+ [& body]
  `(let [~'opts (merge default-opts ~'opts)]
     ~@body
     ~'opts))

(defn- jar-file [{:keys [target lib version]}]
  (format "%s/%s-%s.jar" target (name lib) version))

(defn clean [opts] (b/delete {:path (:target (opts+))}))

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
  (opts+
    (doto opts clean b/write-pom)
    (let [{:keys [class-dir basis]} opts
          jar (jar-file opts)]
      (println (format "Building %s..." jar))
      (b/copy-dir {:src-dirs   (:paths basis)
                   :target-dir class-dir
                   :include    "**"
                   :ignores    [#".+\.java"]})
      (b/jar (assoc opts :jar-file jar)))))

(defn deploy "Deploy the JAR to Clojars." [opts]
  (opts+
    (dd/deploy {:installer :remote
                :artifact (b/resolve-path (jar-file opts))
                :pom-file (b/pom-path opts)})))
