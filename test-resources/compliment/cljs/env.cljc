(ns compliment.cljs.env
  (:require [cljs.env :as env]
            #?@(:clj [[cljs.analyzer.api :as ana]
                      [cljs.build.api :as build]
                      [cljs.compiler.api :as comp]
                      [clojure.java.io :as io]])))

(def test-namespace "compliment/test_ns.cljs" )

(defn create-test-env []
  #?(:clj
     (let [opts (build/add-implicit-options {:cache-analysis true, :output-dir "target/out"})
           env (env/default-compiler-env opts)]
       (comp/with-core-cljs env opts
         (fn []
           (let [r (io/resource test-namespace)]
             (assert r (str "Cannot find " test-namespace " is your lein profile add test-resources correctly?"))
             (ana/analyze-file env r opts))))
       @env)

     :cljs
     (do (repl/eval '(require (quote cljs-tooling.test-ns)) 'cljs-tooling.test-env)
         @env/*compiler*)))
