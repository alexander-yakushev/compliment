(ns generate
  (:require [clojure.java.io :as io]
            [zprint.core :as zprint]
            [clojure.walk :as walk])
  (:import java.io.PushbackReader
           java.io.RandomAccessFile
           java.nio.channels.Channels))

(def files ["compliment/utils.clj"
            "compliment/sources.clj"
            "compliment/sources/class_members.clj"
            "compliment/sources/namespaces.clj"
            "compliment/sources/classes.clj"
            "compliment/sources/vars.clj"
            "compliment/sources/keywords.clj"
            "compliment/sources/special_forms.clj"
            "compliment/core.clj"])

(def destination "src/compliment/lite.clj")

(defn replace-lite-forms [form]
  (walk/prewalk #(cond (contains? (meta %) :lite)
                       (let [lite (:lite (meta %))]
                         (if (and (sequential? lite)
                                  (= (first lite) 'quote))
                           (second lite)
                           lite))

                       ;; Strip internal alias from some functions.
                       (and (symbol? %) (#{"utils" "vars"} (namespace %)))
                       (symbol (name %))

                       :else %)
                form))

(defn find-imports [ns-form]
  (some #(when (and (sequential? %) (= (first %) :import))
           (rest %))
        ns-form))

(defn make-header-form [imports]
  (list 'ns 'compliment.lite
        '(:require [clojure.string :as str])
        (list* :import imports)))

(defn generate-lite-version []
  (let [imports (volatile! [])]
    (with-open [wr (io/writer destination)]
      (binding [*ns* (create-ns 'compliment.lite)]
        (let [body
              (with-out-str
                (doseq [f files
                        :let [ff (io/file "../src/" f)]]
                  (with-open [rdr (PushbackReader. (io/reader (io/file ff) ))]
                    (println "\n;;" f)
                    (loop []
                      (when-let [form (read rdr false nil)]
                        (when-let [form (replace-lite-forms form)]
                          (if (and (sequential? form) (= (first form) 'ns))
                            (vswap! imports into (find-imports form))

                            (binding [*print-meta* true]
                              (println)
                              (zprint/zprint form {:style [:anon-fn :backtranslate]}))))
                        (recur))))))]
          (binding [*out* wr]
            (println ";; This file was generated at" (str (java.util.Date.)))
            (println ";; SPDX-License-Identifier: EPL-1.0")
            (println ";; Do not edit manually! Check https://github.com/alexander-yakushev/compliment/tree/master/lite")
            (zprint/zprint (make-header-form @imports))
            (println body)))))
    @imports))

(generate-lite-version)
