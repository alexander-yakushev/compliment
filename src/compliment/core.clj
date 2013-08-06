(ns compliment.core
  (:require (compliment.sources ns-mappings
                                namespaces-and-classes
                                class-members))
  (:use [compliment.sources :only [all-sources]]
        [compliment.context :only [cache-context]]
        [clojure.string :only [join]]))

(defn completions
  ([prefix context]
     (completions prefix *ns* context))
  ([prefix ns context]
     (let [ctx (cache-context context)]
       (flatten
        (for [[_ {:keys [candidates enabled]}] (all-sources)
              :when enabled
              :let [cands (candidates prefix ns ctx)]
              :when cands]
          cands)))))

(defn documentation
  ([symbol-str]
     (documentation symbol-str *ns*))
  ([symbol-str ns]
     (->> (for [[_ {:keys [doc enabled]}] (all-sources)
                :when enabled
                :let [docstr (doc symbol-str ns)]
                :when docstr]
            docstr)
          (interpose "\n\n")
          join)))
