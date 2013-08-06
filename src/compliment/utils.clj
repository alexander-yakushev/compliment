(ns compliment.utils
  (:require clojure.main))

(defn parts-match? [prefix-parts complete-parts]
  (and (<= (count prefix-parts) (count complete-parts))
       (loop [p prefix-parts, n complete-parts]
         (if (first p)
           (when (.startsWith ^String (first n) ^String (first p))
             (recur (rest p) (rest n)))
           true))))

(defn resolve-symbol [sym]
  (try (resolve sym)
       (catch Exception e
         (when (not= ClassNotFoundException
                     (class (clojure.main/repl-exception e)))
           (throw e)))))

(defn resolve-class [sym]
  (when-let [val (resolve-symbol sym)]
    (when (class? val) val)))

(defn resolve-namespace [sym ns]
  (or (find-ns sym) ((ns-aliases ns) sym)))
