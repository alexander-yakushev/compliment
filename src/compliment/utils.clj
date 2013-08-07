(ns compliment.utils
  "Functions and utilities for source implementations."
  (:require clojure.main))

(defn parts-match?
  "Tests if each part of the complete symbol starts with each
  respective part of the prefix. Both arguments should be lists of
  string."
  [prefix-parts complete-parts]
  (and (<= (count prefix-parts) (count complete-parts))
       (loop [p prefix-parts, n complete-parts]
         (if (first p)
           (when (.startsWith ^String (first n) ^String (first p))
             (recur (rest p) (rest n)))
           true))))

(defn resolve-symbol
  "Tries to resolve a symbol in the current namespace, or returns nil
  if the symbol can't be resolved."
  [sym]
  (try (resolve sym)
       (catch Exception e
         (when (not= ClassNotFoundException
                     (class (clojure.main/repl-exception e)))
           (throw e)))))

(defn resolve-class
  "Tries to resolve a classname from the given symbol, or returns nil
  if classname can't be resolved."
  [sym]
  (when-let [val (resolve-symbol sym)]
    (when (class? val) val)))

(defn resolve-namespace
  "Tries to resolve a namespace from the given symbol, either from a
  fully qualified name or an alias in the given namespace."
  [sym ns]
  (or (find-ns sym) ((ns-aliases ns) sym)))
