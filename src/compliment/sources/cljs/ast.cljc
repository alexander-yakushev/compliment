(ns compliment.sources.cljs.ast
  (:require [clojure.pprint :refer [pprint *print-right-margin*]]
            [clojure.zip :as z])
  #?(:clj (:import [clojure.lang IPersistentList IPersistentMap IPersistentVector ISeq])))

(def V #?(:clj IPersistentVector
          :cljs PersistentVector))
(def M #?(:clj IPersistentMap
          :cljs PersistentArrayMap))
(def L #?(:clj IPersistentList
          :cljs List))
(def S ISeq)

;; Thx @ Alex Miller! http://www.ibm.com/developerworks/library/j-treevisit/
(defmulti tree-branch? type)
(defmethod tree-branch? :default [_] false)
(defmethod tree-branch? V [v] (not-empty v))
(defmethod tree-branch? M [m] (not-empty m))
(defmethod tree-branch? L [l] true)
(defmethod tree-branch? S [s] true)
(prefer-method tree-branch? L S)

(defmulti tree-children type)
(defmethod tree-children V [v] v)
(defmethod tree-children M [m] (->> m seq (apply concat)))
(defmethod tree-children L [l] l)
(defmethod tree-children S [s] s)
(prefer-method tree-children L S)

(defmulti tree-make-node (fn [node children] (type node)))
(defmethod tree-make-node V [v children]
  (vec children))
(defmethod tree-make-node M [m children]
  (apply hash-map children))
(defmethod tree-make-node L [_ children]
  children)
(defmethod tree-make-node S [node children]
  (apply list children))
(prefer-method tree-make-node L S)

(defn tree-zipper [node]
  (z/zipper tree-branch? tree-children tree-make-node node))

(defn print-tree
  "for debugging"
  [node]
  (let [all (take-while (complement z/end?) (iterate z/next (tree-zipper node)))]
    (binding [*print-right-margin* 20]
      (pprint
       (->> all
            (map z/node) (zipmap (range))
            sort)))))
