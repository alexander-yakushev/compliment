(ns compliment.sources.class-members
  (:use [compliment.sources :only [defsource]]
        [compliment.utils :only [parts-match? resolve-class]]
        [clojure.string :only [split join]])
  (:import [java.lang.reflect Method Field Member Modifier]))

(defn static? [^Member member]
  (Modifier/isStatic (.getModifiers member)))

;; ## Regular (non-static) members

(def members-cache (atom {}))

(defn populate-members-cache [ns classes-cnt]
  (loop [cache (transient {})

         [^Member c & r]
         (for [^Class class (vals (ns-map ns))
               :when (class? class)
               ^Member member (concat (.getMethods class) (.getFields class))
               :when (and (not (static? member)))]
           (let [dc (.getDeclaringClass member)]
             (if (= dc class)
               member
               (if (instance? Method member)
                 (.getMethod dc (.getName member)
                             (.getParameterTypes ^Method member))
                 (.getField dc (.getName member))))))]
    (if c
      (let [full-name (.getName c)]
        (if (cache full-name)
          (recur (assoc! cache full-name (conj (cache (.getName c)) c)) r)
          (recur (assoc! cache full-name [c]) r)))
      (swap! members-cache assoc ns {:classes-cnt classes-cnt
                                     :methods (persistent! cache)}))))

(defn update-cache [ns]
  (let [imported-cls-cnt (count (filter class? (vals (ns-map *ns*))))]
    (when (or (nil? (@members-cache ns))
              (not= (get-in @members-cache [ns :classes-cnt])
                    imported-cls-cnt))
      (populate-members-cache ns imported-cls-cnt))))

(defn get-all-members [ns]
  (update-cache ns)
  (get-in @members-cache [ns :methods]))

(defn class-member-symbol? [^String x]
  (.startsWith x "."))

(defn camel-case-matches?  [^String prefix, ^String classname]
  (let [regex #"[A-Z]?[a-z]*"
        prefix-parts (re-seq #"[A-Z]?[a-z]*" prefix)
        cl-parts (re-seq #"[A-Z]?[a-z]*" classname)]
    (parts-match? prefix-parts cl-parts)))

(defn try-get-object-class [context]
  (when (and (= (:idx (first context)) 0))
    (let [sym (second (:form (first context)))]
      (when (symbol? sym)
        (if (= (type (resolve sym)) clojure.lang.Var)
          (type (deref (resolve sym)))
          (type (resolve sym)))))))

(defn members-candidates
  "Returns a list of potential java non-static fields and methods
  candidates for a given namespace."
  [prefix ns context]
  (when (class-member-symbol? prefix)
    (let [prefix (subs prefix 1)
          inparts? (re-find #"[A-Z]" prefix)
          klass (try-get-object-class context)]
      (for [[member-name members] (get-all-members ns)
            :when (if inparts?
                    (camel-case-matches? prefix member-name)
                    (.startsWith ^String member-name prefix))
            :when
            (or (not klass)
                (some #(= klass (.getDeclaringClass ^Member %)) members))]
        (str "." member-name)))))

(defsource ::members
  :candidates #'members-candidates
  :doc (constantly nil))

;; ## Static members

(defn static-member-symbol? [^String x]
  (and (not (.startsWith x ":"))
       (> (.indexOf x "/") -1)))

(def static-members-cache (atom {}))

(defn populate-static-members-cache [^Class class]
  (loop [cache {}, [^Member c & r] (concat (.getMethods class)
                                           (.getFields class))]
    (if c
      (if (static? c)
        (let [full-name (.getName c)]
          (if (cache (.getName c))
            (recur (update-in cache [full-name] conj c) r)
            (recur (assoc cache full-name [c]) r)))
        (recur cache r))
      (swap! static-members-cache assoc class cache))))

(defn static-members
  "Returns a list of potential static members for a given class."
  [^Class class]
  (when-not (@static-members-cache class)
    (populate-static-members-cache class))
  (keys (@static-members-cache class)))

(defn static-members-candidates [^String prefix, ns context]
  (when (static-member-symbol? prefix)
    (let [[cl-name member-prefix] (.split prefix "/")
          cl (resolve-class (symbol cl-name))
          member-prefix (or member-prefix "")]
      (when cl
        (let [inparts? (re-find #"[A-Z]" member-prefix)]
          (for [member (static-members cl)
                :when  (if inparts?
                         (camel-case-matches? member-prefix member)
                         (.startsWith ^String member member-prefix))]
            (str cl-name "/" member)))))))

(defsource ::static-members
  :candidates #'static-members-candidates
  :doc (constantly nil))
