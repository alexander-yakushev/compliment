(ns compliment.sources.class-members
  "Completion for both static and non-static class members."
  (:require [compliment.sources :refer [defsource]]
            [compliment.utils :refer [fuzzy-matches-no-skip? resolve-class]]
            [clojure.string :refer [join]])
  (:import [java.lang.reflect Method Field Member Modifier]))

(defn static?
  "Tests if class member is static."
  [^Member member]
  (Modifier/isStatic (.getModifiers member)))

;; ## Regular (non-static) members

(def members-cache
  "Stores cache of all non-static members for every namespace."
  (atom {}))

(defn populate-members-cache
  "Populate members cache of class members for `ns` from the given list of
  classes. `imported-classes-cnt` is a number that indicates the current number
  of imported classes in this namespace."
  [ns classes imported-classes-cnt]
  (let [members
        (for [^Class class classes
              ^Member member (concat (.getMethods class) (.getFields class))
              :when (not (static? member))]
          (let [dc (.getDeclaringClass member)]
            (if (= dc class)
              member
              (if (instance? Method member)
                (.getMethod dc (.getName member)
                            (.getParameterTypes ^Method member))
                (.getField dc (.getName member))))))

        cache
        (reduce (fn [cache, ^Member m]
                  (let [full-name (.getName m)]
                    (assoc! cache full-name (conj (cache full-name []) m))))
                (transient {}) members)]
    (swap! members-cache assoc ns {:classes (set classes)
                                   :imported-classes-cnt imported-classes-cnt
                                   :members (persistent! cache)})))

(defn update-cache
  "Updates members cache for a given namespace if necessary."
  ([ns] (update-cache ns nil))
  ([ns context-class]
   (let [imported-classes (set (filter class? (vals (ns-map ns))))
         imported-classes-cnt (count imported-classes)
         cache (@members-cache ns)]
     (when (or (nil? cache)
               (not= (:imported-classes-cnt cache) imported-classes-cnt)
               (and context-class
                    (not (contains? (:classes cache) context-class))))
       (let [classes (cond-> (into imported-classes (:classes cache))
                       context-class (conj context-class))]
         (populate-members-cache ns classes imported-classes-cnt))))))

(defn get-all-members
  "Returns all non-static members for a given namespace."
  [ns context-class]
  (update-cache ns context-class)
  (get-in @members-cache [ns :members]))

(defn class-member-symbol?
  "Tests if a symbol name looks like a non-static class member."
  [^String x]
  (.startsWith x "."))

(defn camel-case-matches?
  "Tests if prefix matches the member name following camel case rules.
  Thus, prefix `getDeF` matches member `getDeclaredFields`."
  [prefix member-name]
  (fuzzy-matches-no-skip? prefix member-name #(Character/isUpperCase ^char %)))

(defn try-get-object-class
  "Tries to get the type of the object from the context, which the member will be
  applied to. Object should be a symbol resolving to a Var or have a type tag."
  [ns context]
  (when (= (:idx (first context)) 0)
    (let [sym (second (:form (first context)))]
      (when (symbol? sym)
        (if-let [tag (:tag (meta sym))]
          ;; Symbol has a tag - try to resolve the class from it.
          (resolve-class ns tag)
          ;; Otherwise, try to resolve symbol to a Var.
          (let [obj (ns-resolve ns sym)]
            (and (= (class obj) clojure.lang.Var)
                 (class (deref obj)))))))))

(defn members-candidates
  "Returns a list of Java non-static fields and methods candidates."
  [prefix ns context]
  (when (class-member-symbol? prefix)
    (let [prefix (subs prefix 1)
          inparts? (re-find #"[A-Z]" prefix)
          klass (try-get-object-class ns context)]
      (for [[member-name members] (get-all-members ns klass)
            :when (if inparts?
                    (camel-case-matches? prefix member-name)
                    (.startsWith ^String member-name prefix))
            :when
            (or (not klass)
                (some #(= klass (.getDeclaringClass ^Member %)) members))]
        {:candidate (str "." member-name)
         :type (if (instance? Method (first members))
                 :method :field)}))))

;; ### Member documentation

(defn type-to-pretty-string
  "Takes a type (either a class or a primitive) and returns it's
  human-readable name."
  [^Class t]
  (if (or (.isLocalClass t)
          (.isMemberClass t))
    (.getName t)
    (.getSimpleName t)))

(defn doc-method-parameters
  "Takes a list of method parameters and stringifies it."
  [parameters]
  (->> parameters
       (map type-to-pretty-string)
       (interpose " ")
       join
       (format "(%s)")))

(defn create-members-doc
  "Takes a list of members (presumably with the same name) and turns
  them into a docstring."
  [members]
  (->> members
       (group-by (fn [^Member m] (.getDeclaringClass m)))
       (map (fn [[^Class class, members]]
              (let [^Member f-mem (first members)]
                (str (.getName class) "." (.getName f-mem)
                     (if (instance? Field f-mem)
                       (str " = " (try (.get ^Field f-mem nil)
                                       (catch Exception e "?"))
                            " (" (type-to-pretty-string (.getType ^Field f-mem)) ")\n"
                            (Modifier/toString (.getModifiers f-mem)))
                       (join
                        (map (fn [^Method member]
                               (when (instance? Method member)
                                 (str "\n  " (doc-method-parameters (.getParameterTypes member))
                                      " -> " (type-to-pretty-string (.getReturnType ^Method member))
                                      " (" (Modifier/toString (.getModifiers member)) ")")))
                             (distinct members))))
                     "\n"))))
       (interpose "\n")
       join))

(defn members-doc
  "Documentation function for non-static members."
  [member-str ns]
  (when (class-member-symbol? member-str)
    (update-cache ns)
    (when-let [member (get-in @members-cache [ns :members (subs member-str 1)])]
      (create-members-doc member))))

(defn classname-doc [^Class class]
  (let [members (group-by static? (concat (.getMethods class)
                                          (.getFields class)))
        [static non-static] (for [flag [true false]]
                              (->> (for [^Member m (members flag)]
                                     (.getName m))
                                   distinct
                                   (interpose ", ")
                                   join))]
    (str (.getName class) "\n\n"
         " Non-static members:\n  " non-static "\n\n"
         " Static members:\n  " static "\n")))

(defsource ::members
  :candidates #'members-candidates
  :doc #'members-doc
  :tag-fn (fn [m {:keys [ns]}]
            (assoc m :type (if (->> (get-in @members-cache [ns :members
                                                            (subs (:candidate m) 1)])
                                    first
                                    (instance? Method))
                             :method :field))))

;; ## Static members

(defn static-member-symbol?
  "Tests if prefix looks like a static member symbol."
  [x]
  (re-matches #"[^\/\:\.][^\:]*\/.*" x))

(def ^{:doc "Stores cache of all static members for every class."}
  static-members-cache (atom {}))

(defn populate-static-members-cache
  "Populates static members cache for a given class."
  [^Class class]
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

(defn update-static-cache
  "Updates static members cache for a given class if necessary."
  [class]
  (when-not (@static-members-cache class)
    (populate-static-members-cache class)))

(defn static-members
  "Returns all static members for a given class."
  [^Class class]
  (update-static-cache class)
  (@static-members-cache class))

(defn static-members-candidates
  "Returns a list of static member candidates."
  [^String prefix, ns context]
  (when (static-member-symbol? prefix)
    (let [[cl-name member-prefix] (.split prefix "/")
          cl (resolve-class ns (symbol cl-name))
          member-prefix (or member-prefix "")]
      (when cl
        (let [inparts? (re-find #"[A-Z]" member-prefix)]
          (for [[^String member-name members] (static-members cl)
                :when  (if inparts?
                         (camel-case-matches? member-prefix member-name)
                         (.startsWith member-name member-prefix))]
            {:candidate (str cl-name "/" member-name)
             :type (if (instance? Method (first members))
                     :static-method :static-field)}))))))

(defn resolve-static-member
  "Given a string representation of a static member returns Member object."
  [^String member-str ns]
  (let [[cl-name member-name] (.split member-str "/")
        cl (resolve-class ns (symbol cl-name))]
    (when cl
      (update-static-cache cl)
      (get-in @static-members-cache [cl member-name]))))

(defn static-member-doc
  "Given a member name and class returns its docstring."
  [member-str ns]
  (when (static-member-symbol? member-str)
    (let [member (resolve-static-member member-str ns)]
      (when member
        (create-members-doc member)))))

(defsource ::static-members
  :candidates #'static-members-candidates
  :doc #'static-member-doc)
