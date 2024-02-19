(ns compliment.sources.class-members
  "Completion for both static and non-static class members."
  (:require [clojure.string :refer [join]]
            [compliment.sources :refer [defsource]]
            [compliment.sources.local-bindings :refer [bindings-from-context]]
            [compliment.utils :as utils :refer [fuzzy-matches-no-skip?
                                                resolve-class]])
  (:import [java.lang.reflect Field Member Method Modifier Constructor Executable]))

(defn static?
  "Tests if class member is static."
  [^Member member]
  (Modifier/isStatic (.getModifiers member)))

;; ## Regular (non-static) members

(def members-cache
  "Stores cache of all non-static members for every namespace."
  (atom {}))

(defn- demunge-deftype-field-name
  "If the member is a deftype field, change .x_y to .x-y for compatibility. See
  https://github.com/alexander-yakushev/compliment/issues/33."
  [^Member m, ^Class c, ^String name]
  (if (and (instance? Field m)
           (.isAssignableFrom clojure.lang.IType c))
    (.replaceAll name "_" "-")
    name))

(defn populate-members-cache
  "Populate members cache of class members for `ns` from the given list of
  classes. `imported-classes-cnt` is a number that indicates the current number
  of imported classes in this namespace."
  [ns classes imported-classes-cnt]
  (let [members
        (for [^Class class classes
              ^Member member (concat (.getMethods class) (.getFields class))
              :when (not (static? member))]
          (let [dc (.getDeclaringClass member)
                name (.getName member)
                demunged-name (demunge-deftype-field-name member dc name)]
            [demunged-name
             (if (= dc class)
               member
               (if (instance? Method member)
                 (.getMethod dc name (.getParameterTypes ^Method member))
                 (.getField dc name)))]))

        cache
        (reduce (fn [cache [full-name m]]
                  (assoc! cache full-name (conj (cache full-name []) m)))
                (transient {}) members)]
    (swap! members-cache assoc ns {:classes (set classes)
                                   :imported-classes-cnt imported-classes-cnt
                                   :members (persistent! cache)})))

(defn update-cache
  "Updates members cache for a given namespace if necessary."
  ([ns] (update-cache ns nil))
  ([ns context-class]
   (let [imported-classes (reduce-kv (fn [acc _ mapping]
                                       (if (class? mapping)
                                         (conj acc mapping)
                                         acc))
                                     [] (ns-map ns))
         imported-classes-cnt (count imported-classes)
         cache (@members-cache ns)]
     (when (or (nil? cache)
               (not= (:imported-classes-cnt cache) imported-classes-cnt)
               (and context-class
                    (not (contains? (:classes cache) context-class))))
       (let [classes (cond-> (into (set imported-classes) (:classes cache))
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
  (fuzzy-matches-no-skip? prefix member-name (fn [ch] (Character/isUpperCase ^char ch))))

^{:lite nil}
(defn try-get-object-class
  "Tries to get the type of the object from the context, which the member will be
  applied to. Object should be a symbol resolving to a Var or have a type tag."
  [ns context]
  (when (= (:idx (first context)) 0)
    (let [form (second (:form (first context)))]
      (if-let [tag (or
                    ;; Form might have an immediate tag...
                    (:tag (meta form))
                    ;; ...or a tag somewhere in local scope. Note how getting
                    ;; an element from a set can return itself but with meta.
                    (:tag (meta (get (set (bindings-from-context context ns)) form))))]
        ;; We have a tag - try to resolve the class from it.
        (resolve-class ns tag)
        ;; Otherwise, try to resolve symbol to a Var,
        ;; or literal to class.
        (or (utils/var->class ns form)
            (utils/invocation-form->class ns form)
            (utils/literal->class form))))))

(defn members-candidates
  "Returns a list of Java non-static fields and methods candidates."
  [prefix ns context]
  (when (class-member-symbol? prefix)
    (let [prefix (subs prefix 1)
          inparts? (re-find #"[A-Z]" prefix)
          klass ^{:lite nil} (try-get-object-class ns context)]
      (for [[member-name members] (get-all-members ns klass)
            :when (and (if inparts?
                         (camel-case-matches? prefix member-name)
                         (.startsWith ^String member-name prefix))
                       ^{:lite true}
                       (or (not klass)
                           (some (fn [m] (.isAssignableFrom (.getDeclaringClass ^Member m) klass))
                                 members)))]
        {:candidate (str "." member-name)
         :type (if (instance? Method (first members))
                 :method :field)}))))

;; ### Member documentation

^{:lite nil}
(defn type-to-pretty-string
  "Takes a type (either a class or a primitive) and returns it's
  human-readable name."
  [^Class t]
  (if (or (.isLocalClass t)
          (.isMemberClass t))
    (.getName t)
    (.getSimpleName t)))

^{:lite nil}
(defn doc-method-parameters
  "Takes a list of method parameters and stringifies it."
  [parameters]
  (->> parameters
       (map type-to-pretty-string)
       (interpose " ")
       join
       (format "(%s)")))

(defn- class&members->doc-dispatch [[^Class _cl members]]
  (class (first members)))

(defmulti ^:private class&members->doc #'class&members->doc-dispatch)

(defmethod class&members->doc java.lang.reflect.Method
  [[^Class cl members]]
  (let [^Member f-mem (first members)]
    (str (.getName cl) "." (.getName f-mem)
         (join
          (map (fn [^Method member]
                 (str "\n  " (doc-method-parameters (.getParameterTypes member))
                      " -> " (type-to-pretty-string (.getReturnType ^Method member))
                      " (" (Modifier/toString (.getModifiers member)) ")"))
               members))
         "\n")))

(defmethod class&members->doc java.lang.reflect.Constructor
  [[^Class cl members]]
  (str (.getName cl) ".new"
       (join
        (map (fn [^Constructor member]
               (str "\n  " (doc-method-parameters (.getParameterTypes member))))
             members))
       "\n"))

(defmethod class&members->doc java.lang.reflect.Field
  [[^Class cl members]]
  (let [^Member f-mem (first members)]
    (str (.getName cl) "." (.getName f-mem)
         (str " = " (try (.get ^Field f-mem nil)
                         (catch Exception e "?"))
              " (" (type-to-pretty-string (.getType ^Field f-mem)) ")\n"
              (Modifier/toString (.getModifiers f-mem)))
         "\n")))

^{:lite nil}
(defn create-members-doc
  "Takes a list of members (presumably with the same name) and turns
  them into a docstring."
  [members]
  (let [sort-members (fn [[^Class cl members]]
                       [cl (sort-by #(.getParameterCount ^Executable %)
                                    (distinct members))])]
    (->> members
         (group-by (fn [^Member m] (.getDeclaringClass m)))
         (map (comp class&members->doc sort-members))
         (interpose "\n")
         join)))


^{:lite nil}
(defn members-doc
  "Documentation function for non-static members."
  [member-str ns]
  (when (class-member-symbol? member-str)
    (update-cache ns)
    (when-let [member (get-in @members-cache [ns :members (subs member-str 1)])]
      (create-members-doc member))))

^{:lite nil}
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

^{:lite '(defsource :compliment.lite/members :candidates #'members-candidates)}
(defsource ::members
  :candidates #'members-candidates
  :doc #'members-doc)

;; ## Qualified members

(defn qualified-member-symbol?
  "Tests if prefix looks like a static member symbol, returns parsed parts."
  [x]
  (re-matches #"([^\/\:\.][^\:]*)\/(.*)" x))

(def static-qualified-members-only? (complement utils/clojure-1-12+?))

(def ^:private qualified-members-cache (atom {}))


(defn- populate-qualified-members-cache
  "Populates qualified methods cache for a given class."
  [^Class class]
  (let [member->cache-key #(get {:constructor "new"} %2 (.getName ^Member %1))
        methods-by-type   (group-by (comp {true :static-method false :method} static?)
                                    (.getMethods class))
        members-by-type   (merge methods-by-type
                                 {:static-field (.getFields class)
                                  :constructor  (.getConstructors class)})]
    (swap! qualified-members-cache assoc class
           (reduce (fn [cache [type members]]
                     (reduce
                      (fn [acc ^Member m]
                        (update acc (member->cache-key m type) (fnil conj (with-meta [] {:type type})) m))
                      cache members))
                   {}
                   members-by-type))))

(defn- qualified-members
  "Returns all qualified methods for a given class."
  [^Class class]
  (or (@qualified-members-cache class)
      (get (populate-qualified-members-cache class) class)))

(defn qualified-members-candidates
  "Returns a list of static member candidates for Clojure < 1.12. Else also includes non-static members and constructors."
  [^String prefix, ns _context]
  (when-let [[_ cl-name method-prefix] (qualified-member-symbol? prefix)]
    (when-let [qms (some->> (symbol cl-name)
                            (resolve-class ns)
                            qualified-members
                            not-empty)]
      (let [inparts?             (re-find #"[A-Z]" prefix)
            matching-prefix-pred (if inparts?
                                   #(camel-case-matches? method-prefix %)
                                   #(.startsWith ^String % method-prefix))
            matching-prefix      (comp matching-prefix-pred key)
            member-type-pred     (if (static-qualified-members-only?)
                                   #{:static-field :static-method}
                                   (constantly true))
            matching-member-type (comp member-type-pred :type meta val)
            matching-methods     (filter (every-pred matching-member-type
                                                     matching-prefix) qms)]
        (for [[^String member-name members] matching-methods]
          {:candidate (str cl-name "/" member-name)
           :type      (-> members meta :type)})))))

(defn- resolve-qualified-member
  [^String member-str ns]
  (let [[cl-name member-name] (.split member-str "/")]
    (when-let [cl (resolve-class ns (symbol cl-name))]
      (get (qualified-members cl) member-name))))

(defn qualified-member-doc
  "Yields docstring for `member-str` like \"some.Class/new\", \"Integer/BYTES\"."
  [member-str ns]
  (when (qualified-member-symbol? member-str)
    (when-let [members (not-empty (resolve-qualified-member member-str ns))]
      (create-members-doc members))))

(defsource ::qualified-members
  :candidates #'qualified-members-candidates
  :doc #'qualified-member-doc)
