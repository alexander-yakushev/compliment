(ns compliment.sources.class-members
  "Completion for both static and non-static class members."
  (:require [clojure.string :as str :refer [join]]
            [compliment.sources :refer [defsource]]
            [compliment.sources.local-bindings :refer [bindings-from-context]]
            [compliment.utils :as utils :refer [fuzzy-matches-no-skip?
                                                resolve-class]])
  (:import [java.lang.reflect Field Member Method Modifier Constructor Executable]))

^{:lite nil}
(def ^:private base-priority 40)

(defn- clojure-1-12+? []
  (or (> (:major *clojure-version*) 1)
      (>= (:minor *clojure-version*) 12)))

(defn static?
  "Tests if class member is static."
  [^Member member]
  (Modifier/isStatic (.getModifiers member)))

;; ## Regular (non-static) members

(def global-members-cache
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

(defn populate-global-members-cache
  "Populate members cache of class members for `ns` from the given set of classes."
  [ns classes]
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
    (swap! global-members-cache assoc ns {:classes (set classes)
                                          :members (persistent! cache)})))

(defn update-global-cache
  "Updates members cache for a given namespace if necessary."
  [ns]
  (let [imported-classes (reduce-kv (fn [acc _ mapping]
                                      (if (class? mapping)
                                        (conj acc mapping)
                                        acc))
                                   #{} (ns-map ns))
        cache (@global-members-cache ns)]
    (when (or (nil? cache)
              (not= (count (:classes cache)) (count imported-classes)))
      (populate-global-members-cache ns imported-classes))))

(defn get-all-members
  "Returns all non-static members for a given namespace."
  [ns]
  (update-global-cache ns)
  (get-in @global-members-cache [ns :members]))

(def ^:private class-members-cache
  "Members by class"
  (atom {}))

(defn- populate-class-members-cache
  "Populates qualified methods cache for a given class."
  [^Class class]
  (swap! class-members-cache assoc class
         (let [methods&fields (concat (.getMethods class) (.getFields class))
               constructors   (.getConstructors class)
               member->type   #(if (instance? Field %)
                                 (if (static? %) :static-field :field)
                                 (if (static? %) :static-method :method))
               update-cache   (fn [cache member name type]
                                (update cache name
                                        (fn [members]
                                          (vary-meta (-> (or members [])
                                                         (conj member))
                                                     update :types (fnil conj #{}) type))))
               cache (reduce (fn [cache ^Member m]
                               (update-cache cache m (.getName m) (member->type m)))
                             {} methods&fields)]
           (reduce (fn [cache ^Member m]
                     (update-cache cache m "new" :constructor))
                   cache
                   constructors))))

(defn- get-all-class-members [klass]
  (or (@class-members-cache klass)
      (get (populate-class-members-cache klass) klass)))

(defn qualified-member-symbol? [x]
  (some-> (re-matches #"([^\/\:\.][^\:]*)(?<!\.)\/(.*)" (str x))
          (subvec 1)))

(defn class-member-symbol?
  "Tests if `x` looks like a non-static class member,
  e.g. \".getMonth\" or \"java.util.Date/.getMonth\" (for Clojure v1.12+).

  When true, yields `[klass-name method-name]`."
  [x]
  (cond
    (str/starts-with? x ".") [nil x]
    (clojure-1-12+?) (when-let [[_klass method :as parts] (qualified-member-symbol? x)]
                       (when (or (empty? method)
                                 (str/starts-with? method "."))
                         parts))))

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
  (when-let [[klass-name prefix] (class-member-symbol? prefix)]
    (let [prefix      (str/replace prefix #"^\." "")
          qualified?  (some? klass-name)
          inparts?    (re-find #"[A-Z]" prefix)
          klass       (if qualified?
                        (resolve-class ns (symbol klass-name))
                        ^{:lite nil} (try-get-object-class ns context))
          exact-class? (or qualified? klass)]
      (for [[member-name members] (if exact-class?
                                    (some-> klass get-all-class-members)
                                    (get-all-members ns))
            :when (and (if inparts?
                         (camel-case-matches? prefix member-name)
                         (.startsWith ^String member-name prefix))
                       (if exact-class?
                         (some (:types (meta members)) [:method :field])
                         true))]
        {:candidate (if qualified?
                      (str klass-name "/." member-name)
                      (str "." member-name))
         :type (cond exact-class? (first (:types (meta members)))
                     (instance? Method (first members)) :method
                     :else :field)
         ;; Assign lower (bigger number) priority to non-static members so that
         ;; static members are shown first when qualified.
         :priority ^{:lite 0} (+ base-priority 1)}))))

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

^{:lite nil}
(defn- members->doc-dispatch [[^Class _cl members]]
  (class (first members)))

^{:lite nil}
(defmulti ^:private members->doc #'members->doc-dispatch)

^{:lite nil}
(defmethod members->doc java.lang.reflect.Method
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

^{:lite nil}
(defmethod members->doc java.lang.reflect.Constructor
  [[^Class cl members]]
  (str (.getName cl) ".new"
       (join
        (map (fn [^Constructor member]
               (str "\n  " (doc-method-parameters (.getParameterTypes member))))
             members))
       "\n"))

^{:lite nil}
(defmethod members->doc java.lang.reflect.Field
  [[^Class cl members]]
  (let [^Member f-mem (first members)]
    (str (.getName cl) "." (.getName f-mem)
         (str " = " (try (.get ^Field f-mem nil)
                         (catch Exception _e "?"))
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
         (map (comp members->doc sort-members))
         (interpose "\n")
         join)))

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

;; ## Static members

(defn static-member-symbol?
  "Tests if prefix looks like a static member symbol, returns parsed parts."
  [x]
  (when-let [[_klass method :as parts] (qualified-member-symbol? x)]
    (when-not (str/starts-with? (str method) ".")
      parts)))

(defn static-members-candidates
  "Returns a list of static member candidates."
  [^String prefix, ns _context]
  (when-let [[cl-name member-prefix] (static-member-symbol? prefix)]
    (when-let [cl (resolve-class ns (symbol cl-name))]
      (let [inparts? (re-find #"[A-Z]" member-prefix)]
        (for [[^String member-name members] (get-all-class-members cl)
              :let [types (:types (meta members))]
              :when (and (if inparts?
                           (camel-case-matches? member-prefix member-name)
                           (.startsWith member-name member-prefix))
                         (or (some #{:static-field :static-method} types)
                             (and (clojure-1-12+?) (:constructor types))))]
          {:candidate (str cl-name "/" member-name)
           :type (cond (instance? Constructor (first members)) :constructor
                       (instance? Method (first members)) :static-method
                       :else :static-field)
           :priority ^{:lite 0} base-priority})))))

^{:lite nil}
(defn resolve-static-members
  "Given a string representation of a static member returns Member object."
  [^String member-str ns]
  (let [[cl-name member-name] (.split member-str "/")]
    (when-let [cl (resolve-class ns (symbol cl-name))]
      (let [class-members (get (get-all-class-members cl) member-name)]
        (filter #(or (static? %)
                     (and (clojure-1-12+?) (instance? Constructor %)))
                class-members)))))

^{:lite nil}
(defn- qualified-member-doc [ns klass-name member-name]
  (when-let [klass (some->> klass-name symbol (resolve-class ns))]
    (when-let [members (get-all-class-members klass)]
      (->> (get members member-name)
           (remove static?)
           create-members-doc))))

^{:lite nil}
(defn- non-qualified-member-doc [ns member-name]
  (update-global-cache ns)
  (when-let [members (get-in @global-members-cache [ns :members member-name])]
    (create-members-doc members)))

^{:lite nil}
(defn members-doc
  "Documentation function for non-static members."
  [member-str ns]
  (when-let [[klass-name member-name]
             (class-member-symbol? member-str)]
    (let [member-name (str/replace member-name #"^\." "")]
      (if (some? klass-name)
        (qualified-member-doc ns klass-name member-name)
        (non-qualified-member-doc ns member-name)))))

^{:lite nil}
(defn static-member-doc
  "Given a member name and class returns its docstring."
  [member-str ns]
  (when (static-member-symbol? member-str)
    (some-> (resolve-static-members member-str ns) create-members-doc)))

^{:lite '(defsource :compliment.lite/members :candidates #'members-candidates)}
(defsource ::members
  :candidates #'members-candidates
  :doc #'members-doc)

^{:lite (defsource :compliment.lite/static-members :candidates #'static-members-candidates)}
(defsource ::static-members
  :candidates #'static-members-candidates
  :doc #'static-member-doc)
