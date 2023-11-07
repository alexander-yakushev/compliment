(ns compliment.utils
  "Functions and utilities for source implementations."
  (:import java.io.File
           java.nio.file.Files
           (java.util.function Function Predicate)
           (java.util.jar JarEntry JarFile)
           java.util.stream.Collectors))

(def ^:dynamic *extra-metadata*
  "Signals to downstream sources which additional information about completion
  candidates they should attach . Should be a set of keywords."
  nil)

(defn split-by-leading-literals
  "Separate quote/var/deref qualifiers from a var name."
  [symbol-str]
  (next (re-matches #"(@{0,2}#'|'|@)?(.*)" symbol-str)))

(set! *unchecked-math* true)

(defn fuzzy-matches?
  "Tests if symbol matches the prefix when symbol is split into parts on
  separator."
  [^String prefix, ^String symbol, ^Character separator]
  (let [pn (.length prefix), sn (.length symbol)]
    (or (= pn 0)
        (and (> sn 0)
             (= (.charAt prefix 0) (.charAt symbol 0))
             (loop [pi 1, si 1, skipping false]
               (cond (>= pi pn) true
                     (>= si sn) false
                     :else
                     (let [pc (.charAt prefix pi)
                           sc (.charAt symbol si)
                           match (= pc sc)]
                       (cond skipping (if (= sc separator)
                                        (recur (if match (inc pi) pi) (inc si) false)
                                        (recur pi (inc si) true))
                             match (recur (inc pi) (inc si) false)
                             :else (recur pi (inc si) (not (= pc separator)))))))))))

(defn fuzzy-matches-no-skip?
  "Tests if symbol matches the prefix where separator? checks whether character
  is a separator. Unlike `fuzzy-matches?` requires separator characters to be
  present in prefix."
  [^String prefix, ^String symbol, separator?]
  (let [pn (.length prefix), sn (.length symbol)]
    (or (= pn 0)
        (and (> sn 0)
             (= (.charAt prefix 0) (.charAt symbol 0))
             (loop [pi 1, si 1, skipping false]
               (cond (>= pi pn) true
                     (>= si sn) false
                     :else
                     (let [pc (.charAt prefix pi)
                           sc (.charAt symbol si)]
                       (cond skipping (if (separator? sc)
                                        (recur pi si false)
                                        (recur pi (inc si) true))
                             (= pc sc) (recur (inc pi) (inc si) false)
                             :else (recur pi (inc si) true)))))))))

(set! *unchecked-math* false)

(defn resolve-class
  "Tries to resolve a classname from the given symbol, or returns nil
  if classname can't be resolved."
  [ns sym]
  (when-let [val (try (ns-resolve ns sym)
                      (catch ClassNotFoundException ex nil))]
    (when (class? val) val)))

(defn resolve-namespace
  "Tries to resolve a namespace from the given symbol, either from a
  fully qualified name or an alias in the given namespace."
  [sym ns]
  (or ((ns-aliases ns) sym) (find-ns sym)))

(def primitive-cache (atom {}))

(defmacro cache-last-result
  "If cache for `name` is absent, or `key` doesn't match the key in the cache,
  calculate `v` and return it. Else return value from cache."
  {:style/indent 2}
  [name key value]
  (let [ksym ()]
    `(let [name# ~name
           key# ~key
           [cached-key# cached-value#] (@primitive-cache name#)]
       (if (and (contains? @primitive-cache name#) (= cached-key# key#))
         cached-value#
         (let [value# ~value]
           (swap! primitive-cache assoc name# [key# value#])
           value#)))))

(defn flush-caches
  "Removes all cached values, forcing functions that depend on
  `cache-last-result` to recalculate."
  []
  (reset! primitive-cache {}))

;; Classpath inspection

(defn- classpath
  "Returns a sequence of File objects of the elements on the classpath."
  []
  (mapcat #(some-> (System/getProperty %) (.split File/pathSeparator))
          ["sun.boot.class.path" "java.ext.dirs" "java.class.path"
           ;; This is where Boot keeps references to dependencies.
           "fake.class.path"]))

(defn- file-seq-nonr
  "A tree seq on java.io.Files, doesn't resolve symlinked directories to avoid
  infinite sequence resulting from recursive symlinked directories."
  [dir]
  (tree-seq
   (fn [^File f] (and (.isDirectory f) (not (Files/isSymbolicLink (.toPath f)))))
   (fn [^File d] (seq (.listFiles d)))
   dir))

(defn- list-files
  "Given a path (either a jar file, directory with classes or directory with
  paths) returns all files under that path."
  [^String path, scan-jars?]
  (cond (.endsWith path "/*")
        (for [^File jar (.listFiles (File. path))
              :when (.endsWith ^String (.getName jar) ".jar")
              file (list-files (.getPath jar) scan-jars?)]
          file)

        (.endsWith path ".jar")
        (if scan-jars?
          (try (-> (.stream (JarFile. path))
                   (.filter (reify Predicate
                              (test [_ entry]
                                (not (.isDirectory ^JarEntry entry)))))
                   (.map (reify Function
                           (apply [_ entry]
                             (.getName ^JarEntry entry))))
                   (.collect (Collectors/toList)))
               (catch Exception _))
          ())

        (= path "") ()

        (.exists (File. path))
        (let [root (File. path)
              root-path (.toPath root)]
          (for [^File file (file-seq-nonr root)
                :when (not (.isDirectory file))]
            (let [filename (str (.relativize root-path (.toPath file)))]
              (if (.startsWith filename File/separator)
                (.substring filename 1) filename))))))

(defmacro list-jdk9-base-classfiles
  "Because on JDK9+ the classfiles are stored not in rt.jar on classpath, but in
  modules, we have to do extra work to extract them."
  []
  (if (resolve-class *ns* 'java.lang.module.ModuleFinder)
    `(-> (.findAll (java.lang.module.ModuleFinder/ofSystem))
         (.stream)
         (.flatMap (reify Function
                     (apply [_ mref#]
                       (.list (.open ^java.lang.module.ModuleReference mref#)))))
         (.collect (Collectors/toList)))
    ()))

(defn- all-files-on-classpath
  "Given a list of files on the classpath, returns the list of all files,
  including those located inside jar files."
  [classpath]
  (cache-last-result :all-files-on-classpath classpath
    (let [seen (java.util.HashMap.)
          seen? (fn [x] (.putIfAbsent seen x x))]
      (-> []
          (into (comp (map #(list-files % true)) cat (remove seen?)) classpath)
          (into (remove seen?) (list-jdk9-base-classfiles))))))

(defn classes-on-classpath
  "Returns a map of all classes that can be located on the classpath. Key
  represent the root package of the class, and value is a list of all classes
  for that package."
  []
  (let [classpath (classpath)]
    (cache-last-result :classes-on-classpath classpath
      (let [roots (volatile! #{})
            classes
            (vec
             (for [^String file (all-files-on-classpath classpath)
                   :when (and (.endsWith file ".class")
                              (not (.contains file "__"))
                              (not (.contains file "$"))
                              (not= file "module-info.class"))
                   :let [c (-> (subs file 0 (- (.length file) 6)) ;; .class
                               ;; Resource separator is always / on all OSes.
                               (.replace "/" "."))
                         _ (vswap! roots conj
                                   (subs c 0 (max (.indexOf ^String c ".") 0)))]]
               c))]
        (swap! primitive-cache assoc :root-packages-on-classpath
               (set (remove empty? @roots)))
        classes))))

(defn root-packages-on-classpath
  "Return a set of all classname \"TLDs\" on the classpath."
  []
  (classes-on-classpath)
  (@primitive-cache :root-packages-on-classpath))

(defn namespaces&files-on-classpath
  "Returns a collection of maps (e.g. `{:ns-str \"some.ns\", :file
  \"some/ns.cljs\"}`) of all clj/cljc/cljs namespaces obtained by classpath
  scanning."
  []
  (let [classpath (classpath)]
    (cache-last-result :namespaces-on-classpath classpath
      ;; TODO deduplicate these results by ns-str
      (for [^String file (all-files-on-classpath classpath)
            :when (or (.endsWith file ".clj") (.endsWith file ".cljs")
                      (.endsWith file ".cljc"))]
        (let [ns-str (.. (subs file 0 (.lastIndexOf file "."))
                         (replace "/" ".") (replace "_" "-"))]
          {:ns-str ns-str, :file file})))))

(defn project-resources
  "Returns a list of all non-code files in the current project."
  []
  (let [classpath (classpath)]
    (cache-last-result :project-resources classpath
      (for [path classpath
            ^String file (list-files path false)
            :when (not (re-find #"\.(clj[cs]?|jar|class)$" file))]
        ;; resource pathes always use "/" regardless of platform
        (.replace file File/separator "/")))))

(defn var->class
  "Given a form that may be a var, returns the class that is associated
  to its :tag or its value (in that precedence order)."
  [ns form]
  (when-let [var-ref (and (symbol? form)
                          (let [found (ns-resolve ns form)]
                            (when (var? found)
                              found)))]
    ;; let :tag take precedence - maybe the :tag says "this is an IFoo" (interface),
    ;; and the class says "this is a FooImpl"
    ;; (concrete class, which maybe has worse documentation
    ;; or other inheritance intricacies)
    (or (-> var-ref meta :tag)
        (class (deref var-ref)))))

(defn invocation-form->class
  "Given a form that might be an invocation form (i.e. a list),
  return the class that is returned, according to the invoked function's var metadata."
  [ns form]
  (when-let [var-from-invocation (and (seq? form)
                                      (symbol? (first form))
                                      (ns-resolve ns (first form)))]
    (when (var? var-from-invocation)
      (-> var-from-invocation meta :tag))))

(defn literal->class
  "Extracts the class from a literal.
  This is meant to support interop on strings and Clojure collections."
  [form]
  (when (or (string? form)
            (and (coll? form)
                 ;; invocations happen to be lists, but that's not relevant here:
                 (not (seq? form))))
    (class form)))
