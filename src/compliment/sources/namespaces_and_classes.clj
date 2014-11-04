(ns compliment.sources.namespaces-and-classes
  "Completion for namespace and class names."
  (:require [compliment.sources :refer [defsource]]
            [compliment.utils :refer [fuzzy-matches? defmemoized android-vm?]]
            [compliment.sources.class-members :refer [classname-doc]])
  (:import [java.util.jar JarFile JarEntry]
           java.io.File))

(defn nscl-symbol?
  "Tests if prefix looks like a namespace or classname."
  [x]
  (re-matches #"[^\/\:\.][^\/\:]+" x))

(defn nscl-matches?
  "Tests if prefix partially matches a var name with periods as
  separators."
  [prefix namespace]
  (fuzzy-matches? prefix namespace \.))

;;; Obtaining the list of classes

(defn imported-classes
  "Returns names of all classes imported into a given namespace."
  [ns]
  (for [[_ ^Class val] (ns-map ns) :when (class? val)]
    (.getName val)))

(defn- classfiles-from-path
  "Given a path (either a jar file, directory with classes or directory with
  paths) returns the classes under that path."
  [^String path]
  (cond (.endsWith path "/*")
        (for [^File jar (.listFiles (File. path))
              :when (.endsWith ^String (.getName jar) ".jar")
              file (classfiles-from-path (.getPath jar))]
          file)

        (.endsWith path ".jar")
        (try (for [^JarEntry entry (enumeration-seq (.entries (JarFile. path)))]
               (.getName entry))
             (catch Exception e))

        (= path "") ()

        :else
        (for [^File file (file-seq (File. path))]
          (.replace ^String (.getPath file) path ""))))

(defmemoized ^:private all-files-on-path
  "Returns the list of all files on the classpath."
  []
  (if android-vm?
    ()
    (for [prop ["sun.boot.class.path" "java.ext.dirs" "java.class.path"]
          path (.split (or (System/getProperty prop) "") File/pathSeparator)
          file (classfiles-from-path path)]
      file)))

(defmemoized all-classes
  "Returns a map of all classes that can be located on the classpath. Key
  represent the root package of the class, and value is a list of all classes
  for that package."
  []
  (->> (for [^String file (all-files-on-path)
             :when (and (.endsWith file ".class") (not (.contains file "__"))
                        (not (.contains file "$")))]
         (.. (if (.startsWith file File/separator)
               (.substring file 1) file)
             (replace ".class" "") (replace File/separator ".")))
       doall
       (group-by #(subs % 0 (max (.indexOf ^String % ".") 0)))))

(defmemoized all-namespaces
  "Returns the list of all Clojure namespaces obtained by classpath scanning."
  []
  (for [^String file (all-files-on-path)
        :when (and (.endsWith file ".clj")
                   (not (.startsWith file "META-INF")))
        :let [[_ ^String nsname] (re-matches #"[^\w]?(.+)\.clj" file)]
        :when nsname]
    (.. nsname (replace File/separator ".") (replace "_" "-"))))

(defn candidates
  "Returns a list of namespace and classname completions."
  [^String prefix, ns context]
  (when (nscl-symbol? prefix)
    (let [has-dot (> (.indexOf prefix ".") -1)]
      ((comp distinct concat)
       (for [ns-str (concat (map (comp name ns-name) (all-ns))
                            (imported-classes ns)
                            (when-not has-dot
                              (map name (keys (ns-aliases ns)))))
             :when (nscl-matches? prefix ns-str)]
         ns-str)
       ;; Fuzziness is too slow for all classes, so just startsWith.
       ;; Also have to do clever tricks to keep the performance high.
       (if has-dot
         (concat (for [[root-pkg classes] (all-classes)
                       :when (.startsWith prefix root-pkg)
                       ^String cl-str classes
                       :when (.startsWith cl-str prefix)]
                   cl-str)
                 (for [ns-str (all-namespaces)
                       :when (nscl-matches? prefix ns-str)]
                   ns-str))
         (concat (for [[^String root-pkg _] (all-classes)
                       :when (.startsWith root-pkg prefix)]
                   (str root-pkg "."))
                 (for [^String ns-str (all-namespaces)
                       :when (.startsWith ns-str prefix)]
                   ns-str)))))))

(defn doc [ns-or-class-str curr-ns]
  (when (nscl-symbol? ns-or-class-str)
    (if-let [ns (find-ns (symbol ns-or-class-str))]
      (str ns "\n" (:doc (meta ns)) "\n")
      (when-let [class (try (ns-resolve curr-ns (symbol ns-or-class-str))
                            (catch Exception ex nil))]
        (when (= (type class) Class)
          (classname-doc class))))))

(defsource ::namespaces-and-classes
  :candidates #'candidates
  :doc #'doc)
