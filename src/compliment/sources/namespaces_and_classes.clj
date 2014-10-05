(ns compliment.sources.namespaces-and-classes
  "Completion for namespace and class names."
  (:require [compliment.sources :refer [defsource]]
            [compliment.utils :refer [fuzzy-matches?]]
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
        :else
        (for [^File file (file-seq (File. path))]
          (.replace ^String (.getPath file) path ""))))

(def all-classes
  "Returns a map of all classes that can be located on the classpath. Key
  represent the root package of the class, and value is a list of all classes
  for that package."
  (memoize
   (fn []
     (->>
      (for [prop ["sun.boot.class.path" "java.ext.dirs" "java.class.path"]
            path (.split (System/getProperty prop) File/pathSeparator)
            ^String file (classfiles-from-path path)
            :when (and (.endsWith file ".class") (not (.contains file "__")))]
        (.. file (replace ".class" "") (replace File/separator ".")))
      doall
      (group-by #(subs % 0 (max (.indexOf ^String % ".") 0)))))))

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
         (for [[root-pkg classes] (all-classes)
               :when (.startsWith prefix root-pkg)
               ^String cl-str classes
               :when (.startsWith cl-str prefix)]
           cl-str)
         (for [[^String root-pkg _] (all-classes)
               :when (.startsWith root-pkg prefix)]
           (str root-pkg ".")))))))

(defn doc [ns-or-class-str curr-ns]
  (when (nscl-symbol? ns-or-class-str)
    (if-let [ns (find-ns (symbol ns-or-class-str))]
      (str ns "\n" (:doc (meta ns)) "\n")
      (when-let [class (ns-resolve curr-ns (symbol ns-or-class-str))]
        (when (= (type class) Class)
          (classname-doc class))))))

(defsource ::namespaces-and-classes
  :candidates #'candidates
  :doc #'doc)
