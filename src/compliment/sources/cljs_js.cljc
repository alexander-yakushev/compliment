(ns compliment.sources.cljs-js
  (:require [clojure.pprint :refer [cl-format]]
            [clojure.string :as str]
            [clojure.zip :as zip]
            [compliment.sources :refer [defsource]]
            [compliment.sources.cljs.ast :as ast])
  (:import java.io.StringReader))

(def debug? false)

(defn js-properties-of-object
  "Returns the properties of the object we get by evaluating `:expr`
  filtered by all those that start with `prefix`."
  ([cljs-eval-fn ns expr]
   (js-properties-of-object cljs-eval-fn ns expr nil))
  ([cljs-eval-fn ns expr prefix]
   (try
     ;; :Not using a single expressiont / eval call here like
     ;; (do (require ...) (runtime ...))
     ;; to avoid
     ;; Compile Warning:  Use of undeclared Var
     ;;   compliment.sources.cljs.js-introspection/property-names-and-types
     (let [template "(compliment.sources.cljs.js-introspection/property-names-and-types ~A ~S)"
           code (cl-format nil template expr prefix)]
       (cljs-eval-fn ns "(require 'compliment.sources.cljs.js-introspection)")
       (cljs-eval-fn ns code))
     (catch #?(:clj Exception :cljs js/Error) e {:error e}))))

(defn find-prefix
  [form]
  "Tree search for the symbol '__prefix. Returns a zipper."
  (loop [node (ast/tree-zipper form)]
    (if (= '__prefix__ (zip/node node))
      node
      (when-not (zip/end? node)
        (recur (zip/next node))))))

(defn thread-form?
  "True if form looks like the name of a thread macro."
  [form]
  (->> form
       str
       (re-find #"->")
       nil?
       not))

(defn doto-form? [form]
  (= form 'doto))

(defn expr-for-parent-obj
  "Given the prefix and the context of a completion request, will try to
  find an expression that evaluates to the object being accessed."
  [prefix context]
  (when-let [form (if (string? context)
                    (try
                      (with-in-str context (read *in* nil nil))
                      (catch Exception _e
                        (when debug?
                          (binding [*out* *err*]
                            (cl-format true "Error reading context: ~s" context)))))
                    context)]
    (let [prefix-zipper (find-prefix form)
          left-sibling (zip/left prefix-zipper)
          first? (nil? left-sibling)
          first-sibling (and (not first?) (some-> prefix-zipper zip/leftmost zip/node))
          first-sibling-in-parent (some-> prefix-zipper zip/up zip/leftmost zip/node)
          threaded? (if first? (thread-form? first-sibling-in-parent) (thread-form? first-sibling) )
          doto? (if first? (doto-form? first-sibling-in-parent) (doto-form? first-sibling))
          dot-fn? (str/starts-with? prefix ".")]

      (letfn [(with-type [type maybe-expr]
                (when maybe-expr
                  {:type type
                   :expr maybe-expr}))]
        (cond
          (nil? prefix-zipper) nil

          ;; is it a threading macro?
          threaded?
          (with-type :-> (if first?
                           ;; parent is the thread
                           (-> prefix-zipper zip/up zip/lefts str)
                           ;; thread on same level
                           (-> prefix-zipper zip/lefts str)))

          doto?
          (with-type :doto (if first?
                             ;; parent is the thread
                             (-> prefix-zipper zip/up zip/leftmost zip/right zip/node str)
                             ;; thread on same level
                             (-> prefix-zipper zip/leftmost zip/right zip/node str)))

          ;; a .. form: if __prefix__ is a prop deeper than one level we need the ..
          ;; expr up to that point. If just the object that is accessed is left of
          ;; prefix, we can take that verbatim.
          ;; (.. js/console log) => js/console
          ;; (.. js/console -memory -jsHeapSizeLimit) => (.. js/console -memory)
          (and first-sibling (#{"." ".."} (str first-sibling)) left-sibling)
          (with-type :.. (let [lefts (-> prefix-zipper zip/lefts)]
                           (if (<= (count lefts) 2)
                             (str (last lefts))
                             (str lefts))))

          ;; (.. js/window -console (log "foo")) => (.. js/window -console)
          (and first? (-> prefix-zipper zip/up zip/leftmost zip/node str (= "..")))
          (with-type :.. (let [lefts (-> prefix-zipper zip/up zip/lefts)]
                           (if (<= (count lefts) 2)
                             (str (last lefts))
                             (str lefts))))

          ;; simple (.foo bar)
          (and first? dot-fn?)
          (with-type :. (some-> prefix-zipper zip/right zip/node str)))))))

(def ^:private global-expr-re #"^js/((?:[^\.]+\.)*)([^\.]*)$")
(def ^:private dot-dash-prefix-re #"^\.-?")
(def ^:private dash-prefix-re #"^-")
(def ^:private dot-prefix-re #"\.")

(defn- prepare-eval-data
  "Build a map of data that we can use to fetch the properties from an
  object that is the result of some `:expr` when evaled and that is used
  to convert those properties into candidates for completion."
  [prefix context]
  ;; js-prefix is a massaged prefix, base on Cljs interop syntax
  (if (str/starts-with? prefix "js/")
    ;; js-prefix could be a global like js/console or global/property like js/console.log
    (let [[_ dotted-expr js-prefix] (re-matches global-expr-re prefix)
          expr-parts (keep not-empty (str/split dotted-expr dot-prefix-re))
          ;; builds an expr like
          ;; "(this-as this (.. this -window))" for prefix = "js/window.console"
          ;; or "(this-as this this)" for prefix = "js/window"
          expr (cl-format nil "(this-as this ~[this~:;(.. this ~{-~A~^ ~})~])"
                          (count expr-parts) expr-parts)]
      ;; expr-parts
      {:js-prefix js-prefix
       :prepend-to-candidate (str "js/" dotted-expr)
       :vars-have-dashes? false
       :expr expr
       :type :global})

    ;; otherwise js-prefix is just a property name embedded in some expr
    (let [{:keys [type] :as expr-and-type} (expr-for-parent-obj prefix context)]
      (assoc expr-and-type
             :prepend-to-candidate (if (str/starts-with? prefix ".") "." "")
             :js-prefix (case type
                          :.. (str/replace prefix dash-prefix-re "")
                          (str/replace prefix dot-dash-prefix-re ""))
             :vars-have-dashes? true))))

(def ^:dynamic *cljs-eval-fn* nil)

(defn candidates
  "Returns a sequence of candidate data for JavaScript completions
  matching the given prefix string, ns and context.

  It requires the compliment.sources.cljs-js/*cljs-eval-fn* var to be
  dynamically bound to a function that given a namespace (as string) and
  cljs code (string) will evaluate it and return the value as a clojure
  object.

  See `suitable.middleware/cljs-dynamic-completion-handler` for
  how to setup an eval function with nREPL.

  Currently unsupported options that compliment implements
  are :extra-metadata :sort-order and :plain-candidates."
  [prefix ns context]
  ;; Given some context (the toplevel form that has changed) and the prefix
  ;; string that represents the last typed input, we try to find out if the
  ;; context/prefix are object access (property access or method call). If so,
  ;; we try to extract a form that we can evaluate to get the object that is
  ;; accessed. If we get the object, we enumerate it's properties and methods
  ;; and generate a list of matching completions for those.
  (let [{:keys [js-prefix prepend-to-candidate vars-have-dashes? expr type] :as eval-data}
        (prepare-eval-data prefix context)
        global? (#{:global} type)]
    (when debug?
      (binding [*out* *err*]
        (println "Eval data:" eval-data)))
    (when-let [{error :error properties :value}
               (and expr (js-properties-of-object *cljs-eval-fn* ns expr js-prefix))]
      (if error
        (when debug?
          (binding [*out* *err*]
            (println "Error in JS completions:" error)))
        (for [{:keys [name type]} properties
              :let [maybe-dash (if (and vars-have-dashes? (= "var" type)) "-" "")
                    candidate (str prepend-to-candidate maybe-dash name)]
              :when (str/starts-with? candidate prefix)]
          {:type type :candidate candidate :ns (if global? "js" expr)})))))

(def doc (constantly nil))

(defsource ::js-interop
  :candidates #'candidates
  :doc #'doc)
