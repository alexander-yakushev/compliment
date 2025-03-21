(ns compliment.sources.t-local-bindings
  (:require [clojure.string :as string]
            [clojure.test :refer :all]
            [compliment.context :as ctx]
            [compliment.sources.local-bindings :as src]
            [compliment.t-helpers :refer :all]
            [matcher-combinators.matchers :as mc]))

(defn- -ns [] (find-ns 'compliment.sources.t-local-bindings))

(def thread1
  "A thread identified as such by its class"
  (Thread.))

(def ^Thread thread2
  "A thread identified as such by its :tag"
  nil)

(deftest bindings-from-context
  (testing "The class of a given binding can be identified by the class of its bound value"
    (is? [(mc/via meta {:tag `Thread})]
         (src/bindings-from-context (ctx/parse-context '(let [a thread1] __prefix__))
                                    (-ns))))

  (testing "The class of a given binding can be identified by the :tag of its bound value"
    (is? [(mc/via meta {:tag `Thread})]
         (src/bindings-from-context (ctx/parse-context '(let [a thread2] __prefix__))
                                    (-ns))))

  (testing "The class of a given binding can be identified by the :tag of the var from an invocation"
    (is? [(mc/via meta {:tag `String})]
         (src/bindings-from-context (ctx/parse-context '(let [a (string/trim "a")] __prefix__))
                                    (-ns))))

  (testing "The class of a given binding can be identified by the class of a string literal"
    (is? [(mc/via meta {:tag `String})]
         (src/bindings-from-context (ctx/parse-context '(let [a ""] __prefix__))
                                    (-ns))))

  (testing "The class of a given binding can be identified by the class of a vector literal"
    (is? [(mc/via meta {:tag 'clojure.lang.PersistentVector})]
         (src/bindings-from-context (ctx/parse-context '(let [a []] __prefix__))
                                    (-ns))))

  (testing "The class of a given binding cannot be identified by the class of a list"
    (is? [(mc/via meta nil)]
         (src/bindings-from-context (ctx/parse-context '(let [a ()] __prefix__))
                                    (-ns)))))

(deftest local-bindings
  (defmacro ^{:completion/locals :let} like-let [& _])
  (testing "local bindings are looked for in the context inside let-like forms"
    (is? (mc/in-any-order [{:candidate "a", :type :local}
                           {:candidate "b", :type :local}])
         (src/candidates "" *ns* (ctx/parse-context '(let [a 1, b 2] __prefix__))))

    (is? (mc/in-any-order ["foo-bar" "foo-baz"])
         (strip-tags (src/candidates "f-ba" *ns* (ctx/parse-context
                                                  '(when-let [foo-bar 10, foo-baz 20, bar-baz 30]
                                                     __prefix__)))))

    (is? ["item"]
         (strip-tags (src/candidates "it" *ns* (ctx/parse-context
                                                '(for [item (map inc items), part item]
                                                   __prefix__)))))

    (is? ["f"]
         (strip-tags (src/candidates "" *ns* (ctx/parse-context
                                              '(with-open [f (io/reader file)]
                                                 __prefix__)))))

    (is? ["i"]
         (strip-tags (src/candidates "" *ns* (ctx/parse-context
                                              '(dotimes [i 10]
                                                 __prefix__)))))

    (is? ["i"]
         (strip-tags (src/candidates "" (-ns) (ctx/parse-context
                                               '(like-let [i 10]
                                                          __prefix__))))))

  (defmacro ^{:completion/locals :defn} like-defn [& _])
  (testing "inside defn and defmacro forms the name and the arglist is returned"
    (is? (mc/in-any-order ["amacro" "bindings" "body"])
         (strip-tags (src/candidates "" *ns* (ctx/parse-context
                                              '(defmacro amacro [bindings & body] __prefix__)))))

    (is? (mc/in-any-order ["amacro" "bindings" "body"])
         (strip-tags (src/candidates "" (-ns) (ctx/parse-context
                                               '(like-defn amacro [bindings & body] __prefix__)))))

    (is? (mc/in-any-order ["afunction" "foo" "bar" "rest"])
         (strip-tags (src/candidates "" *ns* (ctx/parse-context
                                              '(defn afunction "docstring" {:meta data}
                                                 [foo bar & rest] __prefix__)))))

    (is? (mc/in-any-order ["multiarg-fn" "arg" "arg1" "arg2"])
         (strip-tags (src/candidates "" *ns* (ctx/parse-context
                                              '(defn multiarg-fn "docstring"
                                                 ([arg] (multiarg-fn arg nil))
                                                 ([arg1 arg2] (do-stuff __prefix__)))))))

    (is? (mc/in-any-order ["multimethod" "foo" "bar" "rest"]) (strip-tags (src/candidates "" *ns* (ctx/parse-context
                                          '(defmethod multimethod "dispatch-val"
                                             [foo bar & rest] __prefix__))))))

  (defmacro ^{:completion/locals :letfn} like-letfn [& _])
  (testing "letfn is supported"
    (is? (mc/in-any-order ["local-fn" "foo" "bar" "rest" "f-2" "a" "b" "c"])
         (strip-tags (src/candidates "" *ns* (ctx/parse-context
                                              '(letfn [(local-fn [foo bar & rest] __prefix__)
                                                       (f-2 ([[a b]] a) ([c] c))])))))

    (is? (mc/in-any-order ["local-fn" "foo" "bar" "rest" "f-2" "a" "b" "c"])
         (strip-tags (src/candidates "" (-ns) (ctx/parse-context
                                               '(like-letfn [(local-fn [foo bar & rest] __prefix__)
                                                             (f-2 ([[a b]] a) ([c] c))]))))))

  (testing "as-> is supported"
    (is? ["number"]
         (strip-tags (src/candidates "" *ns* (ctx/parse-context
                                              '(as-> (+ 1 2) number
                                                 (even? number) __prefix__))))))

  (testing "destructuring is also supported"
    (is? (mc/in-any-order ["foo" "bar" "baz" "a" "b" "c" "d" "key1" "key2" "key3" "key4" "key5"
                          "key6" "key7" "key8" "key9" "key10" "key11" "key12" "key13" "key14"
                          "rec" "urs" "total"])
         (strip-tags (src/candidates "" *ns* (ctx/parse-context
                                              '(let [foo 42,
                                                     [bar baz] lst
                                                     {a :a, {b :b :as c} :b, [d] :d} m
                                                     {:keys [key1 key2] :strs [key3] :syms [key4]} m2
                                                     {:keys [:a/key5 :b/key6 ::src/key7 ::src/key8]} m3
                                                     {:c/keys [key9 key10] :c/syms [key11]} m4
                                                     {::src/keys [key12 key13] ::src/syms [key14]} m5
                                                     [_ rec {urs :ive :as total}] val]
                                                 __prefix__))))))

  (defmacro ^{:completion/locals :doseq} like-doseq [& _])
  (testing "in doseq and for :let bindings are supported"
    (is? (mc/in-any-order ["a" "c" "d" "g" "h" "i"])
         (strip-tags (src/candidates "" *ns* (ctx/parse-context
                                              '(doseq [a b
                                                       :let [c (first a)]
                                                       {:keys [d]} e
                                                       :let [{g :g} f, [h i] j]]
                                                 __prefix__)))))


    (is? (mc/in-any-order ["a" "c" "d" "g" "h" "i"])
         (strip-tags (src/candidates "" (-ns) (ctx/parse-context
                                               '(like-doseq [a b
                                                             :let [c (first a)]
                                                             {:keys [d]} e
                                                             :let [{g :g} f, [h i] j]]
                                                            __prefix__))))))

  (testing "bindings are scanned recursively"
    (is? (mc/in-any-order ["afunction" "arg1" "arg2" "foo"])
         (strip-tags (src/candidates "" *ns* (ctx/parse-context
                                              '(defn afunction [arg1 arg2]
                                                 (distinct (let [foo 13, arg2 14]
                                                             __prefix__))))))))

  (testing "bindings will be completed even if the form is unfinished"
    (is? (mc/in-any-order ["foo" "bar" "baz" "qux"])
         (strip-tags (src/candidates "" *ns* (ctx/parse-context
                                              (#'ctx/safe-read-context-string
                                               "(let [foo 42, [bar baz] 17, qux __prefix__"))))))

  (testing "source silently fails if context is malformed"
    (is? [] (src/candidates "" *ns* "(let __prefix__)"))
    (is? [] (src/candidates "" *ns* "(let [() 1]__prefix__)"))
    (is? [] (src/candidates "" *ns* "(defn [args] \"doc\" x (__prefix__))"))
    (is? [] (src/candidates "" *ns* "(defn resources
                               \"Build api functions for resources\"
                               [{:keys [resources] :as discovery-doc}]
                               (for [[_ {:keys __prefix__}] resources]
                                 (generate-schema s)))"))))
