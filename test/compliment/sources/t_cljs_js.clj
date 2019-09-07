(ns compliment.sources.t-cljs-js
  (:require [clojure.pprint :refer [cl-format]]
            [clojure.string :refer [starts-with?]]
            [clojure.test :as t :refer [deftest is run-tests]]
            [compliment.sources.cljs-js :as cljs-js]))

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; helpers

(defn fake-cljs-eval-fn
  [expected-expression expected-prefix properties]
  (fn [ns code]
    (when-let [[_ expr prefix]
               (re-matches
                #"^\(compliment.sources.cljs.js-introspection/property-names-and-types (.*) \"(.*)\"\)"
                code)]
      (if (and (= expr expected-expression)
               (= prefix expected-prefix))
        {:error nil
         :value properties}
        (is false (cl-format nil "Expected expr / prefix~% ~S / ~S~% passed to fake-js-props-fn does not match actual expr / prefix~% ~S / ~S"
                             expected-expression expected-prefix
                             expr prefix))))))

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; expr-for-parent-obj

(deftest expr-for-parent-obj
  (let [state {:special-namespaces ["js"]}
        tests [ ;; should not trigger object completion
               {:desc "no object in sight"
                :symbol-and-context    [".log" "(__prefix__)"]
                :expected nil}

               {:desc "Normal object/class name is typed, not method or special name."
                :symbol-and-context    ["bar" "(.log __prefix__)"]
                :expected nil}

               ;; should trigger object completion
               {:desc ". method"
                :symbol-and-context    [".log" "(__prefix__ js/console)"]
                :expected {:type :. :expr "js/console"}}

               {:desc ". method nested"
                :symbol-and-context    [".log" "(__prefix__ (.-console js/window) \"foo\")"]
                :expected {:type :. :expr "(.-console js/window)"}}

               {:desc ".- prop"
                :symbol-and-context    [".-memory" "(__prefix__ js/console)"]
                :expected {:type :. :expr "js/console"}}

               {:desc ".- prop nested"
                :symbol-and-context    [".-memory" "(__prefix__ (.-console js/window) \"foo\")"]
                :expected {:type :. :expr "(.-console js/window)"}}

               {:desc ".. method"
                :symbol-and-context    ["log" "(.. js/console __prefix__)"]
                :expected {:type :.. :expr "js/console"}}

               {:desc ".. method nested"
                :symbol-and-context    ["log" "(.. js/console (__prefix__ \"foo\"))"]
                :expected {:type :.. :expr "js/console"}}

               {:desc ".. method chained"
                :symbol-and-context    ["log" "(.. js/window -console __prefix__)"]
                :expected {:type :.. :expr "(.. js/window -console)"}}

               {:desc ".. method chained and nested"
                :symbol-and-context    ["log" "(.. js/window -console (__prefix__ \"foo\"))"]
                :expected {:type :.. :expr "(.. js/window -console)"}}

               {:desc ".. prop"
                :symbol-and-context    ["-memory" "(.. js/console __prefix__)"]
                :expected {:type :.. :expr "js/console"}}

               {:desc "->"
                :symbol-and-context    [".log" "(-> js/console __prefix__)"]
                :expected {:type :-> :expr "(-> js/console)"}}

               {:desc "-> (.)"
                :symbol-and-context    [".log" "(-> js/console (__prefix__ \"foo\"))"]
                :expected {:type :-> :expr "(-> js/console)"}}

               {:desc "-> chained"
                :symbol-and-context    [".log" "(-> js/window .-console __prefix__)"]
                :expected {:type :-> :expr "(-> js/window .-console)"}}

               {:desc "-> (.)"
                :symbol-and-context    [".log" "(-> js/window .-console (__prefix__ \"foo\"))"]
                :expected {:type :-> :expr "(-> js/window .-console)"}}

               {:desc "doto"
                :symbol-and-context    [".log" "(doto (. js/window -console) __prefix__)"]
                :expected {:type :doto :expr "(. js/window -console)"}}

               {:desc "doto (.)"
                :symbol-and-context    [".log" "(doto (. js/window -console) (__prefix__ \"foo\"))"]
                :expected {:type :doto :expr "(. js/window -console)"}}

               {:desc "doto (.)"
                :symbol-and-context    ["js/cons" "(doto (. js/window -console) (__prefix__ \"foo\"))"]
                :expected {:type :doto :expr "(. js/window -console)"}}

               {:desc "no prefix"
                :symbol-and-context    ["xx" "(foo bar (baz))"]
                :expected nil}]]

    (doseq [{[symbol context] :symbol-and-context :keys [expected desc]} tests]
      (is (= expected (cljs-js/expr-for-parent-obj symbol context)) desc))))

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; JS candidates

(deftest global
  (binding [cljs-js/*cljs-eval-fn* (fake-cljs-eval-fn "(this-as this this)" "c"
                                                      [{:name "console" :hierarchy 1 :type "var"}
                                                       {:name "confirm" :hierarchy 1 :type "function"}])]
    (is (= [{:candidate "js/console" :ns "js" :type "var"}
            {:candidate "js/confirm" :ns "js" :type "function"}]
           (cljs-js/candidates  "js/c" "cljs.user" "")))))

(deftest global-prop
  (binding [cljs-js/*cljs-eval-fn* (fake-cljs-eval-fn "(this-as this (.. this -console))" "lo"
                                                      [{:name "log" :hierarchy 1 :type "function"}
                                                       {:name "clear" :hierarchy 1 :type "function"}])]
    (is (= [{:candidate "js/console.log" :ns "js" :type "function"}]
           (cljs-js/candidates "js/console.lo" "cljs.user" "js/console")))))

(deftest global-prop-2
  (binding [cljs-js/*cljs-eval-fn* (fake-cljs-eval-fn "(this-as this (.. this -window -console))" "lo"
                                                      [{:name "log" :hierarchy 1 :type "function"}
                                                       {:name "clear" :hierarchy 1 :type "function"}])]
    (is (= [{:candidate "js/window.console.log" :ns "js" :type "function"}]
           (cljs-js/candidates "js/window.console.lo" "cljs.user" "js/console")))))

(deftest simple
  (binding [cljs-js/*cljs-eval-fn* (fake-cljs-eval-fn "js/console" "l"
                                                      [{:name "log" :hierarchy 1 :type "function"}
                                                       {:name "clear" :hierarchy 1 :type "function"}])]
    (is (= [{:candidate ".log" :ns "js/console" :type "function"}]
           (cljs-js/candidates ".l" "cljs.user" "(__prefix__ js/console)")))
    (is (= [{:candidate "log" :ns "js/console" :type "function"}]
           (cljs-js/candidates "l" "cljs.user" "(. js/console __prefix__)")))
    ;; note: we're testing context with a form here
    (is (= [{:candidate "log" :ns "js/console" :type "function"}]
           (cljs-js/candidates "l" "cljs.user" '(.. js/console (__prefix__ "foo")))))))

(deftest dotdot-completion
  (binding [cljs-js/*cljs-eval-fn* (fake-cljs-eval-fn "js/foo" "ba"
                                                      [{:name "bar" :hierarchy 1 :type "var"}
                                                       {:name "baz" :hierarchy 1 :type "function"}])]
    (is (= [{:candidate "-bar" :ns "js/foo" :type "var"}]
           (cljs-js/candidates "-ba" "cljs.user" "(.. js/foo __prefix__)")))
    (is (= [{:candidate "baz" :ns "js/foo" :type "function"}]
           (cljs-js/candidates "ba" "cljs.user" "(.. js/foo __prefix__)")))))

(deftest dotdot-completion-chained+nested
  (binding [cljs-js/*cljs-eval-fn* (fake-cljs-eval-fn "(.. js/foo zork)" "ba"
                                                      [{:name "bar" :hierarchy 1 :type "var"}
                                                       {:name "baz" :hierarchy 1 :type "function"}])]
    (is (= [{:candidate "-bar" :ns "(.. js/foo zork)" :type "var"}]
           (cljs-js/candidates "-ba" "cljs.user" "(.. js/foo zork (__prefix__ \"foo\"))")))
    (is (= [{:candidate "baz" :ns "(.. js/foo zork)" :type "function"}]
           (cljs-js/candidates "ba" "cljs.user" "(.. js/foo zork (__prefix__ \"foo\"))")))))

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(comment

  (run-tests 'compliment.sources.t-cljs-js)

  (binding [cljs-js/*cljs-eval-fn* (fake-cljs-eval-fn "js/console" "l"
                                                      [{:name "log" :hierarchy 1 :type "function"}
                                                       {:name "clear" :hierarchy 1 :type "function"}])]
    (cljs-js/candidates ".l" "cljs.user" "(__prefix__ js/console)"))

  (binding [cljs-js/*cljs-eval-fn* (fake-cljs-eval-fn "(this-as this this)" "c"
                                                      [{:name "console" :hierarchy 1 :type "var"}])]
    (cljs-js/candidates "js/c" "cljs.user" :context ""))

  (cljs-js/expr-for-parent-obj "foo" nil "(__prefix__ foo)")
  (cljs-js/expr-for-parent-obj ".l" "cljs.user" "(__prefix__ js/console)")

  (with-redefs [cljs-js/js-properties-of-object (fn [obj-expr msg] [])]
    (cljs-js/candidates ".l" "cljs.user" "(__prefix__ js/console)" nil))
  )
