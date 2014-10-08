;;; Test for the revue.interpreter namespace.

(ns revue.interpreter-test
  #+cljs (:require-macros [cemerick.cljs.test :refer (deftest testing is are)]
                          [clojure.test.check.clojure-test :refer (defspec)])
  (:require #+clj [clojure.test :as t :refer (deftest testing is are)]
            #+cljs [cemerick.cljs.test :as t]
            [clojure.string :as string]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop :include-macros true]
            #+clj [clojure.test.check.clojure-test :as ct :refer (defspec)]
            #+cljs [clojure.test.check.clojure-test :as ct]
            [revue.util :as util]
            [revue.mem :as mem]
            [revue.vm :as vm]
            [revue.interpreter :as interp]))


(deftest warn-01
  (testing "Warnings from the Interpreter."
    (is (= (string/trim (with-out-str (interp/warn "XXX!")))
           "Interpreter warning: XXX!"))))


;;; Test for environments
;;; =====================

(deftest clear-initial-bindings-01
  (testing "Clearing of initial bindings"
    (let [old-bindings @interp/*initial-bindings*]
      (interp/clear-initial-bindings)
      (is (= @interp/*initial-bindings* {}))
      (reset! interp/*initial-bindings* {:a 1 :b 2})
      (is (= @interp/*initial-bindings* {:a 1 :b 2}))
      (interp/clear-initial-bindings)
      (is (= @interp/*initial-bindings* {}))
      (reset! interp/*initial-bindings* old-bindings))))

(deftest define-global-01
  (testing "Defining global bindings"
    (let [old-bindings @interp/*initial-bindings*]
      (interp/clear-initial-bindings)
      (is (= (interp/global-env) {}))
      (interp/define-global 'foo :foo)
      (is (= (interp/global-env) {'foo :foo}))
      (interp/define-global 'bar '(bar bar))
      (is (= (interp/global-env) {'foo :foo 'bar '(bar bar)}))
      (interp/define-global 'foo :bar)
      (is (= (interp/global-env) {'foo :bar 'bar '(bar bar)})))))

(deftest extend-env-01
  (testing "Extending an evironment"
    (is (= (interp/extend-env {} [] []) {}))
    (is (= (interp/extend-env {} ['foo] [1])) {'foo 1})
    (is (= (interp/extend-env {} ['foo 'bar] [1 2]) {'foo 1, 'bar 2}))
    (is (= (interp/extend-env {'foo 1} ['bar] [2]) {'foo 1, 'bar 2}))
    (is (= (interp/extend-env {'foo 1} ['foo] [2]) {'foo 2}))
    (is (= (interp/extend-env {'foo 1 'bar 2} ['foo] [2]) {'foo 2, 'bar 2}))
    (is (= (interp/extend-env {'foo 1 'bar 2} ['bar] [1]) {'foo 1, 'bar 1}))
    (is (= (interp/extend-env {'foo 1 'bar 2} ['foo 'baz] [3 4])
           {'foo 3, 'bar 2, 'baz 4}))
    (is (= (interp/extend-env {'foo 1 'bar 2} ['baz 'quux] [3 4])
           {'foo 1, 'bar 2, 'baz 3, 'quux 4}))))

;;; Evaluate this (e.g., with C-x C-e in Cider) to run the tests for
;;; this namespace:
;;; (t/run-tests 'revue.interpreter-test)
;;; Evaluate this to run the test for all namespaces:
;;; (t/run-all-tests #"^revue\..*-test")
