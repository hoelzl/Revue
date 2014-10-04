;;; Test for the revue.compiler namespace.

(ns revue.riley-test
  (:refer-clojure :exclude (compile))
  #+cljs (:require-macros [cemerick.cljs.test :refer (deftest testing is are)]
                          [clojure.test.check.clojure-test :refer (defspec)])
  (:require #+clj [clojure.test :as t :refer (deftest testing is are)]
            #+cljs [cemerick.cljs.test :as t]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop :include-macros true]
            #+clj [clojure.test.check.clojure-test :as ct :refer (defspec)]
            #+cljs [clojure.test.check.clojure-test :as ct]
            [revue.util :as util]
            [revue.vm :as vm]
            [revue.riley :as riley]))

(deftest warn-01
  (testing "Warnings from the compiler."
    (is (= (with-out-str (riley/warn "What?")) "Compiler Warning: What?\n"))))

(deftest compile-01
  (testing "Compile a simple program."
    #_
    (is (= (riley/compile 'foo) 'nop))))

(deftest arg-count-01
  (testing "Arg count matches."
    (is (= (riley/arg-count '(foo) 0) nil))
    (is (= (riley/arg-count '(foo :a) 1) nil))
    (is (= (riley/arg-count '(foo :a :b :c :d) 4) nil))
    (is (= (riley/arg-count '(foo) 0 1)))
    (is (= (riley/arg-count '(foo :a) 0 1)))
    (is (= (riley/arg-count '(foo) 0 3)))
    (is (= (riley/arg-count '(foo :a) 0 3)))
    (is (= (riley/arg-count '(foo :a :b) 0 3)))
    (is (= (riley/arg-count '(foo :a :b :c) 0 3)))))

(deftest arg-count-02
  (testing "Arg count does not match."
    (is (thrown? #+clj java.lang.AssertionError #+cljs js/Error
                 (riley/arg-count '(foo :a) 0)))
    (is (thrown? #+clj java.lang.AssertionError #+cljs js/Error
                 (riley/arg-count '(foo :a :b :c) 0)))
    (is (thrown? #+clj java.lang.AssertionError #+cljs js/Error
                 (riley/arg-count '(foo) 1)))
    (is (thrown? #+clj java.lang.AssertionError #+cljs js/Error
                 (riley/arg-count '(foo :a :b) 1)))
    (is (thrown? #+clj java.lang.AssertionError #+cljs js/Error
                 (riley/arg-count '(foo :a :b :c) 1)))
    (is (thrown? #+clj java.lang.AssertionError #+cljs js/Error
                 (riley/arg-count '(foo) 1 3)))
    (is (thrown? #+clj java.lang.AssertionError #+cljs js/Error
                 (riley/arg-count '(foo :a :b :c :d) 1 3)))))

(deftest gen-01
  (testing "Generating instructions."
    (binding [riley/*current-form* 'x]
      (is (= (riley/gen 'LSET 0 0)
             (list (assoc (vm/->LSET 0 0)
                     :source 'x
                     :function '%unknown-source)))))))

;;; Evaluate this (e.g., with C-x C-e in Cider) to run the tests for
;;; this namespace:
;;; (t/run-tests 'revue.riley-test)
;;; Evaluate this to run the test for all namespaces:
;;; (t/run-all-tests #"^revue\..*-test")
