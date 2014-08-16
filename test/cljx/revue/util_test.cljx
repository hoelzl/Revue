;;; Tests for the revue.util namespace.

(ns revue.util-test
  #+cljs (:require-macros [cemerick.cljs.test :refer (deftest testing is are)]
                          [clojure.test.check.clojure-test :refer (defspec)])
  (:require #+clj [clojure.test :refer (deftest testing is are)]
            #+cljs [cemerick.cljs.test :as t]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop :include-macros true]
            #+clj [clojure.test.check.clojure-test :as ct :refer (defspec)]
            #+cljs [clojure.test.check.clojure-test :as ct]
            [revue.util :as util]))

(deftest error-01
  (testing "Test the `error' function."
    (is (thrown? #+clj java.lang.Exception #+cljs js/Error
                 (util/error "Foo!")))
    (is (thrown-with-msg? #+clj java.lang.Exception #+cljs js/Error
                          #"^Foo!$"
                          (util/error "Foo!")))))

(deftest warn-01
  (testing "Test the `warn' function."
    (is (= (with-out-str (util/warn "Hi!")) "REVUE Warning: Hi!\n"))
    (is (= (with-out-str (util/warn "Foo" "Bar")) "Foo Bar\n"))))

(deftest boolean?-01
  (testing "Test the `boolean?' function."
    (is (util/boolean? true))
    (is (util/boolean? false))
    (is (not (util/boolean? 0)))
    (is (not (util/boolean? ())))
    (is (not (util/boolean? [])))))

(deftest atomic?-01
  (testing "Test the `atomic?' function."
    (is (util/atomic? true))
    (is (util/atomic? false))
    (is (util/atomic? 0))
    (is (util/atomic? 1))
    (is (util/atomic? -1))
    (is (util/atomic? 'foo))
    (is (util/atomic? :foo))
    (is (util/atomic? "foo"))
    (is (util/atomic? ()))
    (is (not (util/atomic? [])))
    (is (not (util/atomic? '(1 2 3))))
    (is (not (util/atomic? [1 2 3])))))

;;; Evaluate this (e.g., with C-x C-e in Cider) to run the tests for
;;; this namespace:
;;; (t/run-tests 'revue.util-test)
;;; Evaluate this to run the test for all namespaces:
;;; (t/run-all-tests #"^revue\..*-test")
