;;; Test for the revue.compiler namespace.

(ns revue.scm-test
  (:refer-clojure :exclude (compile))
  #+cljs (:require-macros [cemerick.cljs.test :refer (deftest testing is are)]
                          [clojure.test.check.clojure-test :refer (defspec)])
  (:require #+clj [clojure.test :refer (deftest testing is are)]
            #+cljs [cemerick.cljs.test :as t]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop :include-macros true]
            #+clj [clojure.test.check.clojure-test :as ct :refer (defspec)]
            #+cljs [clojure.test.check.clojure-test :as ct]
            [revue.util :as util]
            [revue.vm :as vm]
            [revue.scm :as scm]))

(deftest warn-01
  (testing "Warnings from the compiler."
    (is (= (with-out-str (scm/warn "What?")) "Compiler Warning: What?\n"))))

(deftest compile-01
  (testing "Compile a simple program."
    (is (= (scm/compile 'foo) 'nop))))

;;; Evaluate this (e.g., with C-x C-e in Cider) to run the tests for
;;; this namespace:
;;; (t/run-tests 'revue.scm-test)
;;; Evaluate this to run the test for all namespaces:
;;; (t/run-all-tests #"^revue\..*-test")
