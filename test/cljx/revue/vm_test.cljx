;;; Test for the revue.vm namespace.

(ns revue.vm-test
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
            [revue.vm :as vm]))

(deftest warn-01
  (testing "Warnings from the VM."
    (is (= (with-out-str (vm/warn "Hey!")) "VM Warning: Hey!\n"))))

(deftest ->clojure-bool
  (testing "Conversion of denoted Booleans to clojure."
    (is (= (vm/->clojure true []) true))
    (is (= (vm/->clojure false []) false))))

(deftest ->clojure-number
  (testing "Conversion of denoted numbers to clojure."
    (is (= (vm/->clojure 0 []) 0))
    (is (= (vm/->clojure 1 []) 1))
    (is (= (vm/->clojure -1 []) -1))
    #+clj
    (is (= (vm/->clojure 1/2 []) 1/2))
    (is (= (vm/->clojure 0.5 []) 0.5))))

(defspec ->clojure-number-tc
  100
  (testing
      "Random tests for conversion of denoted numbers to clojure"
    (prop/for-all [n gen/int]
                  (= (vm/->clojure n []) n))))

;;; Evaluate this (e.g., with C-x C-e in Cider) to run the tests for
;;; this namespace:
;;; (clojure.test/run-tests 'revue.vm-test)
;;; Evaluate this to run the test for all namespaces:
;;; (clojure.test/run-all-tests #"^revue\..*-test")
