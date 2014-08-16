;;; Test for the revue.vm namespace.

(ns revue.vm-test
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
            [revue.mem :as mem]
            [revue.vm :as vm]))


(deftest warn-01
  (testing "Warnings from the VM."
    (is (= (with-out-str (vm/warn "Hey!")) "VM Warning: Hey!\n"))))

;;; Evaluate this (e.g., with C-x C-e in Cider) to run the tests for
;;; this namespace:
;;; (t/run-tests 'revue.vm-test)
;;; Evaluate this to run the test for all namespaces:
;;; (t/run-all-tests #"^revue\..*-test")
