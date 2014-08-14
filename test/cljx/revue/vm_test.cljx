;;; Test for the revue.vm namespace.

(ns revue.vm-test
  #+cljs (:require-macros [cemerick.cljs.test :refer (deftest testing is are)])            
  (:require #+clj [clojure.test :refer (deftest testing is are)]
            #+cljs [cemerick.cljs.test :as t]
            [revue.util :as util]
            [revue.vm :as vm]))

(deftest warn-01
  (testing "Warnings from the VM."
    (is (= (with-out-str (vm/warn "Hey!")) "VM Warning: Hey!\n"))))

;;; Evaluate this (e.g., with C-x C-e in Cider) to run the tests for
;;; this namespace:
;;; (clojure.test/run-tests 'revue.vm-test)
;;; Evaluate this to run the test for all namespaces:
;;; (clojure.test/run-all-tests #"^revue\..*-test")
