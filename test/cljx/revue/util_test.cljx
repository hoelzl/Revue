;;; Tests for the revue.util namespace.

(ns revue.util-test
  #+cljs (:require-macros [cemerick.cljs.test :refer (deftest testing is are)])            
  (:require #+clj [clojure.test :refer (deftest testing is are)]
            #+cljs [cemerick.cljs.test :as t]
            [revue.util :as util]))

(deftest util-01
  (testing "Test a utility function."
    (is (= 0 0))))


;;; Evaluate this (e.g., with C-x C-e in Cider) to run the tests for
;;; this namespace:
;;; (clojure.test/run-tests 'revue.util-test)
