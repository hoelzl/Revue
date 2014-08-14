;;; Test for the revue.compiler namespace.

(ns revue.scm-test
  #+cljs (:require-macros [cemerick.cljs.test :refer (deftest testing is are)])            
  (:require #+clj [clojure.test :refer (deftest testing is are)]
            #+cljs [cemerick.cljs.test :as t]
            [revue.util :as util]
            [revue.vm :as vm]
            [revue.scm :as scm]))

(deftest compile-01
  (testing "Compile a simple program."
    (is (= (scm/scm->bytecode 'foo) 'nop))))

;;; Evaluate this (e.g., with C-x C-e in Cider) to run the tests for
;;; this namespace:
;;; (clojure.test/run-tests 'revue.scm-test)
