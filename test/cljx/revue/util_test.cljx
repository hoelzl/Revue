;;; Tests for the revue.util namespace.

(ns revue.util-test
  #+cljs (:require-macros [cemerick.cljs.test :refer (deftest testing is are)])            
  (:require #+clj [clojure.test :refer (deftest testing is are)]
            #+cljs [cemerick.cljs.test :as t]
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


;;; Evaluate this (e.g., with C-x C-e in Cider) to run the tests for
;;; this namespace:
;;; (clojure.test/run-tests 'revue.util-test)
;;; Evaluate this to run the test for all namespaces:
;;; (clojure.test/run-all-tests #"^revue\..*-test")
