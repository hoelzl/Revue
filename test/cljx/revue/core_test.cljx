(ns revue.core-test
  #+cljs (:require-macros [cemerick.cljs.test :refer (deftest testing is are)])            
  (:require #+clj [clojure.test :refer (deftest testing is are)]
            #+cljs [cemerick.cljs.test :as t]
            [revue.core :as r]))

(deftest a-test
  (testing "Trivial test."
    (is (= 0 0))))
