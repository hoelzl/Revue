;;; Tests for the revue.util namespace.

(ns revue.util-test
  #+cljs (:require-macros [cemerick.cljs.test :refer (deftest testing is are)]
                          [clojure.test.check.clojure-test :refer (defspec)])
  (:require #+clj [clojure.test :refer (deftest testing is are) :as t]
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

(deftest singleton-01
  (testing "Test the `singleton' function."
    (is (not (util/singleton? [])))
    (is (not (util/singleton? ())))
    (is (not (util/singleton? {})))
    (is (not (util/singleton? #{})))
    (is (util/singleton? [:a]))
    (is (util/singleton? [123]))
    (is (util/singleton? '(:a)))
    (is (util/singleton? {:a 1}))
    (is (util/singleton? #{:a}))
    (is (not (util/singleton? [:a :b])))
    (is (not (util/singleton? '(:a :b))))
    (is (not (util/singleton? {:a 1, :b 2})))
    (is (not (util/singleton? #{:a :b})))))

(deftest maybe-add-01
  (testing "Test the `maybe-add' function."
    (is (= (util/maybe-add 'begin ())) false)
    (is (= (util/maybe-add 'begin () :foobar)) :foobar)
    (is (= (util/maybe-add 'begin '(1))) 1)
    (is (= (util/maybe-add 'begin '((+ 1 2 3)))) '(+ 1 2 3))
    (is (= (util/maybe-add 'begin '((print x) (+ x 1)))
           '(begin (print x) (+ x 1))))))

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

(deftest read-program-from-string-01
  (testing "Reading multiple forms from a string"
    (is (= (util/read-program-from-string "") []))
    (is (= (util/read-program-from-string "   ") []))
    (is (= (util/read-program-from-string "1") [1]))
    (is (= (util/read-program-from-string "1 2") [1 2]))
    (is (= (util/read-program-from-string "[]") [[]]))
    (is (= (util/read-program-from-string "[][]") [[] []]))
    (is (= (util/read-program-from-string "[] []") [[] []]))
    (is (= (util/read-program-from-string "[1 2 3] [4 5 6]") [[1 2 3] [4 5 6]]))))

(defspec read-program-from-string-02 50
  (testing "Random roundtrip test for reading-programs from strings."
    (prop/for-all [d gen/any-printable]
      (let [string (prn-str d)]
        ;; (println "d:     " d)
        ;; (println "string:" string)
        (if (empty? (clojure.string/trim string))
          (is (= (util/read-program-from-string string) []))
          (is (= (util/read-program-from-string string) [d])))))))


(deftest Env-01
  (testing "Env: frames, count"
    (is (= (util/frames (util/->Env [])) []))
    (is (= (util/frames (util/->Env [1])) [1]))
    (is (= (count (util/->Env [])) 0))
    (is (= (count (util/->Env [1])) 1))
    (is (= (count (util/->Env [1 2 3 4])) 4))))

(deftest Env-02
  (testing "Env: nth"
        (is (= (nth (util/->Env [1]) 0) 1))
    ;; Note that the nth indexes from the end of the array
    (is (= (nth (util/->Env [1 2 3 4]) 0) 4))
    (is (= (nth (util/->Env [1 2 3 4]) 1) 3))
    (is (= (nth (util/->Env [1 2 3 4]) 2) 2))
    (is (= (nth (util/->Env [1 2 3 4]) 3) 1))
    (is (thrown? #+clj java.lang.IndexOutOfBoundsException #+cljs js/Error
                 (nth (util/->Env [1 2 3 4]) 4)))
    (is (thrown? #+clj java.lang.IndexOutOfBoundsException #+cljs js/Error
                 (nth (util/->Env [1 2 3 4]) -1)))))

(deftest Env-03
  (testing "Env: ="
    (is (= (seq (util/->Env [])) nil))
    (let [env (util/->Env [1])]
      (is (= (seq env) env)))
    (is (= (util/->Env []) (util/->Env [])))
    (is (= (util/->Env [1]) (util/->Env [1])))
    (is (not (= (util/->Env []) (util/->Env [1]))))))

(deftest Env-04
  (testing "Env: =, conj"
    (is (not (= (util/->Env [1]) (util/->Env []))))
    (is (= (util/->Env [1 2 3 4]) (util/->Env [1 2 3 4])))
    (is (not (= (util/->Env [1 2 3]) (util/->Env [1 2 3 4]))))
    (is (not (= (util/->Env [1 2 3 4]) (util/->Env [1 2 3]))))
    (is (= (util/->Env [1 2 3 4]) [4 3 2 1]))
    (is (not (= (util/->Env [1 2 3 4]) [1 2 3 4])))
    (is (= (conj (util/->Env []) 1) (conj (util/->Env [1]))))))

(deftest Env-05
  (testing "Env: empty?, first"
    (is (empty? (util/->Env [])))
    (is (not (empty? (util/->Env [1]))))
    (is (= (first (util/->Env [])) nil))
    (is (= (first (util/->Env [1])) 1))
    (is (= (first (util/->Env [1 2])) 2))
    (is (= (first (util/->Env [1 2 3])) 3))))

(deftest Env-06
  (testing "Env: rest"
    (is (= (rest (util/->Env [])) ()))
    (is (= (rest (util/->Env [1])) []))
    (is (= (list? (rest (util/->Env [1])))))
    (is (= (rest (util/->Env [1 2])) [1]))
    (is (= (type (rest (util/->Env [1 2]))) revue.util.Env))
    (is (= (rest (util/->Env [1 2 3])) [2 1]))
    (is (= (type (rest (util/->Env [1 2 3]))) revue.util.Env))))

(deftest Env-07
  (testing "Env: next"
    (is (= (next (util/->Env [])) nil))
    (is (= (next (util/->Env [1])) nil))
    (is (= (next (util/->Env [1 2])) [1]))
    (is (= (type (next (util/->Env [1 2]))) revue.util.Env))
    (is (= (next (util/->Env [1 2 3])) [2 1]))
    (is (= (type (next (util/->Env [1 2 3]))) revue.util.Env))))

(deftest Env-08
  (testing "Env: peek, pop"
    (let [env (util/->Env [[4 5 6] [1 2 3]])]
      (is (= (peek (util/->Env [])) nil))
      (is (= (pop (util/->Env [])) []))
      (is (= (type (pop (util/->Env []))) revue.util.Env))
      (is (= (peek env) [1 2 3]))
      (is (= (pop env) [[4 5 6]]))
      (is (= (type (pop env)) revue.util.Env)))))

(deftest Env-09
  (testing "Env: assoc, get"
    (let [env (util/->Env [1 2 3])]
      (is (= (assoc env 0 6) [6 2 1]))
      (is (= (type (assoc env 0 6)) revue.util.Env))
      (is (= (assoc env 1 6) [3 6 1]))
      (is (= (type (assoc env 1 6)) revue.util.Env))
      (is (= (assoc env 2 6) [3 2 6]))
      (is (= (type (assoc env 2 6)) revue.util.Env))
      (is (= (get env 0) 3))
      (is (= (get env 1) 2))
      (is (= (get env 2) 1)))))

(deftest Env-10
  (testing "Env: get-in"
    (let [env (util/->Env [[6 7 8 9] [4 5] [1 2 3]])]
      (is (= (get-in env [0 0]) 1))
      (is (= (get-in env [0 1]) 2))
      (is (= (get-in env [0 2]) 3))
      (is (= (get-in env [1 0]) 4))
      (is (= (get-in env [1 1]) 5))
      (is (= (get-in env [2 0]) 6))
      (is (= (get-in env [2 1]) 7))
      (is (= (get-in env [2 2]) 8))
      (is (= (get-in env [2 3]) 9)))))

(deftest Env-11
  (testing "Env: assoc-in"
    (let [env (util/->Env [[6 7 8 9] [4 5] [1 2 3]])]
      (is (= (assoc-in env [0 0] 10) [[10 2 3] [4 5] [6 7 8 9]]))
      (is (= (assoc-in env [0 1] 10) [[1 10 3] [4 5] [6 7 8 9]]))
      (is (= (assoc-in env [0 2] 10) [[1 2 10] [4 5] [6 7 8 9]]))
      (is (= (assoc-in env [1 0] 10) [[1 2 3] [10 5] [6 7 8 9]]))
      (is (= (assoc-in env [1 1] 10) [[1 2 3] [4 10] [6 7 8 9]]))
      (is (= (assoc-in env [2 0] 10) [[1 2 3] [4 5] [10 7 8 9]]))
      (is (= (assoc-in env [2 1] 10) [[1 2 3] [4 5] [6 10 8 9]]))
      (is (= (assoc-in env [2 2] 10) [[1 2 3] [4 5] [6 7 10 9]]))
      (is (= (assoc-in env [2 3] 10) [[1 2 3] [4 5] [6 7 8 10]])))))

(deftest env-value-01
  (testing "env-value"
    (let [vm-state {:env (util/->Env [[2 3 4] [0 1]])}]
      (is (= (util/env-value vm-state {:frame 0 :slot 0}) 0))
      (is (= (util/env-value vm-state {:frame 0 :slot 1}) 1))
      (is (= (util/env-value vm-state {:frame 1 :slot 0}) 2))
      (is (= (util/env-value vm-state {:frame 1 :slot 1}) 3))
      (is (= (util/env-value vm-state {:frame 1 :slot 2}) 4)))))

(deftest in-env?-01
  (testing "in-env?"
    (let [env (util/->Env [[:d :e :f :g] [:a :b :c]])]
      (is (= (util/in-env? env :a) [0 0]))
      (is (= (util/in-env? env :b) [0 1]))
      (is (= (util/in-env? env :c) [0 2]))
      (is (= (util/in-env? env :d) [1 0]))
      (is (= (util/in-env? env :e) [1 1]))
      (is (= (util/in-env? env :f) [1 2]))
      (is (= (util/in-env? env :g) [1 3]))
      (is (not (util/in-env? env :h))))))

;;; Evaluate this (e.g., with C-x C-e in Cider) to run the tests for
;;; this namespace:
;;; (t/run-tests 'revue.util-test)
;;; Evaluate this to run the test for all namespaces:
;;; (t/run-all-tests #"^revue\..*-test")
