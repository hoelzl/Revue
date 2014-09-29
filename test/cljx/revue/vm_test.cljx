;;; Test for the revue.vm namespace.

(ns revue.vm-test
  #+cljs (:require-macros [cemerick.cljs.test :as t :refer (deftest testing is are)]
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

(deftest Env-01
  (testing "Env: frames, count"
    (is (= (vm/frames (vm/->Env [])) []))
    (is (= (vm/frames (vm/->Env [1])) [1]))
    (is (= (count (vm/->Env [])) 0))
    (is (= (count (vm/->Env [1])) 1))
    (is (= (count (vm/->Env [1 2 3 4])) 4))))

(deftest Env-02
  (testing "Env: nth"
        (is (= (nth (vm/->Env [1]) 0) 1))
    ;; Note that the nth indexes from the end of the array
    (is (= (nth (vm/->Env [1 2 3 4]) 0) 4))
    (is (= (nth (vm/->Env [1 2 3 4]) 1) 3))
    (is (= (nth (vm/->Env [1 2 3 4]) 2) 2))
    (is (= (nth (vm/->Env [1 2 3 4]) 3) 1))
    (is (thrown? #+clj java.lang.IndexOutOfBoundsException #+cljs js/Error
                 (nth (vm/->Env [1 2 3 4]) 4)))
    (is (thrown? #+clj java.lang.IndexOutOfBoundsException #+cljs js/Error
                 (nth (vm/->Env [1 2 3 4]) -1)))))

(deftest Env-03
  (testing "Env: ="
    (is (= (seq (vm/->Env [])) nil))
    (let [env (vm/->Env [1])]
      (is (= (seq env) env)))
    (is (= (vm/->Env []) (vm/->Env [])))
    (is (= (vm/->Env [1]) (vm/->Env [1])))
    (is (not (= (vm/->Env []) (vm/->Env [1]))))))

(deftest Env-04
  (testing "Env: =, conj"
    (is (not (= (vm/->Env [1]) (vm/->Env []))))
    (is (= (vm/->Env [1 2 3 4]) (vm/->Env [1 2 3 4])))
    (is (not (= (vm/->Env [1 2 3]) (vm/->Env [1 2 3 4]))))
    (is (not (= (vm/->Env [1 2 3 4]) (vm/->Env [1 2 3]))))
    (is (= (vm/->Env [1 2 3 4]) [4 3 2 1]))
    (is (not (= (vm/->Env [1 2 3 4]) [1 2 3 4])))
    (is (= (conj (vm/->Env []) 1) (conj (vm/->Env [1]))))))

(deftest Env-05
  (testing "Env: empty?, first"
    (is (empty? (vm/->Env [])))
    (is (not (empty? (vm/->Env [1]))))
    (is (= (first (vm/->Env [])) nil))
    (is (= (first (vm/->Env [1])) 1))
    (is (= (first (vm/->Env [1 2])) 2))
    (is (= (first (vm/->Env [1 2 3])) 3))))

(deftest Env-06
  (testing "Env: rest"
    (is (= (rest (vm/->Env [])) ()))
    (is (= (rest (vm/->Env [1])) []))
    (is (= (list? (rest (vm/->Env [1])))))
    (is (= (rest (vm/->Env [1 2])) [1]))
    (is (= (type (rest (vm/->Env [1 2]))) revue.vm.Env))
    (is (= (rest (vm/->Env [1 2 3])) [2 1]))
    (is (= (type (rest (vm/->Env [1 2 3]))) revue.vm.Env))))

(deftest Env-07
  (testing "Env: next"
    (is (= (next (vm/->Env [])) nil))
    (is (= (next (vm/->Env [1])) nil))
    (is (= (next (vm/->Env [1 2])) [1]))
    (is (= (type (next (vm/->Env [1 2]))) revue.vm.Env))
    (is (= (next (vm/->Env [1 2 3])) [2 1]))
    (is (= (type (next (vm/->Env [1 2 3]))) revue.vm.Env))))

(deftest Env-08
  (testing "Env: peek, pop"
    (let [env (vm/->Env [[4 5 6] [1 2 3]])]
      (is (= (peek (vm/->Env [])) nil))
      (is (= (pop (vm/->Env [])) []))
      (is (= (type (pop (vm/->Env []))) revue.vm.Env))
      (is (= (peek env) [1 2 3]))
      (is (= (pop env) [[4 5 6]]))
      (is (= (type (pop env)) revue.vm.Env)))))

(deftest Env-09
  (testing "Env: assoc, get"
    (let [env (vm/->Env [1 2 3])]
      (is (= (assoc env 0 6) [6 2 1]))
      (is (= (type (assoc env 0 6)) revue.vm.Env))
      (is (= (assoc env 1 6) [3 6 1]))
      (is (= (type (assoc env 1 6)) revue.vm.Env))
      (is (= (assoc env 2 6) [3 2 6]))
      (is (= (type (assoc env 2 6)) revue.vm.Env))
      (is (= (get env 0) 3))
      (is (= (get env 1) 2))
      (is (= (get env 2) 1)))))

(deftest Env-10
  (testing "Env: get-in"
    (let [env (vm/->Env [[6 7 8 9] [4 5] [1 2 3]])]
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
    (let [env (vm/->Env [[6 7 8 9] [4 5] [1 2 3]])]
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
    (let [vm-state {:env (vm/->Env [[2 3 4] [0 1]])}]
      (is (= (vm/env-value vm-state {:frame 0 :slot 0}) 0))
      (is (= (vm/env-value vm-state {:frame 0 :slot 1}) 1))
      (is (= (vm/env-value vm-state {:frame 1 :slot 0}) 2))
      (is (= (vm/env-value vm-state {:frame 1 :slot 1}) 3))
      (is (= (vm/env-value vm-state {:frame 1 :slot 2}) 4)))))

(deftest make-global-env-01
  (testing "make-global-env"
    (is (= (vm/make-global-env) [{} []]))
    (is (= (vm/make-global-env {:foo ['foo '(bar)] :quux [3 2 1]})
           [{:foo #revue.mem.VmVector{:address 2, :size 2}
             :quux #revue.mem.VmVector{:address 4, :size 3}}
            ['bar () 'foo #revue.mem.VmCons{:address 0} 3 2 1]]))))

(deftest LVAR-step-01
  (testing "LVAR -step function."
    (let [env (vm/->Env [[2 3] [0 1]])
          state {:env env :stack '(4 5)}]
      (is (= (vm/-step (vm/->LVAR 0 0 nil) state)
             {:env env :stack '(0 4 5)}))
      (is (= (vm/-step (vm/->LVAR 0 1 nil) state)
             {:env env :stack '(1 4 5)}))
      (is (= (vm/-step (vm/->LVAR 1 0 nil) state)
             {:env env :stack '(2 4 5)}))
      (is (= (vm/-step (vm/->LVAR 1 1 nil) state)
             {:env env :stack '(3 4 5)})))))

(deftest LVAR-step-02
  (testing "LVAR -step function, out of bounds.")
    (let [env (vm/->Env [[2 3] [0 1]])
          state {:env env :stack '(4 5)}]
      (is (thrown? #+clj java.lang.IndexOutOfBoundsException #+cljs js/Error
                   (vm/-step (vm/->LVAR 2 0 nil) state)))
      (is (thrown? #+clj java.lang.IndexOutOfBoundsException #+cljs js/Error
                   (vm/-step (vm/->LVAR 0 2 nil) state)))))

(deftest LSET-step-01
  (testing "LSET -step function"
    (let [env (vm/->Env [[2 3] [0 1]])
          state {:env env :stack '(4 5)}]
      (is (= (vm/-step (vm/->LSET 0 0 nil nil) state)
             {:env (vm/->Env [[2 3] [4 1]]) :stack '(5)}))
      (is (= (vm/-step (vm/->LSET 0 1 nil nil) state)
             {:env (vm/->Env [[2 3] [0 4]]) :stack '(5)}))
      (is (= (vm/-step (vm/->LSET 0 2 nil nil) state)
             {:env (vm/->Env [[2 3] [0 1 4]]) :stack '(5)}))
      (is (= (vm/-step (vm/->LSET 1 0 nil nil) state)
             {:env (vm/->Env [[4 3] [0 1]]) :stack '(5)}))
      (is (= (vm/-step (vm/->LSET 1 1 nil nil) state)
             {:env (vm/->Env [[2 4] [0 1]]) :stack '(5)})))))

(deftest LSET-step-02
  (testing "LSET -step function, out of bounds.")
    (let [env (vm/->Env [[2 3] [0 1]])
          state {:env env :stack '(4 5)}]
      (is (thrown? #+clj java.lang.IndexOutOfBoundsException #+cljs js/Error
                   (vm/-step (vm/->LSET 2 0 nil nil) state)))
      (is (thrown? #+clj java.lang.IndexOutOfBoundsException #+cljs js/Error
                   (vm/-step (vm/->LSET 0 3 nil nil) state)))))

(deftest GVAR-01
  (testing "GVAR"
    (let [state {:global-env {'foo 123} :stack '(1)}]
      (is (= (vm/-step (vm/->GVAR 'foo) state)
             {:global-env {'foo 123} :stack '(123 1)}))
      (is (= (vm/-step (vm/->GVAR 'bar) state)
             {:global-env {'foo 123} :stack '(nil 1)})))))

(deftest GSET-01
  (testing "GSET"
    (let [state {:global-env {'foo 123} :stack '(1 2)}]
      (is (= (vm/-step (vm/->GSET 'foo nil) state)
             {:global-env {'foo 1} :stack '(2)}))
      (is (= (vm/-step (vm/->GSET 'bar nil) state)
             {:global-env {'foo 123 'bar 1} :stack '(2)})))))


(deftest POP-01
  (testing "POP"
    (is (= (vm/-step (vm/->POP nil) {:stack '(1 2 3)})
           {:stack '(2 3)}))
    (is (= (vm/-step (vm/->POP nil) {:stack '(1)})
           {:stack '()}))
    (is (thrown? #+clj java.lang.IllegalStateException #+cljs js/Error
                 (vm/-step (vm/->POP nil) {:stack '()})))))

(deftest CONST-01
  (testing "CONST: numbers"
    (is (= (vm/-step (vm/->CONST 0 nil) {:stack '(1 2 3) :store []})
           {:stack '(0 1 2 3) :store []}))
    (is (= (vm/-step (vm/->CONST 0 nil) {:stack '(1) :store []})
           {:stack '(0 1) :store []}))
    (is (= (vm/-step (vm/->CONST 0 nil) {:stack '() :store []})
           {:stack '(0) :store []}))))

(deftest CONST-02
  (testing "CONST: symbols"
    (is (= (vm/-step (vm/->CONST 'foo nil) {:stack '(1 2 3) :store []})
           {:stack '(foo 1 2 3) :store []}))
    (is (= (vm/-step (vm/->CONST 'foo nil) {:stack '(1) :store []})
           {:stack '(foo 1) :store []}))
    (is (= (vm/-step (vm/->CONST 'foo nil) {:stack '() :store []})
           {:stack '(foo) :store []}))))

(deftest CONST-03
  (testing "CONST: lists"
    (is (= (vm/-step (vm/->CONST () nil) {:stack '(1) :store []})
           {:stack '(() 1) :store []}))
    (is (= (vm/-step (vm/->CONST '(2) nil) {:stack '(1) :store []})
           {:stack '(#revue.mem.VmCons{:address 0} 1) :store [2 ()]}))
    (is (= (vm/-step (vm/->CONST '(2 3 4) nil) {:stack '(1) :store []})
           {:stack '(#revue.mem.VmCons{:address 4} 1),
            :store [4 ()
                    3 #revue.mem.VmCons{:address 0}
                    2 #revue.mem.VmCons{:address 2}]}))))

(deftest CONST-04
  (testing "CONST: arrays"
    (is (= (vm/-step (vm/->CONST [] nil) {:stack '(1) :store []})
           {:stack '(#revue.mem.VmVector{:address 0, :size 0} 1), :store []}))
    (is (= (vm/-step (vm/->CONST [2] nil) {:stack '(1) :store []})
           {:stack '(#revue.mem.VmVector{:address 0, :size 1} 1), :store [2]}))
    (is (= (vm/-step (vm/->CONST [2 3 4] nil) {:stack '(1) :store []})
           {:stack '(#revue.mem.VmVector{:address 0, :size 3} 1), :store [2 3 4]}))))

(deftest JUMP-01
  (testing "JUMP"
    (is (= (vm/-step (vm/->JUMP 10 nil) {:pc 0}) {:pc 10}))))

(deftest FJUMP-01
  (testing "FJUMP"
    (is (= (vm/-step (vm/->FJUMP 10 nil) {:pc 0 :stack '(false)})
           {:pc 10 :stack '()}))
    (is (= (vm/-step (vm/->FJUMP 10 nil) {:pc 0 :stack '(true)})
           {:pc 0 :stack '()}))))

(deftest TJUMP-01
  (testing "TJUMP"
    (is (= (vm/-step (vm/->TJUMP 10 nil) {:pc 0 :stack '(false)})
           {:pc 0 :stack '()}))
    (is (= (vm/-step (vm/->TJUMP 10 nil) {:pc 0 :stack '(true)})
           {:pc 10 :stack '()}))))

;;; Evaluate this (e.g., with C-x C-e in Cider) to run the tests for
;;; this namespace:
;;; (t/run-tests 'revue.vm-test)
;;; Evaluate this to run the test for all namespaces:
;;; (t/run-all-tests #"^revue\..*-test")
