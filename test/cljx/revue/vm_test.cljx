;; Test for the revue.vm namespace.

(ns revue.vm-test
  #+cljs (:require-macros [cemerick.cljs.test :as t :refer (deftest testing is are)]
                          [clojure.test.check.clojure-test :refer (defspec)])
  (:require #+clj [clojure.test :as t :refer (deftest testing is are)]
            #+cljs [cemerick.cljs.test :as t]
            [clojure.string :as string]
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
    (is (= (string/trim (with-out-str (vm/warn "Hey!")))
           "VM Warning: Hey!"))))

(deftest make-global-env-01
  (is (= (vm/make-global-env) [{} []]))
  (is (= (vm/make-global-env {:foo ['foo '(bar)] :quux [3 2 1]})
         [{:foo (mem/->VmVector 2 2)
           :quux (mem/->VmVector 4 3)}
          ['bar () 'foo (mem/->VmCons 0) 3 2 1]])))

(deftest current-instruction
  (let [fun (vm/make-fun :code [(vm/->LVAR 0 0 'x)
                                (vm/->LSET 0 0 'x)
                                (vm/->GVAR 'y)
                                (vm/->GSET 'y)
                                (vm/->POP)
                                (vm/->CONST 1)
                                (vm/->JUMP 10)
                                (vm/->FJUMP 10)
                                (vm/->TJUMP 10)
                                (vm/->SAVE 10)
                                (vm/->RETURN 'foo)
                                (vm/->CALLJ 2)
                                (vm/->ARGS 3 'bar)
                                (vm/->ARGS* 3 'baz)
                                (vm/->FUN {})
                                (vm/->PRIM nil 1)
                                (vm/->SET-CC)
                                (vm/->CC)
                                (vm/->HALT)
                                (vm/->OP 'quux 17)])
        init-state (vm/initial-state fun)
        state #(assoc init-state :pc %1)]
    (is (= (vm/current-instruction (state 0))
           (vm/->LVAR 0 0 'x)))
    (is (= (vm/current-instruction (state 1))
           (vm/->LSET 0 0 'x)))
    (is (= (vm/current-instruction (state 2))
           (vm/->GVAR 'y)))
    (is (= (vm/current-instruction (state 3))
           (vm/->GSET 'y)))
    (is (= (vm/current-instruction (state 4))
           (vm/->POP)))
    (is (= (vm/current-instruction (state 5))
           (vm/->CONST 1)))
    (is (= (vm/current-instruction (state 6))
           (vm/->JUMP 10)))
    (is (= (vm/current-instruction (state 7))
           (vm/->FJUMP 10)))
    (is (= (vm/current-instruction (state 8))
           (vm/->TJUMP 10)))
    (is (= (vm/current-instruction (state 9))
           (vm/->SAVE 10)))
    (is (= (vm/current-instruction (state 10))
           (vm/->RETURN 'foo)))
    (is (= (vm/current-instruction (state 11))
           (vm/->CALLJ 2)))
    (is (= (vm/current-instruction (state 12))
           (vm/->ARGS 3 'bar)))
    (is (= (vm/current-instruction (state 13))
           (vm/->ARGS* 3 'baz)))
    (is (= (vm/current-instruction (state 14))
           (vm/->FUN {})))
    (is (= (vm/current-instruction (state 15))
           (vm/->PRIM nil 1)))
    (is (= (vm/current-instruction (state 16))
           (vm/->SET-CC)))
    (is (= (vm/current-instruction (state 17))
           (vm/->CC)))
    (is (= (vm/current-instruction (state 18))
           (vm/->HALT)))
    (is (= (vm/current-instruction (state 19))
           (vm/->OP 'quux 17)))))

(deftest current-instruction-is
  (let [fun (vm/make-fun :code [(vm/->LVAR 0 0 'x)
                                (vm/->LSET 0 0 'x)
                                (vm/->GVAR 'y)
                                (vm/->GSET 'y)
                                (vm/->POP)
                                (vm/->CONST 1)
                                (vm/->JUMP 10)
                                (vm/->FJUMP 10)
                                (vm/->TJUMP 10)
                                (vm/->SAVE 10)
                                (vm/->RETURN 'foo)
                                (vm/->CALLJ 2)
                                (vm/->ARGS 3 'bar)
                                (vm/->ARGS* 3 'baz)
                                (vm/->FUN {})
                                (vm/->PRIM nil 1)
                                (vm/->SET-CC)
                                (vm/->CC)
                                (vm/->HALT)
                                (vm/->OP 'quux 17)])
        init-state (vm/initial-state fun)
        state #(assoc init-state :pc %1)]
    (is (vm/current-instruction-is (state 0) 'LVAR))
    (is (vm/current-instruction-is (state 1) 'LSET))
    (is (vm/current-instruction-is (state 2) 'GVAR))
    (is (vm/current-instruction-is (state 3) 'GSET))
    (is (vm/current-instruction-is (state 4) 'POP))
    (is (vm/current-instruction-is (state 5) 'CONST))
    (is (vm/current-instruction-is (state 6) 'JUMP))
    (is (vm/current-instruction-is (state 7) 'FJUMP))
    (is (vm/current-instruction-is (state 8) 'TJUMP))
    (is (vm/current-instruction-is (state 9) 'SAVE))
    (is (vm/current-instruction-is (state 10) 'RETURN))
    (is (vm/current-instruction-is (state 11) 'CALLJ))
    (is (vm/current-instruction-is (state 12) 'ARGS))
    (is (vm/current-instruction-is (state 13) 'ARGS*))
    (is (vm/current-instruction-is (state 14) 'FUN))
    (is (vm/current-instruction-is (state 15) 'PRIM))
    (is (vm/current-instruction-is (state 16) 'SET-CC))
    (is (vm/current-instruction-is (state 17) 'CC))
    (is (vm/current-instruction-is (state 18) 'HALT))
    (is (vm/current-instruction-is (state 19) 'OP))))

(deftest LVAR-step-01
  (testing "LVAR -step function."
    (let [env (util/env [0 1] [2 3])
          state {:env env :stack '(4 5)}]
      (is (= (vm/-step (vm/->LVAR 0 0 'x) state)
             {:env env :stack '(0 4 5)}))
      (is (= (vm/-step (vm/->LVAR 0 1 'x) state)
             {:env env :stack '(1 4 5)}))
      (is (= (vm/-step (vm/->LVAR 1 0 'x) state)
             {:env env :stack '(2 4 5)}))
      (is (= (vm/-step (vm/->LVAR 1 1 'x) state)
             {:env env :stack '(3 4 5)})))))

(deftest LVAR-step-02
  (testing "LVAR -step function, out of bounds.")
    (let [env (util/env [0 1] [2 3])
          state {:env env :stack '(4 5)}]
      (is (thrown? #+clj java.lang.IndexOutOfBoundsException #+cljs js/Error
                   (vm/-step (vm/->LVAR 2 0 'x) state)))
      (is (thrown? #+clj java.lang.IndexOutOfBoundsException #+cljs js/Error
                   (vm/-step (vm/->LVAR 0 2 'x) state)))))

(deftest LVAR-step-03
  (testing "LVAR -step function for boxes."
    (let [env (util/env [(mem/->VmBox 0) (mem/->VmBox 1)]
                        [(mem/->VmBox 2) (mem/->VmBox 3)])
          state {:env env :stack '(4 5) :store [0 1 2 3]}]
      (is (= (vm/-step (vm/->LVAR 0 0 'x) state)
             {:env env :stack '(0 4 5) :store [0 1 2 3]}))
      (is (= (vm/-step (vm/->LVAR 0 1 'x) state)
             {:env env :stack '(1 4 5) :store [0 1 2 3]}))
      (is (= (vm/-step (vm/->LVAR 1 0 'x) state)
             {:env env :stack '(2 4 5) :store [0 1 2 3]}))
      (is (= (vm/-step (vm/->LVAR 1 1 'x) state)
             {:env env :stack '(3 4 5) :store [0 1 2 3]})))))

(deftest LSET-step-01
  (testing "LSET -step function"
    (let [env (util/env [0 1] [2 3])
          state {:env env :stack '(4 5) :store []}]
      (is (= (vm/-step (vm/->LSET 0 0 'x) state)
             {:env (util/env [4 1] [2 3]) :stack '(4 5) :store []}))
      (is (= (vm/-step (vm/->LSET 0 1 'x) state)
             {:env (util/env [0 4] [2 3]) :stack '(4 5) :store []}))
      (is (= (vm/-step (vm/->LSET 1 0 'x) state)
             {:env (util/env [0 1] [4 3]) :stack '(4 5) :store []}))
      (is (= (vm/-step (vm/->LSET 1 1 'x) state)
             {:env (util/env [0 1] [2 4]) :stack '(4 5) :store []})))))

(deftest LSET-step-02
  (testing "LSET -step function, out of bounds.")
    (let [env (util/env [0 1] [2 3])
          state {:env env :stack '(4 5) :store []}]
      (is (thrown? #+clj java.lang.IndexOutOfBoundsException #+cljs js/Error
                   (vm/-step (vm/->LSET 2 0 'x) state)))
      (is (thrown? #+clj java.lang.IndexOutOfBoundsException #+cljs js/Error
                   (vm/-step (vm/->LSET 0 2 'x) state)))
      (is (thrown? #+clj java.lang.IndexOutOfBoundsException #+cljs js/Error
                   (vm/-step (vm/->LSET 0 3 'x) state)))))

(deftest LSET-step-03
  (testing "LSET -step function for boxed values"
    (let [env (util/env [(mem/->VmBox 0) (mem/->VmBox 1)]
                        [(mem/->VmBox 2) (mem/->VmBox 3)])
          state {:env env :stack '(4 5) :store [0 1 2 3]}]
      (is (= (vm/-step (vm/->LSET 0 0 'x) state)
             {:env env :stack '(4 5) :store [4 1 2 3]}))
      (is (= (vm/-step (vm/->LSET 0 1 'x) state)
             {:env env :stack '(4 5) :store [0 4 2 3]}))
      (is (= (vm/-step (vm/->LSET 1 0 'x) state)
             {:env env :stack '(4 5) :store [0 1 4 3]}))
      (is (= (vm/-step (vm/->LSET 1 1 'x) state)
             {:env env :stack '(4 5) :store [0 1 2 4]})))))

(deftest GVAR-step-01
  (testing "GVAR"
    (let [state {:global-env {'foo 123} :stack '(1)}]
      (is (= (vm/-step (vm/->GVAR 'foo) state)
             {:global-env {'foo 123} :stack '(123 1)}))
      (is (= (vm/-step (vm/->GVAR 'bar) state)
             {:global-env {'foo 123} :stack '(nil 1)})))))

(deftest GSET-step-01
  (testing "GSET"
    (let [state {:global-env {'foo 123} :stack '(1 2)}]
      (is (= (vm/-step (vm/->GSET 'foo) state)
             {:global-env {'foo 1} :stack '(1 2)}))
      (is (= (vm/-step (vm/->GSET 'bar) state)
             {:global-env {'foo 123 'bar 1} :stack '(1 2)})))))


(deftest POP-step-01
  (testing "POP"
    (is (= (vm/-step (vm/->POP) {:stack '(1 2 3)})
           {:stack '(2 3)}))
    (is (= (vm/-step (vm/->POP) {:stack '(1)})
           {:stack '()}))
    (is (thrown? #+clj java.lang.AssertionError #+cljs js/Error
                 (vm/-step (vm/->POP) {:stack '()})))))

(deftest CONST-step-01
  (testing "CONST: numbers"
    (is (= (vm/-step (vm/->CONST 0) {:stack '(1 2 3) :store []})
           {:stack '(0 1 2 3) :store []}))
    (is (= (vm/-step (vm/->CONST 0) {:stack '(1) :store []})
           {:stack '(0 1) :store []}))
    (is (= (vm/-step (vm/->CONST 0) {:stack '() :store []})
           {:stack '(0) :store []}))))

(deftest CONST-step-02
  (testing "CONST: symbols"
    (is (= (vm/-step (vm/->CONST 'foo) {:stack '(1 2 3) :store []})
           {:stack '(foo 1 2 3) :store []}))
    (is (= (vm/-step (vm/->CONST 'foo) {:stack '(1) :store []})
           {:stack '(foo 1) :store []}))
    (is (= (vm/-step (vm/->CONST 'foo) {:stack '() :store []})
           {:stack '(foo) :store []}))))

(deftest CONST-step-03
  (testing "CONST: lists"
    (is (= (vm/-step (vm/->CONST ()) {:stack '(1) :store []})
           {:stack '(() 1) :store []}))
    (is (= (vm/-step (vm/->CONST '(2)) {:stack '(1) :store []})
           {:stack (list (mem/->VmCons 0) 1) :store [2 ()]}))
    (is (= (vm/-step (vm/->CONST '(2 3 4)) {:stack '(1) :store []})
           {:stack (list (mem/->VmCons 4) 1),
            :store [4 ()
                    3 (mem/->VmCons 0)
                    2 (mem/->VmCons 2)]}))))

(deftest CONST-step-04
  (testing "CONST: arrays"
    (is (= (vm/-step (vm/->CONST []) {:stack '(1) :store []})
           {:stack (list (mem/->VmVector 0 0) 1), :store []}))
    (is (= (vm/-step (vm/->CONST [2]) {:stack '(1) :store []})
           {:stack (list (mem/->VmVector 0 1) 1), :store [2]}))
    (is (= (vm/-step (vm/->CONST [2 3 4]) {:stack '(1) :store []})
           {:stack (list (mem/->VmVector 0 3) 1), :store [2 3 4]}))))

(deftest JUMP-step-01
  (testing "JUMP"
    (is (= (vm/-step (vm/->JUMP 10) {:pc 0}) {:pc 10}))))

(deftest FJUMP-step-01
  (testing "FJUMP"
    (is (= (vm/-step (vm/->FJUMP 10) {:pc 0 :stack '(false)})
           {:pc 10 :stack '()}))
    (is (= (vm/-step (vm/->FJUMP 10) {:pc 0 :stack '(true)})
           {:pc 0 :stack '()}))))

(deftest TJUMP-step-01
  (testing "TJUMP"
    (is (= (vm/-step (vm/->TJUMP 10) {:pc 0 :stack '(false)})
           {:pc 0 :stack '()}))
    (is (= (vm/-step (vm/->TJUMP 10) {:pc 0 :stack '(true)})
           {:pc 10 :stack '()}))))

(deftest SAVE-step-01
  (testing "SAVE"
    (is (= (vm/-step (vm/->SAVE 10) {:stack '(4) :fun 'foo :pc 5 :env (util/env [1 2 3])})
           {:stack (list {:type :return-address :fun 'foo :pc 10 :env (util/env [1 2 3])}
                         4)
            :fun 'foo :pc 5 :env (util/env [1 2 3])}))))

(deftest RETURN-step-01
  (testing "RETURN"
    (let [f {:code (list (vm/->ARGS 1 nil)
                         (vm/->RETURN 'f))}
          ret-addr (vm/make-return-address
                    {:fun f
                     :pc 1
                     :env (util/env [1 2 3])})]
      (is (= (vm/-step (vm/->RETURN 'bar)
                       {:stack (list 1 ret-addr 4)
                        :fun 'bar :pc 10 :env (util/env [4 5])})
             {:stack '(1 4)
              :fun f
              :pc (:pc ret-addr)
              :env (:env ret-addr)})))))

(deftest CALLJ-step-01
  (testing "CALLJ"
    (let [f (vm/make-fun :code (list (vm/->ARGS 1 nil)
                                     (vm/->RETURN 'foo))
                         :env (util/env [1 2 3])
                         :name 'foo
                         :args '(x))]
      (is (= (vm/-step (vm/->CALLJ 1)
                       {:stack (list f 4)
                        :fun 'bar :pc 10 :env (util/env [4 5])})
             {:n-args 1
              :stack '(4)
              :fun f
              :pc 0
              :env (:env f)})))))

(deftest ARGS-step-01
  (testing "ARGS"
    (is (= (vm/-step (vm/->ARGS 0 'foo)
                     {:n-args 0 :stack '(1 2 3 4) :env (util/env [5 6]) :store []})
           {:n-args 0 :stack '(1 2 3 4) :env (util/env [] [5 6]) :store []}))
    (is (= (vm/-step (vm/->ARGS 1 'foo)
                     {:n-args 1 :stack '(1 2 3 4) :env (util/env [5 6]) :store []})
           {:n-args 1 :stack '(2 3 4) :env (util/env [1] [5 6]) :store []}))
    (is (= (vm/-step (vm/->ARGS 2 'foo)
                     {:n-args 2 :stack '(1 2 3 4) :env (util/env [5 6]) :store []})
           {:n-args 2 :stack '(3 4) :env (util/env [2 1] [5 6]) :store []}))
    (is (= (vm/-step (vm/->ARGS 4 'foo)
                     {:n-args 4 :stack '(1 2 3 4) :env (util/env [5 6]) :store []})
           {:n-args 4 :stack () :env (util/env [4 3 2 1] [5 6]) :store []}))))

(deftest ARGS-step-02
  (testing "ARGS: failure"
    (let [state {:n-args 2 :stack '(1 2 3 4) :env (util/env [5 6]) :store []}]
      (is (= (vm/-step (vm/->ARGS 0 'foo) state)
             (assoc state
               :stopped? true
               :reason "Function foo called with 2 argument(s), but wants exactly 0.")))
      (is (= (vm/-step (vm/->ARGS 1 'foo) state)
             (assoc state
               :stopped? true
               :reason "Function foo called with 2 argument(s), but wants exactly 1.")))
      (is (= (vm/-step (vm/->ARGS 3 'foo) state)
             (assoc state
               :stopped? true
               :reason "Function foo called with 2 argument(s), but wants exactly 3."))))))

(deftest ARGS*-step-01
  (testing "ARGS*"
    (is (= (vm/-step (vm/->ARGS* 0 'foo)
                     {:n-args 0 :stack '(1 2 3 4) :env (util/env [5 6]) :store []})
           {:n-args 0 :stack '(1 2 3 4)
            :env (util/env [(mem/->VmVector 0 0)] [5 6]) :store []}))
    (is (= (vm/-step (vm/->ARGS* 0 'foo)
                     {:n-args 1 :stack '(1 2 3 4) :env (util/env [5 6]) :store []})
           {:n-args 1 :stack '(2 3 4)
            :env (util/env  [(mem/->VmVector 0 1)] [5 6]) :store [1]}))
    (is (= (vm/-step (vm/->ARGS* 0 'foo)
                     {:n-args 2 :stack '(1 2 3 4) :env (util/env [5 6]) :store []})
           {:n-args 2 :stack '(3 4)
            :env (util/env [(mem/->VmVector 0 2)] [5 6]) :store [2 1]}))
    (is (= (vm/-step (vm/->ARGS* 0 'foo)
                     {:n-args 4 :stack '(1 2 3 4) :env (util/env [5 6]) :store []})
           {:n-args 4 :stack '()
            :env (util/env [(mem/->VmVector 0 4)] [5 6]) :store [4 3 2 1]}))
    (is (= (vm/-step (vm/->ARGS* 1 'foo)
                     {:n-args 1 :stack '(1 2 3 4) :env (util/env [5 6]) :store []})
           {:n-args 1 :stack '(2 3 4)
            :env (util/env [1 (mem/->VmVector 0 0)] [5 6]) :store []}))
    (is (= (vm/-step (vm/->ARGS* 1 'foo)
                     {:n-args 2 :stack '(1 2 3 4) :env (util/env [5 6]) :store []})
           {:n-args 2 :stack '(3 4)
            :env (util/env [2 (mem/->VmVector 0 1)] [5 6]) :store [1]})) 
    (is (= (vm/-step (vm/->ARGS* 1 'foo)
                     {:n-args 3 :stack '(1 2 3 4) :env (util/env [5 6]) :store []})
           {:n-args 3 :stack '(4)
            :env (util/env [3 (mem/->VmVector 0 2)] [5 6]) :store [2 1]})) 
    (is (= (vm/-step (vm/->ARGS* 2 'foo)
                     {:n-args 2 :stack '(1 2 3 4) :env (util/env [5 6]) :store []})
           {:n-args 2 :stack '(3 4)
            :env (util/env [2 1 (mem/->VmVector 0 0)] [5 6]) :store []}))
    (is (= (vm/-step (vm/->ARGS* 4 'foo)
                     {:n-args 4 :stack '(1 2 3 4) :env (util/env [5 6]) :store []})
           {:n-args 4 :stack ()
            :env (util/env [4 3 2 1 (mem/->VmVector 0 0)] [5 6]) :store []}))))

(deftest ARGS*-step-02
  (testing "ARGS*: failure"
    (let [state {:n-args 2 :stack '(1 2 3 4) :env (util/env [5 6])}]
      (is (= (vm/-step (vm/->ARGS* 3 'foo) state)
             (assoc state
               :stopped? true
               :reason "Function foo called with 2 argument(s), but wants at least 3."))))))

(deftest FUN-step-01
  (let [f (vm/make-fun :code (list (vm/->ARGS 1 'foo)
                                   (vm/->RETURN 'foo))
                       :env (util/env [1 2 3])
                       :name 'foo
                       :args '(x))
        env  (util/env [5 6])]
    (is (= (vm/-step (vm/->FUN f) {:stack '(1 2) :env env})
           {:stack (list (assoc f :env env) 1 2)
            :env env}))))

(deftest PRIM-step-01
  (let [inst (vm/->PRIM 'vector 0)
        state {:stack '(1 2 3) :store []}]
    (is (= (vm/-step inst state)
           (assoc state :stack
                  (conj (:stack state) (mem/->VmVector 0 0)))))))

(deftest PRIM-step-02
  (let [inst (vm/->PRIM 'vector 1)
        state {:stack '(1 2 3)  :store []}]
    (is (= (vm/-step inst state)
           (assoc state
             :stack (conj '(2 3) (mem/->VmVector 0 1))
             :store [1])))))

(deftest PRIM-step-03
  (let [inst (vm/->PRIM 'vector 3)
        state {:stack '(1 2 3) :store []}]
    (is (= (vm/-step inst state)
           (assoc state
             :stack (list (mem/->VmVector 0 3))
             :store [3 2 1])))))

(deftest PRIM-step-04
  (let [inst (vm/->PRIM 'make-vector 1)
        state {:stack '(0 1 2 3) :store []}]
    (is (= (vm/-step inst state)
           (assoc state
             :stack (conj (rest (:stack state)) (mem/->VmVector 0 0))
             :store [])))))

(deftest PRIM-step-05
  (let [inst (vm/->PRIM 'make-vector 1)
        state {:stack '(1 1 2 3) :store []}]
    (is (= (vm/-step inst state)
           (assoc state
             :stack (conj (rest (:stack state)) (mem/->VmVector 0 1))
             :store [nil])))))

(deftest PRIM-step-06
  (let [inst (vm/->PRIM 'make-vector 1)
        state {:stack '(5 1 2 3) :store []}]
    (is (= (vm/-step inst state)
           (assoc state
             :stack (conj (rest (:stack state)) (mem/->VmVector 0 5))
             :store [nil nil nil nil nil])))))

(deftest PRIM-step-07
  (let [inst (vm/->PRIM 'vector-ref 2)
        state {:stack (list 0 (mem/->VmVector 0 5) 10)
               :store [1 2 3 4 5]}]
    (is (= (vm/-step inst state)
           (assoc state
             :stack '(1 10)
             :store [1 2 3 4 5])))))

(deftest PRIM-step-08
  (let [inst (vm/->PRIM 'vector-ref 2)
        state {:stack (list 1 (mem/->VmVector 0 5) 10)
               :store [1 2 3 4 5]}]
    (is (= (vm/-step inst state)
           (assoc state
             :stack '(2 10)
             :store [1 2 3 4 5])))))

(deftest PRIM-step-09
  (let [inst (vm/->PRIM 'vector-set! 3)
        state {:stack (list 6 0 (mem/->VmVector 0 5) 10)
               :store [1 2 3 4 5]}]
    (is (= (vm/-step inst state)
           (assoc state
             :stack '(nil 10)
             :store [6 2 3 4 5])))))

(deftest PRIM-step-10
  (let [inst (vm/->PRIM 'vector-set! 3)
        state {:stack (list 6 3 (mem/->VmVector 0 5) 10)
               :store [1 2 3 4 5]}]
    (is (= (vm/-step inst state)
           (assoc state
             :stack '(nil 10)
             :store [1 2 3 6 5])))))

(deftest PRIM-step-11
  (let [inst (vm/->PRIM 'vector-set! 3)
        state {:stack (list 6 4 (mem/->VmVector 0 5) 10)
               :store [1 2 3 4 5]}]
    (is (= (vm/-step inst state)
           (assoc state
             :stack '(nil 10)
             :store [1 2 3 4 6])))))

(deftest PRIM-step-12
  (let [inst (vm/->PRIM 'vector-ref 2)
        state {:stack (list -1 (mem/->VmVector 0 5) 10)
               :store [1 2 3 4 5]}]
    (is (thrown? #+clj java.lang.AssertionError #+cljs js/Error
         (vm/-step inst state)))))

(deftest PRIM-step-13
  (let [inst (vm/->PRIM 'vector-ref 2)
        state {:stack (list 5 (mem/->VmVector 0 5) 10)
               :store [1 2 3 4 5]}]
    (is (thrown? #+clj java.lang.AssertionError #+cljs js/Error
         (vm/-step inst state)))))

(deftest PRIM-step-14
  (let [inst (vm/->PRIM 'vector-set! 3)
        state {:stack (list 0 -1 (mem/->VmVector 0 5) 10)
               :store [1 2 3 4 5]}]
    (is (thrown? #+clj java.lang.AssertionError #+cljs js/Error
         (vm/-step inst state)))))

(deftest PRIM-step-15
  (let [inst (vm/->PRIM 'vector-set! 3)
        state {:stack (list 0 5 (mem/->VmVector 0 5) 10)
               :store [1 2 3 4 5]}]
    (is (thrown? #+clj java.lang.AssertionError #+cljs js/Error
         (vm/-step inst state)))))


;;; Evaluate this (e.g., with C-x C-e in Cider) to run the tests for
;;; this namespace:
;;; (clojure.test/run-tests 'revue.vm-test)
;;; Evaluate this to run the test for all namespaces:
;;; (clojure.test/run-all-tests #"^revue\..*-test")
