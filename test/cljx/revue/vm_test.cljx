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

(deftest make-global-env-01
  (testing "make-global-env"
    (is (= (vm/make-global-env) [{} []]))
    (is (= (vm/make-global-env {:foo ['foo '(bar)] :quux [3 2 1]})
           [{:foo (mem/->VmVector 2 2)
             :quux (mem/->VmVector 4 3)}
            ['bar () 'foo (mem/->VmCons 0) 3 2 1]]))))

(deftest LVAR-step-01
  (testing "LVAR -step function."
    (let [env (util/->Env [[2 3] [0 1]])
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
    (let [env (util/->Env [[2 3] [0 1]])
          state {:env env :stack '(4 5)}]
      (is (thrown? #+clj java.lang.IndexOutOfBoundsException #+cljs js/Error
                   (vm/-step (vm/->LVAR 2 0 nil) state)))
      (is (thrown? #+clj java.lang.IndexOutOfBoundsException #+cljs js/Error
                   (vm/-step (vm/->LVAR 0 2 nil) state)))))

(deftest LSET-step-01
  (testing "LSET -step function"
    (let [env (util/->Env [[2 3] [0 1]])
          state {:env env :stack '(4 5)}]
      (is (= (vm/-step (vm/->LSET 0 0 nil nil) state)
             {:env (util/->Env [[2 3] [4 1]]) :stack '(5)}))
      (is (= (vm/-step (vm/->LSET 0 1 nil nil) state)
             {:env (util/->Env [[2 3] [0 4]]) :stack '(5)}))
      (is (= (vm/-step (vm/->LSET 0 2 nil nil) state)
             {:env (util/->Env [[2 3] [0 1 4]]) :stack '(5)}))
      (is (= (vm/-step (vm/->LSET 1 0 nil nil) state)
             {:env (util/->Env [[4 3] [0 1]]) :stack '(5)}))
      (is (= (vm/-step (vm/->LSET 1 1 nil nil) state)
             {:env (util/->Env [[2 4] [0 1]]) :stack '(5)})))))

(deftest LSET-step-02
  (testing "LSET -step function, out of bounds.")
    (let [env (util/->Env [[2 3] [0 1]])
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
           {:stack (list (mem/->VmCons 0) 1) :store [2 ()]}))
    (is (= (vm/-step (vm/->CONST '(2 3 4) nil) {:stack '(1) :store []})
           {:stack (list (mem/->VmCons 4) 1),
            :store [4 ()
                    3 (mem/->VmCons 0)
                    2 (mem/->VmCons 2)]}))))

(deftest CONST-04
  (testing "CONST: arrays"
    (is (= (vm/-step (vm/->CONST [] nil) {:stack '(1) :store []})
           {:stack (list (mem/->VmVector 0 0) 1), :store []}))
    (is (= (vm/-step (vm/->CONST [2] nil) {:stack '(1) :store []})
           {:stack (list (mem/->VmVector 0 1) 1), :store [2]}))
    (is (= (vm/-step (vm/->CONST [2 3 4] nil) {:stack '(1) :store []})
           {:stack (list (mem/->VmVector 0 3) 1), :store [2 3 4]}))))

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

(deftest SAVE-01
  (testing "SAVE"
    (is (= (vm/-step (vm/->SAVE nil) {:stack '(4) :function 'foo :pc 5 :env (util/->Env [[1 2 3]])})
           {:stack (list {:type :return-address :function 'foo :pc 5 :env (util/->Env [[1 2 3]])}
                         4)
            :function 'foo :pc 5 :env (util/->Env [[1 2 3]])}))))

(deftest RETURN-01
  (testing "RETURN"
    (let [f {:code (list (vm/->ARGS 1 nil nil)
                         (vm/->RETURN nil))}
          ret-addr (vm/make-return-address
                    {:function f
                     :pc 1
                     :env (util/->Env [[1 2 3]])})]
      (is (= (vm/-step (vm/->RETURN nil)
                       {:stack (list 1 ret-addr 4)
                        :function 'bar :pc 10 :env (util/->Env [[4 5]])})
             {:stack '(1 4)
              :function f
              :code (:code f)
              :pc (:pc ret-addr)
              :env (:env ret-addr)})))))

(deftest CALLJ-01
  (testing "CALLJ"
    (let [f (vm/make-fn :code (list (vm/->ARGS 1 nil nil)
                                    (vm/->RETURN nil))
                        :env (util/->Env [[1 2 3]])
                        :name 'foo
                        :args '(x))]
      (is (= (vm/-step (vm/->CALLJ 1 nil)
                       {:stack (list f 4)
                        :function 'bar :pc 10 :env (util/->Env [[4 5]])})
             {:n-args 1
              :stack '(4)
              :function f
              :code (:code f)
              :pc 0
              :env (:env f)})))))

(deftest ARGS-01
  (testing "ARGS"
    (is (= (vm/-step (vm/->ARGS 0 'foo nil)
                     {:n-args 0 :stack '(1 2 3 4) :env (util/->Env [[5 6]])})
           {:n-args 0 :stack '(1 2 3 4) :env (util/->Env [[5 6] []])}))
    (is (= (vm/-step (vm/->ARGS 1 'foo nil)
                     {:n-args 1 :stack '(1 2 3 4) :env (util/->Env [[5 6]])})
           {:n-args 1 :stack '(2 3 4) :env (util/->Env [[5 6] [1]])}))
    (is (= (vm/-step (vm/->ARGS 2 'foo nil)
                     {:n-args 2 :stack '(1 2 3 4) :env (util/->Env [[5 6]])})
           {:n-args 2 :stack '(3 4) :env (util/->Env [[5 6] [1 2]])}))
    (is (= (vm/-step (vm/->ARGS 4 'foo nil)
                     {:n-args 4 :stack '(1 2 3 4) :env (util/->Env [[5 6]])})
           {:n-args 4 :stack () :env (util/->Env [[5 6] [1 2 3 4]])}))))

(deftest ARGS-02
  (testing "ARGS: failure"
    (let [state {:n-args 2 :stack '(1 2 3 4) :env (util/->Env [[5 6]])}]
      (is (= (vm/-step (vm/->ARGS 0 'foo nil) state)
             (assoc state
               :stopped? true
               :reason "Function foo called with 2 argument(s), but wants exactly 0.")))
      (is (= (vm/-step (vm/->ARGS 1 'foo nil) state)
             (assoc state
               :stopped? true
               :reason "Function foo called with 2 argument(s), but wants exactly 1.")))
      (is (= (vm/-step (vm/->ARGS 3 'foo nil) state)
             (assoc state
               :stopped? true
               :reason "Function foo called with 2 argument(s), but wants exactly 3."))))))

(deftest ARGS*-01
  (testing "ARGS*"
    (is (= (vm/-step (vm/->ARGS* 0 'foo nil)
                     {:n-args 0 :stack '(1 2 3 4) :env (util/->Env [[5 6]])})
           {:n-args 0 :stack '(1 2 3 4) :env (util/->Env [[5 6] [[]]])}))
    (is (= (vm/-step (vm/->ARGS* 0 'foo nil)
                     {:n-args 1 :stack '(1 2 3 4) :env (util/->Env [[5 6]])})
           {:n-args 1 :stack '(2 3 4) :env (util/->Env [[5 6] [[1]]])}))
    (is (= (vm/-step (vm/->ARGS* 0 'foo nil)
                     {:n-args 2 :stack '(1 2 3 4) :env (util/->Env [[5 6]])})
           {:n-args 2 :stack '(3 4) :env (util/->Env [[5 6] [[1 2]]])}))
    (is (= (vm/-step (vm/->ARGS* 0 'foo nil)
                     {:n-args 4 :stack '(1 2 3 4) :env (util/->Env [[5 6]])})
           {:n-args 4 :stack '() :env (util/->Env [[5 6] [[1 2 3 4]]])}))
    (is (= (vm/-step (vm/->ARGS* 1 'foo nil)
                     {:n-args 1 :stack '(1 2 3 4) :env (util/->Env [[5 6]])})
           {:n-args 1 :stack '(2 3 4) :env (util/->Env [[5 6] [1 []]])}))
    (is (= (vm/-step (vm/->ARGS* 1 'foo nil)
                     {:n-args 2 :stack '(1 2 3 4) :env (util/->Env [[5 6]])})
           {:n-args 2 :stack '(3 4) :env (util/->Env [[5 6] [1 [2]]])})) 
    (is (= (vm/-step (vm/->ARGS* 1 'foo nil)
                     {:n-args 3 :stack '(1 2 3 4) :env (util/->Env [[5 6]])})
           {:n-args 3 :stack '(4) :env (util/->Env [[5 6] [1 [2 3]]])})) 
    (is (= (vm/-step (vm/->ARGS* 2 'foo nil)
                     {:n-args 2 :stack '(1 2 3 4) :env (util/->Env [[5 6]])})
           {:n-args 2 :stack '(3 4) :env (util/->Env [[5 6] [1 2 []]])}))
    (is (= (vm/-step (vm/->ARGS* 4 'foo nil)
                     {:n-args 4 :stack '(1 2 3 4) :env (util/->Env [[5 6]])})
           {:n-args 4 :stack () :env (util/->Env [[5 6] [1 2 3 4 []]])}))))

(deftest ARGS*-02
  (testing "ARGS*: failure"
    (let [state {:n-args 2 :stack '(1 2 3 4) :env (util/->Env [[5 6]])}]
      (is (= (vm/-step (vm/->ARGS* 3 'foo nil) state)
             (assoc state
               :stopped? true
               :reason "Function foo called with 2 argument(s), but wants at least 3."))))))

(deftest FN-01
  (testing "FN"
    (let [f (vm/make-fn :code (list (vm/->ARGS 1 nil nil)
                                    (vm/->RETURN nil))
                        :env (util/->Env [[1 2 3]])
                        :name 'foo
                        :args '(x))
          env  (util/->Env [[5 6]])]
      (is (= (vm/-step (vm/->FN f nil) {:stack '(1 2) :env env})
             {:stack (list (assoc f :env env) 1 2)
              :env env})))))

;;; Evaluate this (e.g., with C-x C-e in Cider) to run the tests for
;;; this namespace:
;;; (t/run-tests 'revue.vm-test)
;;; Evaluate this to run the test for all namespaces:
;;; (t/run-all-tests #"^revue\..*-test")
