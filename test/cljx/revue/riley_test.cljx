;;; Test for the revue.compiler namespace.

(ns revue.riley-test
  (:refer-clojure :exclude (compile))
  #+cljs (:require-macros [cemerick.cljs.test :refer (deftest testing is are)]
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
            [revue.vm :as vm]
            [revue.riley :as riley]))

(deftest warn-01
  (testing "Warnings from the compiler."
    (is (= (string/trim (with-out-str (riley/warn "What?"))) "Compiler Warning: What?"))))

(deftest compile-01
  (testing "Compile a simple program."
    #_
    (is (= (riley/compile 'foo) 'nop))))

(deftest arg-count-01
  (testing "Arg count matches."
    (is (= (riley/arg-count '(foo) 0) nil))
    (is (= (riley/arg-count '(foo :a) 1) nil))
    (is (= (riley/arg-count '(foo :a :b :c :d) 4) nil))
    (is (= (riley/arg-count '(foo) 0 1) nil))
    (is (= (riley/arg-count '(foo :a) 0 1) nil))
    (is (= (riley/arg-count '(foo) 0 3) nil))
    (is (= (riley/arg-count '(foo :a) 0 3) nil))
    (is (= (riley/arg-count '(foo :a :b) 0 3) nil))
    (is (= (riley/arg-count '(foo :a :b :c) 0 3) nil))
    (is (= (riley/arg-count '(foo :a :b) 2 3) nil))
    (is (= (riley/arg-count '(foo :a :b :c) 2 3) nil))
    (is (= (riley/arg-count '(foo :a :b) 0 :inf) nil))
    (is (= (riley/arg-count '(foo :a :b :c) 0 :inf) nil))
    (is (= (riley/arg-count '(foo :a :b) 2 :inf) nil))
    (is (= (riley/arg-count '(foo :a :b :c) 2 :inf) nil))))

(deftest arg-count-02
  (testing "Arg count does not match."
    (is (thrown? #+clj java.lang.AssertionError #+cljs js/Error
                 (riley/arg-count 'foo 0)))
    (is (thrown? #+clj java.lang.AssertionError #+cljs js/Error
                 (riley/arg-count () 0)))
    (is (thrown? #+clj java.lang.AssertionError #+cljs js/Error
                 (riley/arg-count '(foo :a) 0)))
    (is (thrown? #+clj java.lang.AssertionError #+cljs js/Error
                 (riley/arg-count '(foo :a :b :c) 0)))
    (is (thrown? #+clj java.lang.AssertionError #+cljs js/Error
                 (riley/arg-count '(foo) 1)))
    (is (thrown? #+clj java.lang.AssertionError #+cljs js/Error
                 (riley/arg-count '(foo :a :b) 1)))
    (is (thrown? #+clj java.lang.AssertionError #+cljs js/Error
                 (riley/arg-count '(foo :a :b :c) 1)))
    (is (thrown? #+clj java.lang.AssertionError #+cljs js/Error
                 (riley/arg-count '(foo) 1 3)))
    (is (thrown? #+clj java.lang.AssertionError #+cljs js/Error
                 (riley/arg-count '(foo :a :b :c :d) 1 3)))
    (is (thrown? #+clj java.lang.AssertionError #+cljs js/Error
                 (riley/arg-count '(foo) 1 :inf)))
    (is (thrown? #+clj java.lang.AssertionError #+cljs js/Error
                 (riley/arg-count '(foo) 2 :inf)))
    (is (thrown? #+clj java.lang.AssertionError #+cljs js/Error
                 (riley/arg-count '(foo 'a) 2 :inf)))
    (is (thrown? #+clj java.lang.AssertionError #+cljs js/Error
                 (riley/arg-count '(foo) 1 :infty)))
    (is (thrown? #+clj java.lang.AssertionError #+cljs js/Error
                 (riley/arg-count '(foo) 2 :infinity)))
    (is (thrown? #+clj java.lang.AssertionError #+cljs js/Error
                 (riley/arg-count '(foo 'a) 2 :infinite)))))

(deftest gen-01
  (testing "Generating instructions: LSET."
    (binding [riley/*current-form* 'x]
      (is (= (riley/gen 'LSET 0 0 'x)
             (list (assoc (vm/->LSET 0 0 'x)
                     :source 'x
                     :function :%unknown-function)))))))

;;; The following tests check mainly that the `-opcode` method and the
;;; `opcodes` table are defined consistenly.

(defn test-gen [opcode args kv-table]
  (let [insts (binding [riley/*current-form* 'foo
                        riley/*current-function* 'bar]
                (apply riley/gen opcode args))
        inst (first insts)]
    (is (= (vm/opcode inst) opcode))
    (is (= (:source inst) 'foo))
    (is (= (:function inst) 'bar))
    (dorun (map (fn [[k v]]
                  (is (= (k inst) v)))
                kv-table))))

(deftest gen-LVAR
  (test-gen 'LVAR [1 2 'x] {:frame 1 :slot 2 :name 'x}))

(deftest gen-LSET
  (test-gen 'LSET [1 2 'x] {:frame 1 :slot 2 :name 'x}))

(deftest gen-GVAR
  (test-gen 'GVAR '[my-fun] {:name 'my-fun}))

(deftest gen-GSET
  (test-gen 'GSET '[my-val] {:name 'my-val}))

(deftest gen-POP
  (test-gen 'POP [] {}))

(deftest gen-CONST-1
  (test-gen 'CONST [123] {:value 123}))

(deftest gen-CONST-2
  (test-gen 'CONST [[:a :b :c]] {:value [:a :b :c]}))

(deftest gen-JUMP
  (test-gen 'JUMP [42] {:target 42}))

(deftest gen-FJUMP
  (test-gen 'FJUMP [42] {:target 42}))

(deftest gen-TJUMP
  (test-gen 'TJUMP [42] {:target 42}))

(deftest gen-SAVE
  (test-gen 'SAVE [42] {:target 42}))

(deftest gen-RETURN
  (test-gen 'RETURN '[my-fun] {:fun 'my-fun}))

(deftest gen-CALLJ
  (test-gen 'CALLJ '[3] {:n-args 3}))

(deftest gen-ARGS
  (test-gen 'ARGS '[3 my-fun] {:n-args 3 :name 'my-fun}))

(deftest gen-ARGS*
  (test-gen 'ARGS* '[3 my-fun] {:n-args 3 :name 'my-fun}))

(deftest gen-FUN
  (test-gen 'FUN '[my-fun] {:fun 'my-fun}))

(deftest gen-PRIM
  (test-gen 'PRIM [println] {:clj-code println}))

(deftest gen-SET-CC
  (test-gen 'SET-CC [] {}))

(deftest gen-CC
  (test-gen 'CC [] {}))

(deftest gen-HALT
  (test-gen 'HALT [] {}))

(deftest gen-OP
  (test-gen 'OP ['println 0] {:name 'println :n-args 0})
  (test-gen 'OP ['println 3] {:name 'println :n-args 3}))

(defn add-source-info [inst & {:keys [function source]
                                       :or {function :%unknown-function
                                            source nil}}]
  (let [inst1 (if (:function inst)
                inst
                (assoc inst :function function))]
    (if (:source inst1)
      inst1
      (assoc inst1 :source source))))

(deftest gen-seq
  (is (= (riley/gen-seq) []))
  (is (= (riley/gen-seq (riley/gen 'LVAR 0 1 'x) (riley/gen 'GSET 'foo))
         (map add-source-info
              [(vm/->LVAR 0 1 'x) (vm/->GSET 'foo)])))
    (is (= (riley/gen-seq [] (riley/gen 'LVAR 0 1 'y) [] (riley/gen 'GSET 'foo) [])
         (map add-source-info
              [(vm/->LVAR 0 1 'y) (vm/->GSET 'foo)]))))

(deftest gen-label
  (reset! riley/label-counter 0)
  ;; Test that the counter is actually incremented
  (is (= (riley/gen-label) (vm/->Label 'L1)))
  (is (= (riley/gen-label) (vm/->Label 'L2)))
  (is (= (riley/gen-label) (vm/->Label 'L3))))

(deftest gen-var
  (let [env (util/env '[x y] '[a b c])]
    (is (= (riley/gen-var 'x env)
           [(assoc (vm/->LVAR 0 0 'x)
              :name 'x :source 'x :function :%unknown-function)]))
    (is (= (riley/gen-var 'y env)
           [(assoc (vm/->LVAR 0 1 'y)
              :name 'y :source 'y :function :%unknown-function)]))
    (is (= (riley/gen-var 'a env)
           [(assoc (vm/->LVAR 1 0 'a)
              :name 'a :source 'a :function :%unknown-function)]))
    (is (= (riley/gen-var 'b env)
           [(assoc (vm/->LVAR 1 1 'b)
              :name 'b :source 'b :function :%unknown-function)]))
    (is (= (riley/gen-var 'c env)
           [(assoc (vm/->LVAR 1 2 'c)
              :name 'c :source 'c :function :%unknown-function)]))
    (is (= (riley/gen-var 'z env)
           [(assoc (vm/->GVAR 'z) :source 'z :function :%unknown-function)]))))

(deftest gen-set
  (let [env (util/env '[x y] '[a b c])]
    (is (= (riley/gen-set 'x env '(set! x 0))
           [(assoc (vm/->LSET 0 0 'x)
              :name 'x :source '(set! x 0) :function :%unknown-function)]))
    (is (= (riley/gen-set 'y env '(set! y 0))
           [(assoc (vm/->LSET 0 1 'y)
              :name 'y :source '(set! y 0) :function :%unknown-function)]))
    (is (= (riley/gen-set 'a env '(set! a 0))
           [(assoc (vm/->LSET 1 0 'a)
              :name 'a :source '(set! a 0) :function :%unknown-function)]))
    (is (= (riley/gen-set 'b env '(set! b 0))
           [(assoc (vm/->LSET 1 1 'b)
              :name 'b :source '(set! b 0) :function :%unknown-function)]))
    (is (= (riley/gen-set 'c env '(set! c 0))
           [(assoc (vm/->LSET 1 2 'c)
              :name 'c :source '(set! c 0) :function :%unknown-function)]))
    (is (= (riley/gen-set 'z env '(set! z 0))
           [(assoc (vm/->GSET 'z) :source '(set! z 0) :function :%unknown-function)]))))

(deftest gen-args-1
  (is (= (riley/gen-args 'foo [])
         [(add-source-info (vm/->ARGS 0 'foo))]))
  (is (= (riley/gen-args 'foo '[a])
         [(add-source-info (vm/->ARGS 1 'foo))]))
  (is (= (riley/gen-args 'foo '[a b])
         [(add-source-info (vm/->ARGS 2 'foo))]))
  (is (= (riley/gen-args 'foo '[a b c])
         [(add-source-info (vm/->ARGS 3 'foo))]))
  (is (= (riley/gen-args 'foo '[& a])
         [(add-source-info (vm/->ARGS* 0 'foo))]))
  (is (= (riley/gen-args 'foo '[a & b])
         [(add-source-info (vm/->ARGS* 1 'foo))]))
  (is (= (riley/gen-args 'foo '[a b & c])
         [(add-source-info (vm/->ARGS* 2 'foo))]))
  (is (= (riley/gen-args 'foo '[a b c & c])
         [(add-source-info (vm/->ARGS* 3 'foo))])))

(deftest gen-args-2
  (is (thrown? #+clj java.lang.IllegalArgumentException #+cljs js/Error
               (riley/gen-args 'foo :args)))
  (is (thrown? #+clj java.lang.Exception #+cljs js/Error
               (riley/gen-args 'foo [1])))
  (is (thrown? #+clj java.lang.AssertionError #+cljs js/Error
               (riley/gen-args 'foo '[a & b c])))
  (is (thrown? #+clj java.lang.AssertionError #+cljs js/Error
               (riley/gen-args 'foo '[a & 1]))))

(deftest gen-return
  (binding [riley/*current-function* 'my-fun]
    (is (= (riley/gen-return))
        [(add-source-info (vm/->RETURN 'my-fun))])))

(deftest comp-const
  (binding [riley/*current-function* 'foo]
    (is (= (riley/comp-const 1 false false) []))
    (is (= (riley/comp-const 1 false true) []))
    (is (= (riley/comp-const 1 true false)
           (map #(assoc %1 :source nil :function 'foo)
                [(vm/->CONST 1) (vm/->RETURN 'foo)])))
    (is (= (riley/comp-const 1 true true)
           [(assoc (vm/->CONST 1) :source nil :function 'foo)]))))

(deftest comp-sequence
  (is (= (riley/comp-sequence [] (util/env) false false) []))
  (is (= (riley/comp-sequence [] (util/env) false true) []))
  (is (= (riley/comp-sequence [] (util/env) true false)
         (map add-source-info
              [(vm/->CONST nil) (vm/->RETURN :%unknown-function)])))
  (is (= (riley/comp-sequence [] (util/env) true true)
         [(add-source-info (vm/->CONST nil))]))
  (is (= (riley/comp-sequence [1] (util/env) false false) []))
  (is (= (riley/comp-sequence [1] (util/env) false true) []))
  (is (= (riley/comp-sequence [1] (util/env) true false)
         (map add-source-info
              [(vm/->CONST 1) (vm/->RETURN :%unknown-function)])))
  (is (= (riley/comp-sequence [1] (util/env) true true)
         [(add-source-info (vm/->CONST 1))]))
  (reset! riley/label-counter 0)
  (let [label (vm/->Label 'K1)]
    (is (= (riley/comp-sequence ['(f) 2 3] (util/env) false false)
           (concat (map (fn [inst] (add-source-info inst :source '(f)))
                        [(vm/->SAVE label) (assoc (vm/->GVAR 'f) :source 'f)
                         (vm/->CALLJ 0)])
                   [label]
                   (map (fn [inst] (add-source-info inst :source '(f)))
                        [(vm/->POP)])))))
  (reset! riley/label-counter 0)
  (let [label (vm/->Label 'K1)]
    (is (= (riley/comp-sequence ['(f) 2 3] (util/env) false true)
           (concat (map (fn [inst] (add-source-info inst :source '(f)))
                        [(vm/->SAVE label) (assoc (vm/->GVAR 'f) :source 'f)
                         (vm/->CALLJ 0)])
                   [label]
                   (map (fn [inst] (add-source-info inst :source '(f)))
                        [(vm/->POP)])))))
  (reset! riley/label-counter 0)
  (let [label (vm/->Label 'K1)]
    (is (= (riley/comp-sequence ['(f) 2 3] (util/env) true false)
           (concat (map (fn [inst] (add-source-info inst :source '(f)))
                        [(vm/->SAVE label) (assoc (vm/->GVAR 'f) :source 'f)
                         (vm/->CALLJ 0)])
                   [label]
                   (map (fn [inst] (add-source-info inst :source '(f)))
                        [(vm/->POP) ])
                   (map add-source-info
                        [(vm/->CONST 3) (vm/->RETURN :%unknown-function)])))))
  (reset! riley/label-counter 0)
  (let [label (vm/->Label 'K1)]
    (is (= (riley/comp-sequence ['(f) 2 3] (util/env) true true)
           (concat (map (fn [inst] (add-source-info inst :source '(f)))
                        [(vm/->SAVE label) (assoc (vm/->GVAR 'f) :source 'f)
                         (vm/->CALLJ 0)])
                   [label]
                   (map (fn [inst] (add-source-info inst :source '(f)))
                        [(vm/->POP)])
                   (map add-source-info
                        [(vm/->CONST 3)]))))))

(deftest macroexpand-1-01
  (is (riley/macroexpand-1 '(define x 1))
      '(set! x 1))
  (is (riley/macroexpand-1 '(define (foo) :bar))
      '(set! foo (lambda () :bar)))
  (is (riley/macroexpand-1 '(define (foo x y) (list x y)))
      '(set! foo (lambda (x y) (list x y))))
  (is (riley/macroexpand-1 '(define (foo x y) (print x) (println y) 'done))
      '(set! foo (lambda (x y) (print x) (println y) 'done))))

(deftest macroexpand-1-02
  (is (thrown? #+clj java.lang.AssertionError #+cljs js/Error
               (riley/macroexpand-1 '(define ((x)) :foo))))
  (is (thrown? #+clj java.lang.AssertionError #+cljs js/Error
               (riley/macroexpand-1 '(define (1) :foo)))))

(deftest macroexpand-1-03
  (is (= (riley/macroexpand-1 '(let ()))
         '((lambda let ()))))
  (is (= (riley/macroexpand-1 '(let ((x 0)) (+ x 1)))
         '((lambda let (x) (+ x 1)) 0)))
  (is (= (riley/macroexpand-1 '(let ((x 0) (y 1)) (+ x y)))
         '((lambda let (x y) (+ x y)) 0 1)))
  (is (= (riley/macroexpand-1
          '(let ((f nil)) (set! f (lambda (x) (f x))) (f 1)))
         '((lambda let (f) (set! f (lambda (x) (f x))) (f 1)) nil))))

(deftest macroexpand-1-04
  (is (= (riley/macroexpand-1 '(let loop () (loop)))
         '(letrec ((loop (lambda () (loop))))
                  (loop))))
  (is (= (riley/macroexpand-1 '(let loop ((x 0)) (loop (+ x 1))))
         '(letrec ((loop (lambda (x) (loop (+ x 1)))))
                  (loop 0))))
  (is (= (riley/macroexpand-1 '(let loop ((x 0) (y 1)) (loop (f x) y)))
         '(letrec ((loop (lambda (x y) (loop (f x) y))))
                  (loop 0 1)))))

(deftest macroexpand-1-05
  (is (= (riley/macroexpand-1 '(letrec ()))
         '(let ())))
  (is (= (riley/macroexpand-1 '(letrec ((f (lambda (x) (f x)))) (f 1)))
         '(let ((f nil)) (set! f (lambda (x) (f x))) (f 1))))
  (is (= (riley/macroexpand-1 '(letrec ((f (lambda (x) (g x x)))
                                        (g (lambda (x y) (f (+ x y)))))
                                       (f 1)))
         '(let ((f nil) (g nil))
            (set! f (lambda (x) (g x x)))
            (set! g (lambda (x y) (f (+ x y))))
            (f 1)))))


(deftest macroexpand-01
  (is (riley/macroexpand '(define x 1))
      '(set! x 1))
  (is (riley/macroexpand '(define (foo) :bar))
      '(set! foo (lambda () :bar)))
  (is (riley/macroexpand '(define (foo x y) (list x y)))
      '(set! foo (lambda (x y) (list x y))))
  (is (riley/macroexpand '(define (foo x y) (print x) (println y) 'done))
      '(set! foo (lambda (x y) (print x) (println y) 'done))))

(deftest macroexpand-02
  (is (thrown? #+clj java.lang.AssertionError #+cljs js/Error
               (riley/macroexpand '(define ((x)) :foo))))
  (is (thrown? #+clj java.lang.AssertionError #+cljs js/Error
               (riley/macroexpand '(define (1) :foo)))))

(deftest macroexpand-03
  (is (= (riley/macroexpand '(let ()))
         '((lambda let ()))))
  (is (= (riley/macroexpand '(let ((x 0)) (+ x 1)))
         '((lambda let (x) (+ x 1)) 0)))
  (is (= (riley/macroexpand '(let ((x 0) (y 1)) (+ x y)))
         '((lambda let (x y) (+ x y)) 0 1)))
  (is (= (riley/macroexpand
          '(let ((f nil)) (set! f (lambda (x) (f x))) (f 1)))
         '((lambda let (f) (set! f (lambda (x) (f x))) (f 1)) nil))))

(deftest macroexpand-04
  (is (= (riley/macroexpand '(let loop () (loop)))
         '((lambda let (loop)
              (set! loop (lambda () (loop)))
              (loop))
           nil)))
  (is (= (riley/macroexpand '(let loop ((x 0)) (loop (+ x 1))))
         '((lambda let (loop)
              (set! loop (lambda (x) (loop (+ x 1))))
              (loop 0))
           nil)))
  (is (= (riley/macroexpand '(let loop ((x 0) (y 1)) (loop (f x) y)))
         '((lambda let (loop)
              (set! loop (lambda (x y) (loop (f x) y)))
              (loop 0 1))
           nil))))

(deftest macroexpand-05
  (is (= (riley/macroexpand '(letrec ()))
         '((lambda let ()))))
  (is (= (riley/macroexpand '(letrec ((f (lambda (x) (f x)))) (f 1)))
         '((lambda let (f)
              (set! f (lambda (x) (f x)))
              (f 1))
           nil)))
  (is (= (riley/macroexpand '(letrec ((f (lambda (x) (g x x)))
                                        (g (lambda (x y) (f (+ x y)))))
                                       (f 1)))
         '((lambda let (f g)
              (set! f (lambda (x) (g x x)))
              (set! g (lambda (x y) (f (+ x y))))
              (f 1))
           nil nil))))

;;; Evaluate this (e.g., with C-x C-e in Cider) to run the tests for
;;; this namespace:
;;; (t/run-tests 'revue.riley-test)
;;; Evaluate this to run the test for all namespaces:
;;; (t/run-all-tests #"^revue\..*-test")
