(ns revue.riley
  "A compiler from a simple Lisp/Scheme/Clojure-like language to the
  Revue VM."
  (:refer-clojure :exclude (compile))
  (:require [revue.util :as util]
            [revue.vm :as vm]))

(defn warn [msg]
  (util/warn "Compiler Warning:" msg))

;;; Some Utilities for the Compiler
;;; ===============================

(defn arg-count
  ([form min]
     (arg-count form min min))
  ([form min max]
     (let [n-args (count (rest form))]
       (assert (<= min n-args max)
               (str "Wrong number of arguments for "
                    (first form) ": " n-args " supplied, "
                    min
                    (if (> max min) (str " to " max) "")
                    " expected.")))))

(def ^:dynamic *current-function* '%unknown-source)
(def ^:dynamic *current-form* nil)

(defn current-source []
  (or *current-form* *current-function*))

(defn gen [opcode & args]
  (let [opcode-descr (get vm/opcodes opcode)]
    (assert opcode-descr
            (str "Unknown bytecode instruction: " opcode))
    (cond (= (:arity opcode-descr) (count args))
          (list (list* opcode args))
          (and (:source opcode-descr) (= (:arity opcode-descr) (inc (count args))))
          (list (list* opcode (concat args (list (current-source)))))
          :else
          (util/error "Bad arity for bytecode instruction " opcode))))

(defn gen-seq [& insts]
  (apply concat insts))

(def label-counter (atom 0))

(defn gen-label
  ([]
     (gen-label "L"))
  ([prefix]
     (symbol (str prefix (swap! label-counter inc)))))

(defn gen-var [var env]
  (let [pos (util/in-env? env var)]
    (if pos
      (gen 'LVAR (first pos) (second pos) var)
      (gen 'GVAR var))))

(defn gen-set [var env form]
  (let [pos (util/in-env? env var)]
    (if pos
      (gen 'LSET (first pos) (second pos) var form)
      ;; TODO: Protect immutable bindings
      (gen 'GSET var form))))

(defn gen-args [name args n-so-far]
  (cond
   ;; Reached the end of the arglist.  Fixed arg call.
   (empty? args)
   (gen 'ARGS n-so-far name)
   ;; Found & in the arglist.  Vararg call
   (= '& (first args))
   (do
     (assert (and (util/singleton? (rest args))
                  (symbol? (first (rest args))))
             "& must be followed by a single rest arg")
     (gen 'ARGS* n-so-far name))
   ;; Non-empty arglist, symbol in the first position.
   (and (sequential? args) (symbol? (first args)))
   (gen-args name (rest args) (inc n-so-far))
   ;; Something's wrong...
   :default
   (util/error "Illegal argument list.")))

(defn gen-return []
  (gen 'RETURN *current-function*))

(declare compile)

(defn compile-const [c val? more?]
  (when val?
    (gen-seq (gen 'CONST c)
             (when-not more? (gen-return)))))

(defn compile-sequence [forms env val? more?]
  (cond (empty? forms)
        (compile-const nil val? more?)
        (util/singleton? forms)
        (compile (first forms) env val? more?)
        :else
        (gen-seq (compile (first forms) env false true)
                 (compile-sequence (rest forms) env val? more?))))

(defn compile-list [forms env]
  (if (empty? forms)
    ()
    (gen-seq (compile (first forms) env true true)
             (compile-list (rest forms) env))))

(defn compile-lambda
  ([args body env]
     (compile-lambda '%anonymous-lambda args body env))
  ([name args body env]
     ;; TODO: Need to invoke the assembler
     (vm/make-fn :env env :args args
                 :code (gen-seq (gen-args name args 0)
                                (compile-sequence
                                 body
                                 (conj env (vec args))
                                 true false)))))

;;; The Main Function
;;; =================

(defn compile-dispatch
  "Compute a dispatch value for the `compile` function"
  [form env val? more?]
  (cond
   (nil? form) ::nil
   (util/boolean? form) ::boolean
   (symbol? form) ::symbol
   (util/atomic? form) ::atom
   :else
   (case (first form)
     quote ::quote
     set! ::setter
     begin ::sequence
     if ::conditional
     lambda ::closure
     ::function-application)))

(defmulti compile
  "Compile Riley code into bytecode instructions for the Revue VM."
  compile-dispatch)

(defmethod compile ::nil [form env val? more?]
  (compile-const nil val? more?))

(defmethod compile ::boolean [form env val? more?]
  (compile-const form val? more?))

(defmethod compile ::symbol [var env val? more?]
  (if val?
    (gen-seq (gen-var var env)
             (when-not more? (gen-return)))
    ()))

(defmethod compile ::atom [atom env val? more?]
  (compile-const atom val? more?))

(defmethod compile ::quote [form env val? more?]
  (arg-count form 1)
  (compile-const form val? more?))

(defmethod compile ::setter [[_ var val :as form] env val? more?]
  (arg-count form 2)
  (assert (symbol? var)
          (str "Only symbols can be set!, not " var))
  (binding [*current-form* form]
    (gen-seq
     (compile val env true true)
     (gen-set var env form)
     (when-not val? (gen 'POP form))
     (when-not more? (gen-return)))))

(defmethod compile ::sequence [form env val? more?]
  (binding [*current-form* form]
    (compile-sequence (rest form) env val? more?)))

(defmethod compile ::conditional
  [[_ pred then & [else] :as form] env val? more?]
  (arg-count form 2 3)
  (binding [*current-form* form]
    (cond
     ;; (if nil x y) or (if false x y) => y
     (not pred)
     (compile else env val? more?)
     ;; (if <truthy> x y) => x
     (util/constant? pred)
     (compile then env val? more?)
     ;; TODO: Fold primitives here...
     :else
     (let [pcode (compile pred env true true)
           tcode (compile then env val? more?)
           ecode (compile else env val? more?)]
       (cond
        ;; (if p x x) => (begin p x)
        (= tcode ecode)
        (gen-seq (compile pred env false true) ecode)
        ;; TODO: Some other optimizations...
        :else
        (let [L1 (gen-label)
              L2 (if more? (gen-label))]
          (gen-seq pcode
                   (gen 'FJUMP L1)
                   tcode
                   (when more? (gen 'JUMP L2))
                   (list L1)
                   ecode
                   (if more? (list L2)))))))))

(defmethod compile ::closure
  [[_ args & body :as form] env val? more?]
  (binding [*current-form* form
            *current-function* form]
    (let [f (compile-lambda args body env)]
      (gen-seq (gen 'FN f)
               (when-not more? (gen-return))))))

(defmethod compile ::function-application
  [[f & args :as form] env val? more?]
  (binding [*current-form* form]
    (let [prim (vm/primitive? f env (count args))]
      (cond
       ;; Invoking a primitive function
       prim
       (if (and (not val?) (not (:side-effects prim)))
         (compile-sequence args env false more?)
         (gen-seq (compile-list args env)
                  (when-not val? (gen 'POP))
                  (when-not more? (gen-return))))
       ;; ((lambda () body)) => (begin body)
       ;; NOTE: lambda is hardcoded here!
       (and (sequential? f) (= (first f) 'lambda) (empty? (second f)))
       (do
         (assert (empty? args)
                 "Calling parameterless function with arguments.")
         (compile-sequence (drop 2 f) env val? more?))
       ;; Further work: save continuation point
       more?
       (let [K (gen-label 'K)]
         (gen-seq (gen 'SAVE K)
                  (compile-list args env)
                  (compile f env true true)
                  (gen 'CALLJ (count args))
                  (list K)
                  (when-not val? (gen 'POP))))
       ;; Function call as rename plus goto
       :else
       (gen-seq (compile-list args env)
                (compile f env true true)
                (gen 'CALLJ (count args)))))))

;;; Evaluate this (e.g., with C-x C-e in Cider) to run the tests for
;;; this namespace:
;;; (clojure.test/run-tests 'revue.riley-test)
;;; Evaluate this to run the test for all namespaces:
;;; (clojure.test/run-all-tests #"^revue\..*-test")

