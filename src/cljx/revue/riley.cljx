(ns revue.riley
  "A compiler from a simple Lisp/Scheme/Clojure-like language to the
  Revue VM."
  (:refer-clojure :exclude (compile))
  (:require [revue.util :as util
             :refer [pprint #+cljs nthrest env env-value]]
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
          (do
            [(assoc (apply (:constructor opcode-descr) args)
               :source (current-source)
               :function *current-function*)])
          :else
          (util/error "Bad arity for bytecode instruction " opcode))))

(defn gen-seq [& insts]
  (doall (apply concat insts)))

(def label-counter (atom 0))

(defn gen-label
  ([]
     (gen-label "L"))
  ([prefix]
     (vm/->Label (symbol (str prefix (swap! label-counter inc))))))

(defn gen-var [var env]
  (binding [*current-form* var]
    (let [pos (util/in-env? env var)]
      (if pos
        (gen 'LVAR (first pos) (second pos) var)
        (gen 'GVAR var)))))

(defn gen-set [var env form]
  (binding [*current-form* form]
    (let [pos (util/in-env? env var)]
      (if pos
        (gen 'LSET (first pos) (second pos) var)
        ;; TODO: Protect immutable bindings
        (gen 'GSET var)))))

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
     (binding [*current-function* name]
       (vm/make-fun :env env :args args
                    :code (gen-seq (gen-args name args 0)
                                   (compile-sequence
                                    body
                                    (conj env (vec args))
                                    true false))))))

;;; Support for macros
;;; ==================

(def macro-environment (atom {}))

(defn riley-macro? [op env]
  (get @macro-environment op false))

(defn expand-riley-macro [op args env]
  (let [expander (get @macro-environment op)]
    (assert expander (str op " has no macro expander."))
    (expander args env)))

(defn riley-macroexpand-1 [[op & args] & [env]]
  (expand-riley-macro op args env))

(defn riley-macroexpand [[op & args] & [env]]
  (let [result (expand-riley-macro op args env)]
    (if (riley-macro? (first result) env)
      (recur result env)
      result)))

(defn define-riley-macro [name expander]
  (swap! macro-environment assoc name expander))

(define-riley-macro 'define
  (fn [[name & forms] env]
    (if (symbol? name)
      (do
        (assert (util/singleton? forms))
        (list 'set! name (first forms)))
      (list 'set! (first name)
            (list 'lambda (first name) (rest name)
                  (util/maybe-add 'begin forms nil))))))

(define-riley-macro 'letrec
  (fn [[bindings & body] env]
    (list* 'let (map #(list (first %) nil) bindings)
           (list* 'begin
                  (map #(list 'set! (first %) (second %)) bindings))
           body)))

(define-riley-macro 'let
  (fn [[bindings & body] env]
    (if (symbol? bindings)
      (let [[name bindings body] [bindings (first body) (rest body)]]
        (list 'letrec (list (list name
                                  (list* 'lambda (map first bindings) body)))
              (list* name (map second bindings))))
      (list* (list* 'lambda 'let (map first bindings)
                    body)
             (map second bindings)))))

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
     (if (riley-macro? (first form) env)
       ::macro-application
       ::function-application))))

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
     (when-not val? (gen 'POP))
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
  [[_ args-or-name & body-or-args-and-body :as form] env val? more?]
  (let [[name args body] (if (symbol? args-or-name)
                           [args-or-name
                            (first body-or-args-and-body)
                            (rest body-or-args-and-body)]
                           ['%anonymous-lambda args-or-name body-or-args-and-body])]
    (binding [*current-form* form]
      (let [f (compile-lambda name args body env)]
        (gen-seq (gen 'FUN f)
                 (when-not more? (gen-return)))))))

(defmethod compile ::macro-application
  [[f & args :as form] env val? more?]
  (binding [*current-form* form]
    (compile (expand-riley-macro f args env) env val? more?)))

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
       #_
       (and (sequential? f) (= (first f) 'lambda)
            (or (and (sequential? (second f)) (empty? (second f)))
                (and (symbol? (second f) (empty? (nth f 3))))))
       #_
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

(defn compiler [form & {:keys [env val? more?]
                        :or {env (util/env) val? true more? false}}]
  (reset! label-counter 0)
  (try
    (compile form env val? more?)
    (catch #+clj java.lang.Exception #+cljs js/Error e
           :compiler-error)))

(defn compile-all [forms & {:keys [env val? more?]
                            :or {env (util/env) val? true more? false}}]
  (reset! label-counter 0)
  (try
    (map #(compile %1 env val? more?) forms)
    (catch #+clj java.lang.Exception #+cljs js/Error e
           :compiler-error)))

(defn comp-show [form & {:keys [env val? more?]
                         :or {env (util/env) val? true more? false}}]
  (vm/show (compiler form :env env :val? val? :more? more?)))

(defn comp-show-all [form & {:keys [env val? more?]
                             :or {env (util/env) val? true more? false}}]
  (let [results (compile-all form :env env :val? val? :more? more?)]
    (if (sequential? results)
      (map vm/show results)
      (println "Compiler error"))))

;;; Evaluate this (e.g., with C-x C-e in Cider) to run the tests for
;;; this namespace:
;;; (clojure.test/run-tests 'revue.riley-test)
;;; Evaluate this to run the test for all namespaces:
;;; (clojure.test/run-all-tests #"^revue\..*-test")

