;;; Interpreters using the memory subsystem

;;; This file contains experimental interpreters that use the memory
;;; subsystem.  I write these interpreters to try the memory subsystem
;;; in a more realistic context than the unit tests.

;;; Currently I'm thinking of implementing a simple
;;; continuation-passing interpreter (that, obviously, also has to be
;;; a storage-passing interpreter).  I'm not sure whether to implement
;;; mutable variables in this interpreter, since this would mean that
;;; all parameters have to be boxed and passed on the heap.  And since
;;; we never release storage on the heap this would probably
;;; prohibitively wasteful.  In a compiler we can avoid this by
;;; performing a closure analysis pass.  Of course, the "interpreters"
;;; could also use a preprocessing pass that performs these kinds of
;;; analysis, but then the interpreters would mutate into compilers
;;; with a different IR.  That's not really the current plan, but
;;; we'll see how things work out.  --tc

(ns revue.interpreter
  (:require [revue.util :as util]
            [revue.mem :as mem]))

;;; Utilities
;;; =========

(defn warn
  "Warn about a problem encountered by an interpreter."
  [msg]
  (util/warn "Interpreter warning:" msg))

;;; Environments for the interpreter
;;; ================================

;;; The interpreters use a simple Clojure map as environment.

;;; We define function to create and update the global environment
;;; which are used to initialize the interpreter.  The interpreter
;;; only uses one non-standard function for manipulating environment:
;;; `extend-env' is called when a new lexical scope is entered and
;;; returns the previous environment extended with the new bindings.
;;; To update the environment we simply use `assoc', to look up values
;;; we use `get'.

(defn empty-env
  "Returns an empty environment."
  []
  {})

(def ^:dynamic *initial-bindings* (atom {}))

(defn clear-initial-bindings
  "Set the value of `*initial-bindings*' to the empty map."
  []
  (reset! *initial-bindings* {}))

(defn define-global
  "Defines a global variable, or redefines it if it already exists"
  [name value]
  (swap! *initial-bindings* assoc name value))

(defn global-env
  "Returns the global environment for the interpreter."
  []
  @*initial-bindings*)

(defn extend-env [env keys values]
  (merge env (zipmap keys values)))


;;; Procedures for the interpreter
;;; ===============================

;;; We define a protocol IProc that specifies how the interpreter
;;; handles procedures, and record types for representing interpreted
;;; procedures as well as primitive procedures.

(defprotocol IProc
  "Procedures that can be invoked by the interpreter"
  (apply-proc [this args state]
    "Apply the procedure to `args' and `state', and return a new
    state"))

;;; An interpreted procedure.  Its `code' is the source code to be
;;; interpreted; `params' is a list of parameter names; `name' is the
;;; name of the procedure (as clojure symbol), or `nil' if the
;;; procedure is anonymous.
;;;
(defrecord Proc [code env params name]
  IProc
  (apply-proc [this args state]
    (assoc state
      :form (:code this)
      :env (extend-env (:env this) (:params this) args)
      :cont `((::return-from-call ~(:env state)) ~@(:cont state))))
  mem/StoredData
  (-->clojure [this store]
    this))

;;; A primitive procedure.  Its `code' is a Clojure function that
;;; should be invoked.  The `params' and `name' fields are as for
;;; `Proc'
;;;
(defrecord Prim [code params name]
  IProc
  (apply-proc [this args state]
    ((:code this) args state))
  mem/StoredData
  (-->clojure [this store]
    this))

(defn define-nary-global [name fun]
  (define-global name
    (->Prim (fn [args state]
              (assoc state :form nil :value (apply fun args)))
            '[& args]
            name)))

(defn define-binary-global [name fun]
  (define-global name
    (->Prim (fn [args state]
              (assoc state :form nil :value (apply fun args)))
            '[x y]
            name)))

(define-nary-global '+ +)
(define-nary-global '- -)
(define-nary-global '* *)
(define-nary-global '/ /)

(define-binary-global '< <)
(define-binary-global '> >)
(define-binary-global '<= <=)
(define-binary-global '>= >=)

(define-nary-global 'print print)
(define-nary-global 'println println)

;;; A simple state-passing interpreter
;;; ==================================

;;; The core of the simple interpreter is a function `step' that
;;; performs one step of the evaluation process.  It operates on an
;;; iterpreter state that contains all information required by the
;;; interpreter; i.e., its sole argument is an interpreter state and
;;; its result is again an interpreter state.

;;; The state contains the following elements:
;;; * the form to be executed
;;; * the environment for the form
;;; * the store
;;; * a continuation
;;; * The value returned by the previous evaluation step

;;; Not sure whether that is a good idea, since we want to share as
;;; much data as possible, and introducing a record will probably
;;; store each state in a fresh object.
;;; TODO: check this
;;;
#_(defrecord State [form env store cont value])

(defn initial-store []
  [])

(defn initial-state
  "Create an initial state for the interpreter"
  ([& forms]
     {:form (util/maybe-add 'begin forms)
      :env (global-env) :store (initial-store)
      :cont nil :value nil}))

;;; TODO: Refactor this into multi-methods

;;; TODO: Should we clear the :value field for forms which have no
;;; return value on their own?

(defn step
  "Perform a single step of the interpreter and return a new state"
  [{:keys [form env store cont value] :as state}]
  (cond
   ;;
   (nil? form)
   (if cont
     (assoc state :form (first cont) :cont (next cont))
     (assoc state :value nil))
   ;;
   (symbol? form)
   (assoc state :form nil :value (mem/->clojure (get env form) store))
   ;;
   (util/atomic? form)
   (assoc state :form nil :value form)
   ;; TODO: integrate macros here...
   ;;
   :else
   (case (first form)
     ;; Quote
     quote
     (assoc state :form nil :value (rest form))
     ;; Definitions
     define
     (let [[box new-store] (mem/new-box nil store)]
       (assoc state
         :form (nth form 2)
         :env (assoc env (nth form 1) box)
         :store new-store
         :cont `((::define ~box) ~@cont)))
     ::define
     (assoc state
       :form nil
       :store (mem/box-set! value (nth form 1) store))
     ;; Sequence
     begin
     (cond
      ;; An empty begin evaluates to false.
      (empty? (rest form))
      (assoc state :form nil :value false)
      ;; A begin containing a single form is equivalent to that form.
      (util/singleton? (rest form))
      (assoc state :form (nth form 1))
      :else
      ;; We have a begin with at least two subforms.  Extract the
      ;; first subform, push the remaining forms onto the
      ;; continuation.
      (assoc state
        :form (nth form 1)
        :cont (cons (cons 'begin (nthrest form 2)) cont)))
     ;; If: Compute the condition and add a continuation that uses
     ;; this value to choose the correct branch.
     if
     (assoc state
       :form (nth form 1)
       :cont (cons (cons ::if (nthrest form 2)) cont))
     ;; The continuation function for the `if' operator
     ::if
     (assoc state
       :form (if (mem/->clojure value store)
               (nth form 1)
               (nth form 2)))
     ;; Function definition
     lambda
     (assoc state
       :form nil
       :value (->Proc (util/maybe-add 'begin (nthrest form 2))
                      env
                      (vec (nth form 1))
                      nil))
     ;; If we arrive here, we have a function application.  First we
     ;; need to pick off the continuation functons for function
     ;; applications, though.
     ::eval-args
     (if (empty? (nthrest form 2)) ;; TODO: Check that new form evaluates function?
       (assoc state
         :form (first cont)
         :cont (next cont)
         :value (nth form 1))
       (assoc state
         :form (nth form 2)
         :cont `((::collect-arg ~(nth form 1) ~@(nthrest form 3)) ~@cont)))
     ::collect-arg
     (assoc state
       :form `(::eval-args ~(conj (nth form 1) value)
                           ~@(nthrest form 2)))
     ::eval-proc
     (assoc state
       :form (nth form 1)
       :cont `((::apply ~value) ~@cont))
     ::apply
     ;; TODO: Need to handle primitive procedures; define protocol for
     ;; application
     (let [proc value
           [_ args] form]
       (apply-proc proc args state))
     ::return-from-call
     (assoc state
       :form nil
       :env (nth form 1))
     (let [[proc & args] form]
       (assoc state
         :form `(::eval-args [] ~@args)
         :cont `((::eval-proc ~proc) ~@cont)
         :value [])))))

(def ^:dynamic *run-n-steps* (atom 100))

(defn run-n-steps
  ([& forms]
     (let [result (take @*run-n-steps*
                        (take-while
                         (fn [{:keys [form cont value]}] (or form cont value))
                         (iterate step (apply initial-state forms))))]
       (clojure.pprint/pprint result)
       (last result))))

(def ^:dynamic *interp-steps* (atom 100000))

(defn interp
  ([& forms]
     (let [result (take @*interp-steps*
                        (take-while
                         (fn [{:keys [form cont value]}] (or form cont value))
                         (iterate step (apply initial-state forms))))]
       result)))


;;; Functions for cleaning up the interpreter trace
;;; ===============================================

(defn recursively-remove-global-vars-in [d]
  (cond
   (map? d)
   (into {}
         (map (fn [[k v]]
                (if (= k :env)
                  (let [genv (global-env)]
                    [k (into {} (keep (fn [[k1 v1]]
                                        (if (contains? genv k1)
                                          nil
                                          [k1 (recursively-remove-global-vars-in v1)]))
                                      v))])
                  [k (recursively-remove-global-vars-in v)]))
              d))
   (or (sequential? d))
   (if (= (first d) ::return-from-call)
     ::return-from-call
     (map recursively-remove-global-vars-in d))
   :else
   d))

(defn remove-global-vars
  "Removes all values from environments in a seq of states that are
  defined in the global environment.  Does not take into account redefinitions!"
  [states]
  (map recursively-remove-global-vars-in states))

(def *internal-ops* #{::eval-args ::collect-arg ::eval-proc})

(defn remove-internal-forms
  "Removes all forms that don't evaluate user code."
  [states]
  (keep (fn [{:keys [form] :as state}]
          (if (or (nil? form)
                  (and (sequential? form) (contains? *internal-ops* (first form))))
            nil
            state))
        states))

(defn remove-continuations
  "Removes all continuations from states."
  [states]
  (map #(dissoc %1 :cont) states))

(def cleanup-trace (comp remove-global-vars remove-internal-forms remove-continuations))


;;; Try the following examples:
(comment
  (:value (last (interp '(+ 1 2))))
  (clojure.pprint/pprint
   (cleanup-trace
    (interp '((lambda (f n) (if (<= n 1) n (* n (f f (- n 1)))))
              (lambda (f n) (if (<= n 1) n (* n (f f (- n 1))))) 3))))
  (:value (last (interp '((lambda (f n) (if (<= n 1) n (* n (f f (- n 1)))))
                          (lambda (f n) (if (<= n 1) n (* n (f f (- n 1))))) 10))))
  (:value (last (interp '((lambda (f n) (if (<= n 1) n (* n (f f (- n 1)))))
                          (lambda (f n) (if (<= n 1) n (* n (f f (- n 1))))) 1000N))))
  (:value (last (interp '(define x (+ 1 2))
                        '(println x)
                        '((lambda () (define x 2) (println x)))
                        '(println x)
                        'x)))
  (:value (last (interp '(define f (lambda (n) n))
                        '(f 1))))
  (:value (last (interp '(define f (lambda (n) (if (>= n 1) f 1)))
                        '(f 1))))
  (clojure.pprint/pprint
   (cleanup-trace
    (interp '(define fact (lambda (n) (if (<= n 1) 1 (* (fact (- n 1)) n))))
            '(fact 4))))
  (:value (last (interp '(define fact (lambda (n) (if (<= n 1) 1 (* (fact (- n 1)) n))))
                        '(fact 4))))
  (:value (last (interp '(define fact (lambda (n) (if (<= n 1) 1 (* (fact (- n 1)) n))))
                        '(fact 1000N))))
  )

;;; Evaluate this (e.g., with C-x C-e in Cider) to run the tests for
;;; this namespace:
;;; (clojure.test/run-tests 'revue.interpreter-test)
;;; Evaluate this to run the test for all namespaces:
;;; (clojure.test/run-all-tests #"^revue\..*-test")
