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

;;; We define a protocol that specifies how the interpreter handles
;;; procedures, and record types for representing interpreted
;;; procedures as well as primitive procedures.

;;; TODO Protocol

;;; An interpreted procedure.  Its `code' is the source code to be
;;; interpreted; `params' is a list of parameter names; `name' is the
;;; name of the procedure (as clojure symbol), or `nil' if the
;;; procedure is anonymous.
;;;
(defrecord Proc [code env params name])

;;; A primitive procedure.  Its `code' is a Clojure function that
;;; should be invoked.  The `name' and `params' fields are as for
;;; `Proc'
;;;
(defrecord Prim [code name params])

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
  ([form]
     (initial-state form (global-env)))
  ([form env]
     #_(->State form env (initial-store) nil nil)
     {:form form :env env :store (initial-store)
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
   (assoc state :form nil :value (get env form))
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
       :form (if value
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
       :form `(::eval-args ~(conj (nth form 1) value) ~@(nthrest form 2)))
     ::eval-proc
     (assoc state
       :form (nth form 1)
       :cont `((::apply ~value) ~@cont))
     ::apply
     ;; TODO: Need to handle primitive procedures; define protocol for
     ;; application
     (let [proc value
           [_ args] form]
       (assoc state
         :form (:code proc)
         :env (extend-env (:env proc) (:params proc) (nth form 1))
         :cont `((::reset-env ~env) ~@cont)))
     ::reset-env
     (assoc state
       :form nil
       :env (nth form 1))
     (let [[proc & args] form]
       (assoc state
         :form `(::eval-args [] ~@args)
         :cont `((::eval-proc ~proc) ~@cont)
         :value [])))))

(defn run-n-steps
  ([form]
     (run-n-steps form 100))
  ([form n]
     (clojure.pprint/pprint
      (take n (take-while
               (fn [{:keys [form cont value]}] (or form cont value))
               (iterate step (initial-state form)))))))

;;; Evaluate this (e.g., with C-x C-e in Cider) to run the tests for
;;; this namespace:
;;; (clojure.test/run-tests 'revue.interpreter-test)
;;; Evaluate this to run the test for all namespaces:
;;; (clojure.test/run-all-tests #"^revue\..*-test")
