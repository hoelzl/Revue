
(ns revue.interpreter
  "A continuation- and state-passing interpreter using the memory
  subsystem."
  (:require [revue.util :as util]
            [revue.mem :as mem]))

;;; This namespace contains an experimental interpreter that uses the
;;; memory subsystem, so that I can

;;; * try out the memory subsystem in a more realistic context than
;;;   the unit tests.
;;; * figure out how exactly the internal representatio used for
;;;   visualizatons and animations should look like; in particular how
;;;   continuations should be represented.

;;; The current implementations is a simple continuation-passing
;;; interpreter (that, obviously, also has to be a storage-passing
;;; interpreter).  Currently only variables defined with `define`
;;; should be destructively modified, function parameters and
;;; variables bound with `let` are not boxed; modifying them may lead
;;; to strange behavior if their values escape their lexical context.
;;; Function parameters are unboxed so that we don't have to allocate
;;; heap storage for every function call and are therefore properly
;;; tail recursive.  We could avoid this by pre-processing the source
;;; and performing a closure analysis pass. 


;;; Utilities
;;; =========

(defn warn
  "Warn about a problem encountered by the interpreter."
  [msg]
  (util/warn "Interpreter warning:" msg))

#+cljs
(defn nthrest
  "Take the `n`th rest of `coll`.  Missing from ClojureScript because
  of an oversight.  Can be removed once this is fixed in
  ClojureScript."
  [coll n]
  (if (zero? n)
    coll
    (recur (rest coll) (dec n))))

(defn pprint
  "The `clojure.pprint` namespace is not yet ported to ClojureScript.
  Define a workaround while this is the case, use `clojure.pprint`
  once the implementation is done."
  [obj]
  #+clj
  (clojure.pprint/pprint obj)
  #+cljs
  (println obj))

;;; Environments for the interpreter
;;; ================================

;;; The interpreter uses a simple Clojure map as environment.

;;; We define functions to create and update the global environment
;;; which are used to initialize the interpreter.  The interpreter
;;; only uses one non-standard function for manipulating environment:
;;; `extend-env` is called when a new lexical scope is entered and
;;; returns the previous environment extended with the new bindings.
;;; To update the environment we simply use `assoc`, to look up values
;;; we use `get` (or one of its alternatives).

(defn empty-env
  "Returns an empty environment."
  []
  {})

(def ^:dynamic *initial-bindings* (atom {}))

(defn clear-initial-bindings
  "Set the value of `*initial-bindings*` to the empty map."
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

;;; The protocol `IProc` specifies how the interpreter handles
;;; procedures.  We define one record type implementing that protocol
;;; for interpreted procedures and one for primitive procedures that
;;; are written in Clojure and executed in a single step.

(defprotocol IProc
  "Procedures that can be invoked by the interpreter"
  (apply-proc [this args state]
    "Apply the procedure to `args` and `state`, and return a new
    state"))

;;; An interpreted procedure.  Its `code` is the source code to be
;;; interpreted; `params` is a list of parameter names; `name` is the
;;; name of the procedure (as Clojure symbol), or `nil` if the
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

;;; A primitive procedure.  Its `code` is a Clojure function that
;;; should be invoked.  The `params` and `name` fields are as for
;;; `Proc`
;;;
(defrecord Prim [code params name]
  IProc
  (apply-proc [this args state]
    ((:code this) args state))
  mem/StoredData
  (-->clojure [this store]
    this))

(defn define-nary-global
  "A utility function that defines a primitive procedure that can take
  an arbitrary number of arguments and stores it in the global
  environment."
  [name fun]
  (define-global name
    (->Prim (fn [args state]
              (assoc state :form nil :value (apply fun args)))
            '[& args]
            name)))

(defn define-binary-global
  "A utility function that defines a primitive procedure that takes
  exactly two arguments and stores it in the global environment."
  [name fun]
  (define-global name
    (->Prim (fn [args state]
              (assoc state :form nil :value (apply fun args)))
            '[x y]
            name)))

;;; We can now define the primitive functions that should be in the
;;; default environment.
;;; 

(define-nary-global '+ +)
(define-nary-global '- -)
(define-nary-global '* *)
(define-nary-global '/ /)

(define-binary-global '< <)
(define-binary-global '> >)
(define-binary-global '<= <=)
(define-binary-global '>= >=)

;;; We handle functions with (non-memory) effects by pushing the
;;; effects onto the `:effects` slot of the state.  This allows the
;;; visualization to show the effects in any manner it wants.

(define-global 'print
  (->Prim
   (fn [args state]
     (assoc state
       :form nil
       :effects (conj (:effects state) `(:print ~args))))
   '[&args]
   'print))

(define-global 'println
  (->Prim
   (fn [args state]
     (assoc state
       :form nil
       :effects (conj (:effects state) `(:println ~args))))
   '[&args]
   'println))

;;; A simple state-passing interpreter
;;; ==================================

;;; The core of the simple interpreter is a function `step` that
;;; performs one step of the evaluation process.  It operates on an
;;; iterpreter state that contains all information required by the
;;; interpreter; i.e., its sole argument is an interpreter state and
;;; its result is again an interpreter state.

;;; The state contains the following elements:
;;;
;;; * the form to be executed
;;; * the environment for the form
;;; * the store
;;; * a continuation
;;; * the value returned by the previous evaluation step
;;; * the effects encountered so far 

;;; Not sure whether that is a good idea, since we want to share as
;;; much data as possible, and introducing a record will probably
;;; store each state in a fresh object.
;;; TODO: check this
;;;
#_(defrecord State [form env store cont value effects])

(defn initial-store []
  [])

(defn initial-state
  "Create an initial state for the interpreter"
  ([& forms]
     {:form (util/maybe-add 'begin forms)
      :env (global-env) :store (initial-store)
      :cont nil :value nil :effects []}))

(defn set-boxed-value [name value-form {:keys [env store cont] :as state} op-name]
  (let [[box new-store] (mem/new-box nil store)]
    (when-not (symbol? name)
      (util/error (str op-name ": first argument must be a symbol.")))
    (assoc state
      :form value-form
      :env (assoc env name box)
      :store new-store
      :cont `((::set! ~name ~box) ~@cont))))

;;; <!-- TODO: Refactor this into multi-methods -->

(defn step
  "Perform a single step of the interpreter and return a new state.

  Initially we pick off a few special cases:

  * If `form` is `nil` we check whether we have a non-empty
    continuation.  If we do, we continue with the first member of the
    continuation.  Otherwise we are done

  * If `form` is a symbol, we look up its value in the environment, 
    convert it into its denotation (i.e., the Clojure equivalent)
    and use that as the `:value` going forward.

  * If `form` is an atomic value, we leave it alone (since atomic
    values are always represented as their Clojure equivalent.

  * Otherwise we have a compound form.  The first thing we do in that
    case is to check whether it is a macro.  If it is, we expand it and
    return the expansion as new `:form` (not implemented yet).
    Otherwise, we check whether `:form` is a special operator.  If it is
    we evaluate it using a special rule.  If `form` is neither a macro
    nor a special operator it is a function call, and we evaluate the
    call."
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
     (set-boxed-value (nth form 1) (nth form 2) state "define")
     set!
     ;; TODO: add a check that the value already exists and is, in
     ;; fact, a box.
     (set-boxed-value (nth form 1) (nth form 2) state "set!")
     ::set!
     (assoc state
       :form nil
       :store (mem/box-set! (if (instance? Proc value)
                              (assoc value :name (nth form 1))
                              value)
                            (nth form 2) store))
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
     ;; The continuation function for the `if` operator
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
  "Run the interpreter for at most `*run-n-steps*` steps, pretty-print
  the trace and return the result."
  ([& forms]
     (let [result (take @*run-n-steps*
                        (take-while
                         (fn [{:keys [form cont value]}] (or form cont value))
                         (iterate step (apply initial-state forms))))]
       (pprint result)
       (last result))))

#_(def ^:dynamic *interp-steps* (atom 100000))

(defn interp
  "Run the interpreter to completion and return the result, i.e.,
  return a finite sequence containing all steps taken by the
  interpreter.  This function will only terminate for terminating
  algorithms, but it saves the user from checking whether the program
  has reached the end of its execution trace."
  ([& forms]
     (let [result (take-while
                   (fn [{:keys [form cont value]}] (or form cont value))
                   (iterate step (apply initial-state forms)))]
       result)))


;;; Functions for cleaning up the interpreter trace
;;; ===============================================

(defn recursively-remove-global-vars-in
  "Walk `d` and remove all global variables (that the function knows
  about).  Currently removes `:env` keys from maps and turns seqences
  starting with `::return-from-call` (which contain an environment if
  they were produced by the interpreter) into a keyword."
  [d]
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
   (sequential? d)
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

(def ^:dynamic *internal-ops* #{::eval-args ::collect-arg ::eval-proc})

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

(def cleanup-trace
  "Clean a trace by removing all global vars and internal forms."
  (comp remove-global-vars remove-internal-forms remove-continuations))

(defn pretty-interp
  "A wrapper around `interp` that removes all global variables from
  the trace and pretty-prints it."
  [& forms]
  (-> (apply interp forms) remove-global-vars pprint))

;;; Try the following examples:
(comment
  (:value (last (interp '(+ 1 2))))
  (pprint
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
  (pprint
   (cleanup-trace
    (interp '(define fact (lambda (n) (if (<= n 1) 1 (* (fact (- n 1)) n))))
            '(fact 4))))
  (:value (last (interp '(define fact (lambda (n) (if (<= n 1) 1 (* (fact (- n 1)) n))))
                        '(fact 4))))
  (:value (last (interp '(define fact (lambda (n) (if (<= n 1) 1 (* (fact (- n 1)) n))))
                        '(fact 1000N))))
  ;; You can even interpet non-terminating functions (just don't try
  ;; to print the whole trace:)
  (pprint
   (take 8
         (-> (interp '(define loop (lambda (n) (loop n))) '(loop 0))
             remove-global-vars
             remove-internal-forms)))
  )

;;; Evaluate this (e.g., with `C-x C-e` in Cider) to run the tests for
;;; this namespace:
(comment
  (clojure.test/run-tests 'revue.interpreter-test)
  )
;;; Evaluate this to run the test for all namespaces:
(comment
  (clojure.test/run-all-tests #"^revue\..*-test")
  )
