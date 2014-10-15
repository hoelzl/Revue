(ns revue.riley
  "A compiler from a simple Lisp/Scheme/Clojure-like language to the
  Revue VM."
  (:refer-clojure :exclude (comp compile macroexpand macroexpand-1))
  (:require [revue.util :as util
             :refer [pprint #+cljs nthrest env env-value]]
            [revue.vm :as vm]))

(defn warn [msg]
  (util/warn "Compiler Warning:" msg))


;;; Some Utilities for the Compiler
;;; ===============================

(defn arg-count
  "Check that `form` is a call with between `min` and `max` arguments.
  If `max` is one of `:inf`, `:infty` (for all you LaTeX users out
  there), `:infinity`, or `:infinite` then check that `form` has at
  least `min` elements.  If only `min` is provided, then check that
  `form` is a call with exactly `min` arguments."
  ([form min]
     (arg-count form min min))
  ([form min max]
     (assert (and (list? form) (not (empty? form)))
             (str form " is not a call."))
     (let [n-args (count (rest form))]
       (if (contains? #{:inf :infty :infinity :infinite} max)
         (assert (<= min n-args)
                 (str "Wrong number of arguments for "
                      (first form) ": " n-args " supplied, "
                      "at least " min " expected."))
         (assert (<= min n-args max)
                 (str "Wrong number of arguments for "
                      (first form) ": " n-args " supplied, "
                      min
                      (if (> max min) (str " to " max) "")
                      " expected."))))))

(def ^:dynamic *current-function*
  "The name of the lexicaly enclosing function for the form we are
  currently compiling, if it has a name.  Otherwise the function form,
  so that we get some information for anonymous functions as well.
  The keyword `:%unknown-function` when no information is available.
  This should only happen for top-level forms."
  :%unknown-function)

(def ^:dynamic *current-form*
  "The form we are currently compiling.  This should contain
  source-location info but currently doesn't because it seems that the
  ClojureScript reader does not provide a way to easily get this
  information."
  nil)

;;; @MargDisable

;;; TODO: We probably should not use plain assert, since this does
;;; throw a type of error that is not compiler specific.  Instead we
;;; should define `compiler-assert` that throws a `CompilerError`
;;; instance, and we should provide more refined handling of these
;;; errors.

;;; @MargEnable

(defn gen [opcode & args]
  "Generate bytecode for a single instruction.  Checks with the VM
  implemntation whether `opcode` is a known bytecode and if the numbe
  of arguments in `args` is acceptable for this opcode.  Throws an
  error if either the opcode is unknown or the arity does not match.
  Otherwise returns a sequence containing a single instruction,
  annotated with the current form and the enclosing function (or the
  function itself, if we are compiling a lambda)."
  (let [opcode-descr (get vm/opcodes opcode)]
    (assert opcode-descr
            (str "Unknown bytecode instruction: " opcode))
    (cond (= (:arity opcode-descr) (count args))
          (let [result (assoc (apply (:constructor opcode-descr) args)
                         :source *current-form*
                         :function *current-function*)]
            [(if (util/debugging?)
               ;; Add an `opcode` attribute for debugging purposes,
               ;; since the Clojure pretty-printer doesn't print the
               ;; type of records.
               (assoc result :opcode opcode)
               result)])
          :else
          (util/error "Bad arity for bytecode instruction " opcode))))

;;; @MargDisable

;;; TODO: Maybe we should revise `gen-seq` so that it calls `gen` on
;;; instructions that are encoded as plain lists?  This would make
;;; bytecode generation somewhat nicer; and it should not be
;;; particularly complicated since we can distinguish compiled
;;; instructions by the protocols they implement.

;;; @MargEnable

(defn gen-seq [& insts]
  "Generate a sequence of instructions.  Each member of `insts` must
  be a call to `gen` or `gen-seq` (or another function that returns a
  sequence of instructions)."
  (vec (apply concat insts)))

(def label-counter
  "Counter for the number that will be appended to the next label
  generated by `gen-label`.  (Actually `(inc label-counter)` will be
  appended to the symbol prefix.)  Incremented by `gen-label`; can be
  safely reset to 0 between compilations of top-level function."
  (atom 0))

(defn gen-label
  "Generate a new label with the specified prefix, or with prefix `L`
  if no prefix is supplied."
  ([]
     (gen-label "L"))
  ([prefix]
     (vm/->Label (symbol (str prefix (swap! label-counter inc))))))

(defn gen-var [var env]
  "Generate a read access for variable `var` in lexical environment
  `env`.  If `var` appears in `env` then a read access to its slot in
  the local environment is generated, otherwise a global reference is
  generated."
  (binding [*current-form* var]
    (let [pos (util/in-env? env var)]
      (if pos
        (gen 'LVAR (first pos) (second pos) var)
        (gen 'GVAR var)))))

(defn gen-set [var env form]
  "Generate a write access for variable `var` ind lexical environment
  `env`.  If `var` appears in `env` then a write to its slot in the
  local environment is generated, otherwise a write access to the
  global environment is generated."
  (binding [*current-form* form]
    (let [pos (util/in-env? env var)]
      (if pos
        (gen 'LSET (first pos) (second pos) var)
        ;; TODO: Protect immutable bindings
        (gen 'GSET var)))))

(defn gen-args [name args]
  "Returns either the `ARGS` or `ARGS*` instruction, depending on
  whether the arglist `args` contains a rest parameter or not.  Since
  the Clojure reader cannot process dotted lists we follow the Clojure
  convention and use an apersand `&` as marker for a rest parameter in
  the arglist.  The value of the `name` parameter is stored in the
  `ARGS` or `ARGS*` instruction since filtering for these instructions
  is a good way to find the entry point for functions in traces, and
  having the name of the function available in the instruction makes
  it easier to get the required info."
  (loop [args args
         n-so-far 0]
    (cond
     ;; Reached the end of the arglist.  Fixed arg call.
     (empty? args)
     (gen 'ARGS n-so-far name)
     ;; Found & in the arglist.  Vararg call
     (= '& (first args))
     (do
       (assert (and (util/singleton? (rest args))
                    (symbol? (first (rest args))))
               "& must be followed by a single rest arg.")
       (gen 'ARGS* n-so-far name))
     ;; Non-empty arglist, symbol in the first position.
     (and (sequential? args) (symbol? (first args)))
     (recur (rest args) (inc n-so-far))
     ;; Something's wrong...
     :default
     (util/error "Illegal argument list."))))

(defn gen-return []
  "Generate a return instruction, passing the name of the current
  function as argument."
  (gen 'RETURN *current-function*))

;;; The compilation function sometimes compile subexpressions,
;;; therefore we need a forward declaration for the main internal
;;; entry point to the compiler, `comp`.  Note that we use the prefix
;;; `comp` for all internal functions of the compiler and therefore
;;; shadow the `comp` function from Clojure in this file.  However,
;;; this allows us to use `compile` (also shadowed from Clojure) as
;;; the main user visible function.

(declare comp)

(defn comp-const [c val? more?]
  "Compile access to a constant.  The parameter `c` is passed in as
  normal Clojure value and converted to VM representation by the
  bytecode interpreter.  If the value is not needed, we simply return
  an empty sequence, since constant references can have no side
  effects.  Otherwise we generate a `CONST` instruction and, if the
  constant reference is in tail position, a `RETURN` instruction."
  (if val?
    (gen-seq (gen 'CONST c)
             (when-not more? (gen-return)))
    []))

(defn comp-sequence [forms env val? more?]
  "Compile a sequence of instructions.  If the list of forms is empty,
  compile the constant `nil`.  If the list of forms contains a single
  form compile this form using the provided values of `val?` and
  `more?`.  Otherwise compile the first form with `val?` set to
  `false` and `more?` set to `true`, since the value is ignored and
  there are more forms to compile."
  (cond
   ;; An empty sequence evaluates to `nil`.
   (empty? forms)
   (comp-const nil val? more?)
   ;; A sequence containing a single form is equivalent to this form.
   (util/singleton? forms)
   (comp (first forms) env val? more?)
   ;; If we have more than one form in the sequence we compile the
   ;; first one for effect and recurse on the rest.  This means that,
   ;; e.g., constants appearing in the sequence will not generate any
   ;; code.
   :else
   (gen-seq (comp (first forms) env false true)
            (comp-sequence (rest forms) env val? more?))))

(defn comp-parameters [forms env]
  "Compile each member of `forms` and push the result on the stack.
  In contrast to `comp-sequence` each parameter is evaluated for
  value."
  (if (empty? forms)
    ()
    (gen-seq (comp (first forms) env true true)
             (comp-parameters (rest forms) env))))

(defn comp-lambda
  "Compile a lambda abstraction.  This is done by compiling the body
  of the function as sequence in a new environment augmented with the
  arguments of the function.  If a `name` is supplied it is used as
  name of the function for debugging purposes, but it is not bound as
  a variable."
  ([args body env]
     (comp-lambda '%anonymous-lambda args body env))
  ([name args body env]
     (binding [*current-function* name]
       (let [body-env (conj env (vec args))]
         (vm/make-fun :env env :args args
                      :body-env body-env
                      :code (gen-seq (gen-args name args)
                                     (comp-sequence
                                      body
                                      body-env
                                      true false)))))))

;;; Support for macros
;;; ==================

;;; We don't want to implement many forms that are directly
;;; manipulated by the compiler.  Therefore we rely on macros to
;;; implement most of the syntax of the language.

(def macro-environment
  "The macro enironment holds the compile-time bindings of macros,
  i.e., function that take a source code form and an environment and
  return a new source code form to be compiled.  We currently only
  support global macros; therefore we only need a flat, global
  environment for macros."
  (atom {}))

(defn riley-macro? [op env]
  "Returns `true` if `op` has a macro binding in `env`.  Since we
  don't support lexical macros for the time being, we ignore the
  environment and just check whether the name is bound in
  `macro-environment`."
  (get @macro-environment op false))

(defn expand-riley-macro [op args env]
  "Expand the macro `op` when given arguments `args`.  This may only
  be called if `op` is known to be a macro."
  (let [expander (get @macro-environment op)]
    (assert expander (str op " has no macro expander."))
    (expander args env)))

(defn macroexpand-1
  "Macroexpand `form` once if it is a macro, or return it unchanged
  if it is not."
  ([form]
     (macroexpand-1 form (util/env)))
  ([[op & args :as form] env]
     (if (riley-macro? op env)
       (expand-riley-macro op args env)
       form)))

(defn macroexpand
  "Repeatedly macroexpand `form` by calling `macroexpand-1` until its
  outermost operator is no longer a macro.  Subexpressions may still
  be macros, i.e., `macroexpand` is not a tree walker."
  ([form]
     (macroexpand form (util/env)))
  ([form env]
     (let [result (macroexpand-1 form env)]
       (if (riley-macro? (first result) env)
         (recur result env)
         result))))

(defn define-riley-macro [name expander]
  "Define a new macro by giving its name and an expander function that
  takes a source form and an environment."
  (swap! macro-environment assoc name expander))

;;; Expand a top-level `define` form by expanding either in a simple
;;; `set!` if the defined value is a symbol, or by building a new
;;; lambda-term if the defined value is a list.

(define-riley-macro 'define
  (fn [[name & forms :as definition] env]
    (if (symbol? name)
      (do
        (assert (util/singleton? forms)
                (str "Definition for " name " is not a single value."))
        (list 'begin
              (list 'set! name (first forms))
              (list 'quote name)))
      (let [fun-name (first name)
            args (rest name)]
        (assert (symbol? fun-name)
                (str "Cannot define a function named " fun-name "."))
        (assert (every? symbol args)
                (str "Parameters of function definition "
                     fun-name " must all be symbols."))
        (list 'begin
              (list 'set! fun-name
                    (list 'lambda fun-name args
                          (util/maybe-add 'begin forms nil)))
              (list 'quote fun-name))))))

;;; `letrec` binds name to values, similar to `let`, however all
;;; values in a `letrec` are in scope of all variables.  This is
;;; implemented by first setting the value of all variables to `nil`
;;; and then assigning the correct values inside the contour
;;; established inside the variable bindings.

(define-riley-macro 'letrec
  (fn [[bindings & body] env]
    (list* 'let (map #(list (first %) '(box nil)) bindings)
           (concat (map #(list 'set! (first %) (second %)) bindings)
                   body))))

;;; `let` is the most commonly used binding form. Normal `let`s are
;;; easily translated into lambdas; named let forms that are very
;;; convenient for loops are translated into `letrec`s.

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

;;; `when` is a simple wrapper around `if` and `begin` for the
;;; positive case only.

(define-riley-macro 'when
  (fn [[test & body] env]
    (list 'if test
          (util/maybe-add 'begin body nil)
          nil)))

(define-riley-macro 'when-not
  (fn [[test & body] env]
    (list 'if (if (and (seq? test) (= (first test) 'not))
                (second test)
                (list 'not test))
          (util/maybe-add 'begin body nil)
          nil)))

(define-riley-macro 'unless
  (fn [[test & body] env]
    (list* 'when-not test body)))

;;; The Main Function
;;; =================

(defn comp-dispatch
  "Compute a dispatch value for the `comp` function"
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

(defmulti comp
  "Compile Riley code into bytecode instructions for the Revue VM."
  comp-dispatch)

(defmethod comp ::nil [form env val? more?]
  (comp-const nil val? more?))

(defmethod comp ::boolean [form env val? more?]
  (comp-const form val? more?))

(defmethod comp ::symbol [var env val? more?]
  (if val?
    (gen-seq (gen-var var env)
             (when-not more? (gen-return)))
    ()))

(defmethod comp ::atom [atom env val? more?]
  (comp-const atom val? more?))

(defmethod comp ::quote [form env val? more?]
  (arg-count form 1)
  (comp-const (nth form 1) val? more?))

(defmethod comp ::setter [[_ var val :as form] env val? more?]
  (arg-count form 2)
  (assert (symbol? var)
          (str "Only symbols can be set!, not " var))
  (binding [*current-form* form]
    (gen-seq
     (comp val env true true)
     (gen-set var env form)
     (when-not val? (gen 'POP))
     (when-not more? (gen-return)))))

(defmethod comp ::sequence [form env val? more?]
  (binding [*current-form* form]
    (comp-sequence (rest form) env val? more?)))

(defmethod comp ::conditional
  [[_ pred then & [else] :as form] env val? more?]
  (arg-count form 2 3)
  (binding [*current-form* form]
    (cond
     ;; (if nil x y) or (if false x y) => y
     (not pred)
     (comp else env val? more?)
     ;; (if <truthy> x y) => x
     (util/constant? pred)
     (comp then env val? more?)
     ;; TODO: Fold primitives here...
     :else
     (let [pcode (comp pred env true true)
           tcode (comp then env val? more?)
           ecode (comp else env val? more?)]
       (cond
        ;; (if p x x) => (begin p x)
        (= tcode ecode)
        (gen-seq (comp pred env false true) ecode)
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

(defmethod comp ::closure
  [[_ args-or-name & body-or-args-and-body :as form] env val? more?]
  (let [[name args body] (if (symbol? args-or-name)
                           [args-or-name
                            (first body-or-args-and-body)
                            (rest body-or-args-and-body)]
                           ['%anonymous-lambda
                            args-or-name
                            body-or-args-and-body])]
    (binding [*current-form* form]
      (let [f (comp-lambda name args body env)]
        (gen-seq (gen 'FUN f)
                 (when-not more? (gen-return)))))))

(defmethod comp ::macro-application
  [[f & args :as form] env val? more?]
  (binding [*current-form* form]
    (comp (expand-riley-macro f args env) env val? more?)))

(defmethod comp ::function-application
  [[f & args :as form] env val? more?]
  (binding [*current-form* form]
    (let [prim (vm/primitive? f env (count args))
          op (vm/operator? f env)]
      (cond
       ;; Invoking a primitive function
       prim
       (if (and (not val?) (not (:side-effects? prim)))
         (comp-sequence args env false more?)
         (gen-seq (comp-parameters args env)
                  (gen 'PRIM (:name  prim) (count args))
                  (when-not val? (gen 'POP))
                  (when-not more? (gen-return))))
       ;; Invoking an operator
       op
       (gen-seq (comp-parameters args env)
                (gen 'OP f (count args))
                (when-not val? (gen 'POP))
                (when-not more? (gen-return)))
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
         (comp-sequence (drop 2 f) env val? more?))
       ;; We have to do more work after calling the function: save a
       ;; continuation point
       more?
       (let [K (gen-label 'K)]
         (gen-seq (gen 'SAVE K)
                  (comp-parameters args env)
                  (comp f env true true)
                  (gen 'CALLJ (count args))
                  (list K)
                  (when-not val? (gen 'POP))))
       ;; Function call as rename plus goto
       :else
       (gen-seq (comp-parameters args env)
                (comp f env true true)
                (gen 'CALLJ (count args)))))))

(defn compile [form & {:keys [env assemble?]
                        :or {env (util/env) assemble? true}}]
  (reset! label-counter 0)
  (try
    (let [fun (comp-lambda 'top-level () (list form) env)]
      (if assemble?
        (vm/assemble fun)
        fun))
    (catch #+clj java.lang.Exception #+cljs js/Error e
           :compiler-error)))

(defn compile-all [forms & {:keys [env assemble?]
                            :or {env (util/env) assemble? true}}]
  (try
    (let [fun (comp-lambda 'top-level () forms env)]
      (if assemble?
        (vm/assemble fun)
        fun))
    (catch #+clj java.lang.Exception #+cljs js/Error e
           :compiler-error)))

(defn compile-str [string & {:keys [env assemble?]
                             :or {env (util/env) assemble? true}}]
  (try
    (let [fun (comp-lambda
               'top-level ()
               (util/read-program-from-string string) env)]
      (if assemble?
        (vm/assemble fun)
        fun))
    (catch #+clj java.lang.Exception #+cljs js/Error e
           :compiler-error)))

(defn comp-show [form & {:keys [env assemble?]
                         :or {env (util/env) assemble? false}}]
  (vm/show (:code (compile form :env env :assemble? assemble?)))
  nil)

(defn comp-show* [& forms]
  (vm/show (:code (compile-all forms)))
  nil)

(defn comp-show-all [form & {:keys [env assemble?]
                             :or {env (util/env) assemble? false}}]
  (let [result (compile-all form :env env :assemble? assemble?)]
    (vm/show (:code result)))
  nil)

(defn comp-show-str [string & {:keys [env assemble?]
                             :or {env (util/env) assemble? false}}]
  (let [result (compile-str string :env env :assemble? assemble?)]
    (vm/show (:code result)))
  nil)

(defn run [& forms]
  (let [prog (compile-all forms :assemble? true)]
    (vm/vm prog)))

(defn run-str [string]
  (let [prog (compile-str string :assemble? true)]
    (vm/vm prog)))

(defn result [& forms]
  (vm/result (apply run forms)))

(defn result-str [string]
  (vm/result (run-str string)))

#_
(result-str
 "(define (fact n) (if (<= n 1) 1 (* n (fact (- n 1))))) (fact 1000N)")

#_
(result
 '(define (fact n)
    (let loop ((result 1) (n n))
         (if (<= n 1)
           result
           (loop (* n result) (- n 1)))))
 '(fact 10))

#_
(comp-show
 '(define (swap! vec i j)
    (let ((temp (vector-ref vec i)))
      (vector-set! vec i (vector-ref vec j))
      (vector-set! vec j temp))
    vec)
 :assemble? true)

#_
(comp-show
 '(define (bubblesort vec)
    (let loop ((swapped? false))
         (let loop2 ((i 1))
              (if (< i (vector-length vec))
                (begin
                 (when (> (vector-ref vec (- i 1))
                          (vector-ref vec i))
                   (swap! vec (- i 1) i)
                   (set! swapped? true))
                 (loop2 (+ i 1)))))
         (when swapped? (loop false)))
    vec)
 :assemble? true)

#_
(result
 '(define (swap! vec i j)
    (let ((temp (vector-ref vec i)))
      (vector-set! vec i (vector-ref vec j))
      (vector-set! vec j temp))
    vec)
 '(define v (vector 3 1 2))
 '(println v)
 ;;'(swap! v 0 1)
 ;;'(println "v = " v)
 ;;'(swap! v 1 2)
 ;;'(println "v = " v)
 'v)


#_
(result
 '(define (swap! vec i j)
    (let ((temp (vector-ref vec i)))
      (vector-set! vec i (vector-ref vec j))
      (vector-set! vec j temp))
    vec)
 '(define (bubblesort vec)
    (let loop ((swapped? false))
         (let loop2 ((i 1))
              (if (< i (vector-length vec))
                (begin
                 (when (> (vector-ref vec (- i 1))
                          (vector-ref vec i))
                   (swap! vec (- i 1) i)
                   (set! swapped? true))
                 (loop2 (+ i 1)))))
         (when swapped? (loop false)))
    vec)
 '(bubblesort (vector 3 1 2)))

;;; Evaluate this (e.g., with C-x C-e in Cider) to run the tests for
;;; this namespace:
;;; (clojure.test/run-tests 'revue.riley-test)
;;; Evaluate this to run the test for all namespaces:
;;; (clojure.test/run-all-tests #"^revue\..*-test")

