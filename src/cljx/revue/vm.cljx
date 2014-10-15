 (ns revue.vm
  "The virtual machine for the Revue system."
  (:require [revue.util :as util
             :refer [pprint #+cljs nthrest env env-value]]
            [revue.mem :as mem]))


;;; Utilities
;;; =========

(defn warn
  "Warn about a problem encountered by the virtual machine."
  [msg]
  (util/warn "VM Warning:" msg))

;;; Local Environments
;;; ==================

;;; Local environments are defined in `util.cljx`.

;;; The Global Environment
;;; ======================

(defn make-global-env
  "Create a hash table and store that represent `table` in VM data
  format."
  [& [table]]
  (loop [ks (keys table)
         vs (vals table)
         result {}
         store []]
    (if ks
      (let [[v new-store] (mem/->vm (first vs) store)]
        (recur (next ks) (next vs) (assoc result (first ks) v) new-store))
      [result store])))


;;; Functions and Return Addresses
;;; ==============================

;;; Byte-code interpreted functions and return addresses are defined
;;; as plain Clojure maps, with a `:type` attribute to simplify
;;; debugging.

(defn make-fun
  "`make-fun` creates a new bytecode-interpreted function.  `code` is
  a sequence of VM instructions, `env` is initially the compilation
  environment in which the function is defined, `body-env` is the
  compilation environment of the function body (i.e., the initial
  value of `env` extended with the `args`), `name` is a function name
  for debugging purposes and `args` is the argument list of the
  function.  When a closure is built from the function, `env` is
  overwritten with the run-time environment for the closure."
  [& {:keys [code env body-env name args]}]
  {:type :bytecode-function
   :code code
   :env env
   :body-env body-env
   :name name
   :args args})

;;; Since the VM works on a function-by-function basis, return
;;; addresses are structured objects that say to which function we
;;; should return, as well as the PC inside that function and the
;;; environment that was current at the time we left the body of
;;; `fun`.

(defn make-return-address
  "Create a new return address that jumps to step `pc` in `fun`."
  [{:keys [fun pc env]}]
  {:type :return-address
   :fun fun
   :pc pc
   :env env})


;;; The VM State
;;; ============

(defn initial-state
  "Create a new state for a VM, initially executing function `f`"
  [f]
  (let [[global-env store] (make-global-env)]
    {:type :vm-state
     :fun f
     :pc 0
     :global-env global-env
     :env (env)
     :stack ()
     :store store
     :n-args 0
     :stopped? false}))

;;; Next we define some functions to access elements of the VM state,
;;; and some functions that will be useful in filtering traces.

(defn code [vm-state]
  "Return the code (i.e., the sequence of bytecode instructions) of
  the function the VM is executing in `vm-state`."
  (:code (:fun vm-state)))

(defn current-instruction [vm-state]
  "Return the instruction that is active in `vm-state`.  Note that
  when the `step` function is called, the program counter has already
  been advanced to the next position, so during the dynamic extent of
  `step` this function returns the *next* instruction that will be
  executed.  However, it should never be necessary to call
  `current-instruction` while a `step` function is running, since the
  current instruction is available as `this` argument of `-step`."
  (get (code vm-state) (:pc vm-state)))

(declare opcode)

(defn current-instruction-is [vm-state desired-opcode]
  "Returns `true` if the current instruction in `vm-state` has opcode
  `desired-opcode`, `false` otherwise."
  (= (opcode (current-instruction vm-state)) desired-opcode))

;;; Operators
;;; =========

;;; Operators are functions that are implemented in Clojure.

(def operator-table
  "A table containing all operators defined for the VM."
  (atom {}))

(defn operator?
  "Returns a truthy value if `op` is an operator defined either
  globally or in `env`, `false` otherwise.  Currently there is no way
  to define local operators, so the `env` argument is superfluous."
  ([op env]
     (get @operator-table op false))
  ([op]
     (operator? op nil)))

;;; @MargDisable
;;; TODO: We should wrap operators into a data structure, similarly to
;;; primitives.
;;; @MargEnable

(defn define-operator
  "Defines operator `name` that takes either exactly `n-args` or
  between `min-args` and `max-args` arguments.  When the operator is
  invoked, `code` is executed.  The arguments passed to `code` are
  already converted to Clojure format; the result of `code` should
  also be a Clojure value (not in VM representation).  If an operation
  requires access to the memory subsystem it should be implemented as
  primitive instead."
  ([name n-args code]
     (define-operator name n-args n-args code))
  ([name min-args max-args code]
     (swap! operator-table assoc name {:clj-fun code
                                       :min-args min-args
                                       :max-args max-args})))

;;; Some predefined operators:

(define-operator '+ 0 :inf +)
(define-operator '- 0 :inf -)
(define-operator '* 0 :inf *)
(define-operator '/ 1 :inf /)
(define-operator '< 2 :inf <)
(define-operator '<= 2 :inf <=)
(define-operator '> 2 :inf >)
(define-operator '>= 2 :inf >=)
(define-operator 'print 0 :inf print)
(define-operator 'println 0 :inf println)

;;; VM Primitives
;;; =============

;;; Primitives are functions that operate on the raw VM memory
;;; representation, i.e., they receive their arguments as raw VM
;;; values.  The first argument of a primitive is always a store.
;;; This means that they can allocate and modify
;;; memory, and also that they are responsible for returning a
;;; correctly updated store.

(def primitive-table
  "A table containing all primitives defined for the VM."
  (atom {}))

(defn make-prim
  "Create a new primitive instruction."
  [name n-args clj-fun constant-value? constant-value side-effects?]
  {:type :primitive-instruction
   ;; The name of this primitive
   :name name
   ;; The number of arguments this primitive accepts, or `nil` if the
   ;; primitive takes an arbitrary number of arguments
   :n-args n-args
   ;; The Clojure function executed for this primitive
   :clj-fun clj-fun
   ;; Truthy if the value returned by the primitive is always the same
   :constant-value? constant-value
   ;; The constant value, if `:constant-value?` is truthy
   :constant-value constant-value
   ;; Truthy if the primitive has side effects
   :side-effects? side-effects?})

(defn primitive?
  "Returns `prim` if `prim` is the name of a primitive defined either
  in `env` or locally that can be invoked with `n-args` arguments.  If
  `n-args` is nil the primitive can be invoked with an arbitrary
  number of arguments.  Currently there is no support for primitives
  with a variable but bounded number of arguments, and also not for
  locally defined primitives."
  [prim env n-args]
  (let [prim-desc (get @primitive-table prim false)]
    (if (and prim-desc (or (not (:n-args prim-desc))
                           (= (:n-args prim-desc) n-args)))
      prim-desc
      false)))

(defn define-primitive
  "Define primitive `name` that takes exactly `n-args` arguments and
  evaluates `code` when invoked."
  [name n-args code & {:keys [constant-value? constant-value side-effects?]
                       :or {constant-value? false
                            constant-value nil
                            side-effects? true}}]
  (swap! primitive-table assoc name
         (make-prim name n-args code
                    constant-value? constant-value side-effects?)))

(define-primitive 'box 1
  (fn [store arg]
    (mem/new-box store arg)))

(define-primitive 'vector nil
  (fn [store & args]
    (mem/new-vector store args)))

(define-primitive 'make-vector 1
  (fn [store n-elts]
    (mem/new-vector store (repeat n-elts nil))))

(define-primitive 'vector-length 1
  (fn [store v]
    [(mem/vector-length store v) store]))

(define-primitive 'vector-ref 2
  (fn [store v i]
    [(mem/vector-ref store v i) store]))

(define-primitive 'vector-set! 3
  (fn [store v i new-value]
    [nil (mem/vector-set! store v i new-value)]))

;;; The Assembler (Well, Not Yet)
;;; =============================

;;; We need to have the VM instructions available for the assembler,
;;; thus we define the `assemble` and `assemble-inst` functions after
;;; the VM instructions. However, for some instructions it is
;;; convenient to have these functions available (e.g., for `CC`), so
;;; we provide a forward declaration here.

(declare assemble assemble-inst)

;;; VM Instructions
;;; ===============

;;; The function `->seq` converts an instruction into a sequence.  We
;;; need it in the definition of `-args` for functions.

(declare ->seq)

(defprotocol VmInst
  "Each instruction of the VM is represented as a datatype that
  implements the `VmInst` protocol.  The function `-step` executes the
  instruction starting in state `vm-state` and returns a new state of
  the VM."
  (-step [this vm-state]))

;;; The variable `*indent*` is dynamically bound to determine the
;;; indentation of nested instructions.

(def ^:dynamic *indent* 1)

(defprotocol VmShow
  "Implemented by instructions and labels, that want to override the
  default behavior of just printing the instruction preceeded by the
  current indentation."
  (-show [this indent newline]))

(extend-type #+clj Object #+cljs object
  VmShow
  (-show [this indent newline]
    (if (sequential? this)
      (doseq [inst this] (-show inst indent newline))
      (println (str (util/indent indent) this
                    ;; Add an additional newline for the benefit of
                    ;; the JavaScript console
                    (if newline "\n" ""))))))

(defprotocol VmSeq
  "Implemented by instructions and labels so that they can be
  converted to sequences.  The function `-opcode` returns the opcode
  of the instruction and `-args` returns the arguments (as keywords or
  functions that extract a usable form of the arguments)."
  (-opcode [this])
  (-args [this]))

(defprotocol VmAssemble
  "Protocol that allows instructions to specify how they want to be
  assembled."
  (-assemble [this]))

(defprotocol ResolveLabel
  "Implemented by instructions that take labels as arguments and need
  to resolve them during assembly.  `-resolve-label` returns the
  instruction `this`, with all label arguments resolved according to
  `label-indices`."
  (-resolve-label [this label-indices]))

(defprotocol VmLabel
  "Marker protocol for labels.")

(defrecord Label [name]
  Object
  (toString [this]
    (str (:name this) ":"))
  VmLabel
  VmShow
  (-show [this indent newline]
    (print (str this))))

;;; `LVAR` pushes the value of the local variable in environment
;;; location `[frame slot]` onto the stack.  `source` is the source
;;; code expression that was compiled into this instruction which is
;;; also the name of the variable in the environment.
(defrecord LVAR [frame slot name]
  Object
  (toString [this]
    (str (-opcode this) " "
         (:frame this) " " (:slot this) " ; " (:name this)))
  VmInst
  (-step [this vm-state]
    (let [raw-value (env-value vm-state this)
          value (if (mem/box? raw-value)
                  (mem/box-ref (:store vm-state) raw-value)
                  raw-value)]
      (assoc vm-state
        :stack (cons value (:stack vm-state)))))
  VmSeq
  (-opcode [this]
    'LVAR)
  (-args [this]
    ((juxt :frame :slot :name) this)))

;;; `LSET` stores to topmost value on the stack into environment
;;; location `[frame slot]`.  `name` is the name of the variable in
;;; the environment, `source` is the source expression that was
;;; compiled into this instruction.  `LSET` does not modify the stack.
(defrecord LSET [frame slot name]
  Object
  (toString [this]
    (str (-opcode this) " "
         (:frame this) " " (:slot this) " ; " (:name this)))
  VmInst
  (-step [this vm-state]
    (let [{:keys [env stack store]} vm-state
          {:keys [frame slot]} this
          current-value (env-value vm-state {:frame frame :slot slot})
          [new-store new-value] (if (mem/box? current-value)
                                  (do
                                    (assert (not (mem/box? (first stack)))
                                            "Cannot box boxes!")
                                    [(mem/box-set! store current-value (first stack)) current-value])
                                  [store (first stack)])]
      (assoc vm-state
        :env (assoc-in env [frame slot] new-value)
        :store new-store)))
  VmSeq
  (-opcode [this]
    'LSET)
  (-args [this]
    ((juxt :frame :slot :name) this)))

;;; `GVAR` pushes the value of the global variable `name` onto the
;;; stack.  If `name` is not defined in the global environment, `nil`
;;; is pushed onto the stack.
(defrecord GVAR [name]
  Object
  (toString [this]
    (str (-opcode this) " " (:name this)))
  VmInst
  (-step [this vm-state]
    (assoc vm-state
      :stack (cons (get (:global-env vm-state) (:name this))
                    (:stack vm-state))))
  VmSeq
  (-opcode [this]
    'GVAR)
  (-args [this]
    [(:name this)]))

;;; `GSET` stores the topmost value on the stack in global variable
;;; `name`.  It does not modify the stack.
(defrecord GSET [name]
  Object
  (toString [this]
    (str (-opcode this) " " (:name this)))
  VmInst
  (-step [this vm-state]
    (assoc vm-state
      :global-env (assoc (:global-env vm-state) name
                         (first (:stack vm-state)))))
  VmSeq
  (-opcode [this]
    'GSET)
  (-args [this]
    [(:name this)]))

;;; `POP` pops a value off the stack and discards it.
(defrecord POP []
  Object
  (toString [this]
    (str (-opcode this)))
  VmInst
  (-step [this vm-state]
    (assert (not (empty? (:stack vm-state)))
            "Cannot pop an empty stack.")
    (update-in vm-state [:stack] rest))
  VmSeq
  (-opcode [this]
    'POP)
  (-args [this]
    []))

;;; `CONST` pushes a constant value onto the stack.  The `value`
;;; parameter is a Clojure value that is converted to VM
;;; representation by `CONST`.
(defrecord CONST [value]
  Object
  (toString [this]
    (str (-opcode this) " " (if (nil? (:value this)) "nil" (:value this))))
  VmInst
  (-step [this vm-state]
    (let [value (:value this)
          {:keys [stack store]} vm-state
          [value new-store] (mem/->vm value store)]
      (assoc vm-state
        :stack (cons value stack)
        :store new-store)))
  VmSeq
  (-opcode [this]
    'CONST)
  (-args [this]
    [(:value this)]))

(defn --resolve-label [this label-indices]
  (let [target (:target this)]
    (if-let [label-name (:name target)]
      (let [addr (get label-indices label-name)]
        (assert addr (str "No address for label " (:name (:target this))
                          " in " label-indices))
        (assoc this :target addr))
      ;; The target address should already be resolved
      (do
        (assert (integer? target) (str "Invalid jump target:" target))
        this))))

(defn target-str [target]
  (if (number? target)
    target
    (:name target)))

;;; `JUMP` jumps unconditially to `target` which is the index of the
;;; pc in the current function (a non-negative integer).
(defrecord JUMP [target]
  Object
  (toString [this]
    (str (-opcode this) " " (target-str (:target this))))
  VmInst
  (-step [this vm-state]
    (assoc vm-state :pc (:target this)))
  VmSeq
  (-opcode [this]
    'JUMP)
  (-args [this]
    [(-> this :target target-str)])
  ResolveLabel
  (-resolve-label [this label-indices]
    (--resolve-label this label-indices)))

;;; `FJUMP` pops a value off the stack and jumps to `target` if the
;;; value is falsy.
(defrecord FJUMP [target]
  Object
  (toString [this]
    (str (-opcode this) " " (target-str (:target this))))
  VmInst
  (-step [this vm-state]
    (let [stack (:stack vm-state)
          value (first stack)]
      (if (not (mem/->clojure value))
        (assoc vm-state
          :pc (:target this)
          :stack (rest stack))
        (assoc vm-state
          :stack (rest stack)))))
  VmSeq
  (-opcode [this]
    'FJUMP)
  (-args [this]
    [(-> this :target target-str)])
  ResolveLabel
  (-resolve-label [this label-indices]
    (--resolve-label this label-indices)))

;;; `TJUMP` pops a value off the stack and jumps to `target` when the
;;; value is truthy.
(defrecord TJUMP [target]
  Object
  (toString [this]
    (str (-opcode this) " " (target-str (:target this))))
  VmInst
  (-step [this vm-state]
    (let [stack (:stack vm-state)
          value (first stack)]
      (if (mem/->clojure value)
        (assoc vm-state
          :pc (:target this)
          :stack (rest stack))
        (assoc vm-state
          :stack (rest stack)))))
  VmSeq
  (-opcode [this]
    'TJUMP)
  (-args [this]
    [(-> this :target target-str)])
  ResolveLabel
  (-resolve-label [this label-indices]
    (--resolve-label this label-indices)))

;;; `SAVE` creates a return address from the current function, program
;;; counter and environment and pushes it on the stack.  Note that
;;; since we increment the PC before executing the instruction (in the
;;; function `step` defined below), the return address points to the
;;; instruction *after* the currently executing one, which is of
;;; course what we want.
(defrecord SAVE [target]
  Object
  (toString [this]
    (str (-opcode this) " " (target-str (:target this))))
  VmInst
  (-step [this vm-state]
    (let [ret-addr (make-return-address (assoc vm-state :pc (:target this)))]
      (assoc vm-state :stack (cons ret-addr (:stack vm-state)))))
  VmSeq
  (-opcode [this]
    'SAVE)
  (-args [this]
    [(-> this :target target-str)])
  ResolveLabel
  (-resolve-label [this label-indices]
    (--resolve-label this label-indices)))

;;; `RETURN` returns from a function.  To this end it removes the
;;; *second* element from the stack (which is the return address to
;;; which we should jump), but leaves the topmost element (which is
;;; the return value) on the stack.
(defrecord RETURN [fun]
  Object
  (toString [this]
    (str (-opcode this) " " (:fun this)))
  VmInst
  (-step [this vm-state]
    (let [stack (:stack vm-state)
          return-address (second stack)
          fun (:fun return-address)]
      (cond
       (= (:type return-address) :return-address)
       (assoc vm-state
         ;; Ensure that the stack remains a list.  Use `apply list`
         ;; instead of `list*` since the latter returns `nil` for an
         ;; empty arglist while the former returns `()`.
         :stack (cons (first stack) (apply list (drop 2 stack)))
         :fun fun
         :env (:env return-address)
         :pc (:pc return-address))
       (= (:fun this) 'top-level)
       (assoc vm-state
         :stopped? true
         :reason "Returning to top level.")
       :else
        (assoc vm-state
          :stopped? true
          :reason (str "Returning to invalid address.")))))
  VmSeq
  (-opcode [this]
    'RETURN)
  (-args [this]
    [(:fun this)]))

;;; `CALLJ` calls a function by jumping to its entry point.  To this
;;; end, it pops the function that should be invoked from the stack
;;; and builds a new state that starts the execution of the new
;;; function at the beginning.
(defrecord CALLJ [n-args]
  Object
  (toString [this]
    (str (-opcode this) " " (:n-args this)))
  VmInst
  (-step [this vm-state]
    (let [[fun & new-stack] (:stack vm-state)]
      (if fun
        (assoc vm-state
         ;; Ensure that the stack remains a list.  Use `apply list`
         ;; instead of `list*` since the latter returns `nil` for an
         ;; empty arglist while the former returns `()`.
          :stack (apply list new-stack)
          :fun fun
          :env (:env fun)
          :pc 0
          :n-args (:n-args this))
        (assoc vm-state
          :stopped? true
          :reason "Calling undefined function."))))
  VmSeq
  (-opcode [this]
    'CALLJ)
  (-args [this]
    [(:n-args this)]))

(defn move-args-from-stack-to-env
  "If `n-rest-args` is falsy, pop `n-args` arguments from the stack
  and put them in a newly created environment frame.  This function
  assumes that the function arguments were pushed onto the stack in
  reverse order.  If `n-rest-args` is truthy it must be an integer, in
  that case `n-args` + `n-rest-args` arguments are popped from the
  stack; the topmost `n-rest-args` on the stack are collected into a
  vector in VM format and put in the (`n-args` + 1)st slot in the new
  environment frame; the remaining `n-args` arguments are put in
  individual slots in the newly created environment frame."
  [n-args vm-state & [n-rest-args]]
  (let [stack (:stack vm-state)
        store (:store vm-state)
        [n-args* tmp-stack new-store]
        (if-not n-rest-args
          [n-args stack store]
          (let [[clj-rest-arg tmp-stack-2] (split-at n-rest-args stack)
                [rest-arg tmp-store] (mem/->vm (vec (reverse clj-rest-arg))
                                               store)]
            [(inc n-args)
             (cons rest-arg (apply list tmp-stack-2))
             tmp-store]))
        [reverse-frame tmp-stack-3] (split-at n-args* tmp-stack)
        [new-frame new-stack] [(vec (reverse reverse-frame))
                               (apply list tmp-stack-3)]]
    (assert (number? n-args*)
            "New argument count is not a number.")
    (assert (vector? new-frame)
            "Did not create a new environment frame vector.")
    (assert (list? new-stack)
            "Did not create a new stack as list.")
    (assoc vm-state
      :env (conj (:env vm-state) new-frame)
      :stack new-stack
      :store new-store)))

;;; `ARGS` is the first bytecode instruction executed by functions
;;; with fixed arity.  It checks that the arity of the currently
;;; executing function matches the number of arguments that were
;;; passed to this call.  If this is the case, it pops `n-args` values
;;; of the stack and puts them into a new environment frame.
;;; Otherwise it stops the computation with an error message.
(defrecord ARGS [n-args name]
  Object
  (toString [this]
    (str (-opcode this)
         " " (:n-args this)
         " " (:name this)))
  VmInst
  (-step [this vm-state]
    (let [{:keys [n-args name]} this]
      (if-not (= n-args (:n-args vm-state))
        (assoc vm-state
          :stopped? true
          :reason (str "Function " name " called with " (:n-args vm-state)
                       " argument(s), but wants exactly " n-args "."))
        (move-args-from-stack-to-env n-args vm-state))))
  VmSeq
  (-opcode [this]
    'ARGS)
  (-args [this]
    ((juxt :n-args :name) this)))

;;; `ARGS*` is the first bytecode instruction executed by functions
;;; with variable arity.  It checks that the number of required
;;; arguments of the currently executing function is less than the
;;; number of arguments that were passed to this call.  If this is the
;;; case, it pops `n-args` values of the stack and puts them into a
;;; new environment frame.  Otherwise it stops the computation with an
;;; error message.
(defrecord ARGS* [n-args name]
  Object
  (toString [this]
    (str (-opcode this)
         " " (:n-args this)
         " " (:name this)))
  VmInst
  (-step [this vm-state]
    (let [{:keys [n-args name]} this
          supplied-args (:n-args vm-state)]
      (if-not (<= n-args supplied-args)
        (assoc vm-state
          :stopped? true
          :reason (str "Function " name " called with " supplied-args
                       " argument(s), but wants at least " n-args "."))
        (move-args-from-stack-to-env
         n-args vm-state (- supplied-args n-args)))))
  VmSeq
  (-opcode [this]
    'ARGS*)
  (-args [this]
    ((juxt :n-args :name) this)))

;;; `FUN` creates a new closure.  It takes the bytecode-compiled
;;; `function` and associates it with the current environment.
(defrecord FUN [fun]
  Object
  ;; This is kind of wrong, since the indentation should be
  ;; dynamically determined so that the function body lines up
  ;; correctly when printing it in different situations.  However,
  ;; until ClojureScript provides a pretty printer this will have to
  ;; do.
  (toString [this]
    (str (-opcode this) " " ((-args this) this)))
  VmInst
  (-step [this vm-state]
    (assoc vm-state :stack
           (cons (assoc fun :env (:env vm-state))
                 (:stack vm-state))))
  VmAssemble
  (-assemble [this]
    (assoc this :fun (assemble (:fun this))))
  VmSeq
  (-opcode [this]
    'FUN)
  (-args [this]
    [(->> this :fun :code (map ->seq))])
  VmShow
  (-show [this indent newline]
    (-show (-opcode this) indent newline)
    (binding [*indent* (inc indent)]
      (-show (:code (:fun this)) (inc indent) newline))))

;;; `PRIM` invokes a primitive instruction.  Primitives are
;;; implemented as Clojure functions that receive the store as first
;;; argument, the raw VM values as other arguments, and they have to
;;; return a result that satisfies the `StoredData` protocol and a new
;;; store.  Primitives are useful for functions that need low-level
;;; access to the memory system; most other functions are better
;;; implemented as operators (see `OP` below).

(defrecord PRIM [name n-args]
  Object
  (toString [this]
    (str (-opcode this) " " (:name this) " " (:n-args this)))
  VmInst
  (-step [this vm-state]
    (let [{:keys [stack]} vm-state
          prim-descr (get @primitive-table (:name this))
          _ (assert prim-descr (str "No primitive `" (:name this) "`."))
          _ (when (:n-args prim-descr)
              (assert (= n-args (:n-args prim-descr))
                      (str "Primitive `" (:name this)
                           "` called with wrong number of arguments: "
                           (:n-args prim-descr) " != " n-args)))
          [raw-args new-stack] (split-at n-args stack)
          [result new-store] (apply (:clj-fun prim-descr)
                                    (:store vm-state) (reverse raw-args))]
      (assoc vm-state
        :stack (cons result (apply list new-stack))
        :store new-store)))
  VmSeq
 (-opcode [this]
    'PRIM)
  (-args [this]
    ((juxt :name :n-args) this)))

;;; `SET-CC` pops a value off the stack and uses this value as the new
;;; continuation.
(defrecord SET-CC []
  Object
  (toString [this]
    (str (-opcode this)))
  VmInst
  (-step [this vm-state]
    (update-in vm-state [:stack] first))
  VmSeq
  (-opcode [this]
    'SET-CC)
  (-args [this]
    []))

;;; `CC` pushes a function onto the stack that captures the current
;;; environment and restores it when it is invoked.  TODO: improve
;;; explanation.
(defrecord CC []
  Object
  (toString [this]
    (str (-opcode this)))
  VmInst
  (-step [this vm-state]
    (assoc vm-state
      :stack (cons
              (make-fun :code (assemble '((ARGS 1 'CC "%built-in")
                                          (LVAR 1 0 stack)
                                          (SET-CC)
                                          (LVAR 0 0 fun)
                                          (RETURN "%built-in")))
                        :env (env (:stack vm-state))
                        :name '%cc
                        :args '[fun])
              (:stack vm-state))))
  VmSeq
  (-opcode [this]
    'CC)
  (-args [this]
    []))


;;; `HALT` stops the execution of the VM; if `step` is invoked after
;;; `HALT` was called it will return the same state indefinitely.
;;; This is done by adding a `stopped?` key to the state; `step`
;;; checks for this key and does not evaluate any instructions when it
;;; is found in the state.
(defrecord HALT []
  Object
  (toString [this]
    (str (-opcode this)))
  VmInst
  (-step [this vm-state]
    (assoc vm-state
      :stopped? true
      :reason "Program terminated."))
  VmSeq
  (-opcode [this]
    'HALT)
  (-args [this]
    []))

;;; `OP` invokes an operator defined in Clojure code.  These operators
;;; receive their arguments as Clojure data structures and return
;;; their result as Clojure data structure; `OP` performs the
;;; necessary conversions from and to the VM representation.
;;; TODO: argument checks, etc.
(defrecord OP [name n-args]
  Object
  (toString [this]
    (str (-opcode this) " "
         (:name this) " " (:n-args this)))
  VmInst
  (-step [this vm-state]
    (let [stack (:stack vm-state)
          [raw-args new-stack] (split-at n-args stack)
          args (mem/->clojure (vec (reverse raw-args)) (:store vm-state))
          clj-result (apply (:clj-fun (get @operator-table (:name this)))
                            args)
          [result new-store] (mem/->vm clj-result (:store vm-state))]
      (assoc vm-state
        :stack (cons result new-stack)
        :store new-store)))
  VmSeq
  (-opcode [this]
    'OP)
  (-args [this]
    ((juxt :name :n-args) this)))

(defn opcode [inst]
  "Return the opcode of `inst`."
  (-opcode inst))

(defn args [inst]
  "Return the arguments of `inst`."
  (-args inst))

(defn ->seq
  "Convert `inst` into a sequence that corresponds roughly to input
  that the assembler (i.e., `assemble-inst`) would convert to this
  instruction.  The correspondence is not exact, since information
  about source locations, containing function, etc. might be lost."
  [inst]
  (if (satisfies? VmLabel inst)
    (:name inst)
    (list* (opcode inst) (args inst))))

(defn step
  "Run a single step of the VM starting in `vm-state`.  If the VM is
  stopped, i.e., `(:stopped? vm-state)` is true, then return the state
  unchanged."
  [vm-state]
  (cond
   (:stopped? vm-state)
   vm-state
   :else
   (if-let [instr (current-instruction vm-state)]
     (-step instr (assoc vm-state
                    :pc (inc (:pc vm-state))
                    :instr instr))
     (assoc vm-state
       :stopped? true
       :reason "Trying to access invalid code address."))))


;;; Printing instructions
;;; =====================

;;; The function `show` displays code is a nicely formatted manner.
;;; It is a wrapper around `-show` that takes care of types that don't
;;; implement `VmShow`.  We need it here to debug errors in the
;;; selection of the current instruction.

(defn show
  "Print the assembly code for `thing` indented by `indent` tabs by
   calling `-show` on `thing`."
  [thing & [indent newline?]]
  (println "newline?:" newline?)
  (let [indent (or indent *indent*)]
    (-show thing indent newline?))
  nil)

;;; The Assembler
;;; =============

(def opcodes
  "A list of the opcodes implemented by the VM.  Each opcode specifies
  a constructor (that generates the bytecode instruction), an arity
  and whether the last argument of the constructor is a source
  location."
  {'LVAR    {:constructor ->LVAR   :arity 3}
   'LSET    {:constructor ->LSET   :arity 3}
   'GVAR    {:constructor ->GVAR   :arity 1}
   'GSET    {:constructor ->GSET   :arity 1}
   'POP     {:constructor ->POP    :arity 0}
   'CONST   {:constructor ->CONST  :arity 1}
   'JUMP    {:constructor ->JUMP   :arity 1}
   'FJUMP   {:constructor ->FJUMP  :arity 1}
   'TJUMP   {:constructor ->TJUMP  :arity 1}
   'SAVE    {:constructor ->SAVE   :arity 1}
   'RETURN  {:constructor ->RETURN :arity 1}
   'CALLJ   {:constructor ->CALLJ  :arity 1}
   'ARGS    {:constructor ->ARGS   :arity 2}
   'ARGS*   {:constructor ->ARGS*  :arity 2}
   'FUN     {:constructor ->FUN    :arity 1}
   'PRIM    {:constructor ->PRIM   :arity 2}
   'SET-CC  {:constructor ->SET-CC :arity 0}
   'CC      {:constructor ->CC     :arity 0}
   'HALT    {:constructor ->HALT   :arity 0}
   'OP      {:constructor ->OP     :arity 2}})

(defn assemble-inst [inst]
  (let [inst2
        (if (or (satisfies? VmInst inst) (satisfies? VmLabel inst))
          inst
          (do
            (let [[opcode & args] inst]
              (if-let [opcode-descr (get opcodes opcode)]
                (cond
                 ;; Operator arity and call arity match
                 (= (:arity opcode-descr) (count args))
                 (apply (:constructor opcode-descr) args)
                 ;; Wrong arity
                 :else
                 (util/error "Opcode " opcode " applied to " (count args)
                             " arguments, but wants " (:arity opcode-descr)))
                (util/error "Unknown opcode " opcode " in " inst ".")))))]
    (if (satisfies? VmAssemble inst2)
      ;; We have an instruction with internal bytecode; call assemble
      ;; recursively
      (-assemble inst2)
      ;; A normal instruction, we're done
      inst2)))

(defn find-label-indices [insts]
  (loop [index 0
         insts insts
         result {}]
    (if-let [inst (first insts)]
      (if (satisfies? VmLabel inst)
        (recur index (rest insts) (assoc result (:name inst) index))
        (recur (inc index) (rest insts) result))
      result)))

(defn resolve-label-indices [label-indices]
  (fn [inst]
    (cond
     (satisfies? ResolveLabel inst)
     (do
       (-resolve-label inst label-indices))
     (satisfies? VmLabel inst)
     nil
     :else
     inst)))

(defn assemble-bytecode [code]
  (let [pass-1 (map assemble-inst code)
        label-indices (find-label-indices pass-1)
        new-code (vec (keep (resolve-label-indices label-indices)
                            pass-1))]
    [new-code label-indices]))

(defn assemble
  "Assemble a sequence of instructions from Clojure lists into
  `VmInst` data structures."
  [{:keys [type code] :as bytecode}]
  (cond (= type :bytecode-function)
        (let [[assembled-code label-indices] (assemble-bytecode code)]
          (assoc bytecode
            :code assembled-code
            :label-indices label-indices))
        :else
        bytecode))

;;; The VM Proper
;;; =============

;;; The VM simply returns an infinite sequence that iterates the
;;; `step` function on an initial state generated from a bytecode
;;; program.  Some utility functions serve to extract interesting
;;; information, e.g., the result value.

(defn vm
  "Run the virtual machine on bytecode program `prog` and generate an
  infinite trace of the execution.  If the program terminates (i.e.,
  some frame in the trace contains the keyword `:stopped?` with a
  truthy value) all frames after the first `:stopped?` frame are
  identical."
  [prog]
  #_(println "Running the VM on " prog)
  (iterate step (initial-state prog)))

(defn active-frames [trace]
  "Return a sequence of all frames generated while the program is
  still running.  This function is non-lazy and will therefore loop
  for non-terinating programs."
  (take-while (complement :stopped?) trace))

(defn stopped-frame [trace]
  "Return the first frame for which `stopped?` is truthy.  This
  function is non-lazy and will therefore loop for non-terinating
  programs."
  (first (drop-while (complement :stopped?) trace)))

(defn result [trace]
  "Return the result value of the program that generated `trace`,
  i.e., the topmost stack value of the first `:stopped?` frame in the
  trace.  This function is non-lazy and will therefore loop for
  non-terinating programs."
  (let [frame (stopped-frame trace)]
    (mem/->clojure (first (:stack frame)) (:store frame))))


;;; Evaluate this (e.g., with C-x C-e in Cider) to run the tests for
;;; this namespace:
;;; (clojure.test/run-tests 'revue.vm-test)
;;; Evaluate this to run the test for all namespaces:
;;; (clojure.test/run-all-tests #"^revue\..*-test")
