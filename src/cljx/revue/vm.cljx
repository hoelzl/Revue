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

;;; The VM State
;;; ============

(defn make-return-address
  "Create a new return address that jumps to step `pc` in `fun`."
  [{:keys [fun pc env]}]
  {:type :return-address
   :fun fun
   :pc pc
   :env env})

(defn make-fun
  "Create a new bytecode-interpreted function."
  [& {:keys [code env name args]}]
  {:type :bytecode-function
   :code code
   :env env
   :name name
   :args args})

(defn initial-state
  "Create a new state for a VM, initially executing function `f`"
  [f]
  (let [[global-env store] (make-global-env)]
    {:type :vm-state
     :fun f
     :code (:code f)
     :pc 0
     :global-env global-env
     :env ()
     :stack ()
     :store store
     :n-args 0
     :stopped? false}))

;;; The Assembler (Well, Not Yet)
;;; =============================

;;; We need to have the VM instructions available for the assembler,
;;; thus we define the `assemble` function after the VM instructions.
;;; However, for some instructions it is convenient to have `assemble`
;;; available (e.g., for `CC`), so we provide a forward declaration
;;; here.

(declare assemble)

;;; VM Instructions
;;; ===============

(defn primitive? [form env n-args]
  ;;; TODO: Implement this!
  nil)

(defn make-prim
  "Create a new primitive instruction."
  [symbol n-args opcode always? side-effects?]
  {:type :primitive-instruction
   :symbol symbol
   :n-args n-args
   :opcode opcode
   :always? always?
   :side-effects? side-effects?})

(defprotocol VmInst
  "Each instruction of the VM is represented as a datatype that
  implements the `VmInst` protocol.  The function `-step` executes the
  instruction starting in state `vm-state` and returns a new state of
  the VM."
  (-step [this vm-state])
  (-opcode [this]))

;;; `LVAR` pushes the value of the local variable in environment
;;; location `[frame slot]` onto the stack.  `source` is the source
;;; code expression that was compiled into this instruction which is
;;; also the name of the variable in the environment.
(defrecord LVAR [frame slot]
  VmInst
  (-step [this vm-state]
    (assoc vm-state
      :stack (conj (:stack vm-state) (env-value vm-state this))))
  (-opcode [this]
    'LVAR))

;;; `LSET` pops a value off the stack and puts it into environment
;;; location `[frame slot]`.  `name` is the name of the variable in
;;; the environment, `source` is the source expression that was
;;; compiled into this instruction.  As a special case, if `slot` is
;;; equal to the size of the respective frame, this frame is extended
;;; by an additional slot.
(defrecord LSET [frame slot]
  VmInst
  (-step [this vm-state]
    (let [{:keys [env stack]} vm-state
          {:keys [frame slot]} this]
      (assoc vm-state
        :env (assoc-in env [frame slot] (peek stack))
        :stack (pop stack))))
  (-opcode [this]
    'LSET))

;;; `GVAR` pushes the value of the global variable `name` onto the
;;; stack.  If `name` is not defined in the global environment, `nil`
;;; is pushed onto the stack.
(defrecord GVAR [name]
  VmInst
  (-step [this vm-state]
    (assoc vm-state
      :stack (conj (:stack vm-state)
                   (get (:global-env vm-state) (:name this)))))
  (-opcode [this]
    'GVAR))

;;; `GSET` pops a value off the stack and stores it in global variable
;;; `name`.
(defrecord GSET [name]
  VmInst
  (-step [this vm-state]
    (assoc vm-state
      :global-env (assoc (:global-env vm-state) name (peek (:stack vm-state)))
      :stack (pop (:stack vm-state))))
  (-opcode [this]
    'GSET))

;;; `POP` pops a value off the stack and discards it.
(defrecord POP []
  VmInst
  (-step [this vm-state]
    (update-in vm-state [:stack] pop))
  (-opcode [this]
    'POP))

;;; `CONST` pushes a constant value onto the stack.  The `value`
;;; parameter is a Clojure value that is converted to VM
;;; representation by `CONST`.
(defrecord CONST [value]
  VmInst
  (-step [this vm-state]
    (let [value (:value this)
          {:keys [stack store]} vm-state
          [value new-store] (mem/->vm value store)]
      (assoc vm-state
        :stack (conj stack value)
        :store new-store)))
  (-opcode [this]
    'CONST))

;;; `JUMP` jumps unconditially to `target` which is the index of the
;;; pc in the current function (a non-negative integer).
(defrecord JUMP [target]
  VmInst
  (-step [this vm-state]
    (assoc vm-state :pc (:target this)))
  (-opcode [this]
    'JUMP))

;;; `FJUMP` pops a value off the stack and jumps to `target` if the
;;; value is falsy.
(defrecord FJUMP [target]
  VmInst
  (-step [this vm-state]
    (let [stack (:stack vm-state)
          value (peek stack)]
      (if (not (mem/->clojure value))
        (assoc vm-state
          :pc (:target this)
          :stack (pop stack))
        (assoc vm-state
          :stack (pop stack)))))
  (-opcode [this]
    'FJUMP))

;;; `TJUMP` pops a value off the stack and jumps to `target` when the
;;; value is truthy.
(defrecord TJUMP [target]
  VmInst
  (-step [this vm-state]
    (let [stack (:stack vm-state)
          value (peek stack)]
      (if (mem/->clojure value)
        (assoc vm-state
          :pc (:target this)
          :stack (pop stack))
        (assoc vm-state
          :stack (pop stack)))))
  (-opcode [this]
    'TJUMP))

;;; `SAVE` creates a return address from the current function, program
;;; counter and environment and pushes it on the stack.  Note that
;;; since we increment the PC before executing the instruction (in the
;;; function `step` defined below), the return address points to the
;;; instruction *after* the currently executing one, which is of
;;; course what we want.
(defrecord SAVE [target]
  VmInst
  (-step [this vm-state]
    (let [ret-addr (make-return-address (assoc vm-state :pc (:target this)))]
      (update-in vm-state [:stack] conj ret-addr)))
  (-opcode [this]
    'SAVE))

;;; `RETURN` returns from a function.  To this end it removes the
;;; *second* element from the stack (which is the return address to
;;; which we should jump), but leaves the topmost element (which is
;;; the return value) on the stack.
(defrecord RETURN [fun]
  VmInst
  (-step [this vm-state]
    (let [stack (:stack vm-state)
          return-address (second stack)
          fun (:fun return-address)]
      (assoc vm-state
        :stack (cons (first stack) (list* (drop 2 stack)))
        :fun fun
        :code (:code fun)
        :env (:env return-address)
        :pc (:pc return-address))))
  (-opcode [this]
    'RETURN))

;;; `CALLJ` calls a function by jumping to its entry point.  To this
;;; end, it pops the function that should be invoked from the stack
;;; and builds a new state that starts the execution of the new
;;; function at the beginning.
(defrecord CALLJ [n-args]
  VmInst
  (-step [this vm-state]
    (let [[fun & new-stack] (:stack vm-state)]
      (assoc vm-state
        :stack (vec new-stack)
        :fun fun
        :code (:code fun)
        :env (:env fun)
        :pc 0
        :n-args (:n-args this))))
  (-opcode [this]
    'CALLJ))

(defn move-args-from-stack-to-env
  "If `n-rest-args` is falsy, pop `n-args` arguments from the stack
  and put them in a newly created environment frame.  If `n-rest-args`
  is truthy it must be an integer, in that case `n-args` +
  `n-rest-args` arguments are popped from the stack; the first
  `n-args` are put in individual slots in the newly created
  environment frame, the last `n-rest-args` are collected into a
  vector and put in the (`n-args` + 1)st slot in the new environment
  frame."
  [n-args vm-state & [n-rest-args]]
  (let [stack (:stack vm-state)
        [tmp-frame tmp-stack] (split-at n-args stack)
        [new-frame new-stack] (if-not n-rest-args
                                [(vec tmp-frame) (apply list tmp-stack)]
                                (let [[rest-args tmp-stack-2] (split-at n-rest-args tmp-stack)]
                                  [(conj (vec tmp-frame) (vec rest-args))
                                   (apply list tmp-stack-2)]))]
    (assoc vm-state
      :env (conj (:env vm-state) new-frame)
      :stack new-stack)))

;;; `ARGS` is the first bytecode instruction executed by functions
;;; with fixed arity.  It checks that the arity of the currently
;;; executing function matches the number of arguments that were
;;; passed to this call.  If this is the case, it pops `n-args` values
;;; of the stack and puts them into a new environment frame.
;;; Otherwise it stops the computation with an error message.
(defrecord ARGS [n-args name]
  VmInst
  (-step [this vm-state]
    (let [{:keys [n-args name]} this]
      (if-not (= n-args (:n-args vm-state))
        (assoc vm-state
          :stopped? true
          :reason (str "Function " name " called with " (:n-args vm-state)
                       " argument(s), but wants exactly " n-args "."))
        (move-args-from-stack-to-env n-args vm-state))))
  (-opcode [this]
    'ARGS))

;;; `ARGS*` is the first bytecode instruction executed by functions
;;; with variable arity.  It checks that the number of required
;;; arguments of the currently executing function is less than the
;;; number of arguments that were passed to this call.  If this is the
;;; case, it pops `n-args` values of the stack and puts them into a
;;; new environment frame.  Otherwise it stops the computation with an
;;; error message.
(defrecord ARGS* [n-args name]
  VmInst
  (-step [this vm-state]
    (let [{:keys [n-args name]} this
          supplied-args (:n-args vm-state)]
      (if-not (<= n-args supplied-args)
        (assoc vm-state
          :stopped? true
          :reason (str "Function " name " called with " supplied-args
                       " argument(s), but wants at least " n-args "."))
        (move-args-from-stack-to-env n-args vm-state (- supplied-args n-args)))))
  (-opcode [this]
    'ARGS*))

;;; `FUN` creates a new closure.  It takes the bytecode-compiled
;;; `function` and associates it with the current environment.
(defrecord FUN [fun]
  VmInst
  (-step [this vm-state]
    (update-in vm-state [:stack] conj
               (assoc fun :env (:env vm-state))))
  (-opcode [this]
    'FUN))

;;; `PRIM` invokes a primitive instruction.  Primitives are
;;; implemented as Clojure functions that receive the store as first
;;; argument, the raw VM values as other arguments, and they have to
;;; return a result that satisfies the `StoredData` protocol and a new
;;; store.  No checking is performed by the VM whether the primitive
;;; was invoked with the correct number of arguments; therefore bad
;;; calls to primitives may crash the VM.  Primitives are useful for
;;; functions that need low-level access to the memory system; most
;;; functions are better implemented as operators (see `OP` below); in
;;; particular user-visible functionality should never be implemented
;;; as primitive.
(defrecord PRIM [clj-code]
  VmInst
  (-step [this vm-state]
    (let [{:keys [n-args stack]} vm-state
          [raw-args new-stack] (split-at n-args stack)
          [result new-store] (apply (:clj-code this) (:store vm-state) raw-args)]
      (assoc vm-state
        :stack (conj new-stack result)
        :store new-store)))
  (-opcode [this]
    'PRIM))

;;; `SET-CC` pops a value off the stack and uses this value as the new
;;; continuation.
(defrecord SET-CC []
  VmInst
  (-step [this vm-state]
    (update-in vm-state [:stack] peek))
  (-opcode [this]
    'SET-CC))

;;; `CC` pushes a function onto the stack that captures the current
;;; environment and restores it when it is invoked.  TODO: improve
;;; explanation.
(defrecord CC []
  VmInst
  (-step [this vm-state]
    (assoc vm-state
      :stack (make-fun :code (assemble '((ARGS 1 'CC "%built-in")
                                        (LVAR 1 0 stack)
                                        (SET-CC)
                                        (LVAR 0 0 fun)
                                        (RETURN "%built-in")))
                      :env (env (:stack vm-state))
                      :name '%cc
                      :args '[fun])))
  (-opcode [this]
    'CC))


;;; `HALT` stops the execution of the VM; if `step` is invoked after
;;; `HALT` was called it will return the same state indefinitely.
;;; This is done by adding a `stopped?` key to the state; `step`
;;; checks for this key and does not evaluate any instructions when it
;;; is found in the state.
(defrecord HALT []
  VmInst
  (-step [this vm-state]
    (assoc vm-state
      :stopped? true
      :reason "Program terminated."))
  (-opcode [this]
    'HALT))

;;; `OP` invokes an operator defined in Clojure code.  These operators
;;; receive their arguments as Clojure data structures and return
;;; their result as Clojure data structure; `OP` performs the
;;; necessary conversions from and to the VM representation.
;;; TODO: argument checks, etc.
(defrecord OP [n clj-code]
  VmInst
  (-step [this vm-state]
    (let [stack (:stack vm-state)
          [raw-args new-stack] (split-at n stack)
          args (mem/->clojure (vec raw-args))
          clj-result (apply (:clj-code this) args)
          [result new-store] (mem/->vm clj-result (:store vm-state))]
      (assoc vm-state
        :stack (conj (vec new-stack) result)
        :store new-store)))
  (-opcode [this]
    'OP))


(defn step
  "Run a single step of the VM starting in `vm-state`.  If the VM is
  stopped, i.e., `(:stopped? vm-state)` is true, then return the state
  unchanged."
  [vm-state]
  (if (:stopped? vm-state)
    vm-state
    (let [instr (nth (:code vm-state) (:pc vm-state))]
      (-step instr (assoc vm-state
                     :pc (inc (:pc vm-state))
                     :instr instr)))))


;;; The Assembler
;;; =============

(def opcodes
  "A list of the opcodes implemented by the VM.  Each opcode specifies
  a constructor (that generates the bytecode instruction), an arity
  and whether the last argument of the constructor is a source
  location."
  {'LVAR    {:constructor ->LVAR   :arity 2 :source true}
   'LSET    {:constructor ->LSET   :arity 2 :source true}
   'GVAR    {:constructor ->GVAR   :arity 1 :source false}
   'GSET    {:constructor ->GSET   :arity 1 :source true}
   'POP     {:constructor ->POP    :arity 0 :source true}
   'CONST   {:constructor ->CONST  :arity 1 :source true}
   'JUMP    {:constructor ->JUMP   :arity 1 :source true}
   'FJUMP   {:constructor ->FJUMP  :arity 1 :source true}
   'TJUMP   {:constructor ->TJUMP  :arity 1 :source true}
   'SAVE    {:constructor ->SAVE   :arity 1 :source true}
   'RETURN  {:constructor ->RETURN :arity 1 :source true}
   'CALLJ   {:constructor ->CALLJ  :arity 1 :source true}
   'ARGS    {:constructor ->ARGS   :arity 2 :source true}
   'ARGS*   {:constructor ->ARGS*  :arity 2 :source true}
   'FUN     {:constructor ->FUN    :arity 1 :source true}
   'PRIM    {:constructor ->PRIM   :arity 1 :source true}
   'SET-CC  {:constructor ->SET-CC :arity 0 :source false}
   'CC      {:constructor ->CC     :arity 0 :source true}
   'HALT    {:constructor ->HALT   :arity 0 :source true}
   'OP      {:constructor ->OP     :arity 2 :source true}})

(defn assemble-inst [inst]
  (if (satisfies? VmInst inst)
    inst
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
        (util/error "Unknown opcode " opcode " in " inst ".")))))

(defn assemble
  "Assemble a sequence of instructions from Clojure lists into
  `VmInst` data structures."
  [instructions]
  ;; TODO: Need to resolve labels for jumps
  (map assemble-inst instructions))

;;; The VM Proper
;;; =============


(defn vm
  "Run the virtual machine."
  [prog]
  (println "Running the VM on " prog))

;;; Evaluate this (e.g., with C-x C-e in Cider) to run the tests for
;;; this namespace:
;;; (clojure.test/run-tests 'revue.vm-test)
;;; Evaluate this to run the test for all namespaces:
;;; (clojure.test/run-all-tests #"^revue\..*-test")
