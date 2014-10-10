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

(defn code [vm-state]
  (:code vm-state))


(defn current-instruction [vm-state]
  (get (code vm-state) (:pc vm-state)))

;;; Operators
;;; =========

;;; Operators are functions that are implemented in Clojure.

(def operator-table (atom {}))

(defn operator? [op env]
  (get @operator-table op false))

(defn define-operator
  ([name n-args code]
     (define-operator name n-args n-args code))
  ([name min-args max-args code]
     (swap! operator-table assoc name {:clj-fun code
                                       :min-args min-args
                                       :max-args max-args})))

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

;;; The function `show` displays code is a nicely formatted manner.
;;; It is a wrapper around `-show` that takes care of types that don't
;;; implement `VmShow`.  We need it here to debug errors in the
;;; selection of the current instruction.

(declare show)

;;; The function `->seq` converts an instruction into a sequence.  We
;;; need it in the definition of `-args` for functions.

(declare ->seq)

(defprotocol VmInst
  "Each instruction of the VM is represented as a datatype that
  implements the `VmInst` protocol.  The function `-step` executes the
  instruction starting in state `vm-state` and returns a new state of
  the VM."
  (-step [this vm-state]))

(defprotocol VmSeq
  "Implemented by instructions and labels so that they can be
  converted to sequences.  The function `-opcode` returns the opcode
  of the instruction and `-args` returns the arguments (as keywords or
  functions that extract a usable form of the arguments)."
  (-opcode [this])
  (-args [this]))

(defprotocol ResolveLabel
  "Implemented by instructions that take labels as arguments and need
  to resolve them during assembly.  `-resolve-label` returns the
  instruction `this`, with all label arguments resolved according to
  `label-indices`."
  (-resolve-label [this label-indices]))

;;; The variable `*indent*` is dynamically bound to determine the
;;; indentation of nested instructions.

(def ^:dynamic *indent* 1)

(defprotocol VmLabel
  "Marker protocol for labels.")

(defrecord Label [name]
  Object
  (toString [this]
    (str (:name this) ":"))
  VmLabel)

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
    (assoc vm-state
      :stack (conj (:stack vm-state) (env-value vm-state this))))
  VmSeq
  (-opcode [this]
    'LVAR)
  (-args [this]
    [:frame :slot :name]))

;;; `LSET` pops a value off the stack and puts it into environment
;;; location `[frame slot]`.  `name` is the name of the variable in
;;; the environment, `source` is the source expression that was
;;; compiled into this instruction.  As a special case, if `slot` is
;;; equal to the size of the respective frame, this frame is extended
;;; by an additional slot.
(defrecord LSET [frame slot name]
  Object
  (toString [this]
    (str (-opcode this) " "
         (:frame this) " " (:slot this) " ; " (:name this)))
  VmInst
  (-step [this vm-state]
    (let [{:keys [env stack]} vm-state
          {:keys [frame slot]} this]
      (assoc vm-state
        :env (assoc-in env [frame slot] (first stack)))))
  VmSeq
  (-opcode [this]
    'LSET)
  (-args [this]
    [:frame :slot :name]))

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
      :stack (conj (:stack vm-state)
                   (get (:global-env vm-state) (:name this)))))
  VmSeq
  (-opcode [this]
    'GVAR)
  (-args [this]
    [:name]))

;;; `GSET` stores the topmost value on the stack in global variable
;;; `name`.  It does not modify the stack.
(defrecord GSET [name]
  Object
  (toString [this]
    (str (-opcode this) " " (:name this)))
  VmInst
  (-step [this vm-state]
    (assoc vm-state
      :global-env (assoc (:global-env vm-state) name (first (:stack vm-state)))))
  VmSeq
  (-opcode [this]
    'GSET)
  (-args [this]
    [:name]))

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
        :stack (conj stack value)
        :store new-store)))
  VmSeq
  (-opcode [this]
    'CONST)
  (-args [this]
    [:value]))

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
    [(comp target-str :target)])
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
    [(comp target-str :target)])
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
    [(comp target-str :target)])
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
      (update-in vm-state [:stack] conj ret-addr)))
  VmSeq
  (-opcode [this]
    'SAVE)
  (-args [this]
    [(comp target-str :target)])
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
         :stack (cons (first stack) (list* (drop 2 stack)))
         :fun fun
         :code (:code fun)
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
    [:fun]))

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
          :stack (vec new-stack)
          :fun fun
          :code (:code fun)
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
    [:n-args]))

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
                                (let [[rest-args tmp-stack-2]
                                      (split-at n-rest-args tmp-stack)]
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
    [:n-args :name]))

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
    [:n-args :name]))

;;; `FUN` creates a new closure.  It takes the bytecode-compiled
;;; `function` and associates it with the current environment.
(defrecord FUN [fun]
  Object
  (toString [this]
    (let [code (:code (:fun this))
          line-break (str util/newline-str (util/indent (inc *indent*)))
          label-break (str util/newline-str (util/indent *indent*))
          line-breaks (map (fn [[prev cur]]
                             (if (satisfies? VmLabel prev)
                               (util/indent 1)
                               (if (satisfies? VmLabel cur)
                                 label-break
                                 line-break)))
                           (partition 2 1 (cons nil code)))]
      (apply str (-opcode this)
             (interleave line-breaks (map str code)))))
  VmInst
  (-step [this vm-state]
    (update-in vm-state [:stack] conj
               (assoc fun :env (:env vm-state))))
  VmSeq
  (-opcode [this]
    'FUN)
  (-args [this]
    [#(->> %1 :fun :code (map ->seq))]))

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

;;; TODO: Should not contain clojure code but a name, referring to the
;;; prim-table...
(defrecord PRIM [clj-code]
  Object
  (toString [this]
    (str (-opcode this)))
  VmInst
  (-step [this vm-state]
    (let [{:keys [n-args stack]} vm-state
          [raw-args new-stack] (split-at n-args stack)
          [result new-store] (apply (:clj-code this) (:store vm-state) raw-args)]
      (assoc vm-state
        :stack (conj new-stack result)
        :store new-store)))
  VmSeq
  (-opcode [this]
    'PRIM)
  (-args [this]
    [:clj-code]))

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
      :stack (make-fun :code (assemble '((ARGS 1 'CC "%built-in")
                                        (LVAR 1 0 stack)
                                        (SET-CC)
                                        (LVAR 0 0 fun)
                                        (RETURN "%built-in")))
                      :env (env (:stack vm-state))
                      :name '%cc
                      :args '[fun])))
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
          args (mem/->clojure (vec (reverse raw-args)))
          clj-result (apply (:clj-fun (get @operator-table (:name this)))
                            args)
          [result new-store] (mem/->vm clj-result (:store vm-state))]
      (assoc vm-state
        :stack (conj new-stack result)
        :store new-store)))
  VmSeq
  (-opcode [this]
    'OP)
  (-args [this]
    [:name :n-args]))

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
    (list* (opcode inst)
           (map #(% inst) (args inst)))))

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

(defn show
  "Print `indent` tabs followed by the string representation of
  `thing`."
  [thing & [indent]]
  (let [indent (or indent *indent*)]
    (if (sequential? thing)
      (doseq [inst thing] (show inst indent))
      (println (str (util/indent indent) (str thing)))))
  thing)

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
   'FUN     {:constructor ->FUN    :arity 1
             :arg-fun #(map assemble-inst %1)}
   'PRIM    {:constructor ->PRIM   :arity 1}
   'SET-CC  {:constructor ->SET-CC :arity 0}
   'CC      {:constructor ->CC     :arity 0}
   'HALT    {:constructor ->HALT   :arity 0}
   'OP      {:constructor ->OP     :arity 2}})

(defn assemble-inst [inst]
  (if (or (satisfies? VmInst inst) (satisfies? VmLabel inst))
    inst
    (let [[opcode & args] inst]
      (if-let [opcode-descr (get opcodes opcode)]
        (cond
         ;; Operator arity and call arity match
         (= (:arity opcode-descr) (count args))
         (apply (:constructor opcode-descr)
                (if-let [arg-fun (:arg-fun opcode-descr)]
                  (list (apply arg-fun args))
                  args))
         ;; Wrong arity
         :else
         (util/error "Opcode " opcode " applied to " (count args)
                     " arguments, but wants " (:arity opcode-descr)))
        (util/error "Unknown opcode " opcode " in " inst ".")))))

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
     (= (opcode inst) 'Label)
     nil
     :else
     inst)))

(defn assemble
  "Assemble a sequence of instructions from Clojure lists into
  `VmInst` data structures."
  [{:keys [code] :as bytecode}]
  (assert (= (:type bytecode) :bytecode-function)
          (str  "Cannot assemble " bytecode
                ", only bytecode functions are supported."))
  (let [pass-1 (map assemble-inst code)
        label-indices (find-label-indices pass-1)
        new-code (vec (keep (resolve-label-indices label-indices)
                            pass-1))]
    (assoc bytecode :code new-code :label-indices label-indices)))

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
  (first (:stack (stopped-frame trace))))


;;; Evaluate this (e.g., with C-x C-e in Cider) to run the tests for
;;; this namespace:
;;; (clojure.test/run-tests 'revue.vm-test)
;;; Evaluate this to run the test for all namespaces:
;;; (clojure.test/run-all-tests #"^revue\..*-test")
