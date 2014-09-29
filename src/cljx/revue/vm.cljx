(ns revue.vm
  "The virtual machine for the Revue system."
  (:require [revue.util :as util :refer [pprint #+cljs nthrest]]
            [revue.mem :as mem]))


;;; Utilities
;;; =========

(defn warn
  "Warn about a problem encountered by the virtual machine."
  [msg]
  (util/warn "VM Warning:" msg))

;;; Local Environments
;;; ==================

;;; Local environments are implemented as a sequence of frames.  We
;;; have to be able to retreive values by giving an index of the form
;;; `[frame slot]`, and we need to be able to push new frames at index
;;; 0.  This means that none of the existing Clojure data structures
;;; is a good fit: Lists don't allow direct indexing and arrays
;;; conjoin new members at the end of the array.  We therefore define
;;; a new data structure `Env` that is essentially a wrapper around an
;;; array that reverses the indices, i.e., the last element of the
;;; array `frames` is accessed with index 0, the first element with
;;; index `(dec (count frames))`.  Unfortunately this is not as simple
;;; as it should be, and to make things worse, we have to implement
;;; two slightly different versions for Clojure and ClojureScript
;;; since the protocol names and methods don't match.

(defprotocol ILocalEnv
  "The protocol for local environments: Return a sequence of
  environment frames."
  (frames [this]))

(declare ->Env)

#+clj
(deftype Env [frames]
  ILocalEnv
  (frames [this]
    frames)
  clojure.lang.Counted
  (count [this]
    (count frames))
  clojure.lang.Indexed
  (nth [this n]
    (nth this n nil))
  (nth [this n default]
    (nth frames (- (count frames) n 1)))
  clojure.lang.Sequential
  clojure.lang.Seqable
  (seq [this]
    (if (empty? frames)
      nil
      this))
  clojure.lang.IPersistentCollection
  ;; Obviously `cons` does not implement `cons` but `conj`, see
  ;; `src/jvm/clojure/lang/RT.java` in the Clojure implementation.
  (cons [this obj]
    (->Env (conj frames obj)))
  (empty [this]
    (println "Called empty")
    (empty? frames))
  (equiv [this obj]
    (if (sequential? obj)
      (loop [lhs (seq this)
             rhs (seq obj)]
        (if lhs
          (and (not (not rhs))
               (= (first lhs) (first rhs))
               (recur (next lhs) (next rhs)))
          (not rhs)))
      false))
  clojure.lang.ISeq
  (first [this]
    (if (empty? frames)
      nil
      (nth frames (- (count frames) 1))))
  (next [this]
    (if (<= (count frames) 1)
      nil
      (->Env (pop frames))))
  (more [this]
    (or (next this) ()))
  clojure.lang.IPersistentStack
  (peek [this]
    (first this))
  (pop [this]
    (or (next this)
        (->Env [])))
  clojure.lang.Associative
  (containsKey [this key]
    (and (integer? key)
         (<= 0 key)
         (< key (count frames))))
  (entryAt [this key]
    (nth this key))
  (assoc [this key val]
    (->Env (assoc frames (- (count frames) key 1) val)))
  clojure.lang.ILookup
  (valAt [this key]
    (get this key nil))
  (valAt [this key not-found]
    (get frames (- (count frames) key 1) not-found)))

#+cljs
(deftype Env [frames]
  ILocalEnv
  (frames [_]
    frames)
  Object
  (entry-at [this key]
    (nth this key))
  cljs.core/ICounted
  (-count [_]
    (count frames))
  cljs.core/IIndexed
  (-nth [this n]
    (nth this n nil))
  (-nth [this n default]
    (nth frames (- (count frames) n 1)))
  cljs.core/ISequential
  cljs.core/ISeqable
  (-seq [this]
    (if (empty? frames)
      nil
      this))
  cljs.core/ICollection
  (-conj [_ obj]
    (->Env (conj frames obj)))
  cljs.core/IEmptyableCollection
  (-empty [_]
    (empty? frames))
  cljs.core/IEquiv
  (-equiv [this obj]
    (if (sequential? obj)
      (loop [lhs (seq this)
             rhs (seq obj)]
        (if lhs
          (and (not (not rhs))
               (= (first lhs) (first rhs))
               (recur (next lhs) (next rhs)))
          (not rhs)))
      false))
  cljs.core/ISeq
  (-first [this]
    (if (empty? frames)
      nil
      (nth frames (- (count frames) 1))))
  (-rest [this]
    (if (<= (count frames) 1)
      ()
      (->Env (pop frames))))
  cljs.core/INext
  (-next [this]
    (if (<= (count frames) 1)
      nil
      (->Env (pop frames))))
  cljs.core/IStack
  (-peek [this]
    (first this))
  (-pop [this]
    (or (next this)
        (->Env [])))
  cljs.core/IAssociative
  (-contains-key? [this key]
    (and (integer? key)
         (<= 0 key)
         (< key (count frames))))
  (-assoc [this key val]
    (->Env (assoc frames (- (count frames) key 1) val)))
  cljs.core/ILookup
  (-lookup [this key]
    (get this key nil))
  (-lookup [this key not-found]
    (get frames (- (count frames) key 1) not-found)))

#+clj
(defmethod print-method Env [env writer]
  (.write writer (str "#" (print-str (class env)) "{:frames " (frames env) "}")))

(defn env-value
  "Return the value at position `[frame slot]` in `vm-state`'s
  environment.  This is similar to `(get-in env [frame slot])` but
  throws if the index is out of bounds (which can only result from a
  compiler error)."
  [vm-state {:keys [frame slot]}]
  (let [env (:env vm-state)
        frame-vector (nth env frame)]
    (nth frame-vector slot)))


;;; The Global Environment
;;; ======================

(defn make-global-env
  "Create a hash table that serves as the global environment."
  []
  {})

;;; The VM State
;;; ============

(defn make-return-address
  "Create a new return address that jumps to step `pc` in `function`."
  [{:keys [function pc env]}]
  {:type :return-address
   :function function
   :pc pc
   :env env})

(defn make-fn
  "Create a new bytecode-interpreted function."
  [{:keys [code env name args]}]
  {:type :bytecode-function
   :code code
   :env env
   :name name
   :args args})

(defn initial-state
  "Create a new state for a VM, initially executing function `f`"
  [f]
  {:type :vm-state
   :function f
   :code (:code f)
   :pc 0
   :global-env (make-global-env)
   :env ()
   :stack ()
   :n-args 0
   :stopped? false})

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

(defn make-prim
  "Create a new primitive instruction."
  [symbol n-args opcode always? side-effect?]
  {:type :primitive-instruction
   :symbol symbol
   :n-args n-args
   :opcode opcode
   :always? always?
   :side-effect? side-effect?})

(defprotocol VmInst
  "Each instruction of the VM is represented as a datatype that
  implements the `VmInst` protocol.  The function `-step` executes the
  instruction starting in state `vm-state` and returns a new state of
  the VM."
  (-step [this vm-state])
  (-opcode [this]))

(defrecord LVAR [frame slot source]
  VmInst
  (-step [this vm-state]
    (assoc vm-state
      :stack (conj (:stack vm-state) (env-value vm-state this))))
  (-opcode [this]
    'LVAR))

(defrecord LSET [frame slot name source]
  VmInst
  (-step [this vm-state]
    (let [{:keys [env stack]} vm-state
          {:keys [frame slot]} this]
      (assoc vm-state
        :env (assoc-in env [frame slot] (peek stack))
        :stack (pop stack))))
  (-opcode [this]
    'LSET))

(defrecord GVAR [name source]
  VmInst
  (-step [this vm-state]
    (assoc vm-state
      :stack (conj (:stack vm-state)
                   (get (:global-env vm-state) (:name this)))))
  (-opcode [this]
    'GVAR))

(defrecord GSET [name source]
  VmInst
  (-step [this vm-state]
    (assoc vm-state
      :global-env (assoc (:global-env vm-state) name (peek (:stack this)))
      :stack (pop (:stack this))))
  (-opcode [this]
    'GSET))

(defrecord POP [source]
  VmInst
  (-step [this vm-state]
    (update-in vm-state [:stack] pop))
  (-opcode [this]
    'POP))

(defrecord CONST [value source]
  VmInst
  (-step [this vm-state]
    (update-in vm-state [:stack] conj (:value this)))
  (-opcode [this]
    'CONST))

(defrecord JUMP [target source]
  VmInst
  (-step [this vm-state]
    (assoc vm-state :pc (:target this)))
  (-opcode [this]
    'JUMP))

(defrecord FJUMP [target source]
  VmInst
  (-step [this vm-state]
    (let [stack (:stack vm-state)
          value (peek stack)
          new-state (update-in vm-state [:stack] pop)]
      (if (not value)
        (update-in new-state [:pc] (:target this))
        new-state)))
  (-opcode [this]
    'FJUMP))

(defrecord TJUMP [target source]
  VmInst
  (-step [this vm-state]
    (let [stack (:stack vm-state)
          value (peek stack)
          new-state (update-in vm-state [:stack] pop)]
      (if value
        (update-in new-state [:pc] (:target this))
        new-state)))
  (-opcode [this]
    'TJUMP))

(defrecord SAVE [source]
  VmInst
  (-step [this vm-state]
    (let [ret-addr (make-return-address vm-state)]
      (update-in vm-state [:stack] conj ret-addr)))
  (-opcode [this]
    'SAVE))

(defrecord RETURN [source]
  VmInst
  (-step [this vm-state]
    (let [stack (:stack vm-state)
          return-address (second stack)
          function (:function return-address)]
      (assoc vm-state
        :stack (conj (vec (nthrest stack 2)) (first stack))
        :function function
        :code (:code function)
        :env (:env return-address)
        :pc (:pc return-address))))
  (-opcode [this]
    'RETURN))

(defrecord CALLJ [n-args source]
  VmInst
  (-step [this vm-state]
    (let [old-stack (:stack vm-state)
          function (second old-stack)
          new-stack (vec (nthrest old-stack 2))]
      (assoc vm-state
        :stack new-stack
        :function function
        :code (:code function)
        :env (:env function)
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
  [n-args vm-state & n-rest-args]
  (let [stack (:stack vm-state)
        [tmp-frame tmp-stack] (split-at n-args stack)
        [new-frame new-stack] (if-not n-rest-args
                                [(vec tmp-frame) (vec tmp-stack)]
                                (let [[rest-args tmp-stack-2] (split-at n-rest-args tmp-stack)]
                                  [(conj (vec tmp-frame) (vec rest-args))
                                   (vec tmp-stack-2)]))]
    (assoc vm-state
      :env (conj (:env vm-state) new-frame)
      :stack new-stack)))

(defrecord ARGS [n-args name source]
  VmInst
  (-step [this vm-state]
    (let [{:keys [n-args name]} (:n-args this)]
      (if-not (= n-args (:n-args vm-state))
        (assoc vm-state
          :stopped? true
          :reason (str "Function " name " called with " (:n-args vm-state)
                       " arguments, but wants exactly " n-args "."))
        (move-args-from-stack-to-env n-args vm-state))))
  (-opcode [this]
    'ARGS))

(defrecord ARGS* [n-args name source]
  VmInst
  (-step [this vm-state]
    (let [{:keys [n-args name]} this
          supplied-args (:n-args vm-state)]
      (if-not (<= n-args supplied-args)
        (assoc vm-state
          :stopped? true
          :reason (str "Function " name " called with " supplied-args
                       " arguments, but wants at least " n-args))
        (move-args-from-stack-to-env n-args vm-state (- supplied-args n-args)))))
  (-opcode [this]
    'ARGS*))

(defrecord FN [function source]
  VmInst
  (-step [this vm-state]
    (update-in vm-state [:stack] conj
               (assoc function :env (:env vm-state))))
  (-opcode [this]
    'FN))

(defrecord PRIM [clj-code source]
  VmInst
  (-step [this vm-state]
    (let [{:keys [n-args stack]} vm-state
          [raw-args new-stack] (split-at n-args stack)]
      (assoc vm-state
        :stack (conj new-stack (apply (:clj-code this) raw-args)))))
  (-opcode [this]
    'PRIM))

(defrecord SET-CC []
  VmInst
  (-step [this vm-state]
    (update-in vm-state [:stack] peek))
  (-opcode [this]
    'SET-CC))

(defrecord CC [source]
  VmInst
  (-step [this vm-state]
    (assoc vm-state :stack
           (make-fn :code (assemble '((ARGS 1 'CC "%built-in")
                                      (LVAR 1 0 stack)
                                      (SET-CC)
                                      (LVAR 0 0 fun)
                                      (RETURN "%built-in")))
                    :env (->Env [(:stack vm-state)])
                    :name '%cc
                    :args '[fun])))
  (-opcode [this]
    'CC))

(defrecord HALT [source]
  VmInst
  (-step [this vm-state]
    (assoc vm-state
      :stopped? true
      :reason "Program terminated."))
  (-opcode [this]
    'HALT))

(defrecord OP [n clj-code source]
  VmInst
  (-step [this vm-state]
    (let [stack (:stack vm-state)
          [raw-args new-stack] (split-at n stack)
          args (mem/->clojure (vec raw-args))]
      (assoc vm-state
        :stack (conj (vec new-stack) (apply (:clj-code this) args)))))
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
  {'LVAR    {:opcode ->LVAR   :arity 3}
   'LSET    {:opcode ->LSET   :arity 4}
   'GVAR    {:opcode ->GVAR   :arity 2}
   'GSET    {:opcode ->GSET   :arity 2}
   'POP     {:opcode ->POP    :arity 1}
   'CONST   {:opcode ->CONST  :arity 2}
   'JUMP    {:opcode ->JUMP   :arity 2}
   'FJUMP   {:opcode ->FJUMP  :arity 2}
   'TJUMP   {:opcode ->TJUMP  :arity 2}
   'SAVE    {:opcode ->SAVE   :arity 1}
   'RETURN  {:opcode ->RETURN :arity 1}
   'CALLJ   {:opcode ->CALLJ  :arity 2}
   'ARGS    {:opcode ->ARGS   :arity 3}
   'ARGS*   {:opcode ->ARGS*  :arity 3}
   'FN      {:opcode ->FN     :arity 2}
   'PRIM    {:opcode ->PRIM   :arity 2}
   'SET-CC  {:opcode ->SET-CC :arity 0}
   'CC      {:opcode ->CC     :arity 1}
   'HALT    {:opcode ->HALT   :arity 1}
   'OP      {:opcode ->OP     :arity 3}})

(defn assemble-inst [inst]
  (let [[opcode & args] inst]
    (if-let [opcode-descr (get opcodes opcode)]
      (cond (= (:arity opcode-descr) (count args))
            (apply (:opcode opcode-descr) args)
            (= (:arity opcode-descr) (inc (count args)))
            (apply (:opcode opcode-descr) (conj (vec args) "no source location"))
            :else
            (util/error "Opcode " opcode " applied to " (count args)
                        " arguments, but wants " (:arity opcode-descr)))
      (util/error "Unknown opcode " opcode " in " inst "."))))

(defn assemble
  "Assemble a sequence of instructions from Clojure lists into
  `VmInst` data structures."
  [instructions]
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
