(ns revue.vm
  "The virtual machine for the Revue system."
  (:require [revue.util :as util]
            [revue.mem :as mem]))


;;; Utilities
;;; =========

(defn warn
  "Warn about a problem encountered by the virtual machine."
  [msg]
  (util/warn "VM Warning:" msg))

;;; Local Environments
;;; ==================

(defprotocol IEnv
  (frames [this]))

(declare ->Env)

#+clj
(deftype Env [frames]
  IEnv
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
  IEnv
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

(defn env-value [vm-state {:keys [frame slot]}]
  "Return the value at position `[frame slot]` in `vm-state`'s
  environment.  This is similar to `(get-in env [frame slot])` but
  throws if the index is out of bounds (which can only result from a
  compiler error)."
  (let [env (:env vm-state)
        frame-vector (nth env frame)]
    (nth frame-vector slot)))

(defn set-local-var-from-stack [{:keys [env stack] :as vm-state} {:keys [frame slot]}]
  "Set the value at position `[frame slot]` in `vm-state`'s
  environment to `new-value`."
  (update-in
   (update-in vm-state [:env] assoc-in [frame slot] (peek stack))
   [:stack] pop))


;;; The Global Environment
;;; ======================

(defn make-global-env []
  "Create a hash table that serves as the global environment."
  {})

;;; The VM State
;;; ============

(defn make-ret-addr [fn pc env]
  {:type :return-address
   :fn fn
   :pc pc
   :env env})

(defn make-fn [code & {:keys [env name args]}]
  {:type :bytecode-function
   :code code
   :env env
   :name name
   :args args})

(defn initial-state [f]
  "Create a new state for a VM, initially executing function `f`"
  {:type :vm-state
   :code (:code f)
   :pc 0
   :global-env (make-global-env)
   :env ()
   :stack ()
   :n-args 0
   :instr nil})

;;; VM Instructions
;;; ===============

(defn make-prim [symbol n-args opcode always? side-effect?]
  "Create a new primitive instruction."
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
    (update-in vm-state [:stack] conj (env-value vm-state this)))
  (-opcode [this]
    'LVAR))

(defrecord LSET [frame slot source]
  VmInst
  (-step [this vm-state]
    (let [stack (:stack vm-state)]
      (set-local-var-from-stack vm-state this)))
  (-opcode [this]
    'LSET))

(defrecord GVAR [source]
  VmInst
  (-step [this vm-state])
  (-opcode [this]
    'GVAR))

(defrecord GSET [source]
  VmInst
  (-step [this vm-state])
  (-opcode [this]
    'GSET))

(defrecord POP [source]
  VmInst
  (-step [this vm-state])
  (-opcode [this]
    'POP))

(defrecord CONST [source]
  VmInst
  (-step [this vm-state])
  (-opcode [this]
    'CONST))

(defrecord JUMP [source]
  VmInst
  (-step [this vm-state])
  (-opcode [this]
    'JUMP))

(defrecord FJUMP [source]
  VmInst
  (-step [this vm-state])
  (-opcode [this]
    'FJUMP))

(defrecord TJUMP [source]
  VmInst
  (-step [this vm-state])
  (-opcode [this]
    'TJUMP))

(defrecord SAVE [source]
  VmInst
  (-step [this vm-state])
  (-opcode [this]
    'SAVE))

(defrecord RETURN [source]
  VmInst
  (-step [this vm-state])
  (-opcode [this]
    'RETURN))

(defrecord CALLJ [source]
  VmInst
  (-step [this vm-state])
  (-opcode [this]
    'CALLJ))

(defrecord ARGS [source]
  VmInst
  (-step [this vm-state])
  (-opcode [this]
    'ARGS))

(defrecord ARGS* [source]
  VmInst
  (-step [this vm-state])
  (-opcode [this]
    'ARGS*))

(defrecord FN [source]
  VmInst
  (-step [this vm-state])
  (-opcode [this]
    'FN))

(defrecord PRIM [source]
  VmInst
  (-step [this vm-state])
  (-opcode [this]
    'PRIM))

(defrecord SET-CC [source]
  VmInst
  (-step [this vm-state])
  (-opcode [this]
    'SET-CC))

(defrecord CC [source]
  VmInst
  (-step [this vm-state])
  (-opcode [this]
    'CC))

(defrecord HALT [source]
  VmInst
  (-step [this vm-state])
  (-opcode [this]
    'HALT))

(defrecord OP0 [source]
  VmInst
  (-step [this vm-state])
  (-opcode [this]
    'OP0))

(defrecord OP1 [source]
  VmInst
  (-step [this vm-state])
  (-opcode [this]
    'OP1))

(defrecord OP2 [source]
  VmInst
  (-step [this vm-state])
  (-opcode [this]
    OP2))

(defrecord OP3 [source]
  VmInst
  (-step [this vm-state])
  (-opcode [this]
    'OP3))

(defrecord OPN  [source]
  VmInst
  (-step [this vm-state])
  (-opcode [this]
    'OPN))


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
