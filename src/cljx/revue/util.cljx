(ns revue.util
  "The `revue.util` namespace contains utilities used by the rest of
  the system, i.e., the memory subsystem, the compiler and the virtual
  machine."
  (:require #+cljs [cljs.reader :as reader]
            ;; Maybe use clojure.core/read-string for Clojure?
            ;; #+clj [clojure.edn :as reader]
            #+clj [clojure.tools.reader :as reader]
            #+clj [clojure.pprint :as pprint]
            [clojure.string :as string]))

;;; Fixes and Missing Libraries
;;; ===========================

(defn pprint
  "The `clojure.pprint` namespace is not yet ported to ClojureScript.
  Define a workaround while this is the case, use `clojure.pprint`
  once the implementation is done."
  [obj]
  #+clj
  (pprint/pprint obj)
  #+cljs
  (println obj))

;;; Debugging
;;; =========

;;; To support additional behavior when debugging, we introduce some
;;; machinery that can be used throughout the other modules.

(def ^:dynamic *debugging?* (atom false))

(defn debugging? []
  @*debugging?*)

(defn enable-debugging! []
  (reset! *debugging?* true))

(defn disable-debugging! []
  (reset! *debugging?* false))

(defn debug-print [& args]
  (when (debugging?)
    (apply print args)
    (flush)))

(defn debug-println [& args]
  (when (debugging?)
    (apply println args)
    (flush)))

;;; Warnings and Errors
;;; ====================

;;; Since Clojure and ClojureScript currently provide no unified story
;;; for error handling, I have implemented quick and dirty versions of
;;; the `error` and `warning` functions.  Later we will have to
;;; add a better error-handling protocol.

;;; @MargDisable
;;; TODO: Replace these functions with more useful implementations
;;; @MargEnable

(defn error
  "The `error` function is a simple wrapper around `throw` that
  provides a suitable class for Clojure and ClojureScript.  Currently
  there is no way to provide specialized classes for different error
  types."
  [& msg]
  (throw (#+clj java.lang.Exception. #+cljs js/Error.
                (apply str msg))))

(def ^:dynamic *print-warnings*
  "If `*print-warnings*` is truthy, warning messages are printed to
  standard output, otherwise warnings are ignored."
  true)

(defn warn
  "The `warn` function in the `revue.util` module prints a message to
  the standard output stream.  Each module that reports warnings
  should define its own `warn` function that calls the two-argument
  `warn` function from this module."
  ([msg]
     (warn "REVUE Warning:" msg))
  ([prefix msg]
     (when *print-warnings*
       (println prefix msg)
       (flush))))

(def ^:dynamic *print-notes*
  "If `*print-notes*` is truthy, notes messages are printed to
  standard output, otherwise notes are ignored."
  true)

(defn note
  "The `note` function is similar to `warn` but controlled by the
  `*print-notes*` variable."
  ([msg]
     (note "REVUE Note:" msg))
  ([prefix msg]
     (when *print-notes*
       #+clj
       (println prefix msg)
       #+cljs
       (.log js/console prefix msg)
       (flush))))

;;; Utilities for sequences
;;; =======================

(defn singleton?
  "Returns true iff `coll` contains exactly one element,
  i.e., `(singleton? x)` is always equivalent to `(= (count x) 1)` but
  more efficient for collections where `count` is not O(1).''"
  [coll]
  (and (not (empty? coll))
       (not (next coll))))

(defn maybe-add
  "Wrap the expressions `exps` in a list starting with `op`, if their
  length is greater than one.  Use `if-empty` if `exps` is empty."
  ([op exps]
     (maybe-add op exps false))
  ([op exps if-empty]
     (cond (empty? exps) if-empty
           (singleton? exps) (first exps)
           :else (cons op exps))))

;;; Utilities for Strings
;;; =====================

(def newline-str (with-out-str #+clj (newline) #+cljs (newline {})))

(defn indent [n]
  (string/join (repeat n "\t")))

;;; Utilities for the VM representation
;;; ===================================

;;; Some utilities that deal with the handling of Clojure data types
;;; by the VM.

(defn boolean?
  "Return true if `x` is either `true` or `false`, false otherwise.
  This is needed for choosing the correct representation in the memory
  subsystem."
  [x]
  (or (= x true) (= x false)))

(defn atomic?
  "Return true if `d` can be represented without comsuming storage on
  the heap."
  [d]
  (cond
   (boolean? d) true
   (number? d) true
   (symbol? d) true
   (keyword? d) true
   (string? d) true
   (identical? d ()) true
   :else false))

(defn constant?
  "Return true if `d` is a compile-time constant, i.e., an expression
  that can be evaluated by the compiler and constant folded."
  [d]
  (and (atomic? d) (not (symbol? d))))

;;; Utilities for reading the program source
;;; ========================================

;;; @MargDisable
;;; TODO: Improve the error handling for this
;;; @MargEnable

(defn read-program-from-string
  "Read all forms from the input string and return them as sequence.
  Return the sequence `[::syntax-error]` if reading fails."
  [string]
  (#+clj with-open #+cljs let
         [stream
          #+clj (-> (java.io.StringReader. string)
                    clojure.lang.LineNumberingPushbackReader.)
          #+cljs (reader/push-back-reader string)]
         (loop [form (reader/read stream false stream false)
                forms []]
           (if (identical? form stream)
             forms
             (recur (reader/read stream false stream false)
                    (conj forms form))))))

;;; Local Environment for Compiler and VM
;;; =====================================

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

;;; We need the `->Env` constructor for creating new environments in
;;; `conj`.
(declare ->Env)

#+clj
(deftype Env [frames]
  ILocalEnv
  (frames [this]
    (vec (reverse frames)))
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
    (vec (reverse frames)))
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

(defn env [& frames]
  "Return a new environment containing `frames` in the given
  order (i.e., `(first frames)` is the first frame, etc.)."
  (->Env (vec (map vec (reverse frames))))) 

(defn env-value
  "Return the value at position `[frame slot]` in `vm-state`'s
  environment.  This is similar to `(get-in env [frame slot])` but
  throws if the index is out of bounds (which can only result from a
  compiler error)."
  [vm-state {:keys [frame slot]}]
  (let [env (:env vm-state)
        frame-vector (nth env frame)]
    (nth frame-vector slot)))

(defn in-env? [env var]
  (if (not env)
    nil
    (first
     (keep-indexed (fn [n-frame frame]
                     (if-let [pos-in-frame
                              (first (keep-indexed (fn [index val]
                                                     (if (= var val) index nil))
                                                   (seq frame)))]
                       [n-frame pos-in-frame]
                       nil))
                   (frames env)))))

;;; @MargDisable
;;; Evaluate this (e.g., with C-x C-e in Cider) to run the tests for
;;; this namespace:
;;; (clojure.test/run-tests 'revue.util-test)
;;; Evaluate this to run the test for all namespaces:
;;; (clojure.test/run-all-tests #"^revue\..*-test")
;;; @MargEnable

