;;; The virtual machine

(ns revue.vm
  (:require [revue.util :as util]))


;;; Utilities
;;; =========

(defn warn
  "Warn about a problem encountered by the virtual machine."
  [msg]
  (util/warn "VM Warning:" msg))


;;; Storage
;;; =======

;;; Since we want to be able to move forward and backward in time,
;;; between arbitrary points in the execution of the VM, we cannot
;;; mutate storage.  Therefore we implement a small-step semantics
;;; of VM instructions in a store-passing VM.

;;; As a first step we map the expressed data values (which are just
;;; Clojure data structures) into an internal representation (the
;;; stored data values).  The expressed values are the values that can
;;; appear syntactically in the program text.  Since we are using the
;;; Clojure reader, the simplest solution for our expressed values is
;;; to allow the following data types.

;;; * Booleans
;;; * Numbers
;;; * Symbols
;;; * Keywords
;;; * Strings
;;; * Lists 
;;; * Vectors
;;; * Maps
;;; * Sets

;;; The denoted values are references to mutable versions of the
;;; expressed values.

;;; To make the storage system of the VM slightly less inefficient, we
;;; store the first five of the denoted types (Booleans, numbers,
;;; symbols, keywords and strings) directly, and only use references
;;; for the compound data types.  The stored values are therefore
;;; Booleans, numbers, symbols, keywords, strings, references to
;;; lists, references to vectors, references to maps and references to
;;; sets.

(defprotocol StoredData
  "Datatypes that are understood by the VM.  The conversion
  `-->clojure' always returns a clojure value that implements the
  ExpressedData protocol and can therefore be turned back into a
  StoredData value."
  (-->clojure [this store]))

(declare ->clojure)

(extend-protocol StoredData
  #+clj java.lang.Boolean #+cljs boolean
  (-->clojure [this store] this)
  #+clj java.lang.Number #+cljs number
  (-->clojure [this store] this)
  #+clj clojure.lang.Symbol #+cljs cljs.core/Symbol
  (-->clojure [this store] this)
  #+clj clojure.lang.Keyword #+cljs cljs.core/Keyword
  (-->clojure [this store] this)
  #+clj java.lang.String #+cljs string
  (-->clojure [this store] this)
  #+clj clojure.lang.PersistentList$EmptyList #+cljs cljs.core/EmptyList
  (-->clojure [this store] ()))

(defprotocol HeapObject
  "Datatypes that are stored on the heap."
  (-address [this])
  (-size [this]))

;;; TODO: The ->clojure method may easily run out of stack space.
(defrecord VmCons [address]
  StoredData
  (-->clojure [this store]
    (cons (->clojure (store (:address this)) store)
          (->clojure (store (inc (:address this))) store)))
  HeapObject
  (-address [this]
    (:address this))
  (-size [this]
    2))

(defn new-cons
  "Allocate a new VmCons cell.  The `car' and `cdr' parameters have to
  be StoredData."
  [[car cdr] store]
  (let [address (count store)
        new-store (conj store car cdr)]
    [(->VmCons address) new-store]))

(defrecord VmVector [address size]
  StoredData
  (-->clojure [this store]
    (into []
          (for [address (range (:address this)
                               (+ (:address this) (:size this)))]
            (->clojure (store address) store))))
  HeapObject
  (-address [this]
    (:address this))
  (-size [this]
    (:size this)))

(defn new-vector
  "Allocate a new VmVector. The contents has to be StoredData.

  There is no way to create an uninitialized vector.  To create an
  vector of a prespecified size with a given element, do something
  like (new-vector (repeat 10 false) store)"
  [contents store]
  (let [address (count store)
        size (count contents)
        new-store (into store contents)]
    [(->VmVector address size) new-store]))


;;; We define the ->clojure function as a wrapper around the protocol
;;; to avoid warnings from the ClojureScript compiler.  This is
;;; probably a bug in ClojureScript.  In addition we can overload the
;;; function to use an empty store when it is called with one
;;; argument.

(defn ->clojure
  "Convert stored data to expressed data, i.e., convert from the VM's
  internal format to plain old Clojure data."
  ([d]
     (->clojure d []))
  ([d store]
     (-->clojure d store)))

(defprotocol ExpressedData
  "Datatypes that can be converted to a VM representation.  The
  function ->vm returns a type that implements StoredData."
  (-->vm [this store]))

(declare ->vm)

(extend-protocol ExpressedData
  #+clj java.lang.Boolean #+cljs boolean
  (-->vm [this store] [this store])
  #+clj java.lang.Number #+cljs number
  (-->vm [this store] [this store])
  #+clj clojure.lang.Symbol #+cljs cljs.core/Symbol
  (-->vm [this store] [this store])
  #+clj clojure.lang.Keyword #+cljs cljs.core/Keyword
  (-->vm [this store] [this store])
  #+clj java.lang.String #+cljs string
  (-->vm [this store] [this store])
  #+clj clojure.lang.PersistentList$EmptyList #+cljs cljs.core/EmptyList
  (-->vm [this store] [() store])
  #+clj clojure.lang.PersistentList #+cljs cljs.core/List
  (-->vm [this store]
    ;; Written in this slightly convoluted way to avoid stack overflow
    ;; for long lists.
    (let [[elts new-store] (reduce (fn [[lst store] elt]
                                     (let [[new-elt tmp-store] (->vm elt store)]
                                       [(cons new-elt lst) tmp-store]))
                                   [() store]
                                   this)]
      (reduce (fn [[vm-cons store] d]
                (new-cons [d vm-cons] store))
              [() new-store]
              elts)))
  #+clj clojure.lang.PersistentVector #+cljs cljs.core/PersistentVector
  (-->vm [this store]
    ;; Quick path for allocating vectors containing atomic data.
    (if (every? util/atomic? this)
      (new-vector this store)
      (let [[store-vec new-store]
            (reduce (fn [[vec store] elt]
                      (let [[new-elt new-store] (->vm elt store)]
                        [(conj vec new-elt) new-store]))
                    [[] store]
                    this)]
        (new-vector store-vec new-store)))))

(defn ->vm
  "Convert expressed data to stored data, i.e., convert Clojure data
  structures into the VM's internal format."
  ([d]
     (->vm d []))
  ([d store]
     (-->vm d store)))

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
