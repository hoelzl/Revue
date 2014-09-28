
(ns revue.mem
  "The memory subsystem for interpreters and the VM"
  (:require [revue.util :as util]))

;;; Storage for Interpreters and the VM
;;; ===================================

;;; Since we want to be able to move forward and backward in time,
;;; between arbitrary points in the execution of the VM, we cannot
;;; mutate storage.  Therefore we implement a small-step semantics
;;; of an interpreter or VM and pass around an explicit store.

;;; As a first step we map the expressed data values (which are just
;;; Clojure data structures) into an internal representation (the
;;; stored data values).  The expressed values are the values that can
;;; appear syntactically in the program text.  Since we are using the
;;; Clojure reader for the Scheme-like internal language, the simplest
;;; solution for expressed values in this language is to allow the
;;; following data types.

;;; * Booleans
;;; * Numbers
;;; * Symbols
;;; * Keywords
;;; * Strings
;;; * Lists 
;;; * Vectors
;;; * Maps

;;; The denoted values are references to mutable versions of the
;;; expressed values.  For other input languages, the expressed values
;;; may differ, and therefore it may be necessary to either encode
;;; them using the provided data structures for stored values, or to
;;; extend the memory subsystem with new data types.

;;; To make the storage system of the VM slightly less inefficient, we
;;; store the first five of the denoted types (Booleans, numbers,
;;; symbols, keywords and strings) directly, and only use references
;;; for the compound data types.  The stored values are therefore
;;; Booleans, numbers, symbols, keywords, strings, references to
;;; lists, references to vectors, and references to maps.  For cases
;;; where we need mutable bindings of the directly stored types, the
;;; memory system provides a box data type that is a simple reference
;;; to another stored data type.

;;; The important functions are `->vm` for converting a represented
;;; value into a stored value (i.e., for converting a Clojure datum
;;; into its internal representation in the VM (and the interpreter),
;;; and `->clojure` for getting back a Clojure value from the VM's
;;; internal representation.  These two functions are mostly inverse
;;; to each other; `#(apply ->clojure (->vm %))` is always the
;;; identity (unless there is a bug in the implementation of the
;;; conversion); the other direction, `#(->vm (->clojure %1 %2) [])`,
;;; might result in a different state than the one that was originally
;;; passed in.

;;; In the conversion to Clojure, information about sharing in the
;;; data is lost.  Some amount of loss is unavoidable, since Clojure
;;; data structures cannot directly represent arbitrary graphs without
;;; using refs, actors or atoms; in other cases it would be possible
;;; to maintain sharing when converting data into Clojure structures
;;; (e.g., shared tails when converting lists).  The loss of
;;; information about sharing means that you cannot convert cyclic
;;; data structures to Clojure, so beware.  If it should become
;;; necessary to maintain information about sharing during the
;;; conversion process, it would be straightforward to add a cache
;;; that remembers the addresses that were already converted to
;;; Clojure, but since this cannot be a general solution it seems not
;;; worth the effort.

(defprotocol StoredData
  "Datatypes that are understood by the VM.  The conversion
  `-->clojure` always returns a Clojure value that implements the
  `ExpressedData` protocol and can therefore be turned back into a
  `StoredData` value."
  (-->clojure [this store]))

;;; The `->clojure` function (defined below) is a simple wrapper
;;; around `-->clojure` that mostly serves to avoid warnings from
;;; ClojureScript, but also makes the `store` argument optional for a
;;; slightly better interactive experience.
;;;
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
  (-->clojure [this store] ())
  #+cljs nil
  #+cljs
  (-->clojure [this store]
    nil))

(defprotocol HeapObject
  "Datatypes that are stored on the heap implement the `HeapObject`
  protocol.

  `HeapObject`s can always report the address where they are stored on
  the heap and their size.  It is not quite clear yet, what the
  `-size` method should report for objects which do not occupy
  contiguous regions of heap space, so the definition of the size
  might change in the future."
  (-address [this store])
  (-size [this store]))

;;; A `VmBox` is a data type for stored values that references a
;;; single value on the heap.  This can be used obtain a level of
;;; indirection that is necessary to store atomic values in mutable
;;; variables.  For example, a function parameter that is referenced
;;; by an escaping closure inside the function body and whose binding
;;; can be destructively modified has to be stored in a box to ensure
;;; that the closure sees the modified value.  Therefore, in the
;;; following Pseudo-Scheme code `x` has to be boxed:
;;;
;;;     (lambda (x)
;;;       (list (lambda (y) (set! x y))
;;;             (lambda () x)))
;;;
(defrecord VmBox [address]
  StoredData
  (-->clojure [this store]
    (->clojure (store (:address this)) store))
  HeapObject
  (-address [this store]
    (:address this))
  (-size [this store]
    1))

(defn new-box
  "Allocate a new `VmBox` cell.  The `value` parameter should be a
  stored value."
  [value store]
  (let [address (count store)
        new-store (conj store value)]
    [(->VmBox address) new-store]))

(defn box-set!
  "Change the value stored in a box"
  [new-value box store]
  (assoc store (-address box store) new-value))

;;; References to a cons cell on the heap, i.e., the stored value for
;;; lists.
;;;
(defrecord VmCons [address]
  StoredData
  ;; A naive implementation of `-->clojure` would be to cons together
  ;; applications of `->clojure` to the two fields of the cons cell.
  ;; Since we don't want to run out of stack space for larger list, we
  ;; have to implement a more complex version.  Note that the current
  ;; implementation only allows proper lists, i.e., you cannot use
  ;; cons cells as dotted pairs.
  (-->clojure [this store]
    (loop [h (store (:address this))
           r (store (inc (:address this)))
           acc ()]
      (if (identical? r ()) ;; Change this to enable dotted pairs.
        (reverse (cons (->clojure h store) acc))
        (recur (store (:address r))
               (store (inc (:address r)))
               (cons (->clojure h store) acc)))))
  HeapObject
  (-address [this store]
    (:address this))
  (-size [this store]
    2))

(defn new-cons
  "Allocate a new VmCons cell.  The `car` and `cdr` parameters have to
  be StoredData."
  [[car cdr] store]
  (let [address (count store)
        new-store (conj store car cdr)]
    [(->VmCons address) new-store]))


;;; Reference to a vector occupying a contiguous part of the heap.
;;; Currently vectors are not resizable in place.
;;;
(defrecord VmVector [address size]
  StoredData
  (-->clojure [this store]
    (into []
          (for [address (range (:address this)
                               (+ (:address this) (:size this)))]
            (->clojure (store address) store))))
  HeapObject
  (-address [this store]
    (:address this))
  (-size [this store]
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

;;; VM representation of maps.  To simplify the implementation, we
;;; define a map as a reference to a vector consisting of key-value
;;; pairs.
;;;
(defrecord VmMap [vector-ref]
  StoredData
  (-->clojure [this store]
    (let [kv-vector (->clojure (:vector-ref this) store)]
      (into {} kv-vector)))
  HeapObject
  (-address [this store]
    (-address (:vector-ref this) store))
  (-size [this store]
    (-size (:vector-ref this) store)))

(defn new-map
  "Allocate a new VmMap.  The contents is passed as a vector of key-value
  pairs; these pairs have to be already converted into stored data."
  [contents store]
  (let [[v new-store] (new-vector contents store)]
    [(->VmMap v) new-store]))

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
  function ->vm always returns a type that implements StoredData and
  can therefore be converted back into expressed data.  The roundtrip
  conversion from expressed data to stored data and back to expressed
  data is always an identity."
  (-->vm [this store]))


;;; A wrapper around the `-->vm` protocol method, to avoid warnings
;;; from ClojureScript.
;;;
(declare ->vm)

(defn convert-to-store-vector
  "Convert a vector containing expressed data to a vector consisting
  only of stored-data and a new store."
  [vector store]
  (reduce (fn [[vec store] elt]
            (let [[new-elt new-store] (->vm elt store)]
              [(conj vec new-elt) new-store]))
          [[] store]
          vector))

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
            (convert-to-store-vector this store)]
        (new-vector store-vec new-store))))
  #+clj clojure.lang.MapEntry ;;; ClojureScript does not have map entries
  #+clj
  (-->vm [this store]
    (->vm (vector (first this) (second this)) store))
  ;; We need both `PersistentArrayMap` and `PersistentHashMap` since
  ;; literal maps give rise to both types, depending on their size.
  #+clj clojure.lang.PersistentArrayMap #+cljs cljs.core/PersistentArrayMap
  (-->vm [this store]
    (let [[store-vec new-store] (convert-to-store-vector (seq this) store)]
      (new-map store-vec new-store)))
  #+clj clojure.lang.PersistentHashMap #+cljs cljs.core/PersistentHashMap
  (-->vm [this store]
    (let [[store-vec new-store] (convert-to-store-vector (seq this) store)]
      (new-map store-vec new-store))))

(defn ->vm
  "Convert expressed data to stored data, i.e., convert Clojure data
  structures into the VM's internal format."
  ([d]
     (->vm d []))
  ([d store]
     (-->vm d store)))

;;; Evaluate this (e.g., with C-x C-e in Cider) to run the tests for
;;; this namespace:
;;; (clojure.test/run-tests 'revue.vm-test)
;;; Evaluate this to run the test for all namespaces:
;;; (clojure.test/run-all-tests #"^revue\..*-test")
