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
;;; denoted data values).  The denoted values are currently references
;;; to expressed values:

;;; * Booleans
;;; * Numbers
;;; * Symbols
;;; * Keywords
;;; * Strings
;;; * Lists 
;;; * Arrays
;;; * Maps
;;; * Sets

;;; To make the storage system of the VM slightly less inefficient, we
;;; store the first five of these types (Booleans, numbers, symbols,
;;; keywords and strings) directly, and only use references for the
;;; compound data types.

(defprotocol DenotedData
  "Datatypes that are understood by the VM."
  (-->clojure [this store]))

(extend-protocol DenotedData
  #+clj java.lang.Boolean #+cljs boolean
  (-->clojure [this store] this)
  #+clj java.lang.Number #+cljs number
  (-->clojure [this store] this))


;;; We define the ->clojure function as a wrapper around the protocol
;;; to avoid warnings from the ClojureScript compiler.  This is
;;; probably a bug in ClojureScript.

(defn ->clojure [d store]
  (-->clojure d store))

(defprotocol ExpressedData
  "Datatypes that can be converted to a VM representation.  The
  function ->vm returns a type that implements DenotedData."
  (->vm [this store]))

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
