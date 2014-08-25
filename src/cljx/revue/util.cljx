;;; Utilities used by the compiler and the virtual machine

(ns revue.util)

;;; Warnings and Errors
;;; ====================

;;; Quick and dirty implementation of warnings and errors.

;;; Error is a simple wrapper around `throw' that provides a suitable
;;; class for Clojure and ClojureScript.  Currently there is no way to
;;; provide specialized classes for different error types.

;;; TODO: Replace this with a better implementation later on

(defn error [msg]
  (throw (#+clj java.lang.Exception. #+cljs js/Error.
                msg)))

;;; Warnings are simply printed to the standard output stream.  Each
;;; module that reports warnings should define its own `warn' function
;;; that calls the two-argument `warn' function from this module.

(def ^:dynamic *print-warnings* true)

(defn warn
  ([msg]
     (warn "REVUE Warning:" msg))
  ([prefix msg]
     (when *print-warnings*
       (println prefix msg)
       (flush))))


;;; Utilities for sequences
;;; =======================

(defn singleton?
  "Returns true iff `coll' contains exactly one element, 
i.e., (singleton? x) is always equivalent to (= (count x) 1)"
  [coll]
  (and (not (empty? coll))
       (not (next coll))))

(defn maybe-add
  "Wrap the expressions `exps' in a list starting with `op', if their
  length is greater than one.  Use `if-empty' if `exps' is empty."
  ([op exps]
     (maybe-add op exps false))
  ([op exps if-empty]
     (cond (empty? exps) if-empty
           (singleton? exps) (first exps)
           :else (cons op exps))))

;;; Utilities for the VM Representation
;;; ===================================

;;; Some utilities that deal with the handling of Clojure data types
;;; by the VM.

(defn boolean?
  "Returns true if `x' is either `true' or `false', false otherwise."
  [x]
  (or (= x true) (= x false)))

(defn atomic?
  "Returns true if `d' can be represented without storage on the
  heap."
  [d]
  (cond
   (boolean? d) true
   (number? d) true
   (symbol? d) true
   (keyword? d) true
   (string? d) true
   (identical? d ()) true
   :else false))

;;; Evaluate this (e.g., with C-x C-e in Cider) to run the tests for
;;; this namespace:
;;; (clojure.test/run-tests 'revue.util-test)
;;; Evaluate this to run the test for all namespaces:
;;; (clojure.test/run-all-tests #"^revue\..*-test")

