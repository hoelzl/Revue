(ns revue.util
  "The `revue.util` namespace contains utilities used by the rest of
  the system, i.e., currently the memory subsystem and the
  interpreter, and in the future by the compiler and the virtual
  machine."
  (:require #+cljs [cljs.reader :as reader]
            ;; Maybe use clojure.core/read-string for Clojure?
            #+clj [clojure.edn :as reader]
            #+clj [clojure.pprint :as pprint]))


;;; Fixes and Missing Libraries
;;; ===========================

#+cljs
(defn nthrest
  "Take the `n`th rest of `coll`.  Missing from ClojureScript because
  of an oversight.  Can be removed once this is fixed in
  ClojureScript."
  [coll n]
  (if (zero? n)
    coll
    (recur (rest coll) (dec n))))

(defn pprint
  "The `clojure.pprint` namespace is not yet ported to ClojureScript.
  Define a workaround while this is the case, use `clojure.pprint`
  once the implementation is done."
  [obj]
  #+clj
  (pprint/pprint obj)
  #+cljs
  (println obj))

;;; Warnings and Errors
;;; ====================

;;; Since Clojure and ClojureScript currently provide no unified story
;;; for error handling, I have implemented quick and dirty versions of
;;; the `error` and `warning` functions.  Later we will have to
;;; add a better error-handling protocol.

;;; <!--
;;; TODO: Replace these functions with more useful implementations
;;; -->

(defn error
  "The `error` function is a simple wrapper around `throw` that
  provides a suitable class for Clojure and ClojureScript.  Currently
  there is no way to provide specialized classes for different error
  types."
  [msg]
  (throw (#+clj java.lang.Exception. #+cljs js/Error.
                msg)))



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
  "The `note` function is similar to `warn`."
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
  i.e., `(singleton? x)` is always equivalent to `(= (count x) 1)`"
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

;;; Utilities for the VM representation
;;; ===================================

;;; Some utilities that deal with the handling of Clojure data types
;;; by the VM.

(defn boolean?
  "Returns true if `x` is either `true` or `false`, false otherwise.
  This is needed for choosing the correct representation in the memory
  subsystem."
  [x]
  (or (= x true) (= x false)))

(defn atomic?
  "Returns true if `d` can be represented without comsuming storage on
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

;;; Utilities for reading the program source
;;; ========================================

#+clj
(defn read-program-from-string
  "Read all forms from the input string."
  [string]
  (try
    (with-open [stream (-> (java.io.StringReader. string)
                           clojure.lang.LineNumberingPushbackReader.)]
      (loop [form (read stream false stream)
             forms []]
        (if (identical? form stream)
          forms
          (recur (read stream false stream)
                 (conj forms form)))))
    (catch java.lang.Exception e
      [::syntax-error])))

#+cljs
(defn read-program-from-string
  "Read all forms from the input string."
  [string]
  (try
    (let [stream (cljs.reader/push-back-reader string)]
      (loop [form (cljs.reader/read stream false stream nil)
             forms []]
        (if (identical? form stream)
          forms
          (recur (cljs.reader/read stream false stream nil)
                 (conj forms form)))))
    (catch js/Error e
      [::syntax-error])))

;;; <!--
;;; Evaluate this (e.g., with C-x C-e in Cider) to run the tests for
;;; this namespace:
;;; (clojure.test/run-tests 'revue.util-test)
;;; Evaluate this to run the test for all namespaces:
;;; (clojure.test/run-all-tests #"^revue\..*-test")
;;; -->

