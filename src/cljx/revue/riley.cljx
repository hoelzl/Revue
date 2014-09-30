(ns revue.riley
  "A compiler from a simple Lisp/Scheme/Clojure-like language to the
  Revue VM."
  (:refer-clojure :exclude (compile))
  (:require [revue.util :as util]
            [revue.vm :as vm]))

(defn warn [msg]
  (util/warn "Compiler Warning:" msg))

(defn gen [inst & args]
  (list (list* inst args)))

(defn gen-seq [& insts]
  (apply concat insts))

(defn compile-dispatch
  "Compute a dispatch value for the `compile` function"
  [form env val? more?]
  (cond
   (nil? form) ::nil
   (util/boolean? form) ::boolean
   (symbol? form) ::symbol
   (util/atomic? form) ::atom
   :else
   (case (first form)
     quote ::quote
     define ::definition
     set! ::setter
     begin ::sequence
     if ::conditional
     lambda ::closure
     ::function-application)))

(defmulti compile
  "Compile Riley code into bytecode instructions for the Revue VM."
  compile-dispatch)

(defmethod compile ::nil [form env val? more?]
  (gen 'CONST nil))

(defmethod compile ::boolean [form env val? more?]
  (gen 'CONST form))

(defn gen-var [var env])

(defmethod compile ::symbol [var env val? more?]
  (if val?
    (gen-seq (gen-var var env)
             (when-not more? (gen 'RETURN)))
    ()))

;;; Evaluate this (e.g., with C-x C-e in Cider) to run the tests for
;;; this namespace:
;;; (clojure.test/run-tests 'revue.riley-test)
;;; Evaluate this to run the test for all namespaces:
;;; (clojure.test/run-all-tests #"^revue\..*-test")

