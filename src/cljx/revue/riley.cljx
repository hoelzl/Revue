(ns revue.riley
  "A compiler from a simple Lisp/Scheme/Clojure-like language to the
  Revue VM."
  (:refer-clojure :exclude (compile))
  (:require [revue.util :as util]
            [revue.vm :as vm]))

(defn warn [msg]
  (util/warn "Compiler Warning:" msg))

(defn compile
  "Compile Riley code into bytecode instructions for the Revue VM."
  [code]
  'nop)

;;; Evaluate this (e.g., with C-x C-e in Cider) to run the tests for
;;; this namespace:
;;; (clojure.test/run-tests 'revue.riley-test)
;;; Evaluate this to run the test for all namespaces:
;;; (clojure.test/run-all-tests #"^revue\..*-test")

