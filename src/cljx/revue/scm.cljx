;;; Compiler for a simple Scheme-like language to the REVUE VM.

(ns revue.scm
  (:refer-clojure :exclude (compile))
  (:require [revue.util :as util]
            [revue.vm :as vm]))

(defn compile
  "Compile SCM code into bytecode instructions for the Revue VM."
  [code]
  'nop)

;;; Evaluate this (e.g., with C-x C-e in Cider) to run the tests for
;;; this namespace:
;;; (clojure.test/run-tests 'revue.scm-test)

