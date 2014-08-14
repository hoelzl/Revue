;;; The virtual machine

(ns revue.vm
  (:require [revue.util :as util]))

(defn warn [msg]
  (util/warn "VM Warning:" msg))

(defn vm
  "Run the virtual machine."
  [prog]
  (println "Running the VM on " prog))

;;; Evaluate this (e.g., with C-x C-e in Cider) to run the tests for
;;; this namespace:
;;; (clojure.test/run-tests 'revue.vm-test)
;;; Evaluate this to run the test for all namespaces:
;;; (clojure.test/run-all-tests #"^revue\..*-test")
