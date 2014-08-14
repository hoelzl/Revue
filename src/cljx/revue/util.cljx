;;; Utilities used by the compiler and the virtual machine

(ns revue.util)

(defn util [& args]
  (println "Utility called:" args))

;;; Evaluate this (e.g., with C-x C-e in Cider) to run the tests for
;;; this namespace:
;;; (clojure.test/run-tests 'revue.util-test)

