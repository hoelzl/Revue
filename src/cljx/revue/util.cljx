;;; Utilities used by the compiler and the virtual machine

(ns revue.util)

;;; Quick and dirty implementation of warnings and errors.
;;; TODO: Replace this with a better implementation later on

(defn error [msg]
  (throw (#+clj java.lang.Exception. #+cljs js/Error.
                msg)))

(def ^:dynamic *print-warnings* true)

(defn warn
  ([msg]
     (warn "REVUE Warning:" msg))
  ([prefix msg]
     (when *print-warnings*
       (println prefix msg)
       (flush))))

;;; Evaluate this (e.g., with C-x C-e in Cider) to run the tests for
;;; this namespace:
;;; (clojure.test/run-tests 'revue.util-test)
;;; Evaluate this to run the test for all namespaces:
;;; (clojure.test/run-all-tests #"^revue\..*-test")

