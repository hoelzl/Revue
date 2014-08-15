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

(defn boolean? [x]
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

