;;; Test for the revue.vm namespace.

(ns revue.vm-test
  #+cljs (:require-macros [cemerick.cljs.test :refer (deftest testing is are)]
                          [clojure.test.check.clojure-test :refer (defspec)])
  (:require #+clj [clojure.test :as t :refer (deftest testing is are)]
            #+cljs [cemerick.cljs.test :as t]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop :include-macros true]
            #+clj [clojure.test.check.clojure-test :as ct :refer (defspec)]
            #+cljs [clojure.test.check.clojure-test :as ct]
            [revue.util :as util]
            [revue.vm :as vm]))

;;; Set this to false to speed up interactive testing.
(def ^:dynamic *large-tests* true)
(def ^:dynamic *large-number-of-repetitions*
  #+clj (if (find-ns 'clojure.repl) 50 100)
  #+cljs 50)


(deftest warn-01
  (testing "Warnings from the VM."
    (is (= (with-out-str (vm/warn "Hey!")) "VM Warning: Hey!\n"))))


;;; Generators and Other Utilities
;;; ==============================

;;; TODO: Define a real generator for stores
(defn gen-state []
  (gen/vector gen/int 0 10))


(def expressed-simple-type
  (gen/one-of [gen/int gen/string #+clj gen/ratio gen/boolean gen/keyword]))


(defn sized-list [inner-type]
  (fn [size]
    (if (zero? size)
      inner-type
      (gen/one-of [inner-type
                   (gen/list (gen/resize (quot size 2)
                                         (gen/sized (sized-list inner-type))))]))))

(def nested-list
  (gen/sized (sized-list expressed-simple-type)))

(defn sized-vector [inner-type]
  (fn [size]
    (if (zero? size)
      inner-type
      (gen/one-of [inner-type
                   (gen/vector (gen/resize (quot size 2)
                                         (gen/sized (sized-vector inner-type))))]))))

(def nested-vector
  (gen/sized (sized-vector expressed-simple-type)))


(defn sized-collection [inner-type]
  (fn [size]
    (if (zero? size)
      inner-type
      (gen/one-of [inner-type
                   (gen/list (gen/resize (quot size 2)
                                         (gen/sized (sized-collection inner-type))))
                   (gen/vector (gen/resize (quot size 2)
                                           (gen/sized (sized-collection inner-type))))]))))

(def nested-collection
  (gen/sized (sized-collection expressed-simple-type)))


;;; Tests for Conversions of Stored Values to Represented Values
;;; =============================================================


(deftest ->clojure-bool
  (testing "Convert stored Booleans to Clojure."
    (is (= (vm/->clojure true []) true))
    (is (= (vm/->clojure false []) false))))

(deftest ->clojure-number
  (testing "Convert stored numbers to Clojure."
    (is (= (vm/->clojure 0 []) 0))
    (is (= (vm/->clojure 1 []) 1))
    (is (= (vm/->clojure -1 []) -1))
    #+clj
    (is (= (vm/->clojure 1/2 []) 1/2))
    (is (= (vm/->clojure 0.5 []) 0.5))))

(defspec ->clojure-number-tc
  100
  (testing
      "Random tests for conversion of stored numbers to Clojure."
    (prop/for-all [n gen/int]
      (is (= (vm/->clojure n []) n)))))

(defspec ->clojure-symbol-tc
  100
  (testing
      "Random tests for conversion of stored symbols to Clojure."
    (prop/for-all [k gen/keyword]
      (let [s (symbol (str (name k)))]
        (is (= (vm/->clojure s) s))))))

(defspec ->clojure-keyword-tc
  100
  (testing
      "Random tests for conversion of stored keywords to Clojure."
    (prop/for-all [k gen/keyword]
      (is (= (vm/->clojure k) k)))))

(defspec ->clojure-string-tc
  100
  (testing
      "Random tests for conversion of stored strings to Clojure."
    (prop/for-all [s gen/string]
      (is (= (vm/->clojure s) s)))))

(deftest vm-cons-01
  (testing "Create VmCons instances."
    (let [[vm-cons state] (vm/new-cons [1 ()] [])]
      (is (= (:address vm-cons) 0))
      (is (= state [1 ()])))))

;;; The values in the following tests depend on the details of the
;;; allocator and may therefore change if the implementation of cons
;;; pairs is changed.  Their main purpose is to check that
;;; approximately the correct amount of storage is allocated.

(deftest vm-cons-1000
  (testing "Create large VmCons instance"
    (let [c (into () (repeat 1000 1))
          [v store] (vm/->vm c)]
      (is (= (:address v) 1998))
      (is (= (vm/->clojure v store) c)))))

(deftest vm-cons-1000-complex
  (testing "Create large, complex VmCons instance"
    (let [c (into () (repeat 1000 [1 '(2)]))
          [v store] (vm/->vm c)]
      (is (= (:address v) 5998))
      (is (= (vm/->clojure v store) c)))))

(deftest vm-cons-1000
  (testing "Create large VmCons instance"
    (let [c (into () (repeat 5000 1))
          [v store] (vm/->vm c)]
      (is (= (:address v) 9998))
      (is (= (vm/->clojure v store) c)))))

(deftest vm-cons-5000-complex
  (testing "Create large, complex VmCons instance"
    (let [c (into () (repeat 5000 [1 '(2)]))
          [v store] (vm/->vm c)]
      (is (= (:address v) 29998))
      (is (= (vm/->clojure v store) c)))))

(deftest vm-cons-250000
  (when *large-tests*
    (testing "Create large VmCons instance"
      (let [c (into () (repeat 250000 1))
          [v store] (vm/->vm c)]
      (is (= (:address v) 499998))
      (is (= (vm/->clojure v store) c))))))

(deftest vm-cons-250000-complex
  (when *large-tests*
    (testing "Create large VmCons instance"
      (let [c (into () (repeat 250000 [1 '(2)]))
          [v store] (vm/->vm c)]
      (is (= (:address v) 1499998))
      (is (= (vm/->clojure v store) c))))))

(deftest ->clojure-cons
  (testing "Convert VmCons to Clojure."
    (let [[vm-cons store] (vm/new-cons [1 ()] [])
          list (vm/->clojure vm-cons store)]
      (is (= list '(1))))
    (let [[vm-cons store] (let [[obj store] (vm/new-cons [2 ()] [])]
                            (vm/new-cons [1 obj] store))
          list (vm/->clojure vm-cons store)]
      (is (= list '(1 2))))))

(deftest vm-vector-01
  (testing "Create VmVector instances."
    (let [[vm-vector state] (vm/new-vector [] [])]
      (is (= (:address vm-vector) 0))
      (is (= (:size vm-vector) 0))
      (is (= state [])))    
    (let [[vm-vector state] (vm/new-vector [1 2 3] [])]
      (is (= (:address vm-vector) 0))
      (is (= (:size vm-vector) 3))
      (is (= state [1 2 3])))
    (let [[vm-vector state] (vm/new-vector (repeat 5 false) [])]
      (is (= (:address vm-vector) 0))
      (is (= (:size vm-vector) 5))
      (is (= state [false false false false false])))))

(deftest vm-vector-1000
  (testing "Create large VmVector instance"
    (is (= (:size (first (vm/->vm (into [] (repeat 1000 1))))) 1000))))

(deftest vm-vector-1000-complex
  (testing "Create large VmVector instance"
    (is (= (:size (first (vm/->vm (into [] (repeat 1000 [1 '(2)]))))) 1000))))

(deftest vm-vector-5000
  (testing "Create large VmVector instance"
    (is (= (:size (first (vm/->vm (into [] (repeat 5000 1))))) 5000))))

(deftest vm-vector-5000-complex
  (testing "Create large VmVector instance"
    (is (= (:size (first (vm/->vm (into [] (repeat 5000 [1 '(2)]))))) 5000))))

(deftest vm-vector-250000
  (testing "Create large VmVector instance"
    (is (= (:size (first (vm/->vm (into [] (repeat 250000 1))))) 250000))))

(deftest vm-vector-250000-complex
  (testing "Create large VmVector instance"
    (is (= (:size (first (vm/->vm (into [] (repeat 250000 [1 '(2)]))))) 250000))))


(deftest ->clojure-vector
  (testing "Convert VmVector to Clojure"
    (let [check (fn [v]
                  (is (= (apply vm/->clojure (vm/new-vector v [])) v)))]
      (check [])
      (check [1])
      (check [2 3 4])
      (check (repeat 100 'foo)))))



;;; Tests for Conversions of represented values to the VM format.
;;; =============================================================

(deftest ->vm-bool
  (testing "Convert represeented Booleans to the VM format."
    (is (= (vm/->vm true []) [true []]))
    (is (= (vm/->vm false []) [false []]))))

(deftest ->vm-number
  (testing "Convert represeented numbers to the VM format."
    (is (= (vm/->vm 0 []) [0 []]))
    (is (= (vm/->vm 1 []) [1 []]))
    (is (= (vm/->vm -1 []) [-1 []]))
    #+clj
    (is (= (vm/->vm 1/2 []) [1/2 []]))
    (is (= (vm/->vm 0.5 []) [0.5 []]))))

(defspec ->vm-number-tc
  100
  (testing
      "Random tests for conversion of represeented numbers to the VM format."
    (prop/for-all [n gen/int]
      (is (= (vm/->vm n []) [n []])))))

(defspec ->vm-symbol-tc
  100
  (testing
      "Random tests for conversion of represeented symbols to the VM format."
    (prop/for-all [k gen/keyword]
      (let [s (symbol (str (name k)))]
        (is (= (vm/->vm s) [s []]))))))

(defspec ->vm-keyword-tc
  100
  (testing
      "Random tests for conversion of represeented keywords to the VM format."
    (prop/for-all [k gen/keyword]
      (is (= (vm/->vm k) [k []])))))

(defspec ->vm-string-tc
  100
  (testing
      "Random tests for conversion of represeented strings to the VM format1."
    (prop/for-all [s gen/string]
      (is (= (vm/->vm s) [s []])))))


(defspec ->clojure->vm-int
  100
  (testing
      "Random roundtrip test for numbers."
    (prop/for-all [n gen/int store (gen-state)]
      (is (= (apply vm/->clojure (vm/->vm n [])) n)))))

#+clj
(defspec ->clojure->vm-ratio
  100
  (testing
      "Random roundtrip test for numbers."
    (prop/for-all [n gen/ratio store (gen-state)]
      (is (= (apply vm/->clojure (vm/->vm n [])) n)))))

(defspec ->clojure->vm-symbol
  100
  (testing
      "Random roundtrip test for symbols."
    (prop/for-all [k gen/keyword store (gen-state)]
      (let [s (symbol (name k))]
        (is (= (apply vm/->clojure (vm/->vm s [])) s))))))

(defspec ->clojure->vm-keyword
  100
  (testing
      "Random roundtrip test for keywords."
    (prop/for-all [k gen/keyword store (gen-state)]
      (is (= (apply vm/->clojure (vm/->vm k [])) k)))))

(defspec ->clojure->vm-string
  100
  (testing
      "Random roundtrip test for strings."
    (prop/for-all [s gen/string store (gen-state)]
      (is (= (apply vm/->clojure (vm/->vm s [])) s)))))

(defspec ->clojure->vm-list
  (if *large-tests* *large-number-of-repetitions* 10)
  (testing
      "Random roundtrip test for lists."
    (prop/for-all [l nested-list store (gen-state)]
      (let [len (count (flatten l))]
        (when (> len 10000)
          (println "Testing with list length" len)))
      (is (= (apply vm/->clojure (vm/->vm l [])) l)))))

(defspec ->clojure->vm-vector
  (if *large-tests* *large-number-of-repetitions* 10)
  (testing
      "Random roundtrip test for vectors."
    (prop/for-all [v nested-vector store (gen-state)]
      (let [len (count (flatten v))]
        (when (> len 10000)
          (println "Testing with vector length" len)))
      (is (= (apply vm/->clojure (vm/->vm v [])) v)))))

(defspec ->clojure->vm-collection
  (if *large-tests* *large-number-of-repetitions* 10)
  (testing
      "Random roundtrip test for vectors."
    (prop/for-all [c nested-collection store (gen-state)]
      (let [len (count (flatten c))]
        (when (> len 10000)
          (println "Testing with collection length" len)))
      (is (= (apply vm/->clojure (vm/->vm c [])) c)))))


;;; Evaluate this (e.g., with C-x C-e in Cider) to run the tests for
;;; this namespace:
;;; (clojure.test/run-tests 'revue.vm-test)
;;; Evaluate this to run the test for all namespaces:
;;; (clojure.test/run-all-tests #"^revue\..*-test")
