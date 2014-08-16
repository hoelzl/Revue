;;; Test for the revue.vm namespace.

(ns revue.mem-test
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
            [revue.mem :as mem]))


;;; Variables that control the number of tests executed
;;; ===================================================

;;; Set this to false to speed up interactive testing.
(def ^:dynamic *large-tests* true)

;;; For some tests, attribute-based test generation creates huge data
;;; structures if we are running them with a high number of
;;; repetitions.  To limit the amount of time needed for interactive
;;; testing, we limit ourselves to 50 repetitions when testing
;;; interactively.  In addition, we reduce the number of repetitions
;;; for ClojureScript, since test are running only in a single thread
;;; there.
(def ^:dynamic *large-number-of-repetitions*
  #+clj (if (find-ns 'clojure.repl) 50 75)
  #+cljs 50)


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

;;; Generator for maps; keys are always keywords.
(defn sized-map [inner-type]
  (fn [size]
    (if (zero? size)
      inner-type
      (gen/one-of [inner-type
                   (gen/map gen/keyword
                            (gen/resize (quot size 2)
                                        (gen/sized (sized-map inner-type))))]))))

(def nested-map
  (gen/sized (sized-map expressed-simple-type)))


(defn sized-collection [inner-type]
  (fn [size]
    (if (zero? size)
      inner-type
      (gen/one-of [inner-type
                   (gen/list (gen/resize (quot size 2)
                                         (gen/sized (sized-collection inner-type))))
                   (gen/vector (gen/resize (quot size 2)
                                           (gen/sized (sized-collection inner-type))))
                   (gen/map gen/keyword
                            (gen/resize (quot size 2)
                                        (gen/sized (sized-map inner-type))))]))))

(def nested-collection
  (gen/sized (sized-collection expressed-simple-type)))


;;; Tests for Conversions of Stored Values to Represented Values
;;; =============================================================


(deftest ->clojure-bool
  (testing "Convert stored Booleans to Clojure."
    (is (= (mem/->clojure true []) true))
    (is (= (mem/->clojure false []) false))))

(deftest ->clojure-number
  (testing "Convert stored numbers to Clojure."
    (is (= (mem/->clojure 0 []) 0))
    (is (= (mem/->clojure 1 []) 1))
    (is (= (mem/->clojure -1 []) -1))
    #+clj
    (is (= (mem/->clojure 1/2 []) 1/2))
    (is (= (mem/->clojure 0.5 []) 0.5))))

(defspec ->clojure-number-tc
  100
  (testing
      "Random tests for conversion of stored numbers to Clojure."
    (prop/for-all [n gen/int]
      (is (= (mem/->clojure n []) n)))))

(defspec ->clojure-symbol-tc
  100
  (testing
      "Random tests for conversion of stored symbols to Clojure."
    (prop/for-all [k gen/keyword]
      (let [s (symbol (str (name k)))]
        (is (= (mem/->clojure s) s))))))

(defspec ->clojure-keyword-tc
  100
  (testing
      "Random tests for conversion of stored keywords to Clojure."
    (prop/for-all [k gen/keyword]
      (is (= (mem/->clojure k) k)))))

(defspec ->clojure-string-tc
  100
  (testing
      "Random tests for conversion of stored strings to Clojure."
    (prop/for-all [s gen/string]
      (is (= (mem/->clojure s) s)))))

(deftest vm-cons-01
  (testing "Create VmCons instances."
    (let [[vm-cons state] (mem/new-cons [1 ()] [])]
      (is (= (:address vm-cons) 0))
      (is (= state [1 ()])))))

;;; The values in the following tests depend on the details of the
;;; allocator and may therefore change if the implementation of cons
;;; pairs is changed.  Their main purpose is to check that
;;; approximately the correct amount of storage is allocated.

(deftest vm-cons-1000
  (testing "Create large VmCons instance"
    (let [c (into () (repeat 1000 1))
          [v store] (mem/->vm c)]
      (is (= (:address v) 1998))
      (is (= (mem/->clojure v store) c)))))

(deftest vm-cons-1000-complex
  (testing "Create large, complex VmCons instance"
    (let [c (into () (repeat 1000 [1 '(2)]))
          [v store] (mem/->vm c)]
      (is (= (:address v) 5998))
      (is (= (mem/->clojure v store) c)))))

(deftest vm-cons-1000
  (testing "Create large VmCons instance"
    (let [c (into () (repeat 5000 1))
          [v store] (mem/->vm c)]
      (is (= (:address v) 9998))
      (is (= (mem/->clojure v store) c)))))

(deftest vm-cons-5000-complex
  (testing "Create large, complex VmCons instance"
    (let [c (into () (repeat 5000 [1 '(2)]))
          [v store] (mem/->vm c)]
      (is (= (:address v) 29998))
      (is (= (mem/->clojure v store) c)))))

(deftest vm-cons-250000
  (when *large-tests*
    (testing "Create large VmCons instance"
      (let [c (into () (repeat 250000 1))
          [v store] (mem/->vm c)]
      (is (= (:address v) 499998))
      (is (= (mem/->clojure v store) c))))))

(deftest vm-cons-250000-complex
  (when *large-tests*
    (testing "Create large VmCons instance"
      (let [c (into () (repeat 250000 [1 '(2)]))
          [v store] (mem/->vm c)]
      (is (= (:address v) 1499998))
      (is (= (mem/->clojure v store) c))))))

(deftest ->clojure-cons
  (testing "Convert VmCons to Clojure."
    (let [[vm-cons store] (mem/new-cons [1 ()] [])
          list (mem/->clojure vm-cons store)]
      (is (= list '(1))))
    (let [[vm-cons store] (let [[obj store] (mem/new-cons [2 ()] [])]
                            (mem/new-cons [1 obj] store))
          list (mem/->clojure vm-cons store)]
      (is (= list '(1 2))))))

(deftest vm-vector-01
  (testing "Create VmVector instances."
    (let [[vm-vector state] (mem/new-vector [] [])]
      (is (= (:address vm-vector) 0))
      (is (= (:size vm-vector) 0))
      (is (= state [])))    
    (let [[vm-vector state] (mem/new-vector [1 2 3] [])]
      (is (= (:address vm-vector) 0))
      (is (= (:size vm-vector) 3))
      (is (= state [1 2 3])))
    (let [[vm-vector state] (mem/new-vector (repeat 5 false) [])]
      (is (= (:address vm-vector) 0))
      (is (= (:size vm-vector) 5))
      (is (= state [false false false false false])))))

(deftest vm-vector-1000
  (testing "Create large VmVector instance"
    (is (= (:size (first (mem/->vm (into [] (repeat 1000 1))))) 1000))))

(deftest vm-vector-1000-complex
  (testing "Create large VmVector instance"
    (is (= (:size (first (mem/->vm (into [] (repeat 1000 [1 '(2)]))))) 1000))))

(deftest vm-vector-5000
  (testing "Create large VmVector instance"
    (is (= (:size (first (mem/->vm (into [] (repeat 5000 1))))) 5000))))

(deftest vm-vector-5000-complex
  (testing "Create large VmVector instance"
    (is (= (:size (first (mem/->vm (into [] (repeat 5000 [1 '(2)]))))) 5000))))

(deftest vm-vector-250000
  (testing "Create large VmVector instance"
    (is (= (:size (first (mem/->vm (into [] (repeat 250000 1))))) 250000))))

(deftest vm-vector-250000-complex
  (testing "Create large VmVector instance"
    (is (= (:size (first (mem/->vm (into [] (repeat 250000 [1 '(2)]))))) 250000))))

(deftest vm-map-01
  (testing "Create VmMap instances"
    (let [[vm-map state] (mem/->vm {})]
      (is (= (mem/-address vm-map state) 0))
      (is (= (mem/-size vm-map state) 0))
      (is (= state [])))
    (let [[vm-map state] (mem/->vm {:a 1 :b 2})]
      ;; This is highly dependent on the particulars of the allocator
      ;; and might change in the future.
      (is (= (mem/-address vm-map state) 4))
      (is (= (mem/-size vm-map state) 2))
      ;; We are allocating four values for :a, 1, :b, 2, and two
      ;; vectors for the key-value mappings
      (is (= (count state) 6)))))

(deftest ->clojure-vector
  (testing "Convert VmVector to Clojure"
    (let [check (fn [v]
                  (is (= (apply mem/->clojure (mem/new-vector v [])) v)))]
      (check [])
      (check [1])
      (check [2 3 4])
      (check (repeat 100 'foo)))))



;;; Tests for Conversions of represented values to the VM format.
;;; =============================================================

(deftest ->vm-bool
  (testing "Convert represeented Booleans to the VM format."
    (is (= (mem/->vm true []) [true []]))
    (is (= (mem/->vm false []) [false []]))))

(deftest ->vm-number
  (testing "Convert represeented numbers to the VM format."
    (is (= (mem/->vm 0 []) [0 []]))
    (is (= (mem/->vm 1 []) [1 []]))
    (is (= (mem/->vm -1 []) [-1 []]))
    #+clj
    (is (= (mem/->vm 1/2 []) [1/2 []]))
    (is (= (mem/->vm 0.5 []) [0.5 []]))))

(defspec ->vm-number-tc
  100
  (testing
      "Random tests for conversion of represeented numbers to the VM format."
    (prop/for-all [n gen/int]
      (is (= (mem/->vm n []) [n []])))))

(defspec ->vm-symbol-tc
  100
  (testing
      "Random tests for conversion of represeented symbols to the VM format."
    (prop/for-all [k gen/keyword]
      (let [s (symbol (str (name k)))]
        (is (= (mem/->vm s) [s []]))))))

(defspec ->vm-keyword-tc
  100
  (testing
      "Random tests for conversion of represeented keywords to the VM format."
    (prop/for-all [k gen/keyword]
      (is (= (mem/->vm k) [k []])))))

(defspec ->vm-string-tc
  100
  (testing
      "Random tests for conversion of represeented strings to the VM format1."
    (prop/for-all [s gen/string]
      (is (= (mem/->vm s) [s []])))))


(defspec ->clojure->vm-int
  100
  (testing
      "Random roundtrip test for numbers."
    (prop/for-all [n gen/int store (gen-state)]
      (is (= (apply mem/->clojure (mem/->vm n [])) n)))))

#+clj
(defspec ->clojure->vm-ratio
  100
  (testing
      "Random roundtrip test for numbers."
    (prop/for-all [n gen/ratio store (gen-state)]
      (is (= (apply mem/->clojure (mem/->vm n [])) n)))))

(defspec ->clojure->vm-symbol
  100
  (testing
      "Random roundtrip test for symbols."
    (prop/for-all [k gen/keyword store (gen-state)]
      (let [s (symbol (name k))]
        (is (= (apply mem/->clojure (mem/->vm s [])) s))))))

(defspec ->clojure->vm-keyword
  100
  (testing
      "Random roundtrip test for keywords."
    (prop/for-all [k gen/keyword store (gen-state)]
      (is (= (apply mem/->clojure (mem/->vm k [])) k)))))

(defspec ->clojure->vm-string
  100
  (testing
      "Random roundtrip test for strings."
    (prop/for-all [s gen/string store (gen-state)]
      (is (= (apply mem/->clojure (mem/->vm s [])) s)))))

(defspec ->clojure->vm-list
  (if *large-tests* *large-number-of-repetitions* 10)
  (testing
      "Random roundtrip test for lists."
    (prop/for-all [l nested-list store (gen-state)]
      (is (= (apply mem/->clojure (mem/->vm l [])) l)))))

(defspec ->clojure->vm-vector
  (if *large-tests* *large-number-of-repetitions* 10)
  (testing
      "Random roundtrip test for vectors."
    (prop/for-all [v nested-vector store (gen-state)]
      (is (= (apply mem/->clojure (mem/->vm v [])) v)))))

(defspec ->clojure->vm-map
  (if *large-tests* *large-number-of-repetitions* 10)
  (testing
      "Random roundtrip test for maps."
    (prop/for-all [v nested-map store (gen-state)]
      (is (= (apply mem/->clojure (mem/->vm v [])) v)))))

(defspec ->clojure->vm-collection
  (if *large-tests* *large-number-of-repetitions* 10)
  (testing
      "Random roundtrip test for vectors."
    (prop/for-all [c nested-collection store (gen-state)]
      #+clj ;; ClojureScript does not have the clojure.pprint namespace
      (when false ;; Set this to true to check which collections are generated.
        (println "Generated collections:")
        (clojure.pprint/pprint c)
        (flush))
      (is (= (apply mem/->clojure (mem/->vm c [])) c)))))

;;; Evaluate this (e.g., with C-x C-e in Cider) to run the tests for
;;; this namespace:
;;; (t/run-tests 'revue.mem-test)
;;; Evaluate this to run the test for all namespaces:
;;; (t/run-all-tests #"^revue\..*-test")
