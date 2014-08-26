(defproject revue "0.0.1-SNAPSHOT"
  :description "# REVersible User Experiences

*REVUE* is an interpreter/virtual machine that returns a trace of a
program's execution.  This trace can be used for visualizing the inner
workings of a program, animating algorithms, etc.

More precisely, `step`, the central function of *REVUE* interprets a
Scheme-like intermediate language by taking a program state consisting
of

* a form to be evaluated,
* an environment
* a store
* a continuation
* the value of the previous step

and returning a new state of the same form.  To evaluate the program,
we can then simply iterate the `step` function to obtain an infinite
sequence of states.  (Calling `step` with the final state of a
terminating computation simply returns the same state again.)  For
visualizing terminating algorithms, a convenience function `interp` is
provided that runs the VM to completion and returns the resulting
seqence of states."
  
  :url "https://github.com/hoelzl/Revue"
  
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :jar-exclusions [#"\.cljx|\.swp|\.swo|\.DS_Store"]
  :source-paths ["src/cljx"]
  :test-paths ["target/test-classes"]
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/clojurescript "0.0-2311"]]

  :cljx {:builds [{:source-paths ["src/cljx"]
                   :output-path "target/classes"
                   :rules :clj}
                  
                  {:source-paths ["src/cljx"]
                   :output-path "target/classes"
                   :rules :cljs}
                  
                  {:source-paths ["test/cljx"]
                   :output-path "target/test-classes"
                   :rules :clj}
                  
                  {:source-paths ["test/cljx"]
                   :output-path "target/test-classes"
                   :rules :cljs}]}

  :cljsbuild {:test-commands {"node" ["node" :node-runner "target/testable.js"]}
              :builds [{:source-paths ["target/classes" "target/test-classes"]
                        :compiler {:output-to "target/testable.js"
                                   :optimizations :advanced
                                   :pretty-print true
                                   :libs [""]}}]}

  :profiles {:dev {:plugins [[com.cemerick/austin "0.1.5-SNAPSHOT"]
                             [com.cemerick/clojurescript.test "0.3.1"]
                             [com.keminglabs/cljx "0.4.0"]
                             [lein-cljsbuild "1.0.3"]]
                   :dependencies [[com.cemerick/double-check "0.5.8-SNAPSHOT"]]
                   :aliases {"cleantest" ["do" "clean," "cljx" "once," "test,"
                                          "cljsbuild" "test"]
                             "jtest" ["do" "cljx" "once," "test"]
                             "jstest" ["do" "cljx" "once," "cljsbuild" "test"]
                             "deploy" ["do" "clean," "cljx" "once," "deploy" "clojars"]}}})
