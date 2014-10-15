(defproject revue "0.0.3-SNAPSHOT"
  :description "REVUE: REVersible User Experiences"
  
  :url "https://github.com/hoelzl/Revue"
  
  :licenses [{:name "MIT License"
              :url "http://opensource.org/licenses/MIT"
              :distribution :repo}
             {:name "Eclipse Public License - v 1.0"
              :url "http://www.eclipse.org/legal/epl-v10.html"
              :distribution :repo}]
  :pom-addition [:developers [:developer
                              [:id "mhoelzl"]
                              [:name "Matthias Hoelzl"]
                              [:url "https://github.com/hoelzl"]
                              [:email "tc@xantira.com"]]]
  :repositories [["local" "file:///Users/tc/.m2/repository"]]
  
  :exclusions [org.clojure/clojure org.clojure/clojurescript]
  :jar-exclusions [#"\.cljx|\.swp|\.swo|\.DS_Store"]
  :source-paths ["src/cljx"]
  :test-paths ["target/test-classes"]
  :dependencies [[org.clojure/clojure "1.7.0-alpha2"]
                 [org.clojure/clojurescript "0.0-2371"]
                 [org.clojure/tools.reader "0.8.9"]]

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

  :prep-tasks [["cljx" "once"] "javac" "compile"]

  :cljsbuild {:test-commands {"node" ["node" :node-runner "target/testable.js"]}
              :builds {:test-js
                       {:source-paths ["target/classes" "target/test-classes"]
                        :compiler {:output-to "target/testable.js"
                                   :optimizations :advanced
                                   :pretty-print true
                                   :libs [""]}}
                       :release-js
                       {:source-paths ["target/classes"]
                        :compiler {:output-dir "target/release"
                                   :output-to "target/release/revue.js"
                                   :source-map "target/release/revue.map"
                                   :optimizations :advanced
                                   :pretty-print false
                                   :libs [""]}}}}

  :profiles {:dev {:plugins [[com.cemerick/austin "0.2.0-SNAPSHOT"]
                             [com.cemerick/piggieback "0.1.4-SNAPSHOT"]
                             [com.cemerick/clojurescript.test "0.3.1"
                              :exclusions [com.google.guava/guava]]
                             [org.clojars.cemerick/cljx "0.5.0-SNAPSHOT"]
                             [lein-cljsbuild "1.0.4-SNAPSHOT"]]
                   :repl-options {:nrepl-middleware [cemerick.piggieback/wrap-cljs-repl]}
                   :dependencies [[com.cemerick/double-check "0.5.8-SNAPSHOT"]]
                   ;; :injections [(require '[weasel.repl :as repl])
                   ;;              (defn start-weasel []
                   ;;                (if-not (repl/alive?)
                   ;;                  (repl/connect "ws://localhost:9001")))]
                   :aliases {"cleantest" ["do" "clean," "cljx" "once," "test,"
                                          "cljsbuild" "test"]
                             "jtest" ["do" "cljx" "once," "test"]
                             "jstest" ["do" "cljx" "once," "cljsbuild" "test"]
                             "deploy" ["do" "clean," "cljx" "once," "deploy" "clojars"]}}})
