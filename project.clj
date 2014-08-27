(defproject revue "0.0.1-SNAPSHOT"
  :description "REVUE: REVersible User Experiences"
  
  :url "https://github.com/hoelzl/Revue"
  
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :pom-addition [:developers [:developer
                              [:id "mhoelzl"]
                              [:name "Matthias Hoelzl"]
                              [:url "https://github.com/hoelzl"]
                              [:email "tc@xantira.com"]]]
  :repositories [["local" "file:///Users/tc/.m2/repository"]]

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
                             "deploy!" ["do" "clean," "cljx" "once," "deploy" "clojars"]}}})
