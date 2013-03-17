;;
;; http://www.pgrs.net/2011/10/30/using-local-jars-with-leiningen/
;;
(defproject cljaction-test "1.0.0-SNAPSHOT"
  :description "executing clojure in action source code"
  :dependencies [[org.clojure/clojure "1.3.0"]
;;                 [clojure-rabbitmq "0.2.1"]
                ]
  :repositories {"local" ~(str (.toURI (java.io.File. "local_jars")))}
  :main cljaction-test.core)
