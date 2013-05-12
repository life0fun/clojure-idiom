(defproject msgqueue "1.0.0-SNAPSHOT"
  :description "A distributed msg queue based on rabbitmq"
  :repositories {"local" ~(str (.toURI (java.io.File. "local_jars")))}
  :dependencies [
    [org.clojure/clojure "1.3.0"]
    [org.clojure/clojure-contrib "1.2.0"]   ;; for clojure.contrib.sql
    [org.clojure/java.jdbc "0.2.3"]         ; jdbc 
    [mysql/mysql-connector-java "5.1.6"]
    [clj-redis "0.0.12"]   ;
    [clojure-rabbitmq "0.2.1"]
    ;[rabbitmq-client "1.7.0"]
    [com.rabbitmq/amqp-client "2.3.1"]
    [org.clojure/data.json "0.2.2"]    ;; json package
    ]
  :main msgqueue.core)   ; set main entry
