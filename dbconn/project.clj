(defproject dbconn "1.0.0-SNAPSHOT"
  :description "storage connection with mysql or redis"
  :repositories {"local" ~(str (.toURI (java.io.File. "local_jars")))}
  :dependencies [
    [org.clojure/clojure "1.3.0"]
    [org.clojure/clojure-contrib "1.2.0"]   ;; for clojure.contrib.sql
    [org.clojure/java.jdbc "0.0.6"]         ;; jdbc 
    [mysql/mysql-connector-java "5.1.6"]
    [clj-redis "0.0.12"]   ;
    [clojure-rabbitmq "0.2.1"]
    ]
  :main dbconn.core)   ; set main entry
