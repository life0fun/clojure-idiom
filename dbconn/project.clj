(defproject dbconn "1.0.0-SNAPSHOT"
  :description "storage connection with mysql or redis"
  :repositories {"local" ~(str (.toURI (java.io.File. "local_jars")))}
  :dependencies [
    [org.clojure/clojure "1.3.0"]
    [org.clojure/clojure-contrib "1.2.0"]   ; for clojure.contrib.sql
    [korma "0.3.0-RC5"]    ; awesome korma
    [org.clojure/java.jdbc "0.2.3"]         ; jdbc 
    [mysql/mysql-connector-java "5.1.6"]    ; mysql jdbc driver
    [org.postgresql/postgresql "9.2-1002-jdbc4"]  ; postgresql
    [org.xerial/sqlite-jdbc "3.7.2"]  ; sqlite
    [clj-redis "0.0.12"]   ;
    [clojure-rabbitmq "0.2.1"]
    [org.clojure/data.json "0.2.2"]    ;; json package
    ]
  :main dbconn.core)   ; set main entry
