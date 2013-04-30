(ns dbconn.core
	(:require [clojure.string :as str])
  (:require [clojure.java.jdbc :as sql])
  (:import [java.io FileReader]
           [java.util Map Map$Entry List ArrayList Collection Iterator HashMap])
  (:require [clj-redis.client :as redis]) 					; bring in redis namespace
  (:gen-class :main true))    ; bring in redis namespace

(def db (redis/init))   ; connection reted is threadsafe, backed up conn pool

; simple test redis
(defn test-redis []
	; redis redis
	(redis/set db "foo" "bar")
	(prn (redis/get db "foo")))

; the main 
(defn -main []
 	(prn " >>>> starting dbconn.core main <<<<< ")
	(test-redis))


