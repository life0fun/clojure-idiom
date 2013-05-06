(ns dbconn.core
	(:require [clojure.string :as str])
  (:require [clojure.java.jdbc :as sql])
  (:import [java.io FileReader]
           [java.util Map Map$Entry List ArrayList Collection Iterator HashMap])
  (:require [clj-redis.client :as redis]) 		; bring in redis namespace
  (:use [dbconn.redis-datamapper ])
  (:use [dbconn.redis-persister])
  (:gen-class :main true))    ; bring in redis namespace


; simple test redis
(defn test-redis []
	; set key
	(redis/set db "foo" "bar")
	(prn (redis/get db "foo"))
	(redis/rpush db "cars" "celica")
	(redis/rpush db "cars" "accord")
	(prn (redis/lrange db "cars" 0 -1))
	(redis/sadd db "lang" "clojure")
	(redis/sadd db "lang" "javascript")
	(prn (redis/smembers db "lang")))

; use def type macro to create a data type to encap data from/to redis
; we call this redis-type and we can instantiate redis objects and use builder pattern.
(def-redis-type mobile-user
	(string-type :id :name :start-time :parent-id)
	(list-type :apps)
	(parimary-key :id :parent-id)
	(format :json)
	(key-separator "##"))

(defn test-mobile-user []
	;(let [m (mobile-user :new)])
	(prn (mobile-user :name))
	(mobile-user :format)
	(let [m (mobile-user :new)]  ; instantiate and using redis object.
		(m :set! :name "sparkle")
		(m :get :name)
		(m :save!)))


; the main 
(defn -main []
 	(prn " >>>> starting dbconn.core main <<<<< ")
	(test-redis)
	(test-mobile-user))


