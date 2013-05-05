;; the main entry for message queue

(ns msgqueue.core
	(:require [clojure.string :as str])
  (:require [clojure.java.jdbc :as sql])
  (:import [java.io FileReader]
           [java.util Map Map$Entry List ArrayList Collection Iterator HashMap])
  (:use [msgqueue.util])   ; use util namespace without fully qualified name.
  (:use [msgqueue.rabbitmq.rabbitmq]
  			[msgqueue.rabbitmq.worker] 
  			[msgqueue.rabbitmq.sender]
  			[msgqueue.rabbitmq.receiver])
  (:gen-class :main true))   ; need gen-class :main in order for lein run


;; main entry, refered from prj.clj and lein run will execute.
(defn -main []
  (prn "  >>> starting message queue listening <<< ")
  (receiver-recv))
