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
  			[msgqueue.rabbitmq.receiver]
  			[msgqueue.rabbitmq.worker-usage])
  (:gen-class :main true))   ; need gen-class :main in order for lein run


(def test-queue-name "test_queue")

(defn test-rabbit [role]
  (condp = (first role)
    "receiver" (test-receive test-queue-name)
    "sender"   (test-send test-queue-name "hello world")))

(defn test-worker [role]
  (condp = (first role)
    "workerhandler" (start-handler-process)
    "workertask"    (worker-task)))

; main entry, refered from prj.clj and lein run will execute.
(defn -main [& args]
  (prn "  >>> starting message queue " args " <<< ")
  ;(test-rabbit args))
  (test-worker args))
  