;;
;; add cljaction-test ns path prefix.
;;
(ns cljaction-test.chapter14-rabbitmq
  (:import (com.rabbitmq.client ConnectionParameters ConnectionFactory QueueingConsumer)))

(def ^:dynamic *rabbit-connection*)

(defn new-connection [q-host q-username q-password]
  (let [params (doto (ConnectionParameters.)
         (.setVirtualHost "/")
         (.setUsername q-username)
         (.setPassword q-password))]
    (prn "new-connection ...")
    (.newConnection (ConnectionFactory. params) q-host)))

(defmacro with-rabbit [[mq-host mq-username mq-password] & exprs]
  `(with-open [connection# (new-connection ~mq-host ~mq-username ~mq-password)]
     (binding [*rabbit-connection* connection#]
       (do ~@exprs))))

(defn send-message [routing-key message-object]
  (with-open [channel (.createChannel *rabbit-connection*)]
    (.basicPublish channel "" routing-key nil (.getBytes (str message-object)))))

(defn delivery-from [channel consumer]
  ;; consumer.nextDelivery is a blocking call, will block here if no msg in queue.
  (let [delivery (.nextDelivery consumer)]
    (.basicAck channel (.. delivery getEnvelope getDeliveryTag) false)
    (String. (.getBody delivery))))

(defn consumer-for [channel queue-name]
  (let [consumer (QueueingConsumer. channel)]
    (.queueDeclare channel queue-name)
    (.basicConsume channel queue-name consumer)
    consumer))

(defn next-message-from [queue-name]
  (with-open [channel (.createChannel *rabbit-connection*)]
    (let [consumer (consumer-for channel queue-name)]
      (println "next-message-from" queue-name)
      (delivery-from channel consumer))))

(defn- lazy-message-seq [channel consumer]
  (lazy-seq
   (let [message (delivery-from channel consumer)]
     (cons message (lazy-message-seq channel consumer)))))

(defn message-seq [queue-name]
  (let [channel (.createChannel *rabbit-connection*)
        consumer (consumer-for channel queue-name)]
    (lazy-message-seq channel consumer)))
