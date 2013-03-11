(ns chapter-protocols.rabbitmq
  (:import (com.rabbitmq.client ConnectionParameters ConnectionFactory QueueingConsumer)))

(def *rabbit-connection*)
(def DEFAULT-EXCHANGE-NAME "remote_workers")
(def DEFAULT-EXCHANGE-TYPE "direct")
(def FANOUT-EXCHANGE-TYPE "fanout")

(defn new-connection [host username password]
  (.newConnection
   (doto (ConnectionFactory.)
     (.setVirtualHost "/")
     (.setUsername username)
     (.setPassword password)
     (.setHost host))))

(defmacro with-rabbit [[mq-host mq-username mq-password] & exprs]
  `(with-open [connection# (new-connection ~mq-host ~mq-username ~mq-password)]
     (binding [*rabbit-connection* connection#]
       (do ~@exprs))))

(defn send-message
  ([routing-key message-object]
     (send-message DEFAULT-EXCHANGE-NAME DEFAULT-EXCHANGE-TYPE routing-key message-object))
  ([exchange-name exchange-type routing-key message-object]
     (with-open [channel (.createChannel *rabbit-connection*)]
       (.exchangeDeclare channel exchange-name exchange-type)
       (.queueDeclare channel routing-key false false false nil)
       (.basicPublish channel exchange-name routing-key nil (.getBytes (str message-object))))))

(defn delivery-from [channel consumer]
  (let [delivery (.nextDelivery consumer)]
    (.basicAck channel (.. delivery getEnvelope getDeliveryTag) false)
    (String. (.getBody delivery))))

(defn consumer-for [channel exchange-name exchange-type queue-name routing-key]
  (let [consumer (QueueingConsumer. channel)]
    (.exchangeDeclare channel exchange-name exchange-type)
    (.queueDeclare channel queue-name false false false nil)
    (.queueBind channel queue-name exchange-name routing-key)
    (.basicConsume channel queue-name consumer)
    consumer))

(defn random-queue-name []
  (str (java.util.UUID/randomUUID)))

(defn next-message-from
  ([queue-name]
     (next-message-from DEFAULT-EXCHANGE-NAME DEFAULT-EXCHANGE-TYPE queue-name queue-name))
  ([exchange-name exchange-type routing-key]
     (next-message-from exchange-name exchange-type (random-queue-name) routing-key))
  ([exchange-name exchange-type queue-name routing-key]
     (with-open [channel (.createChannel *rabbit-connection*)]
       (let [consumer (consumer-for channel exchange-name exchange-type queue-name routing-key)]
         (delivery-from channel consumer)))))

(defn- lazy-message-seq [channel consumer]
  (lazy-seq
   (let [message (delivery-from channel consumer)]
     (cons message (lazy-message-seq channel consumer)))))

(defn message-seq 
  ([queue-name]
     (message-seq DEFAULT-EXCHANGE-NAME DEFAULT-EXCHANGE-TYPE queue-name queue-name))
  ([exchange-name exchange-type routing-key]
     (message-seq exchange-name exchange-type (random-queue-name) routing-key))
  ([exchange-name exchange-type queue-name routing-key]
     (let [channel (.createChannel *rabbit-connection*)
           consumer (consumer-for channel exchange-name exchange-type queue-name routing-key)]
       (lazy-message-seq channel consumer))))