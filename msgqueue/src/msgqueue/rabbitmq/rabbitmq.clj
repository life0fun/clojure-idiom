;;
;; add cljaction-test ns path prefix.
;; (:import (com.rabbitmq.client ConnectionParameters ConnectionFactory QueueingConsumer)))
;; ;[rabbitmq-client "1.7.0"]
;;    [com.rabbitmq/amqp-client "2.3.1"]

(ns msgqueue.rabbitmq.rabbitmq
  (:import (com.rabbitmq.client ConnectionFactory 
                                ;ConnectionParameters 
                                Connection Channel QueueingConsumer)))


; dynamic bindable variable for connection. each thread can re-bind connection
; that is private to the thread whichever rebind it.
(def ^:dynamic *rabbit-connection*)
(def DEFAULT-EXCHANGE-NAME "defaultex")
(def DEFAULT-EXCHANGE-TYPE "direct")
(def FANOUT-EXCHANGE-TYPE "fanout")


; get a uniq queue name. routing key is used rather than qname.
(defn random-queue-name []
  (str (java.util.UUID/randomUUID)))

; amqp-client 2.3.1 version.
(defn new-connection [host username password]
  (prn "new-connection :" host username password)
  (.newConnection
    (doto (ConnectionFactory.)
      (.setVirtualHost "/")
      (.setUsername username)
      (.setPassword password)
      (.setHost host))))

; this is rabbitmq-client 1.7.0 version.
; (defn new-connection [q-host q-username q-password]
;   (let [params (doto (ConnectionParameters.)
;          (.setVirtualHost "/")
;          (.setUsername q-username)
;          (.setPassword q-password))]
;     (.newConnection (ConnectionFactory. params) q-host)))

; eval exprs within a new connection.
(defmacro with-rabbit [[mq-host mq-username mq-password] & exprs]
  `(with-open [connection# (new-connection ~mq-host ~mq-username ~mq-password)]
     (binding [*rabbit-connection* connection#]
       (do ~@exprs))))

; send-message, default routing-key is queue-name
(defn send-message
  ([routing-key message-object]
     (send-message DEFAULT-EXCHANGE-NAME DEFAULT-EXCHANGE-TYPE routing-key message-object))
  ([exchange-name exchange-type routing-key message-object]
     (with-open [channel (.createChannel *rabbit-connection*)]
       (.exchangeDeclare channel exchange-name exchange-type)
       (.queueDeclare channel routing-key false false false nil)
       (.basicPublish channel exchange-name routing-key nil (.getBytes (str message-object))))))

; get the next msg from the consumer. The consumer already attached to a queue.
; this is a blocking call. Better wrap it inside a future task.
(defn delivery-from [channel consumer]
  (let [delivery (.nextDelivery consumer)]
    (.basicAck channel (.. delivery getEnvelope getDeliveryTag) false)
    (String. (.getBody delivery))))

; 
(defn consumer-for 
  ([channel queue-name]
    ; default queue name is queue routing key
    (consumer-for channel DEFAULT-EXCHANGE-NAME DEFAULT-EXCHANGE-TYPE 
                  queue-name queue-name))
  ([channel exchange-name exchange-type queue-name routing-key]
    (let [consumer (QueueingConsumer. channel)]
      (.exchangeDeclare channel exchange-name exchange-type)
      (.queueDeclare channel queue-name false false false nil)
      (.queueBind channel queue-name exchange-name routing-key) ; tell exchg queue is interest in msg with this routing key.
      (.basicConsume channel queue-name consumer)
      consumer)))

; default routing key is queue name.
(defn next-message-from
  ([queue-name]
     (next-message-from DEFAULT-EXCHANGE-NAME DEFAULT-EXCHANGE-TYPE queue-name queue-name))
  ([exchange-name exchange-type routing-key]
     (next-message-from exchange-name exchange-type (random-queue-name) routing-key))
  ([exchange-name exchange-type queue-name routing-key]
     (with-open [channel (.createChannel *rabbit-connection*)]
       (let [consumer (consumer-for channel exchange-name exchange-type queue-name routing-key)]
         (delivery-from channel consumer)))))

; abstract rabbit channel and consumer as a lazy-seq by cons next msg to the rest of msgs from queue.
(defn- lazy-message-seq [channel consumer]
  (lazy-seq
   (let [message (delivery-from channel consumer)]
     (cons message (lazy-message-seq channel consumer)))))

; abstract a rabbitmq queue as a lazy-seq
(defn message-seq 
  ([queue-name]
     (message-seq DEFAULT-EXCHANGE-NAME DEFAULT-EXCHANGE-TYPE queue-name queue-name))
  ([exchange-name exchange-type routing-key]
     (message-seq exchange-name exchange-type (random-queue-name) routing-key))
  ([exchange-name exchange-type queue-name routing-key]
     (let [channel (.createChannel *rabbit-connection*)
           consumer (consumer-for channel exchange-name exchange-type queue-name routing-key)]
       (lazy-message-seq channel consumer))))
