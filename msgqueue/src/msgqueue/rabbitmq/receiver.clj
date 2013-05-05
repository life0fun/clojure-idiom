;;
;; define a set of msg queue recevier in this namespace
;;
(ns msgqueue.rabbitmq.receiver
  (:use msgqueue.rabbitmq.rabbitmq))


; get multicast msg from fanout exchange with specified multicast key
(defn receive-multicast [multicast-key]
	(println "Waiting for broadcast..." multicast-key)
		(with-rabbit ["localhost" "guest" "guest"]
			(println (next-message-from "fanex" FANOUT-EXCHANGE-TYPE multicast-key))))

; get multi-part msg
(defn receive-multipart [qname]
	(with-rabbit ["localhost" "guest" "guest"]
  	(println "Waiting for messages...")
		; abstract msg queue into msg seq, and group 2 msg into a pair
		(let [message-pairs (partition 2 (message-seq qname))]
	    (doseq [message-pair message-pairs]
	      (str-join "::" message-pair)))))

; give a queue name and a handler callback, for each msg, invoke callback.
(defn handle-msg [qname handler]
	(with-rabbit ["localhost" "guest" "guest"]
  	(println "Waiting for messages...")
  	(doseq [message (message-seq qname)]
  		(handler message))))

;







