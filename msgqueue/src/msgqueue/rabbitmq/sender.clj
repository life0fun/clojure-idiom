;;
;; define a set of method to send msg to queue
;;
(ns msgqueue.rabbitmq.sender
  (:use msgqueue.rabbitmq.rabbitmq))

(defn send-single-msg [routingkey]
  (println "Sending test msg to " routingkey)
  (with-rabbit ["localhost" "guest" "guest"]
    (send-message routingkey (str "sending test message to " routingkey)))
  (println "done!"))

(defn send-multicast
	([]
		(send-multicast "fanex" "logs-all-level"))
	([exchgname routingkey]
		(println "Multicasting to " exchgname routingkey)
		(with-rabbit ["localhost" "guest" "guest"]
  		(send-message exchgname FANOUT-EXCHANGE-TYPE routingkey "Broadcast! Multicasting all logs"))
		(println "done!")))

(defn test-send [qname msg]
	(println "Test sending msg to " qname msg)
	(with-rabbit ["localhost" "guest" "guest"]
  	(send-message qname msg))
	(println "done!"))
