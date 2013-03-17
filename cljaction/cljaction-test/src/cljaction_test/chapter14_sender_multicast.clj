(ns chapter14-sender-multicast
  (:use chapter14-rabbitmq-multicast))

(println "Multicasting...")
(with-rabbit ["localhost" "guest" "guest"]
  (send-message "fanex" FANOUT-EXCHANGE-TYPE "chapter-14-ghz" "Broadcast! Chapter 14 multicast!"))
(println "done!")