(ns chapter14-receiver-multicast
  (:use chapter14-rabbitmq-multicast))

(println "Waiting for broadcast...")
(with-rabbit ["localhost" "guest" "guest"]
  (println (next-message-from "fanex" FANOUT-EXCHANGE-TYPE "chapter-14-ghz")))
