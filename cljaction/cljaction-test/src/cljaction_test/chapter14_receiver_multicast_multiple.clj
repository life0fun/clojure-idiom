(ns chapter14-receiver-multicast-multiple
  (:use chapter14-rabbitmq-multicast))

(println "Waiting for broadcast...")
(with-rabbit ["localhost" "guest" "guest"]
  (doseq [message (message-seq "fanex" FANOUT-EXCHANGE-TYPE "chapter-14-ghz")]
    (println message)))
