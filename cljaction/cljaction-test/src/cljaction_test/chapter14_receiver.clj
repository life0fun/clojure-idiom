(ns chapter14-receiver
  (:use chapter14-rabbitmq))

(println "Waiting...")
(with-rabbit ["localhost" "guest" "guest"]
  (println (next-message-from "chapter14-test")))
