(ns chapter14-sender
  (:use chapter14-rabbitmq))

(println "Sending...")
(with-rabbit ["localhost" "guest" "guest"]
  (send-message "chapter14-test" "chapter 14 test method"))
(println "done!")