(ns chapter14-dispatch
  (:use chapter14-rabbitmq chapter14-worker))

(println "Dispatching...")
(with-rabbit ["localhost" "guest" "guest"]
  (send-message "chapter14-test" "chapter 14 test method"))
(println "done!")