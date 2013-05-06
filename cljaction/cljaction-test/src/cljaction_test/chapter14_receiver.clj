(ns cljaction-test.chapter14-receiver
  ;(:use cljaction-test.chapter14-rabbitmq))
  (:use cljaction-test.chapter14-rabbitmq-multicast)

(defn receiver-recv []
  (println "Waiting in receiver-recv :")
  (with-rabbit ["localhost" "guest" "guest"]
    (println (next-message-from "chapter14-test"))))
