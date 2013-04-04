(ns cljaction-test.chapter14-receiver
  (:use cljaction-test.chapter14-rabbitmq))

(defn receiver-recv []
  (println "Waiting...")
  (with-rabbit ["localhost" "guest" "guest"]
    (println (next-message-from "chapter14-test"))))
