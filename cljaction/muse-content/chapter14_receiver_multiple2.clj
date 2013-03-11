(ns chapter14-receiver-multiple2
  (:use chapter14-rabbitmq))

(defn handle-multiple-messages [handler]
  (loop [message (next-message-from "chapter14-test")]
    (handler message)
    (recur (next-message-from "chapter14-test"))))

(with-rabbit ["localhost" "guest" "guest"]
  (println "Waiting for messages...")
  (handle-multiple-messages println))
