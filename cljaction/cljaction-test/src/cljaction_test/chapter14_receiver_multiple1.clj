(ns chapter14-receiver-multiple1
  (:use chapter14-rabbitmq))

(defn print-multiple-messages []
  (loop [message (next-message-from "chapter14-test")]
    (println "Message: " message)
    (recur (next-message-from "chapter14-test"))))

(with-rabbit ["localhost" "guest" "guest"]
  (println "Waiting for messages...")
  (print-multiple-messages))
