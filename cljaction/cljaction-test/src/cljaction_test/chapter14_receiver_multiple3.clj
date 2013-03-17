(ns chapter14-receiver-multiple3
  (:use chapter14-rabbitmq))

(defn handle-multiple-messages [handler]
  (doseq [message (message-seq "chapter14-test")]
    (handler message)))

(with-rabbit ["localhost" "guest" "guest"]
  (println "Waiting for messages...")
  (handle-multiple-messages println))
