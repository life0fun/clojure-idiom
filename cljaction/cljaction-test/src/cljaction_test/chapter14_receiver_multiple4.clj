(ns chapter14-receiver-multiple4
  (:use chapter14-rabbitmq clojure.contrib.str-utils))

(defn print-two-messages [messages]
  (println (str-join "::" messages)))

(with-rabbit ["localhost" "guest" "guest"]
  (println "Waiting for messages...")
  (let [message-pairs (partition 2 (message-seq "chapter14-test"))]
    (doseq [message-pair message-pairs]
      (print-two-messages message-pair))))
