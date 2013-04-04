(ns cljaction-test.chapter14-sender
  (:use cljaction-test.chapter14-rabbitmq))

(defn sender-single-msg []
  (println "Sending...")
  (with-rabbit ["localhost" "guest" "guest"]
    (send-message "chapter14-test" "sending test message to chapter14-test queue"))
  (println "done!"))
