(ns chapter-protocols.worker-server-boot
  (:use chapter-protocols.worker-server chapter-protocols.rabbitmq chapter-protocols.worker-example))

(future
  (with-rabbit ["localhost" "guest" "guest"]
    (start-broadcast-listener)))
                                       
(future
  (try
    (with-rabbit ["localhost" "guest" "guest"]
      (start-handler-process))
    (catch Exception e
      (println "OUCH!")
      (.printStackTrace e))))