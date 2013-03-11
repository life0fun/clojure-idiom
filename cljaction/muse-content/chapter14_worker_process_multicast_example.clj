(ns chapter14-worker-process-multicast-example
  (:use chapter14-worker-process-multicast chapter14-worker-multicast-example chapter14-rabbitmq-multicast))

(future
  (with-rabbit ["localhost" "guest" "guest"]
    (start-broadcast-listener)))
                                       
(future
  (with-rabbit ["localhost" "guest" "guest"]
    (start-handler-process)))