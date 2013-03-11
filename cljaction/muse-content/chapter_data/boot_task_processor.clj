(ns chapter-data.boot-task-processor
  (:use chapter14-rabbitmq-multicast
        chapter14-worker-process-multicast
        chapter-data.dist-fact))

(with-rabbit ["localhost" "guest" "guest"]
  (start-handler-process))