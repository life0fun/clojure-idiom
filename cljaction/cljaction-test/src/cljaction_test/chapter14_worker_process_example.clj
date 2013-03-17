(ns chapter14-worker-process-example
  (:use chapter14-rabbitmq chapter14-worker-process chapter14-worker-example))

(with-rabbit ["localhost" "guest" "guest"]
  (println "Starting worker handler...")
  (start-handler-process))