(ns chapter-data.dist-fact-client
  (:use chapter-data.dist-fact
        chapter-data.master-core
        chapter-data.status
        chapter14-worker-multicast
        chapter14-rabbitmq-multicast))

(defn dispatch-factorial [job-id task-id n]
  (redis/with-server (redis-config)
    (mark-dispatched job-id task-id)
    (factorial job-id task-id [n])))

(comment
  (with-rabbit ["localhost" "guest" "guest"]
    (let [f (dispatch-factorial "test-job" "test-task" 5)]
      (from-swarm [f]
                  (println "Got the answer:" (f :value))))))


(def fact-job (new-job "fact-job" factorial 5 10000 identity))

(with-rabbit-redis 
  (start-job fact-job (map list (take 10 (iterate inc 1))))
  (println "Status:" (job-successful? fact-job))
  (println "Values:" (values-from fact-job)))