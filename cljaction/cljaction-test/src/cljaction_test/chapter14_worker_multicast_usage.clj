(ns chapter14-worker-multicast-usage
  (:use chapter14-worker-multicast chapter14-rabbitmq-multicast chapter14-worker-multicast-example))

(println "Dispatching...")
(with-rabbit ["localhost" "guest" "guest"]
  (let [one (long-computation-one 10 20)
        two (long-computation-two 3 5 7)]
    (run-worker-everywhere expensive-audit-log 777)
    (from-swarm [one two]
                (println "one:" (one :value))
                (println "two:" (two :value)))))
(println "done!")  