(ns chapter14-worker-usage
  (:use chapter14-rabbitmq chapter14-worker chapter14-worker-example))

(println "Dispatching...")
(with-rabbit ["localhost" "guest" "guest"]
  (let [one (long-computation-one 10 20)
        two (long-computation-two 3 5 7)]
    (fire-and-forget expensive-audit-log 100)
    (from-swarm [one two]
                (println "one:" (one :value))
                (println "two:" (two :value)))))
(println "done!")  