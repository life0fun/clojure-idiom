;;
;;
(ns cljaction-test.chapter14-worker-usage
  (:use cljaction-test.chapter14-rabbitmq cljaction-test.chapter14-worker cljaction-test.chapter14-worker-example))

(println "Dispatching...")
;;(with-rabbit ["localhost" "guest" "guest"]
(with-rabbit ["localhost"]
  (let [one (long-computation-one 10 20)
        two (long-computation-two 3 5 7)]
    (fire-and-forget expensive-audit-log 100)
    (from-swarm [one two]
                (println "one:" (one :value))
                (println "two:" (two :value)))))
(println "done!")
