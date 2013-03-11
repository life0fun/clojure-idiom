(ns chapter14-worker-multicast-example
  (:use  chapter14-worker-multicast))

(defworker long-computation-one [x y]
  (Thread/sleep 3000)
  (* x y))

(defworker long-computation-two [a b c]
  (Thread/sleep 2000)
  (+ a b c))

(defworker expensive-audit-log [z]
  (println "expensive audit log:" z)
  (Thread/sleep 4000))