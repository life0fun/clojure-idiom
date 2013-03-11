(ns chapter-data.session-analyzer
  (:use chapter-data.map-reduce
        chapter-data.rails-log
        chapter-data.session-seq))

(defn parse-session [[session-id requests]]
  (let [metrics {:length (count requests)
                 :duration (duration requests)}]
    [[session-id metrics]]))

(defn averages [collected-values]
  (let [num-sessions (count collected-values)
        all-metrics (apply concat (vals collected-values))
        total-length (apply + (map :length all-metrics))
        total-duration (apply + (map :duration all-metrics))]
    {:average-length (/ total-length num-sessions)
     :average-duration (/ total-duration num-sessions)}))

(defn investigate-sessions [filename]
  (let [results (map-reduce parse-session averages (session-seq (request-seq filename)))]
    (println "Avg length:" (* 1.0 (:average-length results)))
    (println "Avg duration:" (* 1.0 (:average-duration results)))))