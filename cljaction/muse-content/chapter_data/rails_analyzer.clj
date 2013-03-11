(ns chapter-data.rails-analyzer
  (:use chapter-data.rails-log
        chapter-data.map-reduce))

(defn parse-record [log-record]
  (let [data {:total 1}
        data (assoc data (controller-name log-record) 1)]
    [[(day-of-request log-record) data]]))

(defn reduce-days [[date date-vals]]
  {date (apply merge-with + date-vals)})

(defn rails-reducer [collected-values]
  (apply merge (map reduce-days collected-values)))

(defn investigate-log [log-file]
  (map-reduce parse-record rails-reducer (request-seq log-file)))