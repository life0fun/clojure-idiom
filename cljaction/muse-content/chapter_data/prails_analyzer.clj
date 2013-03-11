(ns chapter-data.prails-analyzer
  (:use chapter-data.rails-log
        chapter-data.parallel-map-reduce))

(defn parse-record [log-record]
  (let [data {:total 1}
        data (assoc data (controller-name log-record) 1)]
    [[(day-of-request log-record) data]]))

(defn reduce-days [[date date-vals]]
  {date (apply merge-with + date-vals)})

(defn rails-reducer [collected-values]
  (apply merge (map reduce-days collected-values)))

(defn pinvestigate-log [log-file]
  (println "PARALLEL MR")
  (map-reduce parse-record rails-reducer (request-seq log-file)))