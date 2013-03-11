(ns chapter-data.session-seq
  (:use chapter-data.rails-log
        clojure.contrib.seq-utils))

(defn session-seq [requests]
  (group-by session-id requests))

(defn duration [requests]
  (let [begin (time-of-request (first requests))
        end (time-of-request (last requests))]
    (- (.getMillis end) (.getMillis begin))))