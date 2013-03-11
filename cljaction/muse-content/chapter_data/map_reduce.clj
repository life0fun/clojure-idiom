(ns chapter-data.map-reduce
  (:use clojure.contrib.seq-utils))

(defn combine [mapped]
  (->> (apply concat mapped) 
       (group-by first)
       (map (fn [[k v]] 
              {k (map second v)}))
       (apply merge-with conj)))

(defn map-reduce [mapper reducer args-seq]
  (->> (map mapper args-seq)
       (combine)
       (reducer)))