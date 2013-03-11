(ns chapter-data.pmap-reduce
  (:use clojure.contrib.seq-utils))

(defn combine [mapped]
  (->> (apply concat mapped) 
       (group-by first)
       (pmap (fn [[k v]] 
              {k (map second v)}))
       (apply merge-with conj)))

(defn map-reduce [mapper reducer args-seq]
  (->> (pmap mapper args-seq)
       (combine)
       (reducer)))