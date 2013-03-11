(ns chapter-data.parallel-map-reduce
  (:use clojure.contrib.seq-utils))

(def BATCH-SIZE 10000)

(defn pfunction [f]
  (fn [l]
    ;(println "*************")
    (map f l)))

(defn parallel-map [f elements]
  ;(println "Starting mapping...")
  (->> elements
       (partition-all BATCH-SIZE)
       (pmap (pfunction f))
       (apply concat)
       (doall)))

(defn combine [mapped]
  ;(println "Starting combine...")
  (->> (apply concat mapped) 
       (group-by first)
       (parallel-map (fn [[k v]] 
              {k (map second v)}))
       (apply merge-with conj)))

(defn map-reduce [mapper reducer args-seq]
  (->> (parallel-map mapper args-seq)
       (combine)
       (#(do 
           ;(println "Starting reducing...") 
           (reducer %)))))