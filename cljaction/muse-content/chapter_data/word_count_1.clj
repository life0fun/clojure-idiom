(ns chapter-data.word-count-1
  (:use clojure.contrib.duck-streams
        clojure.contrib.seq-utils))

(defn parse-line [line]
  (let [tokens (.split (.toLowerCase line) " ")]
    (map #(vector % 1) tokens)))

(defn combine [mapped]
  (->> (apply concat mapped) 
       (group-by first)
       (map (fn [[k v]] 
              {k (map second v)}))
       (apply merge-with conj)))

(defn sum [[k v]]
  {k (apply + v)})

(defn reduce-parsed-lines [collected-values]
  (apply merge (map sum collected-values)))

(defn word-frequency [filename]
  (->> (read-lines filename)
       (map parse-line)
       (combine)
       (reduce-parsed-lines)))