(ns chapter-data.word-count-2
  (:use chapter-data.map-reduce
        clojure.contrib.duck-streams))

(defn parse-line [line]
  (let [tokens (.split (.toLowerCase line) " ")]
    (map #(vector % 1) tokens)))

(defn sum [[k v]]
  {k (apply + v)})

(defn reduce-parsed-lines [collected-values]
  (apply merge (map sum collected-values)))

(defn word-frequency [filename]
  (map-reduce parse-line reduce-parsed-lines (read-lines filename)))

