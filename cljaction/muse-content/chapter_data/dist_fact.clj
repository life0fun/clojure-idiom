(ns chapter-data.dist-fact
  (:use chapter-data.master-core))

(defn fact [n acc]
  (if (= n 0) 
    acc
    (recur (dec n) (* n acc))))

(defn throw-exception-randomly []
  (when (> 3 (rand-int 10)) 
    (throw (RuntimeException. "Some error occured in fibonacci!"))))

(slave-worker factorial [n]
  (throw-exception-randomly)
  (let [f (fact n 1)]
    (println "Calculated factorial of" n "value:" f)
    f))

