;; lazy qsort
;; (load-file "qsort.clj")

(ns qsort
  (:use clojure.set))

;;
(defn take [l]
  (loop [hd (first l) body (rest l) ret []]
    (if (first l)
      (recur (hd (first (rest l)) (rest (rest l)) (conj ret (first l)))))))

(defn qsort [l]
  (loop [[part & parts] work]
    (recur (list* (filter (smaller? xs) pivot (remove smaller? xs) parts)))))
