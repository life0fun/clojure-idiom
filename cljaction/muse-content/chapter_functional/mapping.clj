(defn square [x]
  (* x x))

(defn cube [x]
  (* x x x))

(defn square-all [numbers]
  (if (empty? numbers)
    ()
    (cons (square (first numbers))
          (square-all (rest numbers)))))

(defn cube-all [numbers]
  (if (empty? numbers)
    ()
    (cons (cube (first numbers))
          (cube-all (rest numbers)))))

(defn do-to-all [f numbers]
  (lazy-seq 
    (if (empty? numbers)
      ()
      (cons (f (first numbers))
            (do-to-all f (rest numbers))))))

(defn total-of [numbers]
  (loop [l numbers sum 0]
    (if (empty? l)
      sum
      (recur (rest l) (+ sum (first l))))))

(defn larger-of [x y]
  (if (> x y) x y))

(defn greatest-of [numbers]
  (loop [l numbers candidate (first numbers)]
    (if (empty? l)
      candidate
      (recur (rest l) (larger-of candidate (first l))))))

(defn compute-across [func elements value]
  (if (empty? elements)
    value
    (recur func (rest elements) (func value (first elements)))))

(defn total-of [numbers]
  (compute-across + numbers 0))

(defn greatest-of [numbers]
  (compute-across larger-of numbers (first numbers)))

(defn all-greater-than [threshold numbers]
  (compute-across #(if (> %2 threshold) (conj %1 %2) %1) numbers []))

(defn all-lesser-than [threshold numbers]
  (compute-across #(if (< %2 threshold) (conj %1 %2) %1) numbers []))

(defn select-if [pred elements]
  (compute-across #(if (pred %2) (conj %1 %2) %1) elements []))

(defn all-lesser-than [threshold numbers]
  (select-if #(< % threshold) numbers))

(defn select-into-if [container pred elements]
  (compute-across #(if (pred %2) (conj %1 %2) %1) elements container))