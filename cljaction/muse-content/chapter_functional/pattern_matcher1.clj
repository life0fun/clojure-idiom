(ns chapter-functional.pattern-matcher1)

(defn atom? [expr]
  (and (not (list? expr))
       (not (nil? expr))))

(defn match [p s]
  (cond 
    (and (= '? p) (atom? s)) true
    (and (atom? p) (atom? s)) (= p s)
    (or (atom? p) (atom? s)) false
    (and (empty? p) (empty? s)) true
    :else (and (match (first p) (first s))
               (match (rest p) (rest s)))))