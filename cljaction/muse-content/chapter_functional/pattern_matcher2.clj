(ns chapter-functional.pattern-matcher2)

(def *pvar-prefix* \?)

(defn atom? [expr]
  (and (not (list? expr))
       (not (nil? expr))))

(defn pattern-variable? [p]
  (and (atom? p)
       (= *pvar-prefix* (first (name p)))))

(defn satisfies-binding [p s bindings]
  (let [value (bindings p)]
    (or (nil? value) (= value s))))

(defn matches?
  ([pattern expr]
     (matches? pattern expr {}))
  ([pattern expr bindings]
     (cond 
       (and (pattern-variable? pattern) (atom? expr)) (if (satisfies-binding pattern expr bindings) 
                                                        [true (assoc bindings pattern expr)]
                                                        [false bindings])
       (and (atom? pattern) (atom? expr)) [(= pattern expr) bindings]
       (or (atom? pattern) (atom? expr)) [false bindings]
       (and (empty? pattern) (empty? expr)) [true bindings]
       :else (let [[mr new-bindings] (matches? (rest pattern) (rest expr) bindings)
                   [mf newest-bindings] (matches? (first pattern) (first expr) new-bindings)]
               [(and mr mf) newest-bindings]))))


;(defn query-processor [query]
;  (if (not (= 'AND (first query)))
;    (simple-query-processor query *rules-db* ())
;    (reduce simple-query-processor )))

(defn combiner [e1 e2]
  (if (sequential? e1)
    (conj e1 e2)
    (list e1 e2)))

(defn simple-query-processor [simple-query db bindings]
  (if (empty? db)
    [true bindings]
    (let [[mf b] (matches? simple-query (first db) bindings)]
      (if mf
        (let [[mr b1] (simple-query-processor simple-query (rest db) bindings)]
          [(and mf mr) (merge-with combiner  b b1)])
        (simple-query-processor simple-query (rest db) bindings)))))