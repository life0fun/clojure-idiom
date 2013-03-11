(ns chapter-macros.anaphoric)

(defmacro anaphoric-if [test-form then-form]
  `(if-let [~'it ~test-form]
     ~then-form))

(defmacro with-it [operator test-form & exprs]
  `(let [~'it ~test-form]
     (~operator ~'it ~@exprs)))

(defmacro thread-it [& [first-expr & rest-expr]]
  (if (empty? rest-expr)
    first-expr
    `(let [~'it ~first-expr]
       (thread-it ~@rest-expr))))

;;;;; examples

(defn some-computation [x]
        (if (even? x) false (inc x))) 

(defn is-positive? [n]
  ;(println "is-positive:" n)
  (if (pos? n) n))

(defn above-threshold? [n threshold]
  ;(println "above-threshold:" n threshold)
  (if (> n threshold) n))

(defn multiple-of? [x n]
  ;(println "multiple-of:" x n)
  (let [m (mod x n)]
    (if (zero? m) x)))

(defn suitablity-checker [threshold factor]
  (fn [x]
    (if-let [p (is-positive? x)]
      (if-let [t (above-threshold? p threshold)]
        (multiple-of? t factor)))))

(defn a-suitablity-checker [x threshold factor]
  (fn [n]
    (with-it and
               (is-positive? x)
               (above-threshold? it threshold x)
               (multiple-of? it factor))))

(def some-pred? even?)
(def a-transform #(* 2 %))
(def another-pred? #(zero? (mod % 8)))
(def another-function)

(defn compute-averages-from [c pred]
  (let [cf (filter pred c)]
    (/ (apply + cf) (count cf))))

(defn some-calculation [a-collection]
  (->> (seq a-collection)
       (filter some-pred?)
       (map a-transform)
       (reduce another-function)))

(defn another-calculation [a-collection]
  (->> (seq a-collection)
       (filter some-pred?)
       (map a-transform)
       (#(compute-averages-from % another-pred?))))

(defn yet-another-calculation [a-collection]
  (thread-it (seq a-collection)
             (filter some-pred? it)
             (map a-transform it)
             (compute-averages-from it another-pred?)))