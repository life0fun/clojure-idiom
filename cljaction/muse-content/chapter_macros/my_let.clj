;(if (odd? (count lettings))
;    (throw (RuntimeException. (str "let form needs even number of bindings:" lettings))))

;(defmacro single-let [lettings & body]
;  (let [let-pairs (partition 2 lettings)
;        params (map first let-pairs)
;        args (map second let-pairs)]
;    `((fn ~(apply vector params) ~@body) ~@args)))

(defmacro single-arg-fn [binding-form & body]
  `((fn [~(first binding-form)] ~@body) ~(second binding-form)))

(defmacro my-let [lettings & body]
  (if (empty? lettings)
    `(do ~@body)
    `(single-arg-fn ~(take 2 lettings)
       (my-let ~(drop 2 lettings) ~@body))))

(use 'clojure.test)

(deftest my-let2-test
  (is (= 200 (my-let [] (* 10 20))))
  (is (= 200 (my-let [x 10 y 20] (* x y))))
  (is (= 100 (my-let [x 10 y x] (* x y))))
  (is (= 1000 (my-let [x 10 y x z y] (* x y z))))
  )