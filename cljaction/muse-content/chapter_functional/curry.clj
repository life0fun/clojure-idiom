(defn price-with-tax [tax-rate amount]
  (->> (/ tax-rate 100)
       (+ 1)
       (* amount)))

(def prices [100 200 300 400 500])

(defn with-california-taxes [prices]
  (map #(price-with-tax 9.25 %) prices))

(defn price-with-ca-tax [price]
  (price-with-tax 9.25 price))

(defn price-with-ny-tax [price]
  (price-with-tax 8.0 price))

(defn price-calculator-for-tax [state-tax]
  (fn [price]
    (price-with-tax state-tax price)))

(defn of-n-args [a b c d e] 
  (str a b c d e ))

(defn of-k-args [d e]
  (of-n-args 1 2 3 d e))

(defn partially-applied [of-n-args & n-minus-k-args]
  (fn [& k-args]
    (apply of-n-args (concat n-minus-k-args k-args))))

(defn add-pair [a b]
  (+ a b))

(defn curry-1 [f]
  (fn [x]
    (fn [y]
      (f x y))))

(defn curry-2 [f]
  (fn [& args]
    (fn [& more-args]
      (apply f (concat args more-args)))))

(defn add-quadruple [a b c d]
  (+ a b c d))

(defn curried-fn [func args-len]
  (fn [& args]
    (let [remaining (- args-len (count args))]
      (if (zero? remaining)
        (apply func args)
        (curried-fn (apply partial func args) remaining)))))

(defmacro defcurried [fname args & body]
  `(let [fun# (fn ~args (do ~@body))]
     (def ~fname (curried-fn fun# ~(count args)))))

(defcurried add-quadruple [a b c d]
  (+ a b c d)) 

(defn uncurry [curried]
  (fn [& args]
    (apply curried args)))

(defn add-numbers [& numbers]
  (apply + numbers))

;;; closures

(defn adder [num1 num2]
  (let [x (+ num1 num2)]
    (fn [y]
      (+ x y))))

(defn try-catch [the-try the-catch]
  (try
   (the-try)
   (catch Exception e
     (the-catch e))))

(defn div-by-zero []
  (/ 1 0))

(defn print-exception [e]
  (println (.getMessage e)))

(defn new-user [login password email]
  (fn [a]
    (condp = a
      :login login
      :password password
      :email email)))

(defn new-user [login password email]
  (fn [a & args]
    (condp = a
      :login login
      :email email
      :authenticate (= password (first args)))))


