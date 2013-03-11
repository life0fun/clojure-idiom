(def multiply *)

(defn total-cost 
  ([item-cost]
    (total-cost item-cost 1))
  ([item-cost number-of-items]
    (multiply item-cost number-of-items)))



(def users [
  {:username "kyle"
   :balance 175.00
   :member-since "2009-04-16"}
           
  {:username "zak"
   :balance 12.95
   :member-since "2009-02-01"}

  {:username "rob"
   :balance 98.50
   :member-since "2009-03-30"}            
])

(defn username [user]
  (user :username))

(defn balance [user]
  (user :balance))

(defn sorter-using [ordering-fn]
  (fn [users]
    (sort-by ordering-fn users)))

(def poorest-first (sorter-using balance))

(def alphabetically (sorter-using username))


(map (fn [user] (user :member-since)) users)


(def *eval-me* 10)

(defn print-the-var [label]
  (println label *eval-me*))

(print-the-var “A:”)

(binding [*eval-me* 20]
  (print-the-var “B:”)
  (binding [*eval-me* 30]
    (print-the-var “C:”))
  (print-the-var “D:”))

(print-the-var “E:”)


;;;; aspect-oriented logging

(defn twice [x]
  (println "original function")
  (* 2 x))

(defn call-twice [y]
  (twice y))

(defn with-log [function-to-call log-statement]
  (fn [& args]
    (println log-statement)
    (apply function-to-call args)))

(call-twice 10)

(binding [twice (with-log twice "Calling the twice function")]
   (call-twice 20))
    
(call-twice 30)

;;;;;;;; destructuring

(defn describe-salary [person]
  (let [first (:first-name person)
        last (:last-name person)
        annual (:salary person)]
  (println first last "earns" annual)))

(defn describe-salary-2 [{first :first-name last :last-name annual :salary}]
  (println first last "earns" annual))


(defn print-amounts [[ amount-1 amount-2]]
  (println "amounts are:" amount-1 "and" amount-2))


(defn print-amounts-multiple [[amount-1 amount-2 & remaining-amounts]]
  (println "Amounts are:" amount-1 "," amount-2 "and" remaining-amounts))

(defn print-all-amounts [[amount-1 amount-2 & remaining :as all]]
  (println "Amounts are:" amount-1 "," amount-2 "and" remaining)
  (println "Also, all the amounts are:" all))

(defn print-first-category [[[category amount] & _ ]]
  (println "First category  was:" category)
  (println "First amount was:" amount))


(defn describe-salary-3 [{first :first-name 
                          last :last-name 
                          annual :salary
                          bonus :bonus-percentage
                          :or {bonus 5}
                          }]
   (println first last "earns" annual "with a" bonus "percent bonus"))

(defn describe-person [{first :first-name 
                        last :last-name
                        bonus :bonus-percentage
                        :or {bonus 5}
                        :as p}]
  (println "Info about" first last "is:" p)
  (println "Bonus is:" bonus "percent"))

(defn greet-user [{:keys [first-name last-name]}]
  (println "Welcome," first-name last-name))

;;;; recursive functions

(defn accumulate-sum [numbers acc]
  (if (empty? numbers)
    acc
    (recur (rest numbers) (+ acc (first numbers)))))

;;;;; partial

(defn above-threshold [threshold number]
  (> number threshold))


;;; memoize

(defn slow-calc [n m]
  (Thread/sleep 1000)
  (* n m))

(def fast-calc (memoize slow-calc))



;;; trampolining

(defn count-down [n]
  (if-not (zero? n)
    (do
      (if (= 0 (rem n 100))
	(println "count-down:" n))
      (count-down (dec n)))))

(defn count-downr [n]
  (if-not (zero? n)
    (do
      (if (= 0 (rem n 100))
	(println "count-down:" n))
      (recur (dec n)))))

(declare hat)

(defn cat [n]
  (if-not (zero? n)
    (do
      (if (= 0 (rem n 100))
	(println "cat:" n))
      (hat (dec n)))))

(defn hat [n]
  (if-not (zero? n)
    (do
      (if (= 0 (rem n 100))
	(println "hat:" n))
      (cat (dec n)))))

(declare hatt)

(defn catt [n]
  (if-not (zero? n)
    (do
      (if (= 0 (rem n 100))
	(println "catt:" n))
      #(hatt (dec n)))))

(defn hatt [n]
  (if-not (zero? n)
    (do
      (if (= 0 (rem n 100))
	(println "hatt:" n))
      #(catt (dec n)))))

(declare hattw)

(defn cattw [n]
  (when-not (zero? n)
    (if (= 0 (rem n 100))
	(println "catt:" n))
    #(hattw (dec n))))

(defn hattw [n]
  (when-not (zero? n)
    (if (= 0 (rem n 100))
	(println "hatt:" n))
    #(cattw (dec n))))

;;; pre and post conditions

(defn item-total [price quantity]
  {:pre [(> price 0) (> quantity 0)]
   :post [(> % 1)]}
  (* price quantity))

(defn basic-item-total [price quantity]
  (* price quantity))

(defn with-line-item-conditions [f & args]
    {:pre [(> (first args) 0) (> (second args) 0)]
     :post [(> % 1)]}
    (apply f args))


(def item-total (partial with-line-item-conditions basic-item-total))

;;; meta-data

(def untrusted (with-meta {:command "clean-table" :subject "users"} {:safe false :io true}))