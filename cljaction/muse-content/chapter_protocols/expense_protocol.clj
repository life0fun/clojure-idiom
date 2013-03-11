(ns chapter-protocols.expense-protocol
  (:import [java.text SimpleDateFormat]
           [java.util Calendar]))

(defn new-expense [date-string dollars cents category merchant-name]
  (let [calendar-date (Calendar/getInstance)]
    (.setTime calendar-date (.parse (SimpleDateFormat. "yyyy-MM-dd") date-string))
    {:date calendar-date
     :amount-dollars dollars
     :amount-cents cents
     :category category
     :merchant-name merchant-name}))

(defprotocol ExpenseCalculations
  (total-cents [e])
  (is-category? [e category]))

(extend-protocol ExpenseCalculations
  
  clojure.lang.IPersistentMap
  (total-cents [e]
    (-> (:amount-dollars e)
        (* 100)
        (+ (:amount-cents e))))

  (is-category? [e some-category]
    (= (:category e) some-category)))

(comment
  (extend-type com.curry.expenses.Expense
    ExpenseCalculations
    (total-cents [e]
                 (.amountInCents e))
    
    (is-category? [e some-category]
                  (= (.getCategory e) some-category))))

(extend com.curry.expenses.Expense
  ExpenseCalculations {
                       :total-cents (fn [e] (.amountInCents e))
                       :is-category? (fn [e some-category] (= (.getCategory e) some-category))})

(extend-protocol ExpenseCalculations nil
  (total-cents [e] 0))

(defn category-is [category]
  #(is-category? % category))

(defn total-amount
  ([expenses-list]
     (total-amount (constantly true) expenses-list))
  ([pred expenses-list]
     (->> expenses-list
         (filter pred)
         (map total-cents)
         (apply +))))
