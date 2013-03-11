(ns chapter-protocols.expense-record
  (:import [java.text SimpleDateFormat]
           [java.util Calendar]))

(defrecord NewExpense [date amount-dollars amount-cents category merchant-name])

(defn new-expense [date-string dollars cents category merchant-name]
  (let [calendar-date (Calendar/getInstance)]
    (.setTime calendar-date (.parse (SimpleDateFormat. "yyyy-MM-dd") date-string))
    (NewExpense. calendar-date dollars cents category merchant-name)))

(defprotocol ExpenseCalculations
  (total-cents [e])
  (is-category? [e category]))

(extend-type NewExpense
  ExpenseCalculations
  (total-cents [e]
    (-> (:amount-dollars e)
        (* 100)
        (+ (:amount-cents e))))

  (is-category? [e some-category]
    (= (:category e) some-category)))

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
