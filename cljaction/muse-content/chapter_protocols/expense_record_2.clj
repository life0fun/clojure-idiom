(ns chapter-protocols.expense-record-2
  (:import [java.text SimpleDateFormat]
           [java.util Calendar]))

(defprotocol ExpenseCalculations
  (total-cents [e])
  (is-category? [e category]))

(defrecord NewExpense [date amount-dollars amount-cents category merchant-name]
  ExpenseCalculations
  (total-cents [this]
    (-> amount-dollars
        (* 100)
        (+ amount-cents)))

  (is-category? [this some-category]
    (= category some-category)))

(defn new-expense [date-string dollars cents category merchant-name]
  (let [calendar-date (Calendar/getInstance)]
    (.setTime calendar-date (.parse (SimpleDateFormat. "yyyy-MM-dd") date-string))
    (NewExpense. calendar-date dollars cents category merchant-name)))

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
