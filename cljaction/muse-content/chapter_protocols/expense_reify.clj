(ns chapter-protocols.expense-reify
  (:import [java.text SimpleDateFormat]
           [java.util Calendar]))

(defprotocol ExpenseCalculations
  (total-cents [e])
  (is-category? [e category]))

(defn new-expense [date-string dollars cents category merchant-name]
  (let [calendar-date (Calendar/getInstance)]
    (.setTime calendar-date (.parse (SimpleDateFormat. "yyyy-MM-dd") date-string))
    (reify ExpenseCalculations
      (total-cents [this]
        (-> dollars
          (* 100)
          (+ cents)))
      (is-category? [this some-category]
        (= category some-category)))))

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
