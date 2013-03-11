(ns chapter-protocols.expense
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

(defn total-cents [e]
  (-> (:amount-dollars e)
      (* 100)
      (+ (:amount-cents e))))

(defn is-category? [e some-category]
  (= (:category e) some-category))

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