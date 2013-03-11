(ns chapter-protocols.expense-multi
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

(defmulti total-cents class)

(defmulti is-category? (fn [e category] (class e)))

(defmethod total-cents clojure.lang.IPersistentMap [e]
  (-> (:amount-dollars e)
      (* 100)
      (+ (:amount-cents e))))

(defmethod total-cents com.curry.expenses.Expense [e]
  (.amountInCents e))

(defmethod is-category? clojure.lang.IPersistentMap [e some-category]
  (= (:category e) some-category))

(defmethod is-category? com.curry.expenses.Expense [e some-category]
  (= (.getCategory e) some-category))

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
