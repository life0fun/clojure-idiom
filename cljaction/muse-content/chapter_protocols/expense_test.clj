(ns chapter-protocols.expense-test
  (:import [com.curry.expenses Expense])
  (:use [chapter-protocols.expense-reify]
        [clojure.test]))

(def clj-expenses [(new-expense "2009-8-20" 21 95 "books" "amazon.com")
                   (new-expense "2009-8-21" 72 43 "food" "mollie-stones")
                   (new-expense "2009-8-22" 315 71 "car-rental" "avis")
                   (new-expense "2009-8-23" 15 68 "books" "borders")])

(deftest test-clj-expenses-total
  (is (= 42577 (total-amount clj-expenses)))
  (is (=  3763 (total-amount (category-is "books") clj-expenses))))


(def java-expenses [(Expense. "2009-8-24" 44 95 "books" "amazon.com")
                    (Expense. "2009-8-25" 29 11 "gas" "shell")])

(deftest test-java-expenses-total
  (let [total-cents (map #(.amountInCents %) java-expenses)]
    (is (= 7406 (apply + total-cents)))))


(def mixed-expenses (concat clj-expenses java-expenses))

(deftest test-mixed-expenses-total
  (is (= 49983 (total-amount mixed-expenses)))
  (is (= 8258 (total-amount (category-is "books") mixed-expenses))))

