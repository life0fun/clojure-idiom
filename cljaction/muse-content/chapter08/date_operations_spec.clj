(ns chapter08.date-operations-spec
  (:use chapter08.date-operations :reload)
  (:use clojure.test))

(deftest test-simple-data-parsing
  (let [d (date "2009-01-22")]
    (is (= (month-from d) 1))
    (is (= (day-from d) 22))
    (is (= (year-from d) 2009))))

(deftest test-as-string
  (let [d (date "2009-01-22")]
    (is (= (as-string d) "2009-01-22"))))

(deftest test-incrementing-date
  (let [d (date "2009-10-31")
        n-day (increment-day d)
        n-month (increment-month d)
        n-year (increment-year d)]
    (is (= (as-string n-day) "2009-11-01"))
    (is (= (as-string n-month) "2009-11-30"))
    (is (= (as-string n-year) "2010-10-31"))))

(deftest test-decrementing-date
  (let [d (date "2009-11-01")
        n-day (decrement-day d)
        n-month (decrement-month d)
        n-year (decrement-year d)]
    (is (= (as-string n-day) "2009-10-31"))
    (is (= (as-string n-month) "2009-10-01"))
    (is (= (as-string n-year) "2008-11-01"))))