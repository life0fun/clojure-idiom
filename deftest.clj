(ns tdd
  (:import (java.text SimpleDateFormat)
           (java.util Calendar GregorianCalendar))
  (:use [clojure.test]))

(defn date [date-string]
  (let [f (SimpleDateFormat. "yyyy-MM-dd")
        d (.parse f date-string)]
    (doto (GregorianCalendar.)
      (.setTime d))))

(defn day-from [d]
  (.get d Calendar/DAY_OF_MONTH))

(defn month-from [d]
  (inc (.get d Calendar/MONTH)))

(defn year-from [d]
  (.get d Calendar/YEAR))

(deftest test-simple-data-parsing
  (let [d (date "2009-1-22")]
    (is (= (day-from d) 22))))

(deftest test-simple-data-parsing
  (let [d (date "2009-01-22")]
    (is (= (month-from d) 1))
    (is (= (day-from d) 22))
    (is (= (year-from d) 2009))))

; 
(defn as-string [date]
  (let [y (year-from date)
        m (month-from date)
        d (day-from date)]
    (str-join "-" [y m d])))

; each test fn test one fn
(deftest test-as-string
  (let [d (date "2009-01-22")]
    (is (= (as-string d) "2009-01-22"))))

(run-tests tdd)