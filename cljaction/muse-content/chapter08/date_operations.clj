(ns chapter08.date-operations
  (:import (java.text SimpleDateFormat)
           (java.util Calendar GregorianCalendar))
  (:use clojure.contrib.str-utils))

(defn date [date-string]
  (let [f (SimpleDateFormat. "yyyy-MM-dd")
        d (.parse f date-string)]
    (doto (GregorianCalendar.)
      (.setTime d))))

;(defmacro date-accessor
;  ([fname field wrapper-fn]
;     `(defn ~fname [d#]
;        (~wrapper-fn (.get d# ~field))))
;  ([fname field]
;     (date-accessor fname field 'identity)))

(defn month-from [d]
  (inc (.get d Calendar/MONTH)))

(defn day-from [d]
  (.get d Calendar/DAY_OF_MONTH))

(defn year-from [d]
  (.get d Calendar/YEAR))

;(date-accessor day-from Calendar/DAY_OF_MONTH)

;(date-accessor month-from Calendar/MONTH inc)

;(date-accessor year-from Calendar/YEAR)

(defn pad [n]
  (if (< n 10) (str "0" n) (str n)))

(defn as-string [date]
  (let [y (year-from date) 
        m (pad (month-from date))
        d (pad (day-from date))]
    (str-join "-" [y m d])))

;(defn increment-day [d]
;  (doto (.clone d) 
;    (.add  Calendar/DAY_OF_MONTH 1)))
;
;(defn increment-month [d]
;  (doto (.clone d) 
;    (.add  Calendar/MONTH 1)))
;
;(defn increment-year [d]
;  (doto (.clone d) 
;    (.add  Calendar/YEAR 1)))
;
;(defn decrement-day [d]
;  (doto (.clone d)
;    (.add Calendar/DAY_OF_MONTH -1)))
;
;(defn decrement-month [d]
;  (doto (.clone d)
;    (.add Calendar/MONTH -1)))
;
;(defn decrement-year [d]
;  (doto (.clone d)
;    (.add Calendar/YEAR -1)))

;(defmacro date-operator [op-name op field]
;  `(defn ~op-name [d#]
;     (doto (.clone d#)
;       (.add ~field (~op 1)))))

(defn date-operator [operation field]
  (fn [d]
    (doto (.clone d)
      (.add field (operation 1)))))

(def increment-day (date-operator  + Calendar/DAY_OF_MONTH))

(def increment-month (date-operator + Calendar/MONTH))

(def increment-year (date-operator + Calendar/YEAR))

(def decrement-day (date-operator - Calendar/DAY_OF_MONTH))

(def decrement-month (date-operator - Calendar/MONTH))

(def decrement-year (date-operator - Calendar/YEAR))
