(import '(java.text SimpleDateFormat))
(import '(java.util Calendar))

(defn date-from-date-string [date-string]
  (let [sdf (SimpleDateFormat. "yyyy-MM-dd")] 
    (.parse sdf date-string)))


(defn the-past-midnight []
  (let [calendar-obj (Calendar/getInstance)]
    (.set calendar-obj Calendar/AM_PM Calendar/AM)
    (.set calendar-obj Calendar/HOUR 0)
    (.set calendar-obj Calendar/MINUTE 0)
    (.set calendar-obj Calendar/SECOND 0)
    (.set calendar-obj Calendar/MILLISECOND 0)
    (.getTime calendar-obj)))


(defn the-past-midnight-2 []
  (let [calendar-obj (Calendar/getInstance)]
    (doto calendar-obj
      (.set Calendar/AM_PM Calendar/AM)
      (.set Calendar/HOUR 0)
      (.set Calendar/MINUTE 0)
      (.set Calendar/SECOND 0)
      (.set Calendar/MILLISECOND 0))
    (.getTime calendar-obj)))


(import '(com.sun.grizzly.tcp.http11 GrizzlyAdapter))
(proxy [GrizzlyAdapter] []
    (service [req res]
      "Service was called!"))


