(defn factorial [n]
  (reduce * 
          (range 1 
                 (+ 1 
                    n))))


(def noc (memoize (fn [n k]
  (if (= k 1)
    1
    (let [ns (range (- k 1) n)
	  sums (map #(noc % (- k 1)) ns)]
      (apply + sums))))))

(declare load-from-hbase-table calculate-average)
(defn average-expenses-for-date [user date]
  (if (load-from-hbase-table user :expenses date)
    (calculate-average (load-from-hbase-table user :expenses date))))

(defn average-expenses-for-date-2 [user date]
  (let [expenses (load-from-hbase-table user :expenses date)]
    (if expenses
      (calculate-average expenses))))

;;; using loop/recur
(defn fact-loop [n]
  (loop [current n fact 1]
    (if (= current 1)
      fact
      (recur (dec current) (* fact current)))
    (println "done")
    ))

;;;; cond
(defn range-info [x] 
  (cond
    (< x 0) (println "Negative!")
    (= x 0) (println "Zero!")
    :default (println "Positive!")))


;;;; list comprehensions

(defn chessboard-labesl []
  (for [alpha "abcdefgh"
	num (range 1 9)]
    (str alpha num)))

(defn prime? [x]
  (let [divisors (range 2 (inc (int (Math/sqrt x))))
	remainders (map #(rem x %) divisors)]
    (not (some zero? remainders))))

(defn primes-less-than [n]
  (for [x (range 2 (inc n))
	:when (prime? x)]
    x))

(defn pairs-for-primes-2 [n]
  (let [range-for-primes #(range 2 (inc %))]
    (for [x (range-for-primes n)
	  y (range-for-primes n)
	  :when (prime? (+ x y))]
      (list x y))))

(defn pairs-for-primes [n]
  (let [z (range 2 (inc n))]
    (for [x z y z :when (prime? (+ x y))]
      (list x y))))

;; assoc-in, update-in, get-in

(def users {:kyle {
                   :date-joined "2009-01-01"
                   :summary {
                             :average {
                                       :monthly 1000
                                       :yearly 12000}}}})

(defn set-average-in [users-map user type amount]
  (let [user-map (users-map user)
        summary-map (:summary user-map)
        averages-map (:average summary-map)]
    (assoc users-map user
           (assoc user-map :summary
                  (assoc summary-map :average
                         (assoc averages-map type amount))))))

(defn average-for [user type]
  (type (:average (:summary (user @users)))))


;;; threading macros

(defn final-amount [principle rate time-periods]
  (* (Math/pow (+ 1 (/ rate 100)) time-periods) principle))

(defn final-amount-> [principle rate time-periods]
  (-> rate
      (/ 100)
      (+ 1)
      (Math/pow time-periods)
      (* principle)))

(defn factorial->> [n]
  (->> n
       (+ 1)
       (range 1)
       (apply *)))


;;;; check-login

(def users {"kyle" {:password "secretk" :number-pets 2}
            "siva" {:password "secrets" :number-pets 4}
            "rob" {:password "secretr" :number-pets 6}
            "george" {:password "secretg" :number-pets 8}})

(defn check-login [username password]
  (let [actual-password (:password (users username))]
    (= actual-password password)))

(defn average-pets []
  (/ (apply + (map :number-pets (vals users))) (count users)))

(defn average-pets [users]
  (let [user-data (vals users)
        number-pets (map :number-pets user-data)
        total (apply + number-pets)]
    (/ total (count users))))


(def no-users {})

(defn average-pets [users]
  (try
   (let [user-data (vals users)
        number-pets (map :number-pets user-data)
        total (apply + number-pets)]
    (/ total (count users)))
   (catch Exception e
     (println "Error!")
     0)))