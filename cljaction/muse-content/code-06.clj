(def all-users (ref {}))

(defn new-user [id login monthly-budget]
  {:id id
   :login login
   :monthly-budget monthly-budget
   :total-expenses 0})

(defn add-new-user [login budget-amount]
  (dosync
   (let [current-number (count @all-users)
         user (new-user (inc current-number) login budget-amount)]
     (alter all-users assoc login user))))

(defn add-expense [login amount]
  (dosync
   (let [user (@all-users login)
         current-total (:total-expenses user)
         updated-user (assoc user :total-expenses (+ amount current-total))]
     (alter all-users assoc login updated-user))))


;;should be called inside a transaction
(defn users-total-for [attribute]
  (let [users (vals @all-users)
        all-values (map attribute users)]
    (apply + all-values)))

(defn website-traction []
  (dosync
   {:budgets-managed (users-total-for :monthly-budget)
    :expenses-recorded (users-total-for :total-expenses)}))


;; futures

(defn long-calculation [num1 num2]
  (Thread/sleep 5000)
  (* num1 num2))

(defn long-run []
  (let [x (long-calculation 11 13)
	y (long-calculation 13 17)
	z (long-calculation 17 19)]
    (* x y z)))

(defn fast-run []
  (let [x (future (long-calculation 11 13))
	y (future (long-calculation 13 17))
	z (future (long-calculation 17 19))]
    (* @x @y @z)))

;;; promise

(defn get-value []
  (Thread/sleep (int (rand 2000)))
  (str (int (rand 10))))

(defn values-q [value-fn]
  (lazy-seq
    (let [p (promise)]
      (future 
        (deliver p (value-fn)))
      (cons p
            (values-q value-fn)))))

(defn processor [a-promise]
  (let [v @a-promise]
    (println "processed" v)))

;;;; promises, promises

(def dreams (atom (repeatedly promise)))

(def pq (lazy-seq @(second @dreams)))

(defn enQ [v]
  (let [[f s] (swap! dreams next)]
    (deliver f (cons v (lazy-seq @s)))
    v))

(defn endQ []
  (let [[f] (swap! dreams #(vector (second %)))]
    (if f (deliver f nil))))

(def all (atom []))

(def f (future
         (doseq [v pq]
           (swap! all conj v))))

;;; pipe dreams (from cgrand)

(defn pipe
 "Returns a pair: a seq (the read end) and a function (the write end).
  The function can takes either no arguments to close the pipe 
  or one argument which is appended to the seq. Read is blocking."
 []
  (let [promises (atom (repeatedly promise))
        p (second @promises)]
    [(lazy-seq @p)
     (fn 
       ([] ;close the pipe
         (let [[a] (swap! promises #(vector (second %)))]
           (if a
             (deliver a nil)
             (throw (Exception. "Pipe already closed")))))
       ([x] ;enqueue x
         (let [[a b] (swap! promises next)]
           (if (and a b)
             (do
               (deliver a (cons x (lazy-seq @b)))
               x)
             (throw (Exception. "Pipe already closed"))))))]))


;;;; watchers

(def adi (atom 0))

(defn on-change [the-key the-ref old-value new-value]
  (println "Hey, seeing change from" old-value "to" new-value))

(add-watch adi :adi-watcher on-change)