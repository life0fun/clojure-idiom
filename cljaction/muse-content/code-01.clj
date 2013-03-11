(defn next-terms [term-1 term-2]
  (let [term-3 (+ term-1 term-2)]
    (lazy-seq (cons term-3 (next-terms term-2 term-3)))))

(defn fibonacci [t1 t2]
  (concat [t1 t2] (next-terms t1 t2)))

(defn connect [h p]
  (str h ":" p))

(defmacro with-connection [[connection hostname port] & exprs]
  `(with-open [~connection (connect ~hostname ~port)]
     (do ~@exprs)))

(def total-expenditure (ref 0))

(defn add-amount [amount]
  (ref-set total-expenditure (+ amount @total-expenditure)))

(defn get-expenses [user-id start-date end-date] 
  (create-audit-log user-id GET-EXPENSES)
  (let [connection (connect-to-expenses-db user-id)
	expenses (find-all-between connection start-date end-date)]
    (close-connection connection)
    expenses))

(defn add-expense [user-id date amount]
  (create-audit-log user-id ADD-EXPENSE)
  (let [connection [connect-to-expenses-db user-id)]
    (save-new-expense connection date amount)
    (flush-connection connection)
    (close-connection connection)))

(defn get-expenses [user-id start-date end-date]
  (with-audited-connection [user-id connection]
    (find-all-between connection start-date end-date)))

(defn add-expense [user-id date amount]
  (with-audited-connection [user-id connection]
    (save-new-expense connection date amount)))