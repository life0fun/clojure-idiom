;; a framework for batch processing
;;
(ns batching
  (:import [java.io FileReader]
           [java.util Map Map$Entry List ArrayList Collection Iterator HashMap])
  (:import [java.text SimpleDateFormat]
           [java.util Calendar TimeZone])
  (:import [java.util.concurrent Executors])
  (:use clojure.contrib.io
        clojure.contrib.seq-utils))


;; example of using executor service.
;; clojure fn is java.util.concurrent.Callable !
(defn test-stm [nitems nthreads niters]
  (let [refs  (map ref (repeat nitems 0))  ; a list of global state refed.
        pool  (Executors/newFixedThreadPool nthreads)
        ; loop create n fn closures, upon invoke, do n times of batch alter refs.
        tasks (map (fn [t]
                      (fn []
                        (dotimes [n niters]
                          (dosync
                            (doseq [r refs]
                              (alter r + 1 t))))))
                   (range nthreads))]
    ; executor invoke all clojure fns, which is java Callables.
    ; ret result is wrapped inside blocking future. get it one by one !
    (doseq [future (.invokeAll pool tasks)]  ; Collection<Callable> task
      (.get future))
    (.shutdown pool)
    (map deref refs)))


;;
;; the job meta map
(defn new-job [job-id worker batch-size batch-wait-time id-generator]
  {:tasks-atom (atom {})   ;; store task id and body as task is running.
   :job-id job-id
   :worker worker
   :batch-size batch-size
   :batch-wait-time batch-wait-time
   :id-gen id-generator})   ;; snowflake id gen

;; composite key to store in redis
(def KEY-SEPARATOR "___")
(defn managed-key [job-id task-id]
  (str job-id KEY-SEPARATOR task-id))

;; update job status in redis
(def STATUS-FIELD "status")
(defn update-status-as [job-id task-id status]
  (redis/hset (managed-key job-id task-id) STATUS-FIELD status))

(def DISPATCHED "dispatched")
(defn mark-dispatched [job-id task-id]
  (update-status-as job-id task-id DISPATCHED))

(def INITIAL "initial")
(def COMPLETE "complete")
(def ERROR "error")
(def RECOVERY "recovery")
(def SECONDARY "secondary")
(def UNKNOWN "unknown")

(defn mark-error [job-id task-id]
  (update-status-as job-id task-id ERROR))

(defn mark-recovery [job-id task-id]
  (update-status-as job-id task-id RECOVERY))

(def next-status {
  DISPATCHED INITIAL
  INITIAL    COMPLETE
  RECOVERY   SECONDARY
  SECONDARY  COMPLETE
  ""         UNKNOWN
  nil        UNKNOWN
  UNKNOWN    UNKNOWN
})

(defn status-of [job-id task-id]
  (redis/hget (managed-key job-id task-id) STATUS-FIELD))

(defn increment-status [job-id task-id]
  (->> (status-of job-id task-id)
        (next-status)
        (update-status-as job-id task-id)))

;; dispatch a job
(defn start-job [{:keys [batch-size] :as job} args-seq]
  (let [args-batches (partition-all batch-size args-seq)]
    (doseq [args-batch args-batches]
      (run-batch job args-batch))))

(defn run-batch [{:keys [id-gen tasks-atom batch-wait-time] :as job}
                 args-batch]
  (doseq [args args-batch]
    (run-task job (apply id-gen args) args mark-dispatched))
  (wait-until-completion (map :proxy (vals @tasks-atom)) batch-wait-time))

;;
(defn run-task [{:keys [job-id worker tasks-atom]}
                        task-id args mark-status]
  (mark-status job-id task-id)
  (let [task-info {:args args
                   :proxy (apply worker [job-id task-id args])}]
    (swap! tasks-atom assoc task-id task-info)))


;;
;; slave wrapper to wrap computation and ret a fn closure.
;; fn wrapper always ret fn closure to be called with args to the computation, and ids.
;;
(defn redis-config []
  {:host "localhost"})

(defn slave-wrapper [worker-function]
  (fn [job-id task-id worker-args]
    (redis/with-server (redis-config)
      (increment-status job-id task-id)
      (try      ;; apply fn with the passed args.
        (let [return (apply worker-function worker-args)]
          (increment-status job-id task-id)
          return)
      (catch Exception e
        (mark-error job-id task-id))))))

(defmacro slave-worker [name args & body]
  `(let [simple-function# (fn ~args (do ~@body))
         slave-function# (slave-wrapper simple-function#)]
    (defworker ~name [~'job-id ~'task-id ~'worker-args']
      (slave-function# ~'job-id ~'task-id ~'worker-args'))))

;; the real computation, simulate factorial
(defn fact [n acc]
  (if (= n 0)
    acc
    (recur (dec n) (* n acc))))

(slave-worker factorial [n]
  (let [f (fact n 1)]
    (println "Calculated factorial of" n "value:" f)
    f))


; sleeping barbar, use Q to connect producer-consumer.
; A queue to tie up producer and consumer. Use ref to sequence the access to queue.
; or deque/ringbuffer, producers lock on write idx, consumers on read idx.
; 1. use wait-notify
; 2. LinkedBlockingQueue, wont blocking if queue is empty. LinkedBlockingQueue will block.
; 3. clojure.lang.PersistentQueue/EMPTY for FIFO Q, ref fence it.
; Pattern: wait, wake, lock, check, mute, notify

(def queue  (ref (with-meta
                   clojure.lang.PersistentQueue/EMPTY
                   {:tally 0})))
(def seats  3)

(defn debug [_ msg n]  ; first unamed args
  (println msg (apply str (repeat (- 35 (count msg)) \space)) n)
  (flush))


(defn the-shop [a]
  ; the-shop is the Q connect producer and consumer
  (debug "(c) entering shop" a)
  (dosync
    (if (< (count @queue) seats)
      (alter queue conj a)    ; mutate ref with update fn
      (debug "(s) turning away customer" a))))


; Pattern: sync, peek, pop, set
; consumer, wait wake lock update notify
(defn the-barber [st q]
  (Thread/sleep (+ 100 (rand-int 600)))
  (dosync
    (when (peek @q)
      (debug "(b) cutting hair of customer" (peek @q))
      (ref-set queue (with-meta (pop @q)
                      {:tally (inc (:tally (meta @q)))})))))


; observe mutable state changed, invoke my callback.
; (add-watch refvar :key (fn [k refvar os ns] (print k refvar os ns)))
(add-watcher queue :send (agent 'barber) the-barber)
(add-watch queue :customer-ready the-barber)
(add-watcher queue :send (agent 'barber) the-barber)

(doseq [customer (range 1 20)]
  ; (Thread/sleep (+ 100 (rand-int 200)))
  (send-off (agent customer) the-shop))

(Thread/sleep 2000)
(println "(!) " (:tally (meta @queue)) "customers got haircuts today")
                        {:tally (inc (:tally (meta @q)))})))))


