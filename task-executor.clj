;; a framework for batch processing
;;
(ns batching
  (:import [java.io FileReader]
           [java.util Map Map$Entry List ArrayList Collection Iterator HashMap])
  (:import [java.text SimpleDateFormat]
           [java.util Calendar TimeZone])
  (:use clojure.contrib.io
        clojure.contrib.seq-utils))

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
