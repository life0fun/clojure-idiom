(ns chapter-data.master-core
  (:use chapter14-rabbitmq-multicast 
        chapter14-worker-multicast
        chapter-data.status
        clojure.contrib.seq-utils)
  (:require redis))

(defn new-job [job-id worker batch-size batch-wait-time id-generator]
  {:tasks-atom (atom {})
   :job-id job-id
   :worker worker
   :batch-size batch-size
   :batch-wait-time batch-wait-time
   :id-gen id-generator})

(defn run-task [{:keys [job-id worker tasks-atom]} task-id args mark-status]
  (when (should-run? job-id task-id)
    (println "Running task [" job-id task-id "]")
    (mark-status job-id task-id)
    (let [task-info {:args args
                     :proxy (apply worker [job-id task-id args])}]
      (swap! tasks-atom assoc task-id task-info))))

(defn run-batch [{:keys [id-gen tasks-atom batch-wait-time] :as job} args-batch]
  (doseq [args args-batch]
    (run-task job (apply id-gen args) args mark-dispatched))
  (wait-until-completion (map :proxy (vals @tasks-atom)) batch-wait-time))

(defn start-job [{:keys [batch-size] :as job} args-seq]
  (redis/with-server (redis-config)
    (let [args-batches (partition-all batch-size args-seq)]
      (doseq [args-batch args-batches]
        (run-batch job args-batch)))))

(defn recover-job [{:keys [tasks-atom] :as job}]
  (doseq [incomplete-id (incomplete-task-ids job)]
    (let [args (get-in @tasks-atom [incomplete-id :args])]
      (run-task job incomplete-id args mark-recovery))))

(defn slave-wrapper [worker-function]
  (fn [job-id task-id worker-args]
    (redis/with-server (redis-config)
      (increment-status job-id task-id)
      (try
       (let [return (apply worker-function worker-args)]
         (increment-status job-id task-id)
         return)
       (catch Exception e
         (mark-error job-id task-id))))))

(defmacro slave-worker [name args & body]
  `(let [simple-function# (fn ~args (do ~@body))
         slave-function# (slave-wrapper simple-function#)]
     (defworker ~name [~'job-id ~'task-id ~'worker-args]
       (slave-function# ~'job-id ~'task-id ~'worker-args))))

(defmacro with-rabbit-redis [& body]
  `(with-rabbit ["localhost" "guest" "guest"]
     (redis/with-server (redis-config)
       ~@(do body))))