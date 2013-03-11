(ns chapter-data.status
  (:require redis))

(def KEY-SEPARATOR "___")

(def STATUS-FIELD "status")

(def DISPATCHED "dispatched")
(def INITIAL "initial")
(def COMPLETE "complete")
(def ERROR "error")
(def RECOVERY "recovery")
(def SECONDARY "secondary")
(def UNKNOWN "unknown")

(def next-status {
    DISPATCHED INITIAL
    INITIAL    COMPLETE
    RECOVERY   SECONDARY
    SECONDARY  COMPLETE

    ""         UNKNOWN
    nil        UNKNOWN
    UNKNOWN    UNKNOWN
})

(defn redis-config []
  {:host "localhost"})

(defn managed-key [job-id task-id]
  (str job-id KEY-SEPARATOR task-id))

(defn status-of [job-id task-id]
  (redis/hget (managed-key job-id task-id) STATUS-FIELD)) 

(defn update-status-as [job-id task-id status]
  (redis/hset (managed-key job-id task-id) STATUS-FIELD status))

(defn mark-dispatched [job-id task-id]
  (update-status-as job-id task-id DISPATCHED))

(defn mark-error [job-id task-id]
  (update-status-as job-id task-id ERROR))

(defn mark-recovery [job-id task-id]
  (update-status-as job-id task-id RECOVERY))

(defn increment-status [job-id task-id]
  (->> (status-of job-id task-id)
       (next-status) 
       (update-status-as job-id task-id)))

(defn task-successful? [job-id task-id]
  (= COMPLETE (status-of job-id task-id)))

(defn job-successful? [job]
  (->> @(:tasks-atom job)
       (keys)
       (map (partial task-successful? (:job-id job)))
       (every? true?)))

(defn from-proxies [job proxy-command]
  (->> @(:tasks-atom job)
       (vals)
       (map :proxy)
       (map #(% proxy-command))))

(defn values-from [job]
  (from-proxies job :value))

(defn job-complete? [job]
  (every? true? (from-proxies job :complete?)))

(defn task-statuses [{:keys [job-id tasks-atom]}]
  (->> @tasks-atom
       (keys)
       (map #(status-of job-id %))))

(defn should-run? [job-id task-id]
  (let [status (status-of job-id task-id)]
    (or (nil? status)
        (some #{status} [DISPATCHED RECOVERY INITIAL SECONDARY ERROR]))))

(defn incomplete-task-ids [{:keys [job-id tasks-atom]}]
  (remove (partial task-successful? job-id) (keys @tasks-atom)))

