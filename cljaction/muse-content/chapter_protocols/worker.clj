(ns chapter-protocols.worker
  (:use chapter-protocols.rabbitmq)
  (:import (java.util UUID)))

(def workers (ref {}))
(def worker-init-value :__worker_init__)
(def WORKER-QUEUE "chapter14_workers_job_queue")
(def BROADCAST-QUEUE "chapter14_workers_broadcast_queue")
(def BROADCAST-EXCHANGE "chapter14_workers_fanex")

(defprotocol RemoteWorker
  (complete? [rw])
  (value [rw])
  (status [rw])
  (disconnect [rw]))

(defn all-complete? [swarm-requests]
  ;(every? #(% :complete?) swarm-requests)
  (every? complete? swarm-requests))

(defn disconnect-worker [[channel q-name]]
  (.queueDelete channel q-name))

(defn disconnect-all [swarm-requests]
  (doseq [req swarm-requests]
    (req :disconnect)))

(defn wait-until-completion [swarm-requests allowed-time]
  (loop [all-complete (all-complete? swarm-requests)
         elapsed-time 0]
    (if (> elapsed-time allowed-time)
      (do
        (disconnect-all swarm-requests)
        (throw (RuntimeException. (str "Remote worker timeout exceeded " allowed-time " milliseconds!"))))
       (if (not all-complete)
	 (do
	   (Thread/sleep 100)
	   (recur (all-complete? swarm-requests) (+ elapsed-time 100)))))))

(defmacro from-swarm [swarm-requests & expr]
  `(do
     (wait-until-completion ~swarm-requests 5000)
     ~@expr))

(defn update-on-response [worker-ref return-q-name]
  (let [channel (.createChannel *rabbit-connection*)
        consumer (consumer-for channel DEFAULT-EXCHANGE-NAME DEFAULT-EXCHANGE-TYPE return-q-name return-q-name)
	on-response (fn [response-message] 
                      (dosync 
                       (ref-set worker-ref (read-string response-message))
                       (.queueDelete channel return-q-name)
                       (.close channel)))]
    (future (on-response (delivery-from channel consumer)))
    [channel return-q-name]))

(defn request-envelope
  ([worker-name args]
     {:worker-name worker-name :worker-args args})
  ([worker-name args return-q-name]
     (assoc (request-envelope worker-name args) :return-q return-q-name)))

(defn dispatch-work [worker-name args worker-ref]
  (let [return-q-name (str (UUID/randomUUID))
        request-object (request-envelope worker-name args return-q-name)
        worker-transport (update-on-response worker-ref return-q-name)]
    (send-message WORKER-QUEUE request-object)
    worker-transport))

(defn attribute-from-response [worker-internal-data attrib-name]
  (if (= worker-init-value worker-internal-data)
    (throw (RuntimeException. "Worker not complete!")))
  (if (not (= :success (keyword (worker-internal-data :status))))
    (throw (RuntimeException. "Worker has errors!")))
  (worker-internal-data attrib-name))

(defn on-swarm [worker-name args]
  (let [worker-data (ref worker-init-value)
        worker-transport (dispatch-work worker-name args worker-data)]
    (reify RemoteWorker
      (complete? [rw]
        (not (= worker-init-value @worker-data)))
      (value [rw]
             (attribute-from-response @worker-data :value))
      (status [rw]
              (@worker-data :status))
      (disconnect [rw]
        (disconnect-worker worker-transport)))))

(defmacro worker-runner [worker-name should-return worker-args]
  `(fn ~worker-args
     (if ~should-return
       (on-swarm ~worker-name ~worker-args))))

(defmacro defworker [service-name args & exprs]
  `(let [worker-name# (keyword '~service-name)]
     (dosync 
      (alter workers assoc worker-name# (fn ~args (do ~@exprs))))
     (def ~service-name (worker-runner worker-name# true ~args))))

(defn run-worker-without-return [worker-name-keyword args]
  (let [request-object (request-envelope worker-name-keyword args)]
    (send-message WORKER-QUEUE request-object)))

(defmacro fire-and-forget [worker-symbol & args]
  `(run-worker-without-return (keyword '~worker-symbol) '~args))

(defn run-worker-on-all-servers [worker-name-keyword args]
  (let [request-object (request-envelope worker-name-keyword args)]
    (send-message BROADCAST-EXCHANGE FANOUT-EXCHANGE-TYPE BROADCAST-QUEUE request-object)))

(defmacro run-worker-everywhere [worker-symbol & args]
  `(run-worker-on-all-servers (keyword '~worker-symbol) '~args))