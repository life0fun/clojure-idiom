;;
;; workers on the server side, get msg from queue, and apply fn to the 
;; data(args) extracted from the queue. Send result back thru callback queue.
;;
(ns msgqueue.rabbitmq.worker
  (:use msgqueue.rabbitmq.rabbitmq)
  (:import (java.util UUID)))

; NameSpace global mapping, the name of computation, and the args for it.
; whenever a thread executes any fns inside this namespace, this global vars
; are closures shared among all threads. It's like class static variable.
(def workers (ref {}))
(def worker-init-value :__worker_init__) ; worker init status
(def WORKER-QUEUE "workers_queue")
(def BROADCAST-QUEUE "workers_bcast_queue")
(def BROADCAST-EXCHANGE "workers_fanex")

; result msg come back to ret-q, changed ret status from init.
(defn all-complete? [swarm-requests]
  (every? #(% :complete?) swarm-requests))

(defn disconnect-worker [[channel q-name]]
  (.queueDelete channel q-name))

(defn disconnect-all [swarm-requests]
  (doseq [req swarm-requests]
    (req :disconnect)))

; spin until all requests completed.
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

; wait for all requests completion, and execute post worker completion
(defmacro from-swarm [swarm-requests & expr]
  `(do
     (wait-until-completion ~swarm-requests 5000)
     ~@expr))

; A FutureTask block on getting msg from return-queue.
; ref-set worker-ref with response msg get from return queue name
; wrapped inside a future task. when blocking call rets, 
; setted up when dispatching a runnable to worker.
(defn update-on-response [worker-ref return-q-name]
  (let [channel (.createChannel *rabbit-connection*)
        consumer (consumer-for channel return-q-name)
        ; when blocking call rets, this closure update global ret status.
        on-response (fn [response-message]  
                      (dosync 
                       (ref-set worker-ref (read-string response-message))
                       (.queueDelete channel return-q-name)
                       (.close channel)))]
    ; dwrap blocking call into a future object will be invoke in another thread
    ; when callback and cache result for def. The same as java FutureTask.
    (future (on-response (delivery-from channel consumer)))
    [channel return-q-name]))  ; ret ch and ret qname

; an env is a map with work fn name and args, send over queue to other end
(defn request-envelope
  ([worker-name args]
     {:worker-name worker-name :worker-args args})
  ([worker-name args return-q-name]
     (assoc (request-envelope worker-name args) :return-q return-q-name)))

; dispatch a computation(fn-name is worker-name, provide args, update ret state)
; dispatching also creates a FutureTask blocking on result and updates ret status ref.
(defn dispatch-work [worker-name args worker-ref]
  (let [return-q-name (str (UUID/randomUUID))  ; random reply queue name
        request-object (request-envelope worker-name args return-q-name)
        worker-transport (update-on-response worker-ref return-q-name)]
    (send-message WORKER-QUEUE request-object)
    worker-transport))

; get attrib from ret object
(defn attribute-from-response [worker-internal-data attrib-name]
  (if (= worker-init-value worker-internal-data)
    (throw (RuntimeException. "Worker not complete!")))
  (if (not (= :success (keyword (worker-internal-data :status))))
    (throw (RuntimeException. "Worker has errors!")))
  (worker-internal-data attrib-name))

; dispatch the worker to msg queue. first creates a ref to store ret value(async call).
; part of dispatch is create a listener for response ret a closure that wraps result for accessor attrib getter.
(defn on-swarm [worker-name args]
  (let [worker-data (ref worker-init-value)  ; init a ref to store ret data
        worker-transport (dispatch-work worker-name args worker-data)]
    (fn [accessor]
      (condp = accessor
        :complete? (not (= worker-init-value @worker-data))
        :value (attribute-from-response @worker-data :value)
        :status (@worker-data :status)
        :disconnect (disconnect-worker worker-transport)))))

; on-swarm dispatch the worker computation request to remote thru rabbitmq queue 
(defmacro worker-runner [worker-name should-return worker-args]
  `(fn ~worker-args
     (if ~should-return
       (on-swarm ~worker-name ~worker-args))))

; def a var with root binding to fn named service-name with args and expr as body, 
; most importantly, add fn object to workers map to make it namespace closure. 
; any distributed thread load the namespace can access the defed fn object.
; so client threads can send name string cross and processor thread can access
; fn object from map and call the fn object with the args.
(defmacro defworker [service-name args & exprs]
  `(let [worker-name# (keyword '~service-name)] ; name to keyword, :name
     (dosync 
       ; workers is global map of worker name to fn object.
       ; store service fn form into workers map, remote worker can retrieve it and perform the fn.
       (alter workers assoc worker-name# (fn ~args (do ~@exprs))))
     ; def a var with root binding to the fn service-name(args, body)
     (def ~service-name (worker-runner worker-name# true ~args))))

; dispatch a request to run worker-name fn on remote process.
(defn run-worker-without-return [worker-name-keyword args]
  (let [request-object (request-envelope worker-name-keyword args)]
    (send-message WORKER-QUEUE request-object)))

; dispatch a request to run worker-name fn without asking for ret value.
(defmacro fire-and-forget [worker-symbol args]
  `(run-worker-without-return (keyword '~worker-symbol) ~args))

; bcast requests to all queues asking for executing worker fn.
(defn run-worker-on-all-servers [worker-name-keyword args]
  (let [request-object (request-envelope worker-name-keyword args)]
    (send-message BROADCAST-EXCHANGE FANOUT-EXCHANGE-TYPE BROADCAST-QUEUE request-object)))

; run everywhere by bcast reqeusts to all queues
(defmacro run-worker-everywhere [worker-symbol & args]
  `(run-worker-on-all-servers (keyword '~worker-symbol) '~args))

