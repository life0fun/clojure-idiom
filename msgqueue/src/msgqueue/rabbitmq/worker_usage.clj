;
; use worker macro create a set of workers to execute computations
; in async mode.
;
(ns msgqueue.rabbitmq.worker-usage
	(:use msgqueue.rabbitmq.rabbitmq)
	(:use msgqueue.rabbitmq.worker))

; def a var with root binding to fn named service-name with args and expr as body, 
; most importantly, add fn object to workers map to make it namespace closure. 
; any distributed thread load the namespace can access the defed fn object.
; so client threads can send name string cross and processor thread can access
; fn object from map and call the fn object with the args.
(defworker long-computation-one [x y]
  (Thread/sleep 3000)
  (* x y))

(defworker long-computation-two [a b c]
  (Thread/sleep 2000)
  (+ a b c))

(defworker expensive-audit-log [z]
  (println "expensive audit log:" z)
  (Thread/sleep 4000))

; test worker, send worker request across.
(defn worker-task []
	(println "Dispatching test worker")
	(with-rabbit ["localhost" "guest" "guest"]
    ; invoke the worker computation, on-swarm send workers to remote
  	(let [one (long-computation-one 10 20)
    	    two (long-computation-two 3 5 7)]
    	;(fire-and-forget expensive-audit-log 100)
    	;(run-worker-everywhere expensive-audit-log 777)
      
      ; now spin on worker status. wait computation done.
    	(from-swarm [one two]  
                (println "one:" (one :value))
                (println "two:" (two :value)))))
	(println "done!"))

; apply(invoke) fn object extracted from worker namespace workers map 
; with the passed in args, ret result in a ret map.
(defn response-for [worker-handler worker-args]
  (try
  	(let [value (apply worker-handler worker-args)]
     	{:value value :status :success})
   	(catch Exception e 
     	{:status :error})))

; Use a FutureTask to block on the result of invoking the fn expr with args
; once FutureTask returns, send back the result thru return-q
(defn process-request [worker-handler worker-args return-q]
  (future 
  	(with-rabbit ["localhost" "guest" "guest"]
      (let [response-envelope (response-for worker-handler worker-args)]
        (if return-q (send-message return-q response-envelope))))))

; worker process listens msg from rabbitmq queue, upon msg available,
; extrace fn object maped to the msg from global workers and run it.
(defn handle-request-message [req-str]
  (try
  	(let [req (read-string req-str)
          worker-name (req :worker-name) 
          worker-args (req :worker-args) 
          return-q (req :return-q)
          ; get fn object from worker namespace workers map.
          worker-handler (@workers worker-name)]
    	(if (not (nil? worker-handler))
      	(do
        	(println "Processing:" worker-name "with args:" worker-args)
        	(process-request worker-handler worker-args return-q))))
  (catch Exception e)))

; start listening on workers queue, which abstracted as message-seq. 
; put it inside a FutureTask as it is blocking call. use clojure seq lib to doseq.
(defn start-handler-process []
  (println "Serving up" (count @workers) "workers.")
  (future 
    (with-rabbit ["localhost" "guest" "guest"]
      (doseq [request-message (message-seq WORKER-QUEUE)]
        (handle-request-message request-message)))))

; start listening on bcast queue
; put it inside a FutureTask as it is blocking call. use clojure seq lib to doseq.
(defn start-broadcast-listener []
  (future
    (with-rabbit ["localhost" "guest" "guest"]
      (println "Starting worker handler...")
      (doseq [request-message (message-seq BROADCAST-EXCHANGE FANOUT-EXCHANGE-TYPE BROADCAST-QUEUE)]
        (handle-request-message request-message)))))

