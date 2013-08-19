;
; use worker macro create a set of workers to execute computations
; in async mode.
; full fledged version is https://github.com/amitrathore/swarmiji
;
(ns msgqueue.rabbitmq.worker-usage
	(:use msgqueue.rabbitmq.rabbitmq)
	(:use msgqueue.rabbitmq.worker))

; def a var with root binding to fn named with args and expr as body, 
; most importantly, add fn object to workers map to make it namespace closure. 
; remote thread running the same code, load this ns and can access the workers map to get fn object from fn name
; so client threads can just send fn name string thru rabbitmq and remote processor just extract fn object defned here.
; fn object from map and evaluate fn object expression with the args.
(defworker long-computation-one [x y]   ; defn fn body expression.
  (Thread/sleep 3000)
  (* x y))

; defworker create a ns global var bind to worker-runner macro, which is an anonymous
; fn that dispatch task name and args to rabbitmq, and get back a closure to check worker status.
(defworker long-computation-two [a b c]
  (Thread/sleep 2000)
  (+ a b c))

(defworker expensive-audit-log [z]
  (println "expensive audit log:" z)
  (Thread/sleep 4000))

; called from main, test worker, send worker request across.
(defn worker-task []
	(println "Dispatching test worker")
	(with-rabbit ["localhost" "guest" "guest"]
    ; evaluate anonymous fn reted from worker-runner, 
    ; which on-swarm send task name and args to remote and get a closure stub to check ret status.
  	(let [one (long-computation-one 10 20)
    	    two (long-computation-two 3 5 7)]
    	;(fire-and-forget expensive-audit-log 100)
    	;(run-worker-everywhere expensive-audit-log 777)
      
      ; from-swarm macor is a loop to check worker closure returned from defworker.
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


; Remote server worker process listens msg from rabbitmq queue, process msg,
; extrace fn expression from global workers by task name and evaluate fn object.
(defn handle-request-message [req-str]
  (try
  	(let [req (read-string req-str)
          worker-name (req :worker-name) 
          worker-args (req :worker-args) 
          return-q (req :return-q)
          ; extract fn object from worker namespace workers map by task name.
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
  (future   ; for blocking calls, use future to create FutureTask for async non-blocking
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

