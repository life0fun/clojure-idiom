(ns transducer.xform
  (:require [clojure.core.async :as async :refer [<! <!! >! >!!]]))

; http://thecomputersarewinning.com/post/Transducers-Are-Fundamental/
; transducer is lead a list of transform function pipeline functions across before final reduce.
; (transduce xform final-reduce-step-fn init coll)
;
; why ? It is for composition of sequence-iterating function that decoupling from sequence.
; 1. avoid incidental sequences in the middle. 
; 2. composition fn logic can be used across things that are not necessarily sequences. core.reducers, channels.
;

;; use sequence to return a new transduced sequence.
(sequence (map inc) (range 10))

; Step fn might want to do a final transofrmation of the value built up for completion process.
; thus, all step functions must have an arity-1 variant that does not take an input
; arity-2 is step operation, with total and cursor value.
; arity-0 is init operation, the value 
(defn mean-reducer 
  ([] {:sum 0 :count 0})        ; init operation, arity-0, provide init value reduce build up.
  ([memo] memo)  ; completion fn, do a final transformation of the value built up.
  ([memo x] (-> memo (update-in [:sum] + x) (update-in [:count] inc)))) ; step operation, build up value at each step.


;; trans/leads xform pipeline that apply to a sequence, before give list to final reduce func.
(reduce mean-reducer {:sum 0 :count 0} (range 10))

; transduce is transform + reduce a collection.
(transduce (map inc) mean-reducer {:sum 0 :count 0} (range 10))
(transduce (map inc) mean-reducer (range 10))  ; with init operation, arity-0 support

;; use into to populate new data structure without intermediate sequence
; ret the chan that contains the single coll result. source chan must be close before into can output value.
(into [] (map inc) (range 10))


; transducer is a function that takes a normal fn that can be apply to cursor args during reduce step,
; return a fn that is plugged into transducer pipeline, takes the pipeline reducer step fn, 
; and lead to the final 3-arity reduce step fn. 
; transducer fn must be 3-arity fn, 
;  0 - flow thru, 
;  1 - do nothing on result on this step, 
;  2 - reduce step result to final result.
;
(defn map                 ; without coll, (map f) return a transducer
  ([f]	      ; take f that apply to each element in list.
    (fn [reducer-f]  ; ret a fn that can be plug into transducer pipeline, take reduce-fn from pipeline
      (fn                 ;  ret a reducer 3-arity step fn
        ([] (reducer-step-f))
        ([result] (reducer-step-f result))
        ([result input]
          (reducer-step-f result (f input)))))))


;
; filter pred without coll will ret transducer.
(defn filter 
  ([pred]
    (fn [step-f]
      (fn 
        [] (step-f)
        [result] (step-f result)
        [result input] 
          (if (pred input)
            (step-f result input) 
            result))))
   ([pred coll]
     (sequence (filter pred) coll)))


;
; without coll as last arg, ret a transducer.
(defn take-with
  [pred]
  (fn [step-f]
    (fn [tot cursor]
      (if (pred cursor)
        (step-f tot cursor)
        (reduced tot)))))
 
;
; transducer with state, need to create state on every step
(defn dropping-while
  [pred]
  (fn [stepf]
    (let [dv (volatile! true)]
      (fn [r x]
        (let [drop? @dv]
          (if (and drop? (pred x))
            r
            (do
              (vreset! dv false)
              (stepf r x))))))))


; transduce can be viewed as threading a seq, but independent of source of input(aseq) and job(lazy seq creation)
(->> aseq (map inc) (filter even?))

(def xform (comp (map inc) (filter even?)))

; transducer take transducre fn, normal reducer step fn,
; use transducer fn as step fn when calling normal reduce, and apply transducer completing on the return result. 
(defn transducer
  ([xform step-f coll] (transducer xform step-f (step-f) coll))
  ([xform step-f init coll]
    (let [xf (xform step-f)
         ret (reduce xf init coll)]
      ;; finally, apply completing helper
      (xf ret))))

;
; application of transducer
;

; lazily transform the data (one lazy sequence, not three as with composed sequence functions)
(sequence xform data)

; reduce with a transformation (no laziness, just a loop)
(transduce xform + 0 data)

; build one collection from a transformation of another, again no laziness
(into [] xform data)

; create a recipe for a transformation, which can be subsequently sequenced, iterated or reduced
(iteration xform data)

; or use the same transducer to transform everything that goes through a channel
; this demonstrates the corresponding new capability of core.async channels - they can take transducers.
(chan 1 xform)



; --------------------------------------------------------
; core.async/pipeline to parallel workload.
(defn to-proc< [in]
  (let [out (async/chan 1)]
    (async/pipe in out)
    out))

(defn pipeline< [desc c]
  (let [p (partition 2 desc)]
    (reduce (fn [prev-c [n f]]
      (-> (dotime [_ n]
            (async/map< f prev-c)
            to-proc<)
	       async/merge))
      c
      p)))


; pattern: pipeline to parallel a series of transoforms, store result in out-ch.
; use a reducer fn to aggregate the result from out-ch.
; (pipeline xforms in-ch out-ch | reduce reduce-f out-ch)
(defn test-pipeline []
  (let [in-ch (async/to-chan (range 10))
        out-ch (async/chan)
        map-f
          (fn [f]
            (fn [pipeline-f]
              (fn
                ([] (pipeline-f)) ; init operation, arity-0, provide init value reduce build up.
                ([result] 
                  (pipeline-f result))  ; completion fn, do a final transformation of the value built up.
                ([result x]
                  (pipeline-f result (f x))))
              ))
        ; async fn must close the out-ch.
        inc-async
          (fn 
            ([inputv out-ch]
              (async/go
                (async/>! out-ch (inc inputv))
              (async/close! out-ch))))
        sink 
          (fn [ch]
            (let [a (atom [])]
              (async/go-loop []
                (let [v (async/<! ch)]
                  (when-not (nil? v)
                    (swap! a conj v)
                    (recur))))
            a))
        mean-reducer
          (fn
            ([] {:sum 0 :count 0})
            ([result] result)  ; completion fn, do a final transformation of the value built up.
            ([result x] (-> result (update-in [:sum] + x) (update-in [:count] inc)))) ; step operation, build up value at each step.
       ]
    ; (add-watch (sink out-ch) :sink (fn [_ _ old new] (prn "sink-atom " new)))
    (async/pipeline 4 out-ch (map-f inc) in-ch)
    ; (async/pipeline-async 4 out-ch inc-async in-ch)

    ; go block to read out-ch from pipeline.
    (async/go
      (let [
            ; drain out-ch into out-col and apply tranducer to out-col
            ;out-col (async/<! (async/into [] out-ch))
            ;xd (transduce (map inc) mean-reducer out-col)

            ; directly use async/reduce to reduce value in out-ch, return single coll in chan.
            reduce-ch (async/reduce mean-reducer (mean-reducer) out-ch)
            rd (async/<! reduce-ch)
           ]
        ; (prn "transduce " xd)
        (prn "final reduce value " rd)
        ))
    ))

; --------------------------------------------------------
; Atlas task is a render task that contains a list of subtasks with only one overall deadline.
; Parent task adds List<AtlasTasks> subtasks to completionService, and track their completions 
; with Map<Future<Object>, Future<Object>> futureObjects.
; when AtlasTask complete() called, block within timeout, iterate all subtasks.
;
; public <T> AsyncResponse<T> add(Callable<T> nonFinalService, long timeout){
;    // Run the service asynchronously, returning an async response can use to get
;    final AtlasTasks tasks = new AtlasTasks(this, timeout, unit); // tasks.subtasks.add(this);
;    final Future<Object> completionServiceFuture = completionService.submit(
;      TransactionLogger.wrapCallable(
;        new Callable<Object>() {
;          @Override public Object call() { 
;            ATLAS_TASKS.set(tasks); 
;            return service.call();}})
;    );
;    Future<Object> atlasTasksFuture = new Future<Object>() {
;      public boolean cancel(boolean mayInterruptIfRunning) {
;          return completionServiceFuture.cancel(mayInterruptIfRunning);
;      }
;      public boolean isCancelled() {
;          return completionServiceFuture.isCancelled();
;      }
;      public boolean isDone() {
;          return completionServiceFuture.isDone();
;      }
;      public Object get() throws InterruptedException, ExecutionException {
;          try {
;              return completionServiceFuture.get();
;          } catch (InterruptedException originalException) {
;              throw newException;
;          }
;      }
;      public Object get(long timeout, TimeUnit unit) throws InterruptedException, ExecutionException, TimeoutException {
;          try {
;              return completionServiceFuture.get(timeout, unit);
;          } catch (TimeoutException originalException) {
;              throw newException;
;          }
;      }
;    };
;    futureObjects.put(completionServiceFuture, atlasTasksFuture);
;    return newAsyncResponse(atlasTasksFuture, tasks.getDeadlineMs());
;  }
; 
;  // block caller on completion service poll when result is not available before timeout.
;  public void complete() {
;    for (AtlasTasks tasks : subtasks) { tasks.complete(); }
;    while (hasNext()) {
;      long timeout = getTimeoutMs();
;      completionService.poll(timeout, TimeUnit.MILLISECONDS); 
;    }
;  public boolean hasNext() {
;    long timeout = getTimeoutMs();
;    if (timeout <= 0) { return false }
;    synchronized (this) {
;      if (!completionQueue.isEmpty()) { return true; }
;      for (Future<Object> futureObject : futureObjects.values()) {
;        if (!futureObject.isDone()) { return true; }
;      }
;      return false;
;    }
;  public Object next() throws Exception {
;    long timeout = getTimeoutMs();
;    if (timeout <= 0) { return null;}
;    Future<Object> completionServiceFuture = completionService.poll(timeout, TimeUnit.MILLISECONDS);
;    Future<Object> atlasTasksFuture = futureObjects.get(completionServiceFuture);
;    return atlasTasksFuture.get();
;  }
;
; --------------------------------------------------------
; we can use async channel to dispatch and collect subtasks result.
(defn execute-call 
  ([^Callable f]
    (execute-call f clojure.lang.Agent/pooledExecutor))

  ([^Callable f executor]
    (let [fut (.submit executor f)]
      (reify
       clojure.lang.IDeref
        (deref [_] (.get fut))
       java.util.concurrent.Future
        (get [_] (.get fut))
        (get [_ timeout unit] (.get fut timeout unit))
        (isCancelled [_] (.isCancelled fut))
        (isDone [_] (.isDone fut))
        (cancel [_ interrupt?] (.cancel fut interrupt?)))))


; TODO: error-handler, assoc task record with exception from future get result.
(defn exec-callabe-chan
  "run callable task inside the executor, return a chan contains the result, if timed out, null"
  ([^java.util.concurrent.Callable task timeout]
    (callable-chan task clojure.lang.Agent/pooledExecutor timeout))

  ([^java.util.concurrent.Callable task ^java.util.concurrent.ExecutorService executor timeout]
    (async/go
      (let [out-ch (async/chan)
            fut (execute-call task executor)
            result (try (.get fut timeout) (catch Exception e e))
            ]
        (async/>! out-ch (.get fut timeout))
        out-ch)))
  )

; one input chan, one output chan, parallel in between with (fn [x] (go (apply f x)))
(defn pmax
  "invoke f for values from in chan with at max n parallel, f ret a chan that contains
   result, and it results will be put to out chan"
  [max f in-chan out-chan]
  (go-loop [tasks #{in-chan}]
    (when (seq tasks)
      (let [[value task] (alts! (vec tasks))]
        (if (= task in-chan)
          ; data avail from in-chan
          (if (nil? value)
            (recur (disj tasks task));
            (recur (if (= max (count tasks))
                      (disj tasks in-chan) ; get rid of in-chan when max
                      tasks)
                   (f value)  ; f ret a chan that contains the reuslt
            ))
          ; result from task chan avail, put into out-chan
          (do 
            (when-not (nil? value ) (async/>! out-chan value))
            (recur (-> tasks (disj task) (conj in-chan))))
        )))))


; each subtask produce result in a chan, alts! on all subtasks result chan.
(defrecord Task [task-id task-fn callable args executor timeout result ex])

(defn create-task [task-id task-fn args]
  (assoc (->Task) 
    :task-id task-id :task-fn task-fn :args args :timeout 1000
    :callable (fn [] (prn "task " task-id " arg " x) (apply task-fn args))))

; fake 10 tasks
(def tasks (map (fn [id] #(create-task % inc (vector %)) (range 10)))

;
(defn exec-task-chan
  [task]
  (let [ch (exec-callabe-chan (:callable task) (:timeout task))
        out-ch (asyn/chan)
        result (async/<! ch)]
        task (if (instance? clojure.lang.ExceptionInfo result)
              (assoc task :ex result)
              (assoc task :result result))
    (async/>! out-ch task)
    out-ch))


; go-block park the execution on chan read. We do not need completionService.poll() to block caller
; when result is not available before timeout.
(defn run-tasks
  ([]
    (run-tasks tasks))
  
  ([task-specs]
    (let [out-ch (async/chan)
          timeout-ch (async/timeout 1000)
          ; with this map, lost control on max thread created.
          task-chans (map exec-task-chan task-specs)
          ]
      ; alts on all subtasks output chan.
      (go-loop [tasks #{task-chans}]
        (when (seq tasks)
          (let [[task port] (async/alts! (vec (conj task timeout-ch)) :priority true)]
            (if (not= port timeout-ch)
              (if-not (nil? task)
                (do
                  (async/>! out-ch task)  ; send task to out-chan
                  (recur (-> task-chans (disj port)))))
              (do
                (prn "timed out")
                ))))))))


(defn all-complete? [tasks]
  "all tasks completed by exam :result or :ex attr"
  (every? #(or (not= nil (:result %) (not= nil (:ex %))) tasks)))




