;; Ref Types
;; (load-file "ref-types.clj")

(ns ns-ref-atom
  (:require clojure.string))


; ref is synchronized multi collection access. Java use lock, clj use dosync
; atom is concurrent collection. Java use strip lock, clj use CAS.
; agent is for sequencing, distribute parallel works, and incur side effect(log) in STM.

; a stub ret a canned val predefined.
; a mock records the fact that it was called with a specific set of args so we can verify api is called properly later from mock log.

; dynamic binding for test driven
; fns are dynamic vars, you can bind it with stub or mock values, elegant.
(defn cal [x y] (prn "real cal " x y) [x y])
(cal x)
(binding [cal (constantly ["mock-val1" "mock-val2"])] (cal "berkeley" "ucla"))

; stub take a list of pairs of [fn-name mock-form] and dynamic binding mock to fn-name.
(defmacro stubbing [stub-forms & body]
  (let [stub-pairs (partition 2 stub-forms)
        returns (map last stub-pairs)
        stub-fns (map #(list 'constantly %) returns)
        real-fns (map first stub-pairs)]
    `(binding [~@(interleave real-fns stub-fns)]
       ~@body)))

(defn calc-x [x1 x2]
  (* x1 x2))
(defn calc-y [y1 y2]
   (/ y2 y1))
(defn some-client []
  (println (calc-x 2 3) (calc-y 3 4)))

(stubbing [calc-x 1 calc-y 2]
  (some-client))

; common stub-fn ret passed in val no matter what args
(defn stub-fn [return-value]
  (fn [& args]
    return-value))

(defmacro stubbing [stub-forms & body]
  (let [stub-pairs (partition 2 stub-forms)
        returns (map last stub-pairs)
        stub-fns (map #(list 'stub-fn %) returns)  ; use stub fn               
        real-fns (map first stub-pairs)]
    `(binding [~@(interleave real-fns stub-fns)]
               ~@body)))

; a mock records when and how API called(time, count, args) so we can verify calls later.


;; REFERENCES AROUND EVIL MUTABLE THINGS
;; Wrapping a mutable object in a Clojure reference type provides absolutely no guarantees 
;; for safe concurrent modification. 
;; Doing this will at best explode immediately or, worse, provide inaccurate results.

;; 4 Refernce types for concurrency.  DeRef @ref @atom @agent
;; Refs are for Coordinated Synchronous access to Many Identities
;; Atoms are for Uncoordinated synchronous access to a single Identity.
;; Agents are for Uncoordinated asynchronous access to a single Identity.
;; Vars are for thread local isolated identities with a shared default value."
;;

; testbed, create a executor service pool and submit n threads repeat k times invoke a fn.
(import '(java.util.concurrent Executors))
(def *pool* (Executors/newFixedThreadPool  ;; dynamic bindable vars with earmuffs
    (+ 2 (.availableProcessors (Runtime/getRuntime)))))

(defn dothread! [f & {thread-count :threads exec-count :times :or (thread-count 1 exec-count 1)}]
  (dotimes [t thread-count]
    (.submit *pool* #(dotimes [_ exec-count] (f)))))

(dothread! move-fn :threads 4 :times 10)

;; create a ref type by name and init it to empty
(def initial-board [[:- :k :-] [:- :- :-] [:- :K :-]])

(defn board-map [f bd]
  (vec (map #(vec (for [s %] (f s))) bd)))

;; now create an ary of ref, with each inited to one row in board
(def board (board-map ref initial-board))

;; mover generator requires sync take from pos and update into to position.
;; (alter ref fun & args) = (apply fn in-trans-val-of-ref args)
(dosync (alter (ref []) conj :k1))
(def to-move (ref [[:K [2 1]] [:k [0 1]]]))
(defn update-to-move [move]
  (alter to-move #(vector (second %) move)))


;; Keep dosync body free of side-effects:
(defn my-thread-unsafe-fn [important-ref]
  (let [start-work (ref false)]
    (dosync
      (when (not @important-ref)
        ;"If a conflict occurs between 2 transactions 
        ;trying to modify the same reference, 
        ;one of them will be retried."
        ;http://clojure.org/concurrent_programming
        (ref-set important-ref true)
        (ref-set start-work true)))
    (when @start-work 
        ;launch side-effects here
    )))

;
; Atom {} = concurrentHashMap<K, FutureTask<T>>, lock free, CAS. reset! swap!  equals putIfAbsent
; Atom [] = concurrentLinkedList, FIFO, how do you do poll ?
(def database (atom {:henk {:username "henk" :password "johnson" :session "test"}
                     :steve {:username "steve" :password "boldwin" :session "test2"}
                     :cane {:username "cane" :password "john" :session "test3"}}))

;; (assoc-in @atom=map [ks] value)
(swap! database assoc-in [:henk :session] "test + swap")

;; update can add new key (update-in map [ks] f args)
(swap! database update-in [:cane :addr] (fnil (fn [n] (conj n "ca")) []))
(swap! database update-in [:cane :age] (fnil inc 0))

;; memoize : cache the result of applying a fn to the args. Arg as key and result as value.
;; Atomic memoization : concurrent hashmap; cache wrapped by fn that get the result from arg. 
;; attach cache as the metadata of fn.
;;
(defn manipulable-memoize [function]
  (let [cache (atom {})]    ;; cache is a atom, a thread safe map
    (with-meta              ;; use fn meta map to store fn static sharable data.
      (fn [& args]          ;; anonym fn object.
        (or (second (find @cache args))    ;; ret map entry second coln is value.
            (let [ret (apply function args)]  ;; really calculate
              (swap! cache  assoc args ret)   ;; cache result. putIfAbsent
              ret)))
      {:cache cache})))

;; without cache
(def slowly (fn [x] (Thread/sleep 3000) x))
(time [(slowly 9) (slowly 9)])

;; with cache
(def sometimes-slowly (manipulable-memoize slowly))
(time [(sometimes-slowly 108) (sometimes-slowly 108)])

;; check cache
(meta sometimes-slowly)
(let [cache (:cache (meta sometimes-slowly))]
  (swap! cache dissoc '(108)'))

;; wrap memoize as the metadata of fn object.
;; apply fn to data. Wrap data into fn.
(def gcd (memoize
           (fn [x y]
             (cond 
               (> x y) (recur (- x y) y)
               (< x y) (recur x (- y x))
               :else x))))

;;
;; A memoization protocol
(defprotocol CacheProtocol
  (lookup  [cache e])
  (has?    [cache e] )
  (hit     [cache e])
  (miss    [cache e ret]))

;; A basic cache type that impl cache protocol.
;; update with result when missing. do not care what actual fn used to compute fn.
;; no concurrent control in the cache type here. wrapper take care of thread-safe.
(deftype BasicCache [cache]
  CacheProtocol       ;; impl the type inside type definition. extend to impl protocol
  (lookup [_ item]
    (get cache item))
  (has? [_ item]
    (contains? cache item))
  (hit [this item] 
    this)
  (miss [_ item result]     ;; create a new type with updated value
    (BasicCache. (assoc cache item result))))

(def cache (BasicCache. {}))
(lookup (miss cache '(servo) :robot') '(servo)')

;; bridge together cache and the fn to compute cache value.
;; if cant found in cache, apply fn to data, set the result into cache.
(defn through [cache f item]
  (if (has? cache item)
    (hit cache item)
    (miss cache item (delay (apply f item)))))   ;; apply fn to key(arg) to get the value.

;; pluggable memoize type. composite of a cache storage and the fn that calculate the value from key(args)
;; Also impl cache protocol, just delegate to underlying concrete cache impl.
;;
(deftype PluggableMemoization [f cache]
  CacheProtocol
  (has? [_ item] (has? cache item))
  (hit  [this item] this)           ;; ret cache itself for chaining.
  (miss [_ item result]             ;; when miss called, takes in result.
    (PluggableMemoization. f (miss cache item result)))  ;; create a new meoize with new value.
  (lookup [_ item]
    (lookup cache item)))


;; alter ref conj to append log to global state syncly
;; defrecord to create a JVM class and bring into current namespace
(defrecord Message [sender text])
(user.Message. "Aaron" "Hello")  ; instantiate the class
(def messages (ref ()))   ; create a ref to an empty list. The list is mutable
(defn add-message [msg]   ; alter mutable with conj as update-fn taking msg as arg
  (dosync (alter messages conj msg)))
(add-message (user.Message. "user 1" "hello"))
(add-message (user.Message. "user 2" "howdy"))

;; swap synch update an-atom by applying fn, (fn @atom-cur-val option-args)
(def current-track (atom {:title "Credo" :composer "Byrd"}))
(deref current-track)  ;; @current-track
(set! *print-length* 10)  ;; set rebindable global dynamic var
(swap! current-track assoc :title "Sancte Deus")  ;; (assoc @atom :title x)

;; send agent update-fn & args, args is de-refed agent value.
(def counter (agent 0 :validator number?))  ; use validator to guard data in agent
(send counter inc)
;; block until update complete if you like
(await-for timeout-mills & agents)
(send bad-agent / 0)  ; send bad operation (div 0) to agent cause agent to crash
(clear-agent-errors agent)


; manage Per-thread state with dynamic vars
(def ^:dynamic foo 10)  ; root binding wont change. Not mutable. If you require mutable, use @atom
(.start (Thread. (fn [] (println foo))))
(binding [foo 42] foo)  ;; bind similar to let, re-bind foo and check its val
(defn print-foo [] (println foo))
(let [foo "let foo"] 
  (print-foo))  ; when print-foo invoked, static scoping looks for var foo.
(binding [foo "bound foo"]    ; temporally change foo val and within binding form
  (print-foo))                ; root binding wont be changed


; Deal with Java Callbacks with dynamic bindings
; contentHandler is a callback got invoked when stream parser encounter certain token
; the current offset in stream, as a mutable pointer, to a specific spot in stream.
; SAX parser call startElement when it encounter start tag, and the callback fn update dynamic var binding
(startElement [uri local-name q-name #^Attributes atts]
  (set! *stack* (conj *stack* *current*)) (set! *current* e)
  (set! *state* :element))

(endElement [uri local-name q-name]
  ; details omitted
  (set! *current* (push-content (peek *stack*) *current*)) (set! *stack* (pop *stack*))
  (set! *state* :between))


;; like Collections.synchronizedMap. Wraps a memoize cache with atom, provide thread-safe.
;; cache-impl is memoization type, created from a fn and a concrete cache protocol
;;
(defn memoization-impl [cache-impl]  ;; Collections.synchronized(new HashMap())
  (let [cache (atom cache-impl)]     ;; wrap cache into Atom ref tyep for concurrent access.
    (with-meta
      (fn [& args]      ;; cache lookup, atomically swap the value of cache by apply through fn to cache.
        (let [cs (swap! cache through (.f cache-impl) args)]  ;; (through cache f args)
          @(lookup cs args)))
      {:cache cache})))

(def slowly (fn [x] (Thread/sleep 3000) x))
(def sometimes-slowly (memoization-impl
                        (PluggableMemoization.
                          slowly                ;; apply this fn to keys to get result.
                          (BasicCache. {}))))

;;
;; Agent: more than executor service as it maintain an identity value.
;; two thread-pools : unbounded thread pool and Bounded thread pool.
;; send : action queued with send are applied by a limited thread pool.
;; send-of : action queued with send-off are applied by unbounded thread pool.
;; send-off is preferred for functions which block on IO, send for those which block on CPU.
;; (await-for blocks on any number of threads until they all complete or the timeout value, specified in milliseconds, is reached.)

; agent patten: distribute fn to run parallel in multiple thread in one box.
; use rabbitmq to distribute fn to run in many processes in many boxes.
(defn get-url [url]
  (let [conn (.openConnection (URL. url))]   ; = openStream
    (.setRequestMethod conn "GET")
    (.connect conn)
    (with-open [stream (BufferedReader.
                       (InputStreamReader. (.getInputStream conn)))]
      (.toString (reduce #(.append %1 %2)
                          (StringBuffer.) (line-seq stream))))))
; map works(args) to a set of agents, one core per agent.
; send-off computation fns to each agent, and collect result.
(defn get-urls [urls]
  (let [agents (doall (map #(agent %) urls))]
    (doseq [agent agents] (send-off agent get-url))
    (apply await-for 5000 agents)
    (doall (map #(deref %) agents))))

(prn (get-urls '("http://lethain.com" "http://willarson.com")))

; STM can not have side effect. So how do you impl update state and save the state to db ?
; Agent can be used to facilitate intended side-effect inside ref change transaction.
; clojure STM holds all actions that needs to be sent to agent until they succeed.
; this ensure only one send to agent even when STM retries.
(dosync
  (send agent-one log-message args-one)
  (send-off agent-two send-message-on-queue args-two)
  (alter a-ref ref-function)
  (some-pure-function args-three))

; agents are submitted functions which are executed with agent val as arg to fn in order.
; if agent wraps file name, submitted fn must open file, write, and close(spit, slurp)
; if agent wraps BufferedWriter, submitted fn just use the writer to output buffer.
; In the end, agent is for sequencing fn operations on a global data structure.
; usage: sequencing writes to file, sequencing update of shared collection.
; one core per agent, each agent has a queue, submit task to the queue, agent runs parallel.
;
; persistent logging. ref alter append log to global, and send fn to store snapshot to agent.
; agent val is log file name, send-off execute write log fn with filename de-ref and snapshot.
(def backup-agent (agent "output/messages-backup.clj"))
(defn add-message-with-backup [msg]
  (letfn [(log [logfile logmsg]
            (spit logfile logmsg)  ; open f with writer, writes, the close.
            logfile)]  ; ret val is set as new val of the reference.
    (dosync ; start transaction 
      ; grab the commute in-trans ref val of message. at commit time, the val is set to be most-recent commit val
      (let [snapshot (commute messages conj msg)]
        ; send the data to backup agent to persist while inside the transation
        (send-off backup-agent log snapshot)))))

;; agent wraps/ref around eveil mutable BufferedWriter
;; agent's new value is set as the ret value of fn send to agent
(ns logger 
  (:import (java.io BufferedWriter FileWriter)))

; agent value stores buffered writer instance.  
(let [wtr (agent (BufferedWriter. (FileWriter. "agent.log")))]
  (defn log [msg]
    (letfn [(write [out msg]
              (.write out msg) ; BufferedWriter.write(String str)
              out)]   ;; .write ret outstream, so it can be set to agent.
      (send wtr write msg)))  ;; send (fn [@agent, data])
  (defn log [msg]
    (send wtr #(doto % (.write msg))))
  (defn close []
    (send wtr #(.close %))))

(log "test\n")
(log "another line\n")
(close)

;;
;; store url as agent value, send-off get-url fn to agent and wait for result.
;; the send-off thread take agent val(url) and execute get-url fn to download url.
;;
(ns parallel-fetch
  (:import (java.io InputStream InputStreamReader BufferedReader)
           (java.net URL HttpURLConnection)))

(defn get-url [url]
  (let [conn (.openConnection (URL. url))]
    (.setRequestMethod conn "GET")
    (.connect conn)
    (with-open [stream (BufferedReader.
                       (InputStreamReader. (.getInputStream conn)))]
      (.toString (reduce #(.append %1 %2)
                          (StringBuffer.) (line-seq stream))))))

(defn get-urls [urls]
  (let [agents (doall (map #(agent %) urls))]
    (doseq [agent agents] (send-off agent get-url))
    (apply await-for 5000 agents)
    (doall (map #(deref %) agents))))

(prn (get-urls '("http://lethain.com" "http://willarson.com")'))

;;
;; send a msg around a ring test
;; create a ring of agent, nested agent, (agent (agent (agent {})))
;; when apply the relay fn to an agent, if agent contains another agent, recursive relay.
;;
(ns agents-queue)

(def logger (agent (list)))
(defn log [msg]
  (send logger #(cons %2 %1) msg))

;; use reduce to create agent ref around agent, recursive. (agent (agent (agent )))
(defn create-relay [n]
  (letfn [(next-agent [previous _] (agent previous))]
    (reduce next-agent nil (range 0 n)))) ; (agent (agent (...(agnent 0))))

;; first arg is agent
(defn relay [relay msg]
  (letfn [(relay-msg [next-actor hop msg]
          (cond (nil? next-actor)  (log "finished relay")
                :else (do (log (list hop msg))
                          (send next-actor relay-msg (+ hop 1) msg))))]
    (send relay relay-msg 0 msg)))


(relay (create-relay 10) "hello")
(. java.lang.Thread sleep 5000)
(prn @logger)

; output from running script is:
;  ("finished relay" (8 "hello") (7 "hello") 
;  (6 "hello") (5 "hello") (4 "hello")
;  (3 "hello") (2 "hello") (1 "hello")
;  (0 "hello"))

;; future and promise
;; promise
;; 1. you create a promise, and pass the promise to any thread.
;; 2. whoever hold the promise, when trying to defer it, before you delivery the promise, will block.
;; 3. you done calculation, delivery the result to the promise object, and whoever de-ref it will get the result.

;; use future to submit tasks to executor pool.
;; task is an expr to be evaled; the expr is a closure that wraps some computation. Closure contains task's args 
;; you de-ref the future, will be blocked if future not calculated. Otherwise, you get the result back.
;; Future<T> f = executor.submit(new Callable<T>(){public T call(){}})
;; f.get()
;
; to close over args to call, use a class to encapsulate.
; public class DoPing implements Callable<String>{
;    private final String ipToPing;
;    public DoPing(String ipToPing) { this.ipToPing = ipToPing; }
;    public String call() throws Exception {
;        InetAddress ipAddress = InetAddress.getByName(ipToPing); } }
;
; ExecutorService.submit(new DoPing("google.com"))

;
; In clojure, all fn is callable, just wrap fn, which is a closure over args, into future,
; which is auto submit to executor to run, and return FutureTask to de-ref
; (map-indexed
;   (fn [image i]  ; wrap blocking io into future, run in separate thread
;     (future (upload-image image (format "myimage-%s.jpg" i))))
;   images)))
;

(def a-promise (promise))
(deliver a-promise :fred)

(def f (future (dosync ( (Thread/sleep 200) @some-ref ))))
(deref f)

;;
;; 1. what really means by submit tasks to executors to get future.
;; 2. task is a fn expr, eval the expr means execute the fn.
;; 3. how task fn expr is a closure that carries the context/arguments to execute.
;;
(require [aws.sdk.s3 :as s3])

(def credentials
  {:access-key "my s3 access key" :secret-key "super secret key"})

(def bucket-name "user-images")

(defn update-image [image filename]
  (with-open [os (ByteArrayOutputStream.)]  ; create a outputstream obj to write image to
    (ImageIO/write image "jpg" os)  ; write jpg to the output stream object
    (let [request (s3/put-object credentials bucket-name filename
                                 (clojure.java.io/input-stream (.toByteArray os)))]
      {:finished true :request request})))

;; list comprehension of all images, map each item with an anonym fn to wrap it and 
;; call upload-image from inside a future.
(defn upload-images [images]
  (doall
    (map-indexed
      (fn [image i]  ; wrap blocking io into future, run in separate thread
        (future (upload-image image (format "myimage-%s.jpg" i))))
      images)))

;; submit the future and wait for the result.
(def f (upload-images images))
(map deref f)


;;
;; promise, one thread create a promise, give it to other threads
(def guest-count (promise))

;; the other thread can poll the promise, or wait for callback event inside a future.
(future (manager-duties guest-count))

(defn talk-to-guests []
  (prn "Talking to guests"))

(defn train-wait-staff []
  (prn "Training wait staff"))

(defn check-party-guest-count [p]
  (if (realized? p)   ;; ret true if a val has been produced for a promise.
    (deref p)
    (prn "Don't know the guest count yet, call later")))

;; periodically check whether promise is realized.
(defn manager-duties [cnt]
  (talk-to-guests)
  (train-wait-staff)
  (if-let [count (check-party-guest-count cnt)]
  (prn (format "Total guest count is %s" count))
  (do
    (Thread/sleep 1000)
    (manager-duties cnt))))

;;
;; locking macro
(defn make-safe-array [t sz]
  (let [a (make-array t sz)]
    (reify
      SafeArray
      (count [_] (clj/count a))
      (seq [_] (clj/seq a))
      (aget [_ i]
        (locking a
          (clj/aget a i)))
      (aset [this i f]
        (locking a)))))

(def A (make-safe-array Integer/TYPE 8))
(pummel A)

;; use java explicit lock if you do customize locking strategy, e.g., lock striping, etc.
(import 'java.util.concurrent.locks.ReentrantLock')


;; Watching for mutation.
;; add an event listener that gets notified when val of stateful var changes.
;; (add-watch ) allow you to register callback to be invoked when state changed.

(def adi (atom 0))
(defn on-change [the-key the-ref old-value new-value]
  (println "Hey, seeing change from" old-value "to" new-value))

(add-watch adi :adi-watcher on-change)
(swap! adi inc)
(remove-watch adi :adi-watcher)
