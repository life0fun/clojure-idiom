;; Ref Types
;; (load-file "ref-types.clj")

(ns ns-ref-atom
  (:require clojure.string))


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

;; testbed, create a executor service pool and submit n threads repeat k times invoke a fn.
;;
(import '(java.util.concurrent Executors))
(def *pool* (Executors/newFixedThreadPool
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

;;
;; Atom {} = concurrentHashMap<K, FutureTask<T>>, CAS. reset! swap!  equals putIfAbsent
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

;; A memoization protocol
(defprotocol CacheProtocol
  (lookup  [cache e])
  (has?    [cache e] )
  (hit     [cache e])
  (miss    [cache e ret]))

;; A basic cache type that impl cache protocol.
;; note no concurrent control in the cache type here.
;; we will add thread-safe layer outside cache itself.
(deftype BasicCache [cache]
  CacheProtocol
  (lookup [_ item]
  (get cache item))
  (has? [_ item]
  (contains? cache item))
  (hit [this item] this)
  (miss [_ item result]     ;; create a new type with updated value
    (BasicCache. (assoc cache item result))))

def cache (BasicCache. {}))
(lookup (miss cache '(servo) :robot') '(servo)')

;; pass an element through the cache and return the value
;; if cant found in cache, apply fn to data, set the result into cache.
(defn through [cache f item]
  (if (has? cache item)
    (hit cache item)
    (miss cache item (delay (apply f item)))))   ;; apply fn to key(arg) to get the value.

;; pluggable memoize type, just delegate to underlying concrete cache impl.
;; composite of a cache storage and the fn that calculate the value from key(args)
;;
(deftype PluggableMemoization [f cache]
  CacheProtocol
  (has? [_ item] (has? cache item))
  (hit  [this item] this)           ;; ret cache itself for chaining.
  (miss [_ item result]
    (PluggableMemoization. f (miss cache item result)))
  (lookup [_ item]
    (lookup cache item)))

;; A function for applying pluggable memoization to a function
;; arg is memoization type, created from a fn and a concrete cache protocol
;;
;; memozie-impl is a fn that wraps a concrete cache-impl, and added concurrent access control.
;;
(defn memoization-impl [cache-impl]
  (let [cache (atom cache-impl)]     ;; wrap cache into Atom ref tyep for concurrent access.
    (with-meta
      (fn [& args]
        ;; apply through to cache args, through apply cache-impl's fn to args again.
        (let [cs (swap! cache through (.f cache-impl) args)]  
          @(lookup cs args)))
      {:cache cache})))

(def slowly (fn [x] (Thread/sleep 3000) x))
(def sometimes-slowly (memoization-impl
                        (PluggableMemoization.
                          slowly                ;; apply this fn to keys to get result.
                          (BasicCache. {}))))


;; Agent: more than executor service as it maintain an identity value.
;; two thread-pools : unbounded thread pool and Bounded thread pool.
;; send : action queued with send are applied by a limited thread pool.
;; send-of : action queued with send-off are applied by unbounded thread pool.

(defn exercise-agents [send-fn]
  (let [agents (map #(agent %) (range 10))]
    (doseq [a agents]
      (send-fn a (fn [_] (Thread/sleep 1000))))
    (doseq [a agents]
      (await a))))

(time (exercise-agents send-off))
(time (exercise-agents send))

;; future and promise
;; promise
;; 1. you create a promise, and pass the promise to any thread.
;; 2. whoever hold the promise, when trying to defer it, before you delivery the promise, will block.
;; 3. you done calculation, delivery the result to the promise object, and whoever de-ref it will get the result.

;; future as callback.
;; you submit computation to executor service, get a future.
;; you de-ref the future, will be blocked if future not calculated. Otherwise, you get the result back.
;; future<T> f = executor.submit(new Callable<T>(){public T call(){}})
;; f.get()

(def a-promise (promise))
(deliver a-promise :fred)

(def f (future (dosync ( (Thread/sleep 200) @some-ref ))))
(deref f)


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
