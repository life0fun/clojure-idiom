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



