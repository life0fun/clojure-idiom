(ns transducer.xform
  (:require [clojure.core.async :as async :refer [<!! !!>]]))


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


; transducer is a function that takes a reducer step fn and 
; return another reducer step fn.
; transducer fn must be 3-arity fn, 
;  0 - flow thru, 
;  1 - do nothing on result on this step, 
;  2 - reduce step result to final result.
;
(defn map 
  ([f]
    (fn [reducer-step-f]
      (fn
        ([] (reducer-step-f))
        ([result] (reducer-step-f result))
        ([result input]
          (reducer-step-f result (f input)))))))


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



