;;
;; to use core.async, need to add core.async dependency.
; core.async is a lib, which consists of macros that rewrite
; your code into CSP style. As it is a lib, just dependent on it.
; 
; (defproject com.colorcloud/parallel "0.1.0-SNAPSHOT"
;   :dependencies [[org.clojure/clojure "1.5.1"]
;                  [org.clojure/core.async "0.1.256.0-1bf8cf-alpha"]])



(ns com.colorcloud.parallel
  (:require [clojure.core.async :refer [go go-loop <! >! alts! close!]]))

(defn parallel
  "Processes values from input channel in parallel on n 'go' blocks.

  Invokes f on values taken from input channel. Values returned from f
  are written on output channel.

  Returns a channel which will be closed when the input channel is
  closed and all operations have completed.

  Note: the order of outputs may not match the order of inputs."
  [n f input output]
  (let [tasks (doall
               (repeatedly n
                #(go-loop []
                   (let [in (<! input)]
                     (when-not (nil? in)
                       (let [out (f in)]
                         (when-not (nil? out)
                           (>! output out))
                         (recur)))))))]
    (go (doseq [task tasks]
          (<! task)))))


; process a vector of chan endpoints with maximum number of go blocks or threads.
; the function that process the value must ret a chan, where the result is stored.
; alts! will multi-wait on a vector of chan endpoints.
;
(defn pmax
  "Process messages from input in parallel with at most max concurrent
  operations.

  Invokes f on values taken from input channel. f must return a
  channel, whose first value (if not closed) will be put on the output
  channel.

  Returns a channel which will be closed when the input channel is
  closed and all operations have completed.

  Creates new operations lazily: if processing can keep up with input,
  the number of parallel operations may be less than max.

  Note: the order of outputs may not match the order of inputs."
  [max f ichan ochan]
  (go-loop [tasks #{ichan}]  ; go-loop on a vector of chan endpoints
    (when (seq tasks)   ; nil pun with seq
      (let [[value task] (alts! (vec tasks))] ; multi-wait on a vector of chan endpoints
        (if (= task ichan)
          (if (nil? value)
            (recur (disj tasks task))  ; ichan is closed
            (recur (conj (if (= max (count tasks))  ; max - 1 tasks running
                           (disj tasks ichan)  ; temporarily stop reading ichan
                           tasks)
                         (f value))))  ; f must ret a chan, where result is stored
          ;; one processing task finished: continue reading ichan
          (do (when-not (nil? value) (>! ochan value))
              (recur (-> tasks (disj task) (conj ichan)))))))))


