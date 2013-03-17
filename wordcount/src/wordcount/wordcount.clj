(ns wordcount.wordcount
  (:import [backtype.storm StormSubmitter LocalCluster])
  (:use [backtype.storm clojure config])
  (:use [clojure.string])
  (:gen-class))

;;
;; (defspout name output-declare option-map % impl)
;; (defbolt name output-declare option-map % impl)
;; the input map is for each bolt is defined in topology.
;;
;; topology map {"comp_id" (spout-spec (spec-impl)) }
;; spout-spec = spec impl and parallelism. output stream map defined inside spout-spec
;; bolt-spec = input-map [spec-impl arglists] spec impl body
;;
;; output map stream-id and stream spec { "1" ["f1" "f2"] "2" (direct-stream ["f1" "f2"])}
;; input-map = {[comp-id stream-id] :stream-grp [comp-id stream-id] ["field1" "field2"]}
;;

;;
;; (defspout name output-declare option-map % impl)
;; output map reduced to a vector of fields iff default stream.
;; impl has [arglist] (impl exprs)
;;
(defspout sentence-spout-parameterized ["word"] {:params [sentences] :prepare false}
  [collector]
  (Thread/sleep 500)
  (emit-spout! collector [(rand-nth sentences)]))

;;
;; random emit sentence.
;;
(defspout sentence-spout ["logevent"]
  [conf context collector]
  (let [logs [ "11806 [Thread-44] INFO  backtype.storm.util  - Async loop interrupted!"
               "11807 [main] INFO  backtype.storm.daemon.executor  - Shut down executor 1:[1 1]"
               "11807 [main] INFO  backtype.storm.daemon.worker  - Shut down executors"
               "11808 [main] INFO  backtype.storm.daemon.worker  - Shutting down transfer thread"
               "11809 [Thread-46] INFO  backtype.storm.util  - Async loop interrupted!"
               "11226 [Thread-38] INFO  backtype.storm.daemon.executor  - Processing received message source: 2:7, stream: default, id: {}, [\"the\"]"
               "11226 [Thread-32] INFO  backtype.storm.daemon.executor  - Processing received message source: 2:7, stream: default, id: {}, [\"cat\"]"
               "11226 [Thread-38] INFO  backtype.storm.daemon.task  - Emitting: 3 default [\"the\" 49]"
               "11226 [Thread-32] INFO  backtype.storm.daemon.task  - Emitting: 3 default [\"cat\" 17]"
               "11226 [Thread-40] INFO  backtype.storm.daemon.executor  - Processing received message source: 2:7, stream: default, id: {}, [\"door\"]"
               "11226 [Thread-32] INFO  backtype.storm.daemon.executor  - Processing received message source: 2:7, stream: default, id: {}, [\"over\"]"
               "11227 [Thread-32] INFO  backtype.storm.daemon.task  - Emitting: 3 default [\"over\" 17]"
               "11227 [Thread-40] INFO  backtype.storm.daemon.task  - Emitting: 3 default [\"door\" 17]"
             ]]
    (spout
      (nextTuple []
        (Thread/sleep 100)
        (emit-spout! collector [(rand-nth logs)]))
     (ack [id]
        ;; You only need to define this method for reliable spouts
        ;; (such as one that reads off of a queue like Kestrel)
        ;; This is an unreliable spout, so it does nothing here
        ))))

;;
;; filter sentence based on keyword, output stream defined with a vector of fields.
;;
(defbolt filter-sentence ["sentence"] {:params [keywords] :prepare false}  ;; output map = a vector of fields
  [tuple collector]  ;; default output stream, reduce to a vector of output field rather than a output map.
                     ;; non-prepared bolt, body impl execute(tuple)
  (let [ sentence (.getString tuple 0)  ;; tuple = list<fields> list<values> get value at position i
         words (.split (.getString tuple 0) " ")
         wordset (into #{} words)
         sect (clojure.set/intersection wordset (set keywords))
       ]
    (if (not-empty sect)
      (emit-bolt! collector [sentence] :anchor tuple)   ;; anchored tuple ensure replay if lost in downstreams.
    (ack! collector tuple) )))

;;
;; (defbolt name output-declare option-map % impl)
;; impl is [arglists] and the body of execute Iface.
;; parameterized the bolt as parameterized collection with {:params []} option. Coll<Type> ~= (bolt "-params")
;; output map stream-id and stream spec { "1" ["f1" "f2"] "2" (direct-stream ["f1" "f2"])}
;; a bolt has many tasks, input stream grping partition streams and dispatch to designated tasks.
;; partition streams with grping: shuffle, fields, global, direct
;;
(defbolt split-sentence {"1" ["word"] "2" ["word" "index"]} {:prepare true}
  [conf context collector]  ; prepared bolt impl takes conf, context, collector
  (let [counts (atom 0)
       ]
    (bolt
      (execute [tuple]
        (let [ nprocessed (swap! counts inc) ]
          (if (odd? @nprocessed)
            (do
                (prn "odd time " @nprocessed)
                (emit-bolt! collector [w i] :anchor tuple :stream "1"))
            (do
                (prn "even time " @nprocessed)
                (emit-bolt! collector [w i] :anchor tuple :stream "2")))
        (ack! collector tuple))))))

;;
;; prepared bolt stores states locally for join and stream aggregation.
;; state is stored in the closure in a mutable map collection.
;;
;; using atom to store mutable state inside clojure.
;; atom to store intermediate states, @atom to de-ref to get state.
;; swap! to update state.
;;
(defbolt word-count ["word" "count"] {:prepare true}
  [conf context collector]  ; prepared bolt impl takes conf, context, collector
  (let [counts (atom {})]   ; mutable state in a mutable collection in the closure.
    (bolt
     (execute [tuple]       ; execute func only takes tuple.
       (let [word (.getString tuple 0)]
         (swap! counts (partial merge-with +) {word 1})  ; merge map zip map set/union  conj collection.
         (emit-bolt! collector [word (@counts word)] :anchor tuple)
         (ack! collector tuple)
         )))))

;;
;; tuple(list<fields> list<values>)
;;
(defbolt combiner ["word" "count"] {:prepare true}
  [conf context collector]
  (let [counts (atom {})
       ]
    (bolt
      (execute [tuple]
        (let [ word (.getString tuple 0)   ;; first value, word
               count (.getLong tuple 1)    ;; second value, count
             ]
          (prn "combiner tuple <<<"  tuple)
          (prn "word " word)
          (prn "count " count)
        )))))

;;
;; topology is a map of component id and its spec
;; spout-spec = spec impl and parallelism. output stream map defined inside spout-spec
;; bolt-spec = input-map and spec impl.
;; bolt-spec input-map = { [comp-id stream-id] :stream-grp [comp-id stream-id] ["field1" "field2"]
;;
(defn mk-topology []
  ; toplogy is a map of component id and corresponding spec.(spout-spec, bolt-spec)
  (topology
    ; spout-spec : spout impl and parallel tasks.
    {"1" (spout-spec (sentence-spout-parameterized   ;; with the params vector
                      ["the cat jumped over the door"
                       "greetings from the faraway land"])
                       :p 2)

     "2" (spout-spec sentence-spout)}

    ; bolt-spec, input declaration, bolt implementation, parallel tasks.
    ; input declaration: a map of stream id and stream groupings.
    ; stream id = [==component id== ==stream id==]
    ; stream grp ["id" "name"]: subscribes with a fields grouping on the specified fields
    {"3" (bolt-spec {"2" :shuffle}   ;; subscribe to 2 default stream
                     (filter-sentence  ;; with params vector
                      ["Processing" "Emitting:"])
                    :p 2)
     "4" (bolt-spec {"3" :shuffle}   ;; default stream from component 3
                     split-sentence
                     :p 2)

     "5" (bolt-spec {["4" "1" ] ["word"]}  ;; grouping by word
                    word-count
                    :p 2)

     "6" (bolt-spec {["4" "2"] ["word" "index"]}
                    combiner
                    :p 2)}))

(defn run-local! []
  (let [cluster (LocalCluster.)]
    (.submitTopology cluster "word-count" {TOPOLOGY-DEBUG false} (mk-topology))
    (Thread/sleep 10000)
    (.shutdown cluster)
    ))

(defn submit-topology! [name]
  (StormSubmitter/submitTopology
   name
   {TOPOLOGY-DEBUG false
    TOPOLOGY-WORKERS 3}
   (mk-topology)))

(defn -main
  ([]
    (run-local!))
  ([name]
    (submit-topology! name)))

