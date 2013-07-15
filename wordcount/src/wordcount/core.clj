(ns wordcount.core
  (:import [backtype.storm StormSubmitter LocalCluster])
  (:use [backtype.storm clojure config])
  (:use [clojure.string])
  (:use [clojure.pprint])
  (:gen-class))

;;
;; (defspout name output-declare option-map % impl)
;; (defbolt  name output-declare option-map % impl) 
;; spout-spec take spout-imp. bolt-spec specify the input to the bolt.
;; spout spec impl body takes topl conf, topl context, and spoutoutputcollector.
;; bolt spec impl execute method [tuple collector]
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
;; unprepared spout only defines nextTuple method.
;;
(defspout sentence-spout-parameterized ["word"] 
  {:params [sentences] :prepare false}
  [collector]
    (Thread/sleep 500)
    (emit-spout! collector [(rand-nth sentences)]))

;;
;; defspout takes 2 method(nexstTuple, ack) impls and ret an ISpout object. 
;; emit tuple is a list of key value pairs.
;;   {"logevent" "Apr 21 01:00:08 haijin-mac kernel[0]: image 2166576128"}
;;
(defspout sentence-spout ["logevent"]  ; output stream has tuples with logevent field.
  [conf context collector]
  (let [logs [
            "Apr 21 01:00:08 haijin-mac kernel[0]: image 2166576128, uncompressed 5045297152 (183186), compressed 2155141936 (42%), sum1 6c67ac3, sum2 72119025"
            "Apr 21 01:00:08 haijin-mac kernel[0]: wired_pages_encrypted 77034, wired_pages_clear 106654, dirty_pages_encrypted 1048074"
            "Apr 21 01:00:08 haijin-mac kernel[0]: hibernate_write_image done(0)"
            "Apr 21 01:00:08 haijin-mac kernel[0]: sleep"
            "Apr 21 01:00:08 haijin-mac kernel[0]: SMC::smcHandleInterruptEvent WARNING status=0x0 (0x40 not set) notif=0x0"
            "Apr 21 08:44:12 haijin-mac kernel[0]: Wake reason: EC LID0"
            "Apr 21 08:44:12 haijin-mac kernel[0]: HID tickle 135 ms"
            "Apr 21 08:44:12 haijin-mac kernel[0]: Previous Sleep Cause: 5"
            "Apr 21 08:44:12 haijin-mac kernel[0]: wlEvent: en1 en1 Link DOWN virtIf = 0"
            "Apr 21 08:44:12 haijin-mac kernel[0]: AirPort: Link Down on en1. Reason 8 (Disassociated because station leaving)."
            "Apr 21 08:44:12 haijin-mac kernel[0]: en1: 802.11d country code set to 'X0'."
            "Apr 21 08:44:12 haijin-mac kernel[0]: en1: Supported channels 1 2 3 4 5 6 7 8 9 10 11 36 40 44 48 52 56 60 64 100 104 108 112 116 120 124 128 132 136 140 149 153 157 161 165"
            "Apr 21 08:44:15 haijin-mac kernel[0]: 00000000  00000020  NVEthernet::setLinkStatus - not Active"
            "Apr 21 08:44:18 haijin-mac kernel[0]: en1: 802.11d country code set to 'US'."
            "Apr 21 08:44:18 haijin-mac kernel[0]: en1: Supported channels 1 2 3 4 5 6 7 8 9 10 11 36 40 44 48 52 56 60 64 100 104 108 112 116 120 124 128 132 136 140 149 153 157 161 165"
            "Apr 21 08:44:35 haijin-mac kernel[0]: MacAuthEvent en1   Auth result for: 5c:d9:98:65:83:d4  MAC AUTH succeeded"
            "Apr 21 08:44:35 haijin-mac kernel[0]: wlEvent: en1 en1 Link UP virtIf = 0"
            "Apr 21 08:44:35 haijin-mac kernel[0]: AirPort: Link Up on en1"
            "Apr 21 08:44:35 haijin-mac kernel[0]: en1: BSSID changed to 5c:d9:98:65:83:d4"
            "Apr 21 08:44:35 haijin-mac kernel[0]: AirPort: RSN handshake complete on en1"
            "Apr 21 09:04:51 haijin-mac kernel[0]: CODE SIGNING: cs_invalid_page(0x1000): p=99354[GoogleSoftwareUp] clearing CS_VALID"
            "Apr 21 10:03:34 haijin-mac kernel[0]: CODE SIGNING: cs_invalid_page(0x1000): p=367[GoogleSoftwareUp] clearing CS_VALID"
            "Apr 21 11:02:18 haijin-mac kernel[0]: CODE SIGNING: cs_invalid_page(0x1000): p=1436[GoogleSoftwareUp] clearing CS_VALID"
            "Apr 21 11:57:07 haijin-mac kernel[0]: MacAuthEvent en1   Auth result for: 5c:d9:98:65:83:d4  MAC AUTH succeeded"
            "Apr 21 11:57:07 haijin-mac kernel[0]: wlEvent: en1 en1 Link UP virtIf = 0"
            "Apr 21 11:57:07 haijin-mac kernel[0]: AirPort: RSN handshake complete on en1"
            "Apr 21 11:57:07 haijin-mac kernel[0]: wl0: Roamed or switched channel, reason #4, bssid 5c:d9:98:65:83:d4"
            "Apr 21 11:57:07 haijin-mac kernel[0]: en1: BSSID changed to 5c:d9:98:65:83:d4"
        ]]
    (spout      ;; spout macro takes 2 method (nextTuple, ack) impl and ret an ISpout object
      (nextTuple []
        (Thread/sleep 100)
        (emit-spout! collector [(rand-nth logs)]))  ;; emit tuple to collector 
        ;; You only need to define this method for reliable spouts
        ;; (such as one that reads off of a queue like Kestrel)
        ;; This is an unreliable spout, so it does nothing here
      (ack [id]))))

;;
;; bolt filters sentence based on a vec of keywords, 
;; in stream tuple: {"logevent" "11226 [Thread-32] INFO  backtype.storm.daemon.task  - Emitting: 3 default [\"cat\" 17]"}
;; output stream defined with a vector of fields.
;; default output stream, reduce to a vector of output field rather than a output map.
;;
(defbolt filter-sentence ["sentence"] ;; output map = a vector of fields(keyword)
  ; params to the bolt in :params, passed in when assembling bolt in topology.
  {:params [keywords] :prepare false} 

  ; for non-prepared bolt, impl fn (execute [tuple collector]) 
  [tuple collector] 
    (let [ sentence (.getStringByField tuple "logevent")  ;; the first string in tuple string list is sentence.
           words (.split sentence " ")
           wordset (into #{} words) ]
      ;(pprint tuple)   ;; what's inside a tuple.
      ;(pprint sentence)   ;; what's inside a tuple.
      ; only emit sentence that has keywords
      (if (some (set keywords) wordset) ;; (clojure.set/intersection wordset (set keywords))
        (emit-bolt! collector [sentence] :anchor tuple)   ;; anchored tuple ensure replay if lost in downstreams.
      (ack! collector tuple) )))   ;; ack the upstream this tuple has been processed.

;;
;; (defbolt name output-declare option-map % impl)
;; impl is [arglists] and the body of execute Iface.
;; parameterized the bolt as parameterized collection with {:params []} option. Coll<Type> ~= (bolt "-params")
;; output map stream-id and stream spec { "1" ["f1" "f2"] "2" (direct-stream ["f1" "f2"])}
;; a bolt has many tasks, input stream grping partition streams and dispatch to designated tasks.
;; partition streams with grping: shuffle, fields, global, direct
;;
(defbolt split-sentence
  { "1" ["word"]          ; out stream 1, output tuple has only one field "word" 
    "2" ["word" "index"]} ; out stream 2, output tuple has two fields "word" and "index"
  {:prepare true}         ; prepare bolt, topology will call prepare(stormConf, ctx)
  [conf context collector] ; prepared bolt impl takes conf, context, collector
  (let [index (atom 0) ]  ; index is atomic counter
    (bolt    ;; impl execute fn for bolt IF.
      (prepare [conf context]  ; prepare fn takes conf and context
        (prn "creating table inside prepare"))

      (execute [tuple]
        (let [ nprocessed (swap! index inc)
               words (.split (.getStringByField tuple "sentence") " ")]   ;; split tuple's string content
          ;(pprint words)
          (if (odd? nprocessed)
            (doseq [w words]  ; odd line to stream 1
              (emit-bolt! collector [w] :anchor tuple :stream "1"))
            (doseq [w words]  ; even line to stream 2 with line no.
              (emit-bolt! collector [w nprocessed] :anchor tuple :stream "2")))  ;; emit tuple to stream 2. with :id x
        (ack! collector tuple))))))

;;
; prepared bolt stores states locally for join and stream aggregation.
; state is stored in the closure in a mutable map collection.
;
; using atom to store mutable state inside clojure.
; atom to store intermediate states, @atom to de-ref to get state.
; swap! to update state.
(defbolt word-count ["word" "count"] 
  {:prepare true}
  [conf context collector]  ; prepared bolt impl takes conf, context, collector
  (let [counts (atom {})]   ; concurrent hash map to store cnt for each word
    (bolt
     (execute [tuple]       ; input tuple has field word
       (let [word (.getStringByField tuple "word")]
         (swap! counts (partial merge-with +) {word 1})  ; merge map zip map set/union  conj collection.
         (emit-bolt! collector [word (@counts word)] :anchor tuple)
         (ack! collector tuple))))))

;;
;; combiner get input from stream 2 of split-sentence. Tuple has two fields, word/count
;;
(defbolt combiner ["word" "count"] 
  {:prepare true}
  [conf context collector]
  (let [counts (atom {})]
    (bolt
      (execute [tuple]  ; tuple in the stream has two fields, word, index
        (let [ word (.getStringByField tuple "word")
               line (.getLongByField tuple "index")]
          (prn "combiner word " word)
          (prn "combiner count " line))))))

;;
;; topology is a map of component id [1 2 3 4] and its spec
;; spout-spec = spec impl and parallelism. output stream map defined inside spout-spec
;; bolt-spec = input-map and spec impl.
;; bolt-spec input-map = { [comp-id stream-id] :stream-grp [comp-id stream-id] ["field1" "field2"]
;; components exchange data in *tuple*. Tuple has fields.
;;
(defn mk-topology []
  ; toplogy is a map of component id and component spec.(spout-spec, bolt-spec)
  (topology
    ; spout-spec : spout impl and parallel tasks.
    {"1" (spout-spec (sentence-spout-parameterized ["the cat jumped over the door" "greetings from the faraway land"])
                      :p 2)

     "2" (spout-spec sentence-spout) }  ; done with spout spec.

    ;; bolt-spec, input declaration, bolt implementation, parallel tasks.
    ;; input declaration: a map of stream id and stream groupings.
    ;; stream id = [==component id== ==stream id==]
    ;; stream grp ["id" "name"]: subscribes with a fields grouping on the specified fields
    {"3" (bolt-spec {"2" :shuffle}  ; input component 2, shuffle grouping
                    (filter-sentence ["Link" "channel"])  ; filter bolt take a list of keyword to filter
                    :p 2)

     "4" (bolt-spec {"3" :shuffle}   ;; input from component 3, shuffle grouping
                     split-sentence
                     :p 2)

     "5" (bolt-spec {["4" "1" ] ["word"]}  ;; comp 4, stream 1, grouping by field word
                    word-count
                    :p 2)

     "6" (bolt-spec {["4" "2"] ["word" "index"]} ; comp 4, stream 2, group by word index
                    combiner
                    :p 2)}))

(defn run-local! []
  (let [cluster (LocalCluster.)]
    (.submitTopology cluster "word-count" {TOPOLOGY-DEBUG false} (mk-topology))
    (Thread/sleep 10000)   ;; run 10 seconds
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

