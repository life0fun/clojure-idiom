(ns trident-clj.tweet-spout
  (:import [java.io FileReader]
           [java.util Random Map Map$Entry List ArrayList Collection Iterator HashMap])
  (:import [backtype.storm Config StormSubmitter LocalCluster LocalDRPC]
           [backtype.storm.spout SchemeAsMultiScheme]
           [storm.trident.operation TridentCollector Function]
           [backtype.storm.tuple Fields Tuple]
           [storm.trident.spout.IBatchSpout]
           [backtype.storm.tuple Values])
  (:require [clojure.string :as str]
            [clojure.tools.logging :as log]
            [clojure.contrib.io :refer [pwd]]
            [clj-time.core :as clj-time :exclude [extend]]  ; clj-time abbrev to core
            [clj-time.format])
  (:require [clj-redis.client :as redis])     ; bring in redis namespace
  (:gen-class
    :name com.colorcloud.trident.TweetSpout  ; convert this ns to class Tweet
    :state state ; put serialiazable object here.
    :init init   ; Must return [ [superclass-constructor-args] state] 
    :constructors {[] []   ; empty arg constructor
                   [String int] []}  ; a map of constructor sig to superclass construcotr signature
    :implements [storm.trident.spout.IBatchSpout]))  ; this ns impl Function

; init always ret a vector with the first be superclass constructor arg, and
; the second is instance state.
; Must return [ [superclass-constructor-args] state] 
(defn -init
  "init state, no this pointer, ret a global concurrent map stores all states"
  ([]  ; empty arg constructor, use default file at current pwd is project root.
    [[] {:batchsize 10 :srcfile "data/500_sentences_en.txt"}]) 
  ([srcfile batchsize]
    ; cant use atom, as it is not serializable
    ;[[] (atom {:batchsize 10 :sentences [] :random-gen (Random.) })])
    [[] {:batchsize batchsize :srcfile srcfile}]))

; open fn only called once, so instantiate global state here.
(defn -open      ; gen-class method prefix by -
  "called once when instantiate spout instance, init state here"
  [this conf context]
  ; populate sentence vector inside atom map
  ; (let [state (.state this)
  ;       sentence-vec (:sentence @state)]
  ;   (swap! state assoc :sentence (conj sentence-vec "this is first tweet"))
  ;   (swap! state assoc :sentence (conj sentence-vec "those are second tweet"))))
  (let [state (.state this)
        srcfile (:srcfile state)]
    ; def namespace global mutable shared state.
    (prn "spout open called : " srcfile (pwd))
    (def TWEETCNT (atom 0))
    (def ACTORS ["stefan" "dave" "pere" "nathan" "doug" "ted" "mary" "rose"])
    (def LOCATIONS ["Spain" "USA" "Spain" "USA" "USA" "USA" "UK" "France"])
    (def SUBJECTS ["berlin" "justinbieber" "hadoop" "life" "bigdata"])
    ; connect to rabbitmq or logstash src, abstract queue as a lazy sequence.
    (def srcseq (atom (line-seq (clojure.java.io/reader srcfile))))))

(defn -ack
  [this batch-id]
  (prn " ack " batch-id))

(defn -close
  [this])

(defn -getComponentConfiguration
  [this]
  (Config.))  ; just instantiate a new config object

; output stream field spec.
(defn -getOutputFields
  [this]
  (Fields. ["id" "actor" "text" "location" "time"]))

; feed the top with next tweet
(defn getNextTweet
  "get next tweet from preload sentence vector"
  [this]
  (let [state (.state this)
        text (first @srcseq)
        idx (rand-int (count ACTORS))
        actor (ACTORS idx)
        location (LOCATIONS idx)
        ts (clj-time.format/unparse (clj-time.format/formatters :date-time) (clj-time/now))]
        ;text (["this is first tweet" "that are second tweet"] (rand-int 2))]
    ; for now, just simple fake random between 0 1
    (swap! TWEETCNT inc)
    (swap! srcseq (fn [s] (next s)))  ; srcseq = (next srcseq)
    ;(prn "emit " text @TWEETCNT actor location ts)
    (Values. (to-array [(str @TWEETCNT) actor text location ts]))))

(defn -emitBatch 
  "emit a batch of tuples"
  [this batchId ^storm.trident.operation.TridentCollector collector]
  (let [state (.state this)
        sz (:batchsize state)]
    (doseq [i (range sz)]
      (.emit collector (getNextTweet this)))))