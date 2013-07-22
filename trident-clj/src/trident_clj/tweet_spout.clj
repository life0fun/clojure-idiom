(ns trident-clj.tweet-spout
  (:import [java.io FileReader]
           [java.util Random Map Map$Entry List ArrayList Collection Iterator HashMap])
  (:import [backtype.storm Config StormSubmitter LocalCluster LocalDRPC]
           [backtype.storm.spout SchemeAsMultiScheme]
           [storm.trident.operation TridentCollector Function]
           [backtype.storm.tuple Fields Tuple]
           [storm.trident.spout.IBatchSpout]
           [backtype.storm.tuple Values])
  ;(:use [backtype.storm clojure config])
  (:require [clojure.string :as str]
            [clojure.tools.logging :as log])
  (:require [clj-redis.client :as redis])     ; bring in redis namespace
  (:gen-class
    :name com.colorcloud.trident.TweetSpout  ; convert this ns to class Tweet
    :state state
    :init init   ; Must return [ [superclass-constructor-args] state] 
    :implements [storm.trident.spout.IBatchSpout]))  ; this ns impl Function

; init always ret a vector with the first be superclass constructor arg, and
; the second is instance state.
; Must return [ [superclass-constructor-args] state] 
(defn -init
  "init state, a global concurrent map stores all states"
  []
  [[] (atom {:batch-size 10 
             :sentences []
             :random-gen (Random.) })])

(defn -open      ; gen-class method prefix by -
  "called once when instantiate spout instance, init state here"
  [this conf context]
  ; populate sentence vector inside atom map
  (let [state (.state this)
        sentence-vec (:sentence @state)]
    (swap! state assoc :sentence (conj sentence-vec "this is first tweet"))
    (swap! state assoc :sentence (conj sentence-vec "those are second tweet"))))

(defn -ack
  [batch-id]
  (prn "<ack>"))

(defn -close
  [])

(defn -getComponentConfiguration
  []
  (Config.))  ; just instantiate a new config object

(defn -getOutputFields
  []
  (Fields. ["id" "text" "actor" "location" "date"]))


(defn getNextTweet
  "get next tweet from preload sentence vector"
  [this]
  (let [state (.state this)
        sentence-vec (:sentence @state)
        rnd (:random-gen @state)
        odd (.nextBoolean rnd)]
    ; for now, just simple fake random between 0 1
    (if odd
      (sentence-vec 0)
      (sentence-vec 1))))

(defn -emitBatch 
  "emit a batch of tuples"
  [this batchId ^storm.trident.operation.TridentCollector collector]
  (let [state (.state this)
        sz (:batch-size @state)]
    (prn "emit batch size : " sz)
    (for [i (range sz)]
      (.emit collector (getNextTweet this)))))