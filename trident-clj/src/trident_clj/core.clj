(ns trident-clj.core
	(:require [clojure.string :as str])
  (:import [java.io FileReader]
           [java.util Map Map$Entry List ArrayList Collection Iterator HashMap])
  (:require [clj-redis.client :as redis]) 		; bring in redis namespace
  (:import [backtype.storm Config StormSubmitter LocalCluster LocalDRPC]
           [backtype.storm.spout SchemeAsMultiScheme]
           [backtype.storm.tuple Fields Tuple]
           [storm.trident TridentTopology]
           [storm.trident.tuple TridentTupleView]
           [storm.trident.testing MemoryMapState$Factory]
           [storm.trident.operation.Filter]
           [storm.trident.operation.builtin Count Sum MapGet FilterNull])
  (:require [trident-clj.loc-aggregator]
            [trident-clj.tweet-spout]
            [trident-clj.prn-filter]
            [clojure.tools.logging :as log])
  (:use [backtype.storm clojure config])
  (:gen-class))

; instantiate a fake tweet spout
(defn fake-tweet-spout
  [batch-size]
  (prn " >>> fake tweet spout <<<")
  (com.colorcloud.trident.TweetSpout.))  ; just ret fake batch spout default constructor

; build a storm top by config the passed in trident top
(defn bld-tweet-top
  [trident-top]
  (let [tweet-spout (fake-tweet-spout 100)
        dbstore-bolt (com.colorcloud.trident.LocAggregator.)
        prnfilter (com.colorcloud.trident.PrintFilter.)]
    (-> trident-top
      (.newStream "spout" tweet-spout)
      (.each (Fields. ["location"]) dbstore-bolt (Fields. ["duploc"]))
      (.each (Fields. ["id" "text" "actor" "duploc"]) prnfilter))))

(defn run-local-topology
  [config drpc]
  (let [cluster (LocalCluster.)
        cluster-config (Config.)
        tweet-top (bld-tweet-top (TridentTopology.))]
    ;(.setDebug cluster-config true)
    (.submitTopology cluster      ; submit top to cluster with the following config
      "location_groupaggregate"
      cluster-config
      (.build tweet-top))))

; gen-class - main
(defn -main
  [& args]
  (let [drpc (LocalDRPC.)
        config (Config.)]  ; create a local drpc
    (prn " >>>> starting dbconn.core main <<<<< ")
    (run-local-topology config drpc)
    (while true
      (log/infof "Word count: %s" (.execute drpc "words" "baby"))
      (Thread/sleep 1000))))