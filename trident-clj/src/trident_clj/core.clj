(ns trident-clj.core
  "trident in clj example"
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
            [trident-clj.persister]
            [clojure.tools.logging :as log])
  (:use [backtype.storm clojure config])
  (:gen-class))

; instantiate a fake tweet spout class. We gen-class from clj module(ns)
(defn fake-tweet-spout
  [batch-size]
  (prn " >>> creating fake tweet spout <<<")
  ; constructor takes 2 args
  ;(com.colorcloud.trident.TweetSpout. "data/500_sentences_en.txt" 100))
  (com.colorcloud.trident.TweetSpout.))

; build a storm top by config the passed in trident top
; connect spout and each filters and aggregators
(defn bld-tweet-top
  [trident-top]
  (let [tweet-spout (fake-tweet-spout 100)  ; spout
        counter (storm.trident.operation.builtin.Count.)
        persister (com.colorcloud.trident.Persister.)  
        locaggregator (com.colorcloud.trident.LocAggregator.) ; loc aggregator
        prnfilter (com.colorcloud.trident.PrintFilter.)]
    (-> trident-top
      (.newStream "spout" tweet-spout)
      (.each (Fields. ["id" "actor" "text" "location" "time"]) persister (Fields. ["rediskey"]))
      ;(.each (Fields. ["id" "actor" "text" "location" "time" "rediskey"]) prnfilter)
      ; groupBy create virtual streams grouped to next, must followed by aggregator
      ; grouped stream, after aggregation, only contains grouping key and other fields emitted from aggregator.
      (.groupBy (Fields. ["location"]))
      ;(.aggregate (Fields. ["location"]) counter (Fields. ["count"])) ; [grp-key other-key]
      (.aggregate (Fields. ["id" "actor" "text" "location" "time" "rediskey"]) locaggregator (Fields. ["id" "actor" "count" "rediskey"]))
      (.each (Fields. ["id" "actor" "location" "count" "rediskey"]) prnfilter))
    trident-top))  ; return configurated trident top

; give a config, build a top and run it on a cluster
(defn run-local-topology
  [config drpc]
  (let [cluster (LocalCluster.)   ; create a cluster
        tweet-top (bld-tweet-top (TridentTopology.))] ; build tweet topology
    ;(.setDebug cluster-config true)
    (.submitTopology cluster      ; submit top to cluster with the following config
      "location_groupaggregate"   ; top instance name
      config
      (.build tweet-top))))

; gen-class - main
(defn -main
  [& args]
  (let [drpc (LocalDRPC.)
        config (Config.)]  ; create a local drpc
    (prn " >>>> starting dbconn.core main <<<<< ")
    (run-local-topology config drpc)
    (while true
      ; drpc execute function-name function-arg
      (log/info "Word count: %s" (.execute drpc "words" "baby"))
      (Thread/sleep 1000))))