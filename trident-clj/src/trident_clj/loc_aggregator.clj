(ns trident-clj.loc-aggregator
  (:import [java.io FileReader]
           [java.util Map Map$Entry List ArrayList Collection Iterator HashMap])
  (:import [storm.trident.operation TridentCollector Function]
           [backtype.storm.tuple Values])
  (:require [clojure.string :as str]
            [clojure.tools.logging :as log])
  (:require [clj-redis.client :as redis])     ; bring in redis namespace
  (:gen-class
    :name com.colorcloud.trident.LocAggregator  ; convert this ns to class Tweet
    :implements [storm.trident.operation.Function]))  ; this ns impl Function


(defn -prepare      ; gen-class method prefix by -
  " called once, better for init global var and db conn "
  [this conf context]
  (prn "LocAggregator prepare once")
  (def redis-db (redis/init :url "redis://localhost")))  ; shall use dynamic binding

(defn -execute  ; 
  "process each tuple, persist to redis"
  [this ^storm.trident.tuple.TridentTuple tuple ^TridentCollector collector]
  (let [loc (.getString tuple 0)]
    (prn "TweetAggregator : execute " loc)
    ;(redis/hset redis-db "location" loc)
    (redis/rpush redis-db "tweetloc" loc)
    (.emit collector (Values. (to-array ["test"])))))