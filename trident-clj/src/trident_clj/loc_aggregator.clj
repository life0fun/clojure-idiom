(ns trident-clj.loc-aggregator
  (:import [java.io FileReader]
           [java.util Map Map$Entry List ArrayList Collection Iterator HashMap])
  (:import [storm.trident.operation TridentCollector Function]
           [backtype.storm.tuple Values])
  (:require [clojure.string :as str]
            [clojure.tools.logging :as log])
  (:require [clj-redis.client :as redis])     ; bring in redis namespace
  (:require [trident-clj.redis.redis-datamapper :refer :all])
  (:require [trident-clj.redis.redis-persister :refer :all])
  (:gen-class
    ; should extends BaseAggregator
    :name com.colorcloud.trident.LocAggregator  ; convert this ns to class Tweet
    :implements [storm.trident.operation.Function]))  ; this ns impl Function


; create redis model tweet-rant to store aggregated for each tweet
(defn create-tweet-model []
  (def-redis-type tweet-rant
    (string-type :id :actor :location :text :time)
    (list-type :followers)
    (primary-key :id :actor)
    (format :json)
    (key-separator "##")))


; create a data object to persist data into redis
(defn store-tweet [id actor text location ts cnt]
  (let [tweet (tweet-rant :new)]
    (tweet :set! :id (str id))
    (tweet :set! :actor actor)
    (tweet :set! :location location)
    (tweet :set! :text text)
    (tweet :set! :time ts)
    (tweet :add! :followers (str actor "-follower-" 1))
    (tweet :add! :followers (str actor "follower-" 2))
    (tweet :save!)))

; defed redis type to find by primary key.
(defn find-tweet [id actor]
  (tweet-rant :find id actor))


(defn verify-tweet [id actor text loc ts]
  (let [db-tweet (find-tweet id actor)]
    ;(prn text " -- " (db-tweet :get :followers) (db-tweet :get-state))))
    (prn text " -- " (db-tweet :get-state))))

; prepare called once on start. init global state here.
(defn -prepare      ; gen-class method prefix by -
  " perpare : init global var and db conn "
  [this conf context]
  (prn "LocAggregator prepare once")
  ; init redis connection to db within redis data mapper
  (init-redis-db)
  (create-tweet-model))
  

; Function protocol method, invoked per each tuple emitted from upstream
(defn -execute
  "process each tuple, aggregate group by location, persist to redis"
  [this ^storm.trident.tuple.TridentTuple tuple ^TridentCollector collector]
  (let [id (.getString tuple 0)
        actor (.getString tuple 1)
        text (.getString tuple 2)
        loc (.getString tuple 3)
        ts (.getString tuple 4)]
    ;(prn "TweetAggregator : execute " id actor text loc tm)
    (store-tweet id actor text loc ts 1)
    (verify-tweet id actor text loc ts)
    (.emit collector (Values. (to-array [1 id])))))
