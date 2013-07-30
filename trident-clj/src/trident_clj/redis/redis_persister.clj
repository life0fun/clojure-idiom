(ns trident-clj.redis.redis-persister
  (:require [clj-redis.client :as redis])  ; bring in redis namespace
  (:require [clojure.data.json :as json]))

; clojure does not support cyclic dependencies. break cyclic by having a third ns
; contains common defs. If you have cyclic deps, you did not get your abstraction right.

; init redis connection
;(def redis-db (redis/init :url "redis://localhost"))
(defn init-redis-db []
  (def redis-db (redis/init :url "redis://localhost")))

; serialization, two formats, json or plain string
(defmulti serialize (fn [format key-type value]
                      [format key-type]))

(defmethod serialize [:json :string-type] [format key-type value]
  (json/write-str value))

(defmethod serialize [:json :list-type] [format key-type value]
  (map json/write-str value))

(defmethod serialize [:clj-str :string-type] [format key-type value]
  (pr-str value))  ; pr-str output double quote as part of str

(defmethod serialize [:clj-str :list-type] [format key-type value]
  (map pr-str value))


; deserialization
(defmulti deserialize (fn [format key-type serialized]
                        [format key-type]))

(defmethod deserialize [:json :string-type] [format key-type serialized]
  (json/read-str serialized))

(defmethod deserialize [:json :list-type] [format key-type serialized]
  (map json/read-str serialized))

(defmethod deserialize [:clj-str :string-type] [format key-type serialized]
  (read-string serialized))

(defmethod deserialize [:clj-str :list-type] [format key-type serialized]
  (map read-string serialized))

; model itself is a map, it can contain plain string type, or list type.
; for map type, just flat k,v directly to top model level.
(def inserters {
  :string-type redis/set
  :list-type redis/rpush
})

; fetchers for different types. get for key type, lrange for list type.
; key = "1##doug##:id"
(def fetchers {
  :string-type (fn [key]
                  {key {:value (redis/get redis-db key) 
                        :key-type :string-type}})
  :list-type (fn [key]
                {key {:value (redis/lrange redis-db key 0 (redis/llen redis-db key)) 
                      :key-type :list-type}})})

; serialize a map with a seq of k,v pairs into redis. 
; just map insert fn, which redis/rpush k,v into redis.
(defn insert-into-redis [persistable]
  (let [inserter (fn [[k v]]
    (prn "insert-into-redis" k v (v :key-type) "v:value" (v :value))
      (cond
        (= (v :key-type) :string-type) ((inserters :string-type) redis-db k (v :value))
        (= (v :key-type) :list-type) (doall (map #((inserters :list-type) redis-db k %) (v :value)))))]
    (doall (map inserter persistable))))


; convert each row in model to a map that contains a seq of k,v pairs.
; each row has a pk-val, the key for each column is pk-val + colname
; use merge to merge a seq of k,v pairs in single maps to form a big map.
; val is a map with two kv, :key-type str or list, :value=json-str
(defn persistable-for [redis-object]
  (let [redis-type (redis-object :type)
        separator (redis-type :key-separator)
        format (redis-type :format)
        pk-value (redis-object :primary-key-value)
        kv-persister (fn [[k v]]  ; transform k v to {pk+k {:ky}}
          (let [key-type (redis-type :key-type k)]
             {(str pk-value separator k)
              {:value (serialize format key-type v)
               :key-type key-type}}))]
    ; maped fn, kv-persister, ret a map.
    (apply merge (map kv-persister (redis-object :get-state)))))

(defn persist [redis-object]
  (insert-into-redis (persistable-for redis-object))
  true)

; first, 
(defn deserialize-state [serialized redis-type]
  (let [format (redis-type :format)
        separator (redis-type :key-separator)
        key-from (fn [k] 
                   (read-string (last (.split k separator))))
        deserializer (fn [[k {:keys [key-type value]}]]
                       (if-not value
                       {}
                       {(key-from k) (deserialize format key-type value)}))]
    (apply merge (map deserializer serialized))))

; retrive from redis obj based on redis-type name and p key values 
; (def d (consumer :find "adi" "14")))
; each obj(row) has n keys(cols), maps to n keys in redis (pkv1+pkv2+k1, pkv1+pkv2+k2, ...)
; pk-values ("105" "ted") string-keys ("105##ted##:id" "105##ted##:actor" "105##ted##:location" "105##ted##:text" "105##ted##:time") ("105##ted##:followers") 
; { "105##ted##:time" {:value "\"2013-07-29T05:27:44.783Z\"", :key-type :string-type}, 
;   "105##ted##:text" {:value "\"Wine glass heels are to be found in both high and semi-heights. \"", :key-type :string-type}, 
;   "105##ted##:location" {:value "\"USA\"", :key-type :string-type}, "105##ted##:actor" {:value "\"ted\"", :key-type :string-type}, "105##ted##:id" {:value "\"105\"", :key-type :string-type}}
; {"36##ted##:followers" {:value ("\"tedfollower-2\"" "\"ted-follower-1\"" "\"tedfollower-2\"" "\"ted-follower-1\""), :key-type :list-type}} 
; {:id "36", :actor "ted", :location "USA", :text "xx", :followers ("tedfollower-2" "ted-follower-1" "tedfollower-2" "ted-follower-1")}
(defn find-by-primary-key [redis-type pk-values]
  (let [string-keys (redis-type :string-keys pk-values)
        list-keys (redis-type :list-keys pk-values)
        ; fetch from redis all string-keys and list-keys belongs to this row(pk-value)
        string-maps (apply merge (map #((fetchers :string-type) %) string-keys))
        list-maps (apply merge (map #((fetchers :list-type) %) list-keys))
        serialized (merge string-maps list-maps)
        deserialized (deserialize-state serialized redis-type)]
    ;(prn "Redis Mapper : " pk-values string-keys list-keys string-maps list-maps serialized deserialized)
    (if (empty? deserialized)
      nil
      (redis-type :new-with-state deserialized))))
