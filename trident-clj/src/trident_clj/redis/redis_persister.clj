(ns dbconn.redis.redis-persister
  (:require [clj-redis.client :as redis])  ; bring in redis namespace
  (:require [clojure.data.json :as json]))

; clojure does not support cyclic dependencies. break cyclic by having a third ns
; contains common defs. If you have cyclic deps, you did not get your abstraction right.
; init redis connection
(def redis-db (redis/init :url "redis://localhost"))

;; serialization
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


;; deserialization
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

(def inserters {
  :string-type redis/set
  :list-type redis/rpush
})

(def fetchers {
  :string-type (fn [key] 
         {key {:value (redis/get key) 
                       :key-type :string-type}})
  :list-type (fn [key]
           {key {:value (redis/lrange key 0 (redis/llen key)) 
                     :key-type :list-type}})})

(defn insert-into-redis [persistable]
  (let [inserter (fn [[k v]]
           (prn "insert-into-redis" k v (v :key-type) "v:value" (v :value))
           (cond
             (= (v :key-type) :string-type) ((inserters :string-type) redis-db k (v :value))
             (= (v :key-type) :list-type) (doall (map #((inserters :list-type) redis-db k %) (v :value)))))]
    (doall (map inserter persistable))))

(defn persistable-for [redis-object]
  (let [redis-type (redis-object :type)
        separator (redis-type :key-separator)
        format (redis-type :format)
        pk-value (redis-object :primary-key-value)
        kv-persister (fn [[k v]]
          (let [key-type (redis-type :key-type k)]
             {(str pk-value separator k) 
              {:value (serialize format key-type v)
               :key-type key-type}}))]
    (apply merge (map kv-persister (redis-object :get-state)))))

(defn persist [redis-object]
  (insert-into-redis (persistable-for redis-object))
  true)

(defn deserialize-state [serialized redis-type]
  (let [format (redis-type :format)
    separator (redis-type :key-separator)
    key-from (fn [k] (read-string (last (.split k separator))))
    deserializer (fn [[k {:keys [key-type value]}]]
               (if-not value
             {}
             {(key-from k) (deserialize format key-type value)}))]
    (apply merge (map deserializer serialized))))

(defn find-by-primary-key [redis-type pk-values]
  (let [string-keys (redis-type :string-keys pk-values)
    list-keys (redis-type :list-keys pk-values)
    string-maps (apply merge (map #((fetchers :string-type) %) string-keys))
    list-maps (apply merge (map #((fetchers :list-type) %) list-keys))
    serialized (merge string-maps list-maps)
    deserialized (deserialize-state serialized redis-type)]
    (if (empty? deserialized)
      nil
      (redis-type :new-with-state deserialized))))
