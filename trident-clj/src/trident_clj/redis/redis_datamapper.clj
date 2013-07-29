(ns trident-clj.redis.redis-datamapper
  (:require [clj-redis.client :as redis])  ; bring in redis namespace
  (:use clojure.contrib.str-utils)
  (:require [clojure.data.json :as json])
  (:use [trident-clj.redis.redis-persister]))

; form pk-val for a row(object) by 
(defn primary-key-value [redis-obj]
  (let [pk-keys ((redis-obj :type) :primary-key)
        separator ((redis-obj :type) :key-separator)
        values (map #(redis-obj :get %) pk-keys)]
    (str-join separator values)))

; redis obj is a closure over a ref map where state(k,v) pairs are stored.
; the map is serialized into redis when save called to persist.
(defn new-redis-object [redis-type]
  (let [state (ref {})]
    (fn thiz [accessor & args]
      (condp = accessor
        :type redis-type
        :set! (let [[k v] args] 
                (redis-type :valid-key? k)
                (dosync
                 (alter state assoc k v))
                 v)
        :set-all! (let [[kv-map] args]
                    (doseq [kv kv-map]
                      (let [[k v] kv]
                        (thiz :set! k v))))
        :copy-from-redis-object (let [from (first args)
                                      attribs (rest args)]
                                  (doseq [attrib attribs]
                                    (thiz :set! attrib (from :get attrib))))
        :add! (let [[k v] args
                    add-to-inner-list (fn [current-state ke valu] 
                                        (update-in current-state [ke] conj valu))]
                (dosync
                  (alter state add-to-inner-list k v))
                  v)
        :get (let [[k] args]
                (redis-type :valid-key? k)
                (state k))
        :primary-key-value (primary-key-value thiz)
        :save! (persist thiz)
        :get-state @state
        :replace-state (let [[new-state] args] 
                          (dosync
                            (ref-set state new-state)))))))

;
(defn key-type-for [key-name string-types list-types]
  (if (some #(= % key-name) string-types) 
    :string-type
    (if (some #(= % key-name) list-types)
      :list-type)))


; from pk-value, get all keys belong to this row.
; b/c each row has n cols(keys), so we have n keys in redis, named (pkv1+pkv2+k1)...
(defn keys-for [keys separator values]
  (let [pk-value (str-join separator values)]
    (map #(str pk-value separator %) keys)))


(defn check-key-validity [key redis-type string-attribs list-attribs]
  (if-not (some #(= % key) string-attribs)
    (if-not (some #(= % key) list-attribs)
      (throw (RuntimeException. (str "Attempt to use unknown key " key " in redis-object of type " (redis-type :name))))))
  true)


; model type metadata. closure with condp = accessor
(defn new-redis-type [name separator format primary-keys string-attribs list-attribs]
  (fn redis-type [accessor & args]   ; named anonym fn, passed to redis object.
    (condp = accessor       ; switch dispatcher
      :name name            ; closure bind to name.
      :format format
      :key-separator separator
      :primary-key primary-keys  ; a seq of keys
      :key-type (let [[k] args]
                  (key-type-for k string-attribs list-attribs))
      :valid-key? (let [[key] args]
                    (check-key-validity key redis-type string-attribs list-attribs))
      
      ; usage : (redis-type :string-keys pk-values)
      ; pk-value already in a list, and being wraped into args list again when passed in here, de-list.
      :string-keys (let [[values] args]
                     (keys-for string-attribs separator values))
      :list-keys (let [[values] args]
                   (keys-for list-attribs separator values))
      ; new an object of this type.
      :new (new-redis-object redis-type)
      :new-with-state (let [[new-state] args
                            nh (new-redis-object redis-type)]
                        (nh :replace-state new-state)
                        nh)
      ; retrieve row by pkvals
      ; (def d (consumer :find "adi" "14")))
      :find (find-by-primary-key redis-type args)
      :exists? (let [key-value (str-join separator args)
                     key-value (str key-value separator (first primary-keys))]
                 (redis/exists key-value))
      :attrib-exists? (let [attrib-key (first args)
                            pk-value (str-join separator (rest args))]
                        (redis/exists (str pk-value separator attrib-key))))))

; ret a vec of [colume names] from (string-type :id :start-time :timezone)
(defn specs-for [redis-datatype specs]
  (let [type-spec? #(= redis-datatype (first %))
        extractor (comp next first)]
    (extractor (filter type-spec? specs))))

; model scheme. extract a list of col names from specs.
(defmacro def-redis-type [name & specs]
  (let [string-types (specs-for 'string-type specs)
        list-types (specs-for 'list-type specs)
        pk-keys (specs-for 'primary-key specs)
        format (or (first (specs-for 'format specs)) :clj-str)
        separator (or (first (specs-for 'key-separator specs)) "___")]
    `(def ~name 
      (new-redis-type '~name ~separator ~format '~pk-keys '~string-types '~list-types))))

;;
;; a redis type is a object type that maps to a storage model with a list of attributes.
;; in FP, a type is a fn closure with meta, data, etc.
;; Fn returns a function that accepts commands with arguments makes things look like OO object.
;;(def-redis-type consumer
;;  (string-type :id :merchant-id :start-time :timezone)
;;  (list-type :cart-items)
;;  (primary-key :id :merchant-id))
;;  (format :json)
;;  (key-separator "##"))
;;
;;
;;(consumer :name)
;;(consumer :format)
;;
;; to instantiating and using the object, call the fn new closure.
;;(def c (consumer :new))
;;(c :set! :merchant-id "14")
;;(c :add! :cart-items {:sku "XYZ" :cost 10.95})
;;
;; persistent
;;(redis/with-server {:host "127.0.0.1" :port 6379 :db 0}
;;       (c :save!))
;;
;; retrieve
;;(redis/with-server {:host "127.0.0.1" :port 6379 :db 0}
;;  (def d (consumer :find "adi" "14")))
