(ns chapter-macros.session
  (:require redis))

(def redis-key-for :consumer-id)

(declare *session*)

(defn save-session [session]
  (redis/set (redis-key-for session) (pr-str session)))

(defn find-session [consumer-id]
  (read-string (redis/get consumer-id)))

(defmacro in-session [consumer-id & body]
  `(binding [*session* (find-session ~consumer-id)]
     (do ~@body)))