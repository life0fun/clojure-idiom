(ns chapter-macros.segment
  (:use chapter-macros.dsl-store
        clojure.walk))

(defn drop-first-char [name]
  (apply str (rest name)))

(defn session-lookup [dollar-name]
  (->> (drop-first-char dollar-name)
       (keyword)
       (list '*session*)))

(defn transform-lookups [dollar-attribute]
  (let [prefixed-string (str dollar-attribute)]
    (if-not (.startsWith prefixed-string "$")
      dollar-attribute
      (session-lookup prefixed-string))))

(defmacro defsegment [segment-name & body]
  (let [transformed (postwalk transform-lookups body)]
    `(let [segment-fn#  (fn [] ~@transformed)]
       (register-segment ~(keyword segment-name) segment-fn#)
       (def ~segment-name segment-fn#))))