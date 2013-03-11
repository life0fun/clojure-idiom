(ns chapter-macros.shifting)

(def ALPHABETS [\a \b \c \d \e \f \g \h \i \j \k \l \m \n \o \p \q \r \s \t \u \v \w \x \y \z])

(def NUM-ALPHABETS (count ALPHABETS))

(def INDICES (range 1 (inc NUM-ALPHABETS)))

(def lookup (zipmap INDICES ALPHABETS))

(defn shift [shift-by index]
  (let [shifted (+ (mod shift-by NUM-ALPHABETS) index)]
    (cond
      (<= shifted 0) (+ shifted NUM-ALPHABETS)
      (> shifted NUM-ALPHABETS) (- shifted NUM-ALPHABETS)
      :default shifted)))

(defn shifted-tableau [shift-by]
  (println "Computing tableau for" shift-by)
  (->> (map #(shift shift-by %) INDICES)
       (map lookup)
       (zipmap ALPHABETS )))

(defn encrypt [shift-by plaintext]
  (let [shifted (shifted-tableau shift-by)]
    (apply str (map shifted plaintext))))

(defn decrypt [shift-by encrypted]
  (encrypt (- shift-by) encrypted))

(def encrypt-with-rot13 (partial encrypt 13))

(def decrypt-with-rot13 (partial decrypt 13))

(defmacro def-rot-encrypter [name shift-by]
  (let [tableau (shifted-tableau shift-by)]
    `(defn ~name [~'message]
       (apply str (map ~tableau ~'message)))))

(defmacro define-rot-encryption [shift-by]
  `(do
     (def-rot-encrypter ~(symbol (str "encrypt" shift-by)) ~shift-by)
     (def-rot-encrypter ~(symbol ( str "decrypt" shift-by)) ~(- shift-by))))