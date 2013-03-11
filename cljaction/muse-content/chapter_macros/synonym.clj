;(ns chapter-macros.synonym)

(defmacro make-synonym-working [new-name old-name]
  `(defmacro ~new-name [& ~'args]
     `(~'~old-name ~@~'args)))

(defmacro b [& stuff]
  `(binding ~@stuff))

(defmacro make-synonym [new-name old-name]
  `(defmacro ~new-name [& ~'stuff]
     `(~'~old-name ~@~'stuff)))