;(ns chapter-macros.abbreviation)

(defmacro abbrev [short long]
  `(defmacro ~short [& ~'args]
     `(~'~long ~@~'args)))

