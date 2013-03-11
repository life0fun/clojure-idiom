(ns chapter19-meta-macros)

(defmacro b [binds & body]
  `(binding ~binds ~@body))

