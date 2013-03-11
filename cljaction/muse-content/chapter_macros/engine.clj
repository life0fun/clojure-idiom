(ns chapter-macros.engine
  (:use chapter-macros.segment
        chapter-macros.session
        chapter-macros.dsl-store))

(defn load-code [code-string]
  (binding [*ns* (:ns (meta load-code))]
    (load-string code-string)))

(defn matches? [^String superset ^String subset]
  (and
   (not (empty? superset))
   (> (.indexOf superset subset) 0)))

(defn segment-satisfied? [[segment-name segment-fn]]
  (if (segment-fn)
    segment-name))

(defn classify []
  (->> (all-segments)
       (map segment-satisfied?)
       (remove nil?)))