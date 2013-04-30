(ns perm
  ;;(:use [clojure.string :exclude (replace)])
  )

(defn permute [l cursol]
  (let [hd (first l)
        body (rest l)
       ]
    (if (empty? body)
      (do
        (prn 'leaf hd)
        (list hd))
      (do
        (prn 'recur hd cursol )
        (def m (permute body (conj cursol hd)))
        (prn m)
        (for x m (conj x hd))
        ))))

(permute ['a 'b 'c 'd] [])
