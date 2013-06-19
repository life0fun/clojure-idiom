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


;; all permutations
(defn all-permutations [things]
  (if (= 1 (count things))
    (list things)
    (for [head things
          tail (all-permutations (disj (set things) head))]
      (do
        (cons head tail)))))

(all-permutations '(a b c))
;; ((a c b) (a b c) (b a c) (b c a) (c a b) (c b a))
