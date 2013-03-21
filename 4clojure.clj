;; clojure github
;;
;; the diff between list/vec and use flatten and apply when calling fn with arg list.
;; (conj nil 4) returns (4)
;; (conj [] 4) return [4]

;;
;; compress a sequence
;;
(= (apply str (__ "Leeeeeerrroyyy")) "Leroy")
(= (__ [1 1 2 3 3 2 2 3]) '(1 2 3 2 3)')
(= (__ [[1 2] [1 2] [3 4] [1 2]]) '([1 2] [3 4] [1 2])')

(fn [l]
  (loop [x l ret []]
    (if (nil? (seq x))
      ret
      (let [z (conj ret (first x))]
        (if-not (= (first x) (last ret))
          (recur (rest x) z)
          (recur (rest x) ret))))))


;;
;; pack a sequence.
;;
(= (__ [1 1 2 1 1 1 3 3]) '((1 1) (2) (1 1 1) (3 3))')
(= (__ [:a :a :b :b :c]) '((:a :a) (:b :b) (:c))')
(= (__ [[1 2] [1 2] [3 4]]) '(([1 2] [1 2]) ([3 4]))')

(fn [l]
  (reduce (fn [ret this] 
    (let [l (last ret)]
      (if-not (= this (last l))
        (conj ret [this])
        (-> ret (pop) (conj (conj l this)) )))) [] l))

;;
;; Duplicate a Sequence
;; two ways of list comprehension, map/reduce, or loop with destructuring.
;;
(= (__ [1 2 3]) '(1 1 2 2 3 3)')
(= (__ [[1 2] [3 4]]) '([1 2] [1 2] [3 4] [3 4])')

(fn [l]
  (loop [[hd & body] l ret []]
    (if (nil? hd)
      ret
      (recur body (conj ret hd hd)))))

;;
;; Replicate a Sequence
;;
(= (__ [[1 2] [3 4]] 2) '([1 2] [1 2] [3 4] [3 4])')
(= (__ [44 33] 2) [44 44 33 33])

(fn [l n]
  (loop [[hd & body] l ret []]
    (if (nil? hd)
      ret
      (recur body (reduce (fn [r c] (conj r c)) ret (repeat n hd))))))

;;
;; interpose
;; (= (apply str (__ ", " ["one" "two" "three"])) "one, two, three")
;;
(fn [sep col]
  (drop-last (reduce (fn [ret this]
    (conj ret (first this) (second this)))
      [] (map (fn [e] [e sep]) col) )))

;;
;; drop every nth
;; (= (__ [1 2 3 4 5 6 7 8] 3) [1 2 4 5 7 8])
;;
(fn [col step]
  (loop [i 1 c col ret []]
    (if (nil? (seq c))
      ret
      (do
        (if-not (= 0 (mod i step))
          (recur (inc i) (rest c) (conj ret (first c)))
          (recur (inc i) (rest c) ret) )))))

(fn [col step]
  (keep-indexed
    (fn [idx item]
      (if-not (= 0 (mod (+ idx 1) step)) item)) col))


;;
;; Flipping out, ret a fn that with arg order reversed.
;; (= 3 ((__ nth) 2 [1 2 3 4 5]))
(fn [origfn]
  (fn [ & args ]
    (apply origfn (reverse args))))

;;
;; split a seq by type, reduce to a map and get the value.
;; (= (set (__ [1 :a 2 :b 3 :c])) #{[1 2 3] [:a :b :c]})
;; (= (set (__ [:a "foo"  "bar" :b])) #{[:a :b] ["foo" "bar"]})
;;
(fn [col]
  (vals
    (reduce (fn [ret this]
              (condp = (type this)
                java.lang.String (assoc ret :string ((fnil conj []) (get ret :string) this)) 
                java.lang.Integer (assoc ret :number ((fnil conj []) (get ret :number) this))
                java.lang.Long (assoc ret :number ((fnil conj []) (get ret :number) this))
                clojure.lang.Keyword (assoc ret :keyword ((fnil conj []) (get ret :keyword) this))
                clojure.lang.PersistentList (assoc ret :list ((fnil conj []) (get ret :list) this))
                clojure.lang.PersistentVector (assoc ret :vector ((fnil conj []) (get ret :vector) this))
            )) {} col)))

;;
;; Longest Increasing Sub-Seq, consecutive sub-sequence of increasing numbers
;; thinking functionly, enum all increasing list from each pos, or break list into sublists with each sublist
;; an increasing sublist that matches the requirement. And reduce on the sublist.
;; optimze to O(n)
;; (= (__ [1 0 1 2 3 0 4 5]) [0 1 2 3])
;; (= (__ [5 6 1 3 2 7]) [5 6])
;; (= (__ [2 3 3 4 5]) [3 4 5])
;;
(fn [col]
  (let [reslt
    (loop [[hd & rst] col ret []]
      (if (nil? hd)
        ret
        (do
          (let [t (last (last ret))]
          (if-not (nil? t)
            (if (= 1 (- hd t))
              (recur rst (conj (pop ret) (conj (last ret) hd)))
              (recur rst (conj ret [hd])))
            (recur rst (conj ret [hd])) ) ))))]
    (reduce (fn [ret this] 
              (if (and (> (count this) (count ret)) (> (count this) 1))
                this
                ret)) [] reslt)))


;;
;; partition a seq
;; (= (__ 3 (range 8)) '((0 1 2) (3 4 5))')
;;
(fn [neach col]
  (filter #(= (count %) neach)
    (reduce (fn [ret this]
      (let [t (last ret)]
        (if (nil? t)
          (conj ret [this])
          (if (< (count t) neach)
            (conj (pop ret) (conj t this))
            (conj ret [this]) ))))
    [] col)))

;;
;; find distinct items
;;
(fn [col]
  (loop [[hd & rst] col m {} out []]
    (if (nil? hd)
      out
      (if (contains? m hd)
        (recur rst m out)
        (recur rst (assoc m hd 1) (conj out hd)) ))))

;;
;; comp
;; (= 5 ((__ (partial + 3) second) [1 2 3 4]))
;; (= true ((__ zero? #(mod % 8) +) 3 5 7 9))
;;
(fn
  ([f1 f2 ]
    (fn [& args]
      (f1 (apply f2 args))))
  ([f g & fs]
    (fn [& args]
      (let [fs (reverse (list* f g fs))]
        (loop [ret (apply (first fs) args) fs (next fs)]   ;; binding eval is left -> right, in order.
          (if fs
            (recur ((first fs) ret) (next fs))   ;; use next, not rest, as next is strict than rest(lazy)
            ret))))))

;;
;; juxtaposition
(fn [f & fns]
  (fn [& args]
    (let [fs (list* f fns) ret []]
      (loop [nxt (next fs) ret (conj ret (apply (first fs) args))] 
        (if nxt
          (recur (next nxt) (conj ret (apply (first nxt) args)))
          ret)))))

;;
;; reductions
;; (= (__ conj [1] [2 3 4]) [[1] [1 2] [1 2 3] [1 2 3 4]])
;;
(fn reduction
  ([ f col ]
    (reduction f (first col) (rest col)))
  ([f init col]
    (loop [c col reduceval init interim (conj [] reduceval)]
      (if c
        (let [ resl (f reduceval (first c))]
          (recur (rest c) resl (conj interim resl)))
      interim ))))

;;
;; lazy reductions
;; do not use loop, instead, recursive call
;; init actually is the intermediate result at each step. If you need it, then cons it to return seq.
;; (= (take 5 (__ + (range))) [0 1 3 6 10])
;;
(fn reduction
  ([ f col ]
    (lazy-seq
      (reduction f (first col) (rest col))))
  ([f init col]
    (lazy-seq
      (if-not (seq col)
        [init]
        (let [rslt (f init (first col))]   ;; carry the intermediate result to recursion.
          (cons init (reduction f rslt (rest col))))))))

;;
;; my own iterate (x f(x) f(f(x)))
;; use lazy-seq to wrap the result. Like use lazy-seq to wrap the rabbitmq stream.
;;
(fn myiter [f init]
  (let [rslt (f init)]
    (cons init (lazy-seq (myiter f rslt)))))

;;
;; group-by
;; use update-in and (fnil conj []) to create the ret map and loop carry interim result.
;;
(fn [f col]
  (loop [c col grp {}]
    (if c
      (recur (next c) (update-in grp [(f (first c))] (fnil conj []) (first c)))
      grp)))

;;
;;

