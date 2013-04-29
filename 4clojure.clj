;; 
;; 4clojure problem solving  http://www.4clojure.com/
;;
;; Username: life0fun
;; Rank: 171 out of 13992
;; Problems Solved: 134
;; Rank: 162 out of 14099
;; Problems Solved: 135
;; Rank: 175 out of 13915
;; Problems Solved: 133
;;
;; find indices of a val in a vector
;; for string array, use string array's .indexOf method.
;;
 (use '[clojure.contrib.seq-utils :only (positions)])
 (positions #{99} [0 99 3334 53 2 5 99 2 55 63])
 (def v ["one" "two" "three"])
 (.indexOf v "two")

;; use java.lang.String to process strings.
(defn parse-line [line]
  (let [tokens (.split (.toLowerCase line) " ")]
      (map #(vector % 1) tokens)))
(parse-line "Twas brillig and the slithy toves")

(use 'clojure.contrib.io')
(read-line "/Users/e51141/tmp/x")

;;
;; compress a sequence
;;
(= (apply str (__ "Leeeeeerrroyyy")) "Leroy")
(= (__ [1 1 2 3 3 2 2 3]) '(1 2 3 2 3))
(= (__ [[1 2] [1 2] [3 4] [1 2]]) '([1 2] [3 4] [1 2]))

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
;; re-impl map with recursion
;; pattern: build list from leaf where empty [] reted for parents to conj.
;; lazy-seq cons head results to the result of self recursion on the rest .
;;
(fn mymap [f xs] 
  (if (empty? xs)
    []
   (lazy-seq (cons (f (first xs)) (mymap f (rest xs))))))

(= [3 4 5 6 7]
   (__ inc [2 3 4 5 6]))

;;
;; Infix Calulator
;; first pass transform by consolidate * /, then left to right cal.
;;
(fn [ & infix]
  (letfn [ (rm-timediv [infix]
                     (loop [ infix infix operator [] operand []]
                       (let [hd (first infix) hdtype (type hd)]
                         (if (nil? hd)
                           [operator operand]
                           (if (= hdtype java.lang.Integer)
                             (recur (next infix) operator (conj operand hd))
                             (if (or (= hdtype clojure.core$_STAR_)
                                     (= hdtype clojure.core$_SLASH_))
                               (recur (next (next infix)) operator (conj (vec (butlast operand)) (hd (last operand) (first (next infix)))))
                               (recur (next infix) (conj (vec operator) hd) operand)))))))]
    (let [[operator operand] (rm-timediv infix)]
      (prn operator operand)
      (loop [op operator opd (rest operand) tot (first operand) ]
        (if (empty? op)
          tot
          (recur (rest op) (rest opd) ((first op) tot (first opd))))))))
                                        

;; 
;; valid whether a tree is btree
(defn tree [tree]
  (letfn [(btree [root]
                 (let [ t (type root) ]
                   (if (or (= t clojure.lang.Keyword)
                           (= t java.lang.Integer)
                           (= t java.lang.Long)
                           (= t java.lang.Boolean)
                           (= t nil))
                     true
                     (if (and (= 3 (count root))
                              (btree (first root))
                              (btree (second root))
                              (btree (nth root 2)))
                       true
                       false))))
                 
          
;;
;; symmetry
;; if the left half of the tree is the mirror image of the right half of the tree
;;
(fn symmetry [xs]
  (letfn [(seqns? [xs]
                (if (or (= (type xs) clojure.lang.PersistentVector)
                        (= (type xs) clojure.lang.PersistentList))
                  true
                  false))
          (mirror [xs]
                  ;; ret a seq of mirror-ed btree
                  (if-not (seqns? xs)
                    xs
                    (vector (first xs) (mirror (last xs)) (mirror (second xs)))))]
    (let [root (first xs) lc (second xs) rc (last xs)]
        (if (= (mirror (second xs))
               (last xs))
          true
          false))))
 
(= (__ [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
          [2 [3 nil [4 [6 nil nil] [5 nil nil]]] nil]])
   true)
  
  
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
;;
(fn [f & fns]
  (fn [& args]
    (let [fs (list* f fns) ret []]
      (loop [nxt (next fs) ret (conj ret (apply (first fs) args))] 
        (if nxt
          (recur (next nxt) (conj ret (apply (first nxt) args)))
          ret)))))

;;
;; reductions
;; carry the interim result inside recur bindings.
;; when loop condition not met, can ret the interim from recur binding directly.
;; when using loop, not a lazy seq.
;; (= (__ conj [1] [2 3 4]) [[1] [1 2] [1 2 3] [1 2 3 4]])
;;
(fn reduction
  ([ f col ]
    (reduction f (first col) (rest col)))
  ([f init col]
    (loop [c col reduceval init interim (conj [] reduceval)]  ;; carry partial result in recur bindings.
      (if c
        (let [ resl (f reduceval (first c))]
          (recur (rest c) resl (conj interim resl)))   ;; carry interim inside bindings.
      interim ))))

;;
;; lazy reductions
;; lazy seq can not use loop, use recursive call, carry partial result as fn arguments.
;; init actually is the intermediate result at each step. If you need it, then cons it to return seq.
;; (= (take 5 (__ + (range))) [0 1 3 6 10])
;;
(fn reduction
  ([ f col ]
    (lazy-seq
      (reduction f (first col) (rest col))))
  ([f init col]
    (lazy-seq           ;; lazy-seq to wrap result seq, can put inside cons expr also.
      (if-not (seq col)
        [init]
        (let [rslt (f init (first col))]   ;; carry partial result as recursion arguments.
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
  (loop [c col grp {}]   ;; carry partial result inside recur binding.
    (if c
      (recur (next c) (update-in grp [(f (first c))] (fnil conj []) (first c)))
      grp)))

;;
;; Black Box testing of sequence.
;; (= :map (__ {:}))
;; (= [:map :set :vector :list] (map __ [{} #{} [] ()]))
;;
(fn mytest-type [col]
  (if (or (= 2 (count (flatten (vector (last col)))))  ;; use flatten to convert list.
          (and (empty? col)
               (= (into col {:test 1}) {:test 1})))    ;; insert empty map eqs itself.
    :map
    (if (= (count (conj col :test :test)) (+ 1 (count col)))
      :set
      (if (= (first (conj col :test1 :test2)) :test2)
        :list
        :vector))))

;;
;; sieve of prime number
;; all are lazy seq, the magic is that seq needs to starts from 2, not 1.
;;
(fn sieve
  ([n]
    (sieve n (iterate inc 2)))  ;; iter to gen a lazy list starting from 2, [2 3 4 ...]
  ([n l]
    (let [hd (first l) bd (rest l)]
      (if (zero? n)
        []          ;; ret empty [] from bottom for parent to cons result recursion bottom up.
        (take n
          (lazy-seq   ;; lazy-seq is cons head on the recursive self call result
            (cons hd (sieve (- n 1) (filter #(not (zero? (mod % hd))) bd))) ))))))  ;; filter out all head's multipliers

;;
;; prime sandwich, whether a prime which is also the mean of prev and next prime.
;; memoize recursion fn.
;;
(fn prime-sandwich [p]
  (let [siftlist-cache {}] ;; cache intermediate dynamic table
    (letfn [(sieve [n l]   ;; ret a sifted out list without head's multiplies to get prime
              (let [hd (first l) bd (rest l)]
                (if (zero? n)    ;; bottom situation, zero n
                  []             ;; ret empty [] to built-up from bottom to upper parent root. 
                  (take n (lazy-seq 
                    (cons hd (sieve (dec n) (filter #(not (zero? (rem % hd))) bd))))))))
          (prime-idx [primes p]
            (if-not (some #{p} primes)
              -1  ;; ret -1
              ;; find out the index of prime in seq so to get prev and next
              (loop [start 0 end (dec p)]
                (let [mid (int (/ (+ start end) 2))]
                  (if (= p (nth primes mid))
                    mid
                    (if (> p (nth primes mid))
                      (recur (inc mid) end)
                      (recur start (dec mid))))))))

          (prime? [n]
            ;; celebrates clojure's java interop, using BigInteger isProbablePrime with 5% certainty
            (.isProbablePrime (BigInteger/valueOf n) 5)) 

          (siftlist [n]
            (if (= 2 n)   ;; bottom, build-up with inf list
              (let [ret (filter #(not (zero? (rem % n))) (iterate inc 2))]
                (assoc siftlist-cache n ret)
                ret)
              (if (contains? siftlist-cache n)
                (siftlist-cache n)
                (let [xs (siftlist (dec n))
                      hd (first xs)
                      bd (rest xs)
                      ret (filter #(not (zero? (rem % hd))) bd)]
                  (assoc siftlist-cache n ret)
                  ret))))

          (siftlist-yb []
            (let [dp (fn [mem-dp n]    ;; def dp fn to take memoized boxed dp as first arg
                        ;; inside recursion body, impl logic without worrying dp tab, unbox memoized fn
                        (let [dp (fn [n] (mem-dp mem-dp n))]  ;; mem-dp takes 2 args, recursion fn, and args
                          (if (= 2 n)
                            (filter #(not (zero? (rem % n))) (iterate inc 2))
                            (let [xs (dp (dec n))
                                  hd (first xs)
                                  bd (rest xs)
                                  ret (filter #(not (zero? (rem % hd))) bd)]
                              ret))))
                  mem-dp (memoize dp)]      ;; mem-dp memoized
                  (partial mem-dp mem-dp))) ;; pass mem-dp as the second arg to memoized dp

          (primelist [n]
            (loop [ idx 2 rslt [2]]
              ;;(let [ sl (siftlist idx) hd (first sl)]
              (let [ sl ((siftlist-yb) idx) hd (first sl)]  
                (if (= hd n)
                  (conj rslt hd)
                  (if (> hd n)
                    rslt
                    (recur (inc idx) (conj rslt hd)))))))]
    
    (if (and (> p 2)
             (even? p))
      false
      ; (let [primes (sieve p (iterate inc 2))
      ;      idx (prime-idx primes p)]
      ;      (if (< idx 1)
      ;        false
      ;        (let [pre (nth primes (dec idx))
      ;              nxt (nth primes (inc idx))]
      ;          (if (= p (/ (+ pre nxt) 2))
      ;            true
      ;            false)))))))

      ; (let [pl (primelist p) idx (count pl)]
      ;   (if (or (not= (last pl) p)
      ;           (<= idx 2))
      ;     false
      ;     (let [pre (last (butlast pl)) nxt (first ((siftlist-yb) (inc (count pl))))]
      ;       (prn pre p nxt)
      ;       (if (= p (/ (+ pre nxt) 2))
      ;         true
      ;         false))))))))
      (if (or (not (prime? p))
              (< p 5))
        false
        (letfn [(prep [n]
                  (loop [v (dec n)]
                    (if (prime? v)
                      v
                      (recur (dec v)))))
                (nxtp [n]
                  (loop [v (inc n)]
                    (if (prime? v)
                      v
                      (recur (inc v)))))]
          (if (= p (/ (+ (prep p) (nxtp p)) 2))
            true
            false)))) )))

;;
;; merge-with
;;
(fn [f & maps]
  (loop [[m & cdr] maps ret {}]
    (if (nil? (seq m))
      ret
      (recur cdr (reduce (fn [ret cur]
                          (if (contains? ret (first cur))
                            (update-in ret [(first cur)] f (second cur))
                            (assoc ret (first cur) (second cur)))) ret m) ))))


 ;;
 ;; tic tac
 ;; create lists using nth nth list logic and interleave.
 ;; if-let as if else for intermediate value
 ;;
 (fn [col]
   (letfn [(check [col]
              (reduce (fn [ret c]
                        (let [[x y z] c]
                          (if (and (= x y z)
                                   (or (= x :x )
                                       (= x :o )))
                              x ret)))
                          nil col))
           (intlv [col]
                  (partition 3 (apply interleave col)))
           (diag [col]
                 (for [x [0 1 2]] (nth (nth col x) x)))
           (rdiag [col]
                  (for [x [0 1 2]] (nth (nth col x) (- 2 x))))
           ]
     (if-let [ret (check col)]
       ret
       (if-let [ret (check (intlv col))]
         ret
         (if-let [ret (check (vector (diag col)))]
           ret
           (if-let [ret (check (vector (rdiag col)))]
             ret
             nil))))))


;;
;; totient
;;
(fn [n]
  (letfn [(gcd [larger smaller]
            (loop [l larger s smaller]
              (if (not= 0 s)
                (recur s (mod l s))
                l)))]
    (count (filter (fn [i] (= 1 (gcd i n))) (range 1 (inc n))))))

;;
;; fib, iterate gen lazy seq by keeping apply fn to the intermediate results.
;;
(defn fibo [] (map first (iterate (fn [[a b]] [b (+ a b)]) [0N 1N])))

;;
;; intervals is a vec of two int, start end that all int between are contained.
;;
(fn intervals [col]
  (let [scol (sort col)]
    (reduce (fn [ret n]
              (if (= (last (last ret)) (dec n))
                (conj (vec (butlast ret)) (vector (first (last ret)) n))
                (if (or (nil? (last (last ret)))
                        (> n (last (last ret))))
                  (conj ret (vector n n))
                   ret)))
            [] scol)))

(= (__ [10 9 8 1 2 3]) [[1 3] [8 10]])
(= (__ [19 4 17 1 3 10 2 13 13 2 16 4 2 15 13 9 6 14 2 11])
       [[1 4] [6 6] [9 11] [13 17] [19 19]])

;;
;; trampoline [fn]
;; you return a function that does the work instead of doing it directly and then 
;; call a trampoline function that repeatedly calls its result until it turnes into a real value instead of a function
;; use loop [ret (f)]  and invoke the function during recur on loop.
;; use let [ret (f)] and pass the value to recur on the fn recursive call.
;;
(fn mytrampoline
  ([f]
    (loop [ret (f)]      ;; or (let [ret (f)]      ;; use let, recur on fn call
      (if (fn? ret)      ;;      (if (fn? ret)
        (recur (ret))    ;;        (recur  ret)
        ret)))           ;;        ret))
  ([f & args]
    (mytrampoline #(apply f args))))


;;
;; powerset.
;; to expand a seq, do NOT map, use reduce, as input is a seq, output is a single seq.
;; pass partial result as arg to fn, recur on fn to top-down.
;;
(fn powerset 
  ([coll]
    (powerset coll #{}))
  ([coll ret]
    (if (empty? coll)
      (conj ret #{})
        (recur (rest coll) (reduce (fn [ret this]
                                    (conj ret
                                        (conj this (first coll))))
                                   (conj ret (hash-set (first coll))) ret)))))

;;
;; k-comb, powerset filter at len k.
;; trans fn taking partial result as args, and top-down built final result based on partial result step by step.
;; (= (__ 2 #{[1 2 3] :a "abc" "efg"}) #{#{[1 2 3] :a} #{[1 2 3] "abc"} #{[1 2 3] "efg"}
;;                                    #{:a "abc"} #{:a "efg"} #{"abc" "efg"}})
;;
(fn kcomb
  ([k col]
     (kcomb k col #{}))
  ([k col pret]
    (if (empty? col)
      (into #{} (filter (fn [e] (= (count e) k)) pret))
      (recur k (rest col)
             (reduce (fn [ret this]
               (conj ret
                 (conj this (first col))))
                   (conj pret (hash-set (first col))) pret)))))


;; partial flatten sequence
;; always look at head, cons partial result to the ret value of recursive rest body to form tot solution.
;; no need to carry partial result during recursion
;;
;; (= (__ [[[[:a :b]]] [[:c :d]] [:e :f]])
;;   [[:a :b] [:c :d] [:e :f]])
;; (= (__ '((1 2)((3 4)((((5 6))))))))
;; '((1 2)(3 4)(5 6))'

(fn myfltn
  ([col]
    (myfltn col []))
  ([col init]   ;; no need to carry partial result during recursion.
    (if (and (coll? col)
             (not (empty? col)))
      (if (coll? (first col))
        (concat (myfltn (first col)) (myfltn (rest col)))
        (conj [] col) ))))    ;; when first  of col is not collection, one level nested. can ret.

;; decurry, accepts a curried fn of unknown arity n, ret an equivalent fn of n arguments
;;
(fn decurry [f]
  (fn [ & xs] 
    (loop [ argv xs partFn f]
      (if (= 1 (count argv))
        (partFn (first argv))
        (recur (next argv) (partFn (first argv)))))))
 
(= 10 ((__ (fn [a]
             (fn [b]
               (fn [c]
                 (fn [d]
                   (+ a b c d))))))
       1 2 3 4))

;;
;; pascal triangle.
;; list transform, take the relationship between neighbor elements.
;; traditional map etc only take individual items.
;; create a new list by shifting the current list, then apply op on a list of vectors.
;;
(fn pascal
  ([n]
    (if (= n 1)
      [1]
      (if (= n 2)
        [1 1]
        (let [xs (pascal (dec n)) ys (rest xs)]
          (cons 1 (conj (vec (map + (drop-last xs) ys)) 1)))))))


;;
;; lazy search the smallest item that appears in all sorted sequence.
;;
(fn smallest 
  [& colv]
  (let [veccols (vec colv)
        hd (first (apply map vector colv))
        minhd (apply min-key second (map-indexed vector hd))
        smallestidx (first minhd)
        smallestv (second minhd)
       ]
    (if (= (count hd) (count (filter #(= % smallestv) hd)))
      smallestv
      (recur (concat (take smallestidx veccols)
             (drop (inc smallestidx) veccols)
             (vector (rest (veccols smallestidx))) )))))

(= 64 (__ (map #(* % % %) (range)) ;; perfect cubes
          (filter #(zero? (bit-and % (dec %))) (range)) ;; powers of 2
          (iterate inc 20))) ;; at least as large as 20


;; take a nest collection, and a sub collection of it that sum to certain number. maintain nested structure.
;; For/loop comprehents flatten list. Nested collection, need explict loop or reduce and carry partial result along.
;; for list comprehen
;;
(fn SequsHorribilis
  ([tot xs]
    (sequs tot xs []))

    ([tot xs partResult]     ;; xs must be a seq when calling.
      (loop [ remain tot
              xs xs
              partResult partResult]
        (if (empty? xs)      ;; break out loop when empty list
          partResult

          (let [ hd (first xs)
                 body (rest xs)
                 t (type hd) ]
            (if (or (= t clojure.lang.PersistentVector)
                    (= t clojure.lang.PersistentList))
              ;;
              ;; if header is a collection, call this fn recursively to get result for header,
              ;; and continue loop the rest of the list with the result from head conjed to partial result.
              ;;
              (let [headrslt (sequs remain hd [])  ;; call myself to get result for head collection.
                    headtot (apply + (flatten headrslt))]
                (recur (- remain headtot) body (conj partResult headrslt)))  ;; loop the rest with head's result conjed to partial result.
              (if (>= remain hd)
                (recur (- remain hd) body (conj partResult hd))
                partResult)))))))

(=  (__ 10 [1 2 [3 [4 5] 6] 7]) '(1 2 (3 (4))))
(=  (__ 30 [1 2 [3 [4 [5 [6 [7 8]] 9]] 10] 11]) '(1 2 (3 (4 (5 (6 (7)))))))
(=  (__ 9 (range)) '(0 1 2 3)
(=  (__ 1 [[[[[1]]]]]) '(((((1))))))
(=  (__ 0 [1 2 [3 [4 5] 6] 7]) '())
(=  (__ 0 [0 0 [0 [0]]]) '(0 0 (0 (0))))
(=  (__ 1 [-10 [1 [2 3 [4 5 [6 7 [8]]]]]]) '(-10 (1 (2 3 (4)))))

;; 
;; Read Roman numerals with subtractive principle. 
;; just cover the following condition: IV 4 IX 9 XL 40 XC 90 CD 400 CM 900
;; look ahead for each item, if matches one of the above, consume both.
(fn roman-num [numstr]
  (let [vmap { "I" 1 "V" 5 "X" 10 "L" 50 "C" 100 "D" 500 "M" 1000 "IV" 4 "IX" 9 "XL" 40 "XC" 90 "CD" 400 "CM" 900 }]
  (loop [numstr numstr tot 0]
    (if (clojure.string/blank? numstr)
      tot
      (if (<= (count numstr) 1)
        (+ tot (vmap (subs numstr 0 1)))
        (let [hd (subs numstr 0 1) hdpair (subs numstr 0 2)]
          (if (nil? (vmap hdpair))
            (recur (subs numstr 1) (+ tot (vmap hd)))
            (recur (subs numstr 2) (+ tot (vmap hdpair))))))))))
  
(= 3999 (__ "MMMCMXCIX"))
(= 827 (__ "DCCCXXVII"))

;;
;; lazy seq of pronunciations
;; lazy-seq : replace recursive with laziness.
;; wrap the recursive part of a function body with lazy-seq 
;; to replace recursion with laziness.
;; recursive part of fn body : (cons this_result (recursive-call (next iteration)))
;;
(fn lazy-pron
  ([xs]
    (lazy-pron xs nil []))
  ([xs prev result]
    (letfn [(stepHd [xs prev result]   ;; carry prev val to this iteration of head processing.
              (if (empty? xs)
                result
                (if (= (first xs) prev)
                  (recur (rest xs) prev (conj (vec (drop-last 2 result)) (inc (first (take-last 2 result))) prev))
                  (recur (rest xs) (first xs) (conj result 1 (first xs)) ))))]
      (let [curpron (stepHd xs prev result)]
        (lazy-seq (cons curpron (lazy-pron curpron)))))))   ;; wrap recursion body to lazy-seq

;; solution 2, recur loop, not recur stepHd fn itself.
(fn lazy-pron [xs]
  (letfn [(stepHd [xs]
            (loop [xs xs
                   prev nil   ;; carry prev val to this iteration head processing.
                   result []]
              (if (empty? xs)
                result
                (if (= (first xs) prev)
                  (recur (rest xs) prev (conj (vec (drop-last 2 result)) (inc (first (take-last 2 result))) prev))
                  (recur (rest xs) (first xs) (conj result 1 (first xs)))))))]
    (let [curpron (stepHd xs)]
      (lazy-seq (cons curpron (lazy-pron curpron))) )))


;;
;; Insert between two items, returns a new collection where the value is inserted between every two items
;; first, tranform to ary of each pair dup, map add predicate each pair, remove the prev tail to cur head dup.
;; The principle is used in count consecutive headers, or gen fib sequence.
(defn fibo [] (map first (iterate (fn [[a b]] [b (+ a b)]) [0N 1N])))

(fn [pred v coll]
  (letfn [(take-pair [c] (if (next c) (take 2 c) c))
          (by-pair [c]
                   (let [pair (seq (take-pair c))]
                     (when pair
                       (lazy-seq (cons pair (by-pair (rest c)))))))]
    (let [ matched-pairs 
            (map (fn [e] 
                   (if (pred (first e) (last e))
                     (vector (first e) v (last e))
                     e)) (by-pair coll)) ]
      (reduce (fn [ret this] (apply conj ret (rest this))) (vec (first matched-pairs)) (rest matched-pairs)))))
        
    
(= [0 1 :x 2 :x 3 :x 4]  (__ #(and (pos? %) (< % %2)) :x (range 5)))

;;
;; reduce not work for lazy seq, we need to lazy-seq con result from this iteration on top of the result of rest.
;; many ways for recursion, recur fn, recur loop, or lazy-seq con stepHd.
;;  - recur loop, (loop [lst l curk nil partRslt {} ] (recur ...))
;;  - fn self recursion with recur, (stepHd [xs prev partialResult] ... (recur ...)
;;  - stepHd co-recursion. ret a seq formed by processing hd, recursion on the further data gened by head. 
;;      loop (let [curpron (stepHd xs)] (lazy-seq (cons curpron (lazy-seq (stepHd curpron)))) )))
;;  
(fn intrapol
  ([pred v coll]
    (if (or (empty? coll)
            (< (count (take 2 coll)) 2))
      coll
      (intrapol pred v coll [])))
  ([pred v coll partRslt]        ;; partRslt is not used here, this is recursive fn call.
    (let [ hd (first coll) 
           nxthd (first (rest coll))]
      (if (nil? nxthd)          
        (vector hd) 
        (if (pred hd nxthd)
          (lazy-seq (cons hd (cons v (intrapol pred v (rest coll) partRslt))))  ;; cant use self recur call, as recur not in tail
          (lazy-seq (cons hd (intrapol pred v (rest coll) partRslt))))))))

(= [0 1 :same 1 2 3 :same 5 8 13 :same 21]
   (take 12 (->> [0 1]
                 (iterate (fn [[a b]] [b (+ a b)]))
                 (map first) ; fibonacci numbers
                 (__ (fn [a b] ; both even or both odd
                       (= (mod a 2) (mod b 2)))
                     :same))))

;;
;; take-while but stop only when n items satisfied, not whenever pred is false.
;;
(fn take-while-n [n pred seqns]
  (let [hd (first seqns) bd (rest seqns) pred? (pred hd)]
    (if (or (zero? n)      ;; should check header.
            (and (= 1 n)
                 pred?))
        []    ;; ret empty seq at leaf so parent can conj its result on top of it to bottom up.
        (if pred?
          (lazy-seq (cons hd (take-while-n (dec n) pred bd)))
          (lazy-seq (cons hd (take-while-n n pred bd)))))))

(= ["this" "is" "a" "sentence"]
   (__ 3 #(some #{\i} %)
         ["this" "is" "a" "sentence" "i" "wrote"]))
;;
;; create a map such that each key in the map is a keyword, and the value is a sequence of all the numbers (if any) 
;; between it and the next keyword in the sequence.
;;
(fn keyvals [ l ]
  (loop [lst l curk nil partRslt {} ]
    (let [hd (first lst) body (rest lst)]
      (if (nil? hd)
        partRslt
        (if (= clojure.lang.Keyword (type hd))
          (recur body hd (assoc partRslt hd []))
          (recur body curk (update-in partRslt [curk] (fnil conj []) hd)))))))

(= {:a [1 2 3], :b [], :c [4]} (__ [:a 1 2 3 :b :c 4]))

;;
;; oscillating iterate: a function that takes an initial value and a variable number of functions.
;; stepHd rets a lazy seq gened by processing then head item. We then lazy-cons head onto it forms the ret lazy-seq.
;;
(fn oscilrate [v & fns ]
  (let [ cycledfns (cycle fns) ]
    (letfn [ (stepHd [v & fns]
                     (let [ hdfn (first fns) 
                           nextv (hdfn v) ]
                       (lazy-seq (cons nextv (apply stepHd nextv (rest fns)))))) ]
            (cons v (apply stepHd v cycledfns)))))

(= (take 12 (__ 0 inc dec inc dec inc)) [0 1 0 1 0 1 2 1 2 1 2 3])
    
;;
;; universal compute engine, take a prefix math form, and param map, compute the value.
;; compute form closure to wrap the form and val map. resolve symb closure recursively call computer form closure. 
;;
(fn compute-engine [form]
  (fn [vmap]
    (letfn [(numb? [x]
                 (let [t (type x)]
                   (if (or (= java.lang.Integer t)
                           (= java.lang.Long t))
                     true
                     false)))
            (symb? [x]
                 (let [t (type x)]
                   (if (= clojure.lang.Symbol t)
                     true
                     false)))
            (compute-form [form vmap]
                          (letfn [ (symb-val [x]    ;; a fn closure to resolve each symbol. recursively.
                                             (if (numb? x)
                                               x
                                               (if (symb? x)
                                                 (vmap x)
                                                 (compute-form x vmap))))]
                            (let [op (first form)]
                              (condp = op
                                '* (apply * (map symb-val (rest form)))
                                '/ (apply / (map symb-val (rest form)))
                                '+ (apply + (map symb-val (rest form)))
                                '- (apply - (map symb-val (rest form)))))))]
      (compute-form form vmap))))

(= [6 0 -4]
     (map (__ '(* (+ 2 a)
  	              (- 10 b)))
	        '[{a 1 b 8}
	          {b 5 a -2}
	          {a 2 b 11}]))

;;
;; levenshtein distance.
;; http://www.codeproject.com/Articles/13525/Fast-memory-efficient-Levenshtein-algorithm
;;
(fn levenshtein [src tgt]
  (let [srclen (count src) tgtlen (count tgt) rowsz (inc tgtlen)]
    (if (= src tgt)
      0
      (if (or (= (count src) 0)
              (= (count tgt) 0))
        (max (count src) (count tgt))
        (loop [srcidx 0 tgtidx 0 preRow (range 0 rowsz) curRow (conj [] (inc srcidx))] ;; curRow[0]=srcidx+1
          (let [srclt (nth src srcidx)
                tgtlt (nth tgt tgtidx)
                nxtsrcidx (inc srcidx)
                nxttgtidx (inc tgtidx)
                leftv (nth preRow nxttgtidx)
                leftupperv (nth preRow tgtidx)
                upperv (nth curRow tgtidx)
                cost (fn [slt dlt] (if (= slt dlt) 0 1))
                mincurv (min (inc leftv) (inc upperv) (+ leftupperv (cost srclt tgtlt)))]
            ;; does cur row iteration done ?
            ;;(prn srclt tgtlt nxtsrcidx preRow curRow)
            (if (= nxttgtidx tgtlen)   ;; done one iteration of tgt row
              (if (= nxtsrcidx srclen)
                mincurv     ;; the result is in last of cur-row after iterating all.
                (recur nxtsrcidx 0 (conj curRow mincurv) (conj [] (inc nxtsrcidx))))  ;; next src letter
              (recur srcidx nxttgtidx preRow (conj curRow mincurv)))))))))

(= (__ "ttttattttctg" "tcaaccctaccat") 10)

;;
;; word chain, get Levenshtein distance 1 neighors and TSP visit all nodes.
;; TSP, recu loop each children, self-recursion travelsman fn. one of them will succ or all will fail.
;; remove cur from map to avoid cycle. bottom up build the path from succ leaf.
(fn word-chain [wdset]
  (letfn [(levenshtein [src tgt]
                       (let [srclen (count src) tgtlen (count tgt) rowsz (inc tgtlen)]
                         (if (= src tgt)
                           0
                           (loop [srcidx 0 tgtidx 0 preRow (range 0 rowsz) curRow (conj [] (inc srcidx))] ;; curRow[0]=srcidx+1
                             (let [srclt (nth src srcidx)
                                   tgtlt (nth tgt tgtidx)
                                   nxtsrcidx (inc srcidx)
                                   nxttgtidx (inc tgtidx)
                                   leftv (nth preRow nxttgtidx)
                                   leftupperv (nth preRow tgtidx)
                                   upperv (nth curRow tgtidx)
                                   cost (fn [slt dlt] (if (= slt dlt) 0 1))
                                   mincurv (min (inc leftv) (inc upperv) (+ leftupperv (cost srclt tgtlt)))]
                               
                               ;; does cur row iteration done ?
                               ;;(prn srclt tgtlt nxtsrcidx preRow curRow)
                               (if (= nxttgtidx tgtlen)   ;; done one iteration of tgt row
                                   (if (= nxtsrcidx srclen)
                                     mincurv     ;; the result is in last of cur-row after iterating all.
                                     (recur nxtsrcidx 0 (conj curRow mincurv) (conj [] (inc nxtsrcidx))))  ;; next src letter
                                   (recur srcidx nxttgtidx preRow (conj curRow mincurv))))))))
          (nbmap [wdset]
                 (reduce (fn [ret this]
                           (assoc ret this (filter #(= 1 (levenshtein this %)) wdset))) {} wdset))
          ;; This is travelling salesman problem. Not suitable for dfs or bfs. 
          (dfs [cur nbmap]
               (loop [partmap nbmap stack [cur] discovered #{cur} partRslt []]
                 (if (= (count stack) (count (keys nbmap)))
                   stack
                   (if (empty? stack)        ;;  dfs stack from cur node done. return all explored nodes
                     nil
                     (let [topnod (peek stack)
                           children (remove #(contains? discovered %) (nbmap cur))
                           child (first children)]
                       (if child
                         (recur partmap (conj stack child) (conj discovered child) (conj partRslt [cur child]))
                         ;; all children explored, pop stack top, back to parent.
                         (recur partmap (pop stack) discovered partRslt)))))))
                           
          (travelsman [cur partmap]
                  ;; TSP, recu loop each children, self-recursion travelsman fn. one of them will succ or all will fail.
                  ;; remove cur from map to avoid cycle. bottom up build the path from succ leaf.
                  (if (and (= 1 (count partmap))  ;; the
                           (= cur (first (keys partmap))))
                    [true [cur]]
                    (loop [curnbs (partmap cur) filtermap (dissoc partmap cur)]  ;; loop all nbs, dissoc cur to avoid cycle
                      (if (empty? curnbs)    ;; explored all neighbors of cur node, not found, ret false.
                        [false []]
                        (let [childresult (travelsman (first curnbs) filtermap)]
                          (if (first childresult)
                            [true (conj (second childresult) cur)]  
                            (recur (next curnbs) filtermap)))))))]  ;; for each nb, recur to call travelsman
    
    (let [ sortednbmap (into {} (sort-by (comp count val) < (nbmap wdset)))]
      (loop [src (keys sortednbmap)]
        (if (empty? src)
          false
          (let [rslt (travelsman (first src) sortednbmap)]
            (if (first rslt)
              (prn rslt) ;; true
              (recur (next src)))))))))

(= true (__ #{"hat" "coat" "dog" "cat" "oat" "cot" "hot" "hog"}))               
                       
;;
;; graph tour, each edge visited only once.
;; An undirected graph has an Eulerian trail if and only if at most two vertices have odd degree, 
;; and if all of its vertices with nonzero degree belong to a single connected component.
;; first, use bfs to verify we only have one connected components. then check at most two vertices have odd degree.
;;
(fn graph-tour [ edges ]
  (letfn [(nbmap [col]  
                 ;; collection of edges [[a b] [c d]]
                 (let [bi-edges (into col (map (fn [e] [(second e) (first e)]) col))]
                   (reduce (fn [ret e]
                             (update-in ret [(first e)] (fnil conj #{}) (last e))) {} bi-edges)))
          (bfs [nbmap q discovered]  
                  ;; q is PersistentQueue, conj to discovered set when we find a new node, until q is empty.
                  (let [hd (peek q)   ;;  
                        hdnb (remove #(contains? discovered %) (nbmap hd))]
                    (if (empty? hdnb)
                      discovered
                      (recur nbmap (into (pop q) hdnb) (reduce #(conj %1 %2) discovered hdnb)))))
          (stepHd [nbmap q discovered]
                  ;; stepHd process head and ret a lazy list of node reachable from head.
                  (when-let [hd (peek q)]
                    (lazy-seq
                      (cons hd 
                            (let [hdnb (remove #(contains? discovered %) (nbmap hd))]
                              (stepHd nbmap (into (pop q) hdnb) (reduce #(conj %1 %2) discovered hdnb)))))))
          (odd-degrees [edges]
                      ;; ret the num of nodes that have odd degrees
                      (let [out-deg-map (reduce (fn [ret e] (update-in ret [(first e)] (fnil inc 0))) {} edges)
                            in-deg-map (reduce (fn [ret e] (update-in ret [(second e)] (fnil inc 0))) {} edges)
                            deg-map (merge-with + out-deg-map in-deg-map)
                            odd-deg (filter (fn [e] (odd? (val e))) deg-map)]
                        (count odd-deg)))]
    (let [graph (nbmap edges)
          root (key (first graph))
          q (conj clojure.lang.PersistentQueue/EMPTY root)
          discovered (conj #{} root) 
          stephdcomponent (stepHd graph q discovered)
          bfscomponent (bfs graph q discovered)]
      ;;(prn graph)
      ;;(prn (take 5 (stepHd graph q discovered)))
      (if (and (= (count bfscomponent) (count (keys graph)))
               (<= (odd-degrees edges) 2))   ;; at most 2 vertices with odd edges
        true
        false))))
               
(= true (__ [[:a :b] [:a :c] [:c :b] [:a :e]
              [:b :e] [:a :d] [:b :d] [:c :e]
              [:d :e] [:c :f] [:d :f]]))

(= false (__ [[:a :b] [:a :b] [:a :c] [:c :a]
               [:a :d] [:b :d] [:c :d]]))

;;
;; traiangle min path
;; choose the min path of the two children path. 
;; at each step, ret a pair of min-path-val to min path
;;
(fn triangle-min-path 
  ([coll]
    (if (empty? coll)
      nil
      (first (triangle-min-path coll 0))))   ;; colidx inside the cur row(the top)
  ([coll colidx]
    ;; at each step, return a map of min-val to min-path vec
    ;; as we are building from leaf bottom up, recursion exits at leaf
    (let [v (nth (first coll) colidx)
          nextrow (rest coll)]
      (if (empty? nextrow)
        [ v [v] ]  ;; ret a pair of min val and min-path vec
        (let [[lv lp] (triangle-min-path nextrow colidx)
              [rv rp] (triangle-min-path nextrow (inc colidx))]
          (if (< lv rv)
            [(+ lv v) (conj lp v)]
            [(+ rv v) (conj rp v)]))))))
             
(= 20 (__ '([3]
           [2 4]
          [1 9 3]
         [9 9 2 4]
        [4 6 6 7 8]
       [5 7 3 5 1 4]))) ; 3->4->3->2->7->1       
        
    
;; 
;; transitive closure
;; merge all ele of the same set into a sorted list(set), then gen powerset of the list
;; due to transitive nature, we only look at a segment's head and tail when merging.
;;
(fn transitive-closure [coll]
  (letfn [(add-node [partRslt curnode]
                    ;; not used here, as map can not augment cur list.
                    (map (fn [this]
                              (if (= (first this) (last curnode))
                                (cons (first curnode) this)
                                (if (= (last this) (first curnode))
                                  (conj this (first curnode))
                                  this)))))
          (update-or-add [closure curnode]
                         ;; from existing closure, maps to merged closure, if not aug existing entry, append to end.
                         (loop [closure closure mergedclosure [] updated false]  ;; curnode aug an existing entry?
                           (if (empty? closure)
                             (if updated
                               mergedclosure
                               (conj mergedclosure curnode))  ;; append curnode to the merged closure
                             (let [[hdst hded] (first closure)
                                   [st ed] curnode]
                               (if (= hded st)
                                 (recur (next closure) (conj mergedclosure (conj (vec (first closure)) ed)) true)
                                 (if (= hdst ed)
                                   (recur (next closure) (conj mergedclosure  (cons st (first closure))) true)
                                   (recur (next closure) (conj mergedclosure (first closure)) updated)))))))
          (powerset [coll]
                    ;; coll is [1 2 3 4]
                    (loop [coll coll rslt []]
                      (let [hd (first coll) bd (rest coll)]
                        (if (empty? bd)
                          rslt
                          (let [pairs (map (fn [e] [hd e]) bd)]
                            (recur bd (into rslt pairs)))))))]
          (let [mergedclosure (reduce update-or-add [] coll)]
            (loop [coll mergedclosure result #{}]
              (if (empty? coll)
                result
                (recur (next coll) (into result (powerset (first coll)))))))))
                          
(let [divides #{[8 4] [9 3] [4 2] [27 9]}]
  (= (__ divides) #{[4 2] [8 4] [8 2] [9 3] [27 9] [27 3]}))

(let [more-legs
      #{["cat" "man"] ["man" "snake"] ["spider" "cat"]}]
  (= (__ more-legs)
     #{["cat" "man"] ["cat" "snake"] ["man" "snake"]
       ["spider" "cat"] ["spider" "man"] ["spider" "snake"]}))
    


;;
;; subset sum and subset max(knapsack).
;; 1. bottom-up, what is the solution with just 1 item at bottom, and build-up to 2 items, and nth items.
;; 1.When iterate to nth item, fn return the solution with nth item based on the solution of previous [0..n-1] ROW. 
;;   ROW are items 0, 1, ..., n-1, tab[i] is the solution of item subset[0..i]. recursion to ROW+1 based on ROW. 
;;   tab[i, val] is the solution of the subset items from item pool[0..i], with optimal(sum, max) val = val. 
;;   e.g tab[2, 5] of set [1 2 3 7] can have subset val [2 3]. tab[4, 10] of V[10 40 30 50] W[5 4 6 3] has max v=90
;;   incl/excl:   tab[idx, v] = or { tab[idx-1, v], tab[idx-1, v-vi]}       ;; subset-sum, cell store T/F, or items.
;;                tab[idx, w] = max{ tab[idx-1, w], tab[idx-1, W-wi] + vi } ;; cell store max values of items.
;;                tab[val] = min [ for-each coin Vi {tab[val-Vi] + 1} ]     ;; coin types/items is fixed len list.
;;
;; I varies of item idx[0..len(item)] from bottom up, J varies in value space[0..max]
;; Use memoize to tab the sol during bottom-up.
;; use with-local-vars to maek the recursive function see its own memoized bindings
;;
(fn subsetsum [ & sets ]
  (letfn [(subsetsum-with-local-vars [ ]
                         (with-local-vars   ;; with-local-var create local dynamic vars.
                           [subsetsum-mem (memoize (fn [itemsvec idx sumval]
                                 ;; items in vec[0..n]
                                 ;; bottom, what happened with only 1 item
                                 (if (= idx 0)
                                   (if (= sumval (nth itemsvec idx))  ;; bottom, ret true if found, false if not.
                                     true
                                     false)
                                   ;; not bottom, first check direct hit before incl/excl recursion.
                                   (if (>= idx (count itemsvec))
                                     false    ;; idx out of bound
                                     (let [curval (nth itemsvec idx)]
                                       (if (= sumval curval)
                                         true
                                         (if (> sumval curval)  ;; incl only when val > curval
                                           (or (subsetsum-mem itemsvec (dec idx) (- sumval curval))
                                               (subsetsum-mem itemsvec (dec idx) sumval))
                                           (subsetsum-mem itemsvec (dec idx) sumval))))))))]
                           (.bindRoot subsetsum-mem @subsetsum-mem)
                           @subsetsum-mem)) ;; ret memoize fn
          ;; Y-combinator version that pass recursion fn into to itself.
          ;; all the trouble to make the recursive function see its own memoized bindings
          (subsetsum-ycombinator []  ;; dp take a memoized fn as first arg
             (let [dp (fn [mem-dp itemsvec idx sumval]
                        (let [dp (fn [itemsvec idx sumval]  
                                   ;; redef dp, so pass down memoized dp
                                   (mem-dp mem-dp itemsvec idx sumval))]
                          (if (= idx 0)
                            (if (= sumval (nth itemsvec idx))  ;; bottom, ret true if found, false if not.
                              true
                              false)
                            ;; not bottom, first check direct hit before incl/excl recursion.
                            (if (>= idx (count itemsvec))
                              false    ;; idx out of bound
                              (let [curval (nth itemsvec idx)]
                                (if (= sumval curval)
                                  true
                                  (if (> sumval curval)  ;; incl only when val > curval
                                    (or (dp itemsvec (dec idx) (- sumval curval))
                                        (dp itemsvec (dec idx) sumval))
                                    (dp itemsvec (dec idx) sumval))))))))
                   mem-dp (memoize dp)]
               (partial mem-dp mem-dp)))
               
          (has-subsetsum-vars [itemsvec sumval]  ;; whether this set has subset sum to sumval
            ((subsetsum-with-local-vars) itemsvec (dec (count itemsvec)) sumval))
          
          (has-subsetsum-yb [itemsvec sumval]  
            ; whether this set has subset sum to sumval
            ((subsetsum-ycombinator) itemsvec (dec (count itemsvec)) sumval))
          (max-value [ sets ]
            (apply max (map #(apply + %) (map (fn [e] (map #(Math/abs %) e)) sets))))
   
          (all-true [vs]
            (let [hd (first vs)]
              (and (= hd true) (every? #(= hd %) vs))))]

      (loop [v (- 0 (max-value sets))]
        (if (> v (max-value sets))
          false
          (if (all-true (for [s sets] (has-subsetsum-yb (vec s) v)))
            true
            (recur (inc v)))))))

(subsetsum  #{-1 1 99} 
             #{-2 2 888}
             #{-3 3 7777})

(= false (subsetsum #{1 -3 51 9} 
             #{0} 
             #{9 2 81 33}))

(= true  (subsetsum #{1 3 5}
             #{9 11 4}
             #{-3 12 3}
             #{-3 4 -2 10}))

;;
;; Number Maze find a path between the two using only three possible operations:
;;  - double, halve, add 2
;; sol: x + 1 = (2x+2)/2
;; for odd x, (2x+2), if half is odd and great than dest, add 2. Otherwise, half.
;; 
(fn [start end]
  (letfn [(dist [x end]
              (if (= end x)
                0   ;; one step away
                (if (or (= end (+ x 2))
                        (= end (* x 2))
                        (= end (/ x 2)))
                  1
                  -1)))

          (add-one [x v]
            (let [x2 (* 2 x) x22 (+ 2 x2) x1 (/ x22 2)]
              [x1 (conj v x x2 x22)]))
  
          (half-odd [x v end]
            (let [[x1 v1] (add-one x v)]
              (half-even x1 v1 end)))

          yyyyy-even [x v end]
            ;; when counting down, consider the dist to end between 
            ;; half, add 2 half, ensure we can continue recuring on 
            ;; half by selecting the even half between half and add 2 half
            (let [xh (/ x 2) xh1 (/ (+ x 2) 2) d (dist xh end)]
              (if (= 0 d)
                [xh (conj v x end) d]
                (if (= 1 d)
                  [xh (conj v x xh end) d]
                  (if (= 0 (dist xh1 end))
                    [xh1 (conj v x (+ x 2) end)]
                    (if (> xh end)
                      (if (even? xh)  ;; recur on the half that is even
                        (half-even xh (conj v x) end)
                        (half-even xh1 (conj v x (+ x 2)) end))))))))

          (double-up [x v end]
            ;; double up or add two, mono increasing
            (let [dbx (* x 2) x2 (+ x 2) d (dist x end)]
              (if (= 0 d)
                [x (conj v x)]
                (if (= 1 d)
                  [x (conj v x end)]
                  (if (< dbx end)
                    (double-up dbx (conj v x) end)
                    (if (= 1 (- end x))
                      [(inc x) (conj (second (add-one x v)) end)]
                      (double-up x2 (conj v x) end)))))))]
  (if (<= start end)
    (count (second (double-up start [] end)))
    (if (odd? start)
      (count (second (half-odd start [] end)))
      (count (second (half-even start [] end)))))))


;;
;; clojure quine
;; refs to wiki for java version 
;; The idea for the program is that there are two copies of the same lambda expression. 
;; The first is the program and the second is data
;;
((fn [x] (list x (list (quote quote) x))) (quote (fn [x] (list x (list (quote quote) x)))))

(fn [] (list (quote (fn [] (list (quote quote))))))






                                    