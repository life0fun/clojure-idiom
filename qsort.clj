;; lazy qsort
;; (load-file "qsort.clj")

(ns qsort
  (:import [java.util.collections])
  (:use clojure.set))

; take n from a random seq
(defn nom [n] (take n (repeatedly #(rand-int n))))

;
; qsort, divide and conquer, pull apart work list, destructuring.
; pull work apart as two lists, for each list, again pull apart as pivot(head) and rest
;

(defn sort-parts [work]
  (lazy-seq
    (loop [[part & parts] work]
      (if-let [[pivot & xs] (seq part)]
        (let [smaller? #(< % pivot)]
          (recur (list*
            (filter smaller? xs)        ;; filter ret a new list, recured as sub part
            pivot
            (remove smaller? xs )       ;; remove ret a new list, recur as sub parts
            parts)))                    ;; still carry cur parts
        (when-let [[x & parts] parts]   ;; for the second part, cons pivot on top of sorted list
          (cons x (sort-parts parts)))))))  ;; recursion, interim result ret from each iteration.


;;
;; what focus on result, not procedure really means in fn lang
;  1. do not moving index, just make another list of pairs of idx, item.
;  2. list tranform, filter; build solution for simplest case, recursive induction on this base.
;; in imperative lang, we need to carefully adjust offset/index/pointer to avoid off-by-one error
;; and manually swap items. For example, qsort in python:
;;
;; def qsort(l, beg, end):
;;     def swap(l, i, j):
;;         l[i], l[j] = l[j], l[i]
;;
;;     if beg <= end:
;;        return l
;;
;;     pivot = l[beg]
;;     i, j = beg + 1, end
;;
;;     while i <= j:
;;         while i < end and l[i] <= pivot:
;;             i += 1
;;         while j > beg and l[j] > pivot:
;;             j -= 1
;;
;;         if(i < j):
;;             swap(l, i, j)
;;             i += 1
;;             j -= 1
;;         else:
;;             break
;;
;;     # swap pivot to j as i was manually moved for pivot
;;     swap(l, beg, j)
;;     qsort(l, beg, i-1)
;;     qsort(l, j+1, end)
;;
;; in fn lang, focus on result by transform list from [head rest] into [smaller pivot higher].
;; then what left is just recursion.
;;
;;

(defn myqsort [l]
  (loop [[low & high] l]                ;; first destructuring top list into low and high partition
      (prn "looping low " low " high " high)
      (if-let [[pivot & xs] (seq low)]  ;; low half is as left child, go all the way down. Note the nil pun with seq
        (let [ smaller? #(< % pivot) ]
          (do
            (prn "recur pivot " pivot " low " low " low rest " xs " divided smaller " (filter smaller? xs))
            (prn "recur binding l=" (list* (filter smaller? xs) pivot (remove smaller? xs)))

            (recur (list*          ;; recur by transform list into [ [smaller-smaller] [pivot [smaller-higher] higher]]
              (filter smaller? xs) ;; recur divide smaller partition into smaller and higher partition around pivot
              pivot                ;; constr
              (remove smaller? xs) ;; larger partition of the low partition
              high))))             ;; original large partition of high partition
      (when-let [[p & xs] high]           ;; if-let false, smaller partition done, recursion on larger partition
        (prn "low nil, cons " p " into result of high " xs)
        (cons p (myqsort xs))))))

(defn qsort [xs]
  (myqsort (list xs)))

;
; test
(qsort [2 1 4 3])

; lazy-cat is used to merge intermediate result during recursion to ret single list to caller.
; with lazy-cat to merge intermediate result during recursion, we can use map to divide and distribute works.
; first, destruct the passed-in list as (p & body), then partition body into 
; low hi bodies, full sol = cons solution for low, pivot, solution for hi.
(defn lazyqsort [l]
  (if-not (seq l)   ; idiomatic. (seq l) ret a seq view of the collection, or nil
    []  ; ret empty seq so high level can lazy-cat vectors
    (let [p (first l) body (rest l) 
          lol (filter #(<= % p) body) hil (remove #(<= % p) body) ]
      (lazy-cat (lazyqsort lol) [p] (lazyqsort hil)))))

(lazyqsort [1])
(lazyqsort [1 1 1])
(lazyqsort [1 2 3])
(lazyqsort [9 8 7 6 1 2 3 4])

; lazy-cat merge-sort
(defn mergesort [xs]
  (letfn [(merge [p q]
            (cond
              (not (seq p)) q  ; p is done, take entire q
              (not (seq q)) p  ; q is done, take entire p
              :else
                (let [ph (first p) qh (first q)]
                  (if (< ph qh)
                    (lazy-seq (cons ph (merge (rest p) q)))
                    (lazy-seq (cons qh (merge p (rest q))))))))]
    (if (<= (count xs) 1)
      xs  ; base, only one ele left, ret
      (let [[l q] (split-at (quot (count xs) 2) xs)]  ; split-at half
        (merge (mergesort l) (mergesort q))))))

(mergesort [1])
(mergesort [1 1 1])
(mergesort [1 2 3])
(mergesort [9 8 7 6 1 2 3 4])


;;
;; bisect, if not found, insert to the end.
;; To make this really fast you will want to use int throughout, though,
;; and unchecked arithmetic:
;;
(defn bisect [l v]
  "binary search for value in l, if not found, insert v at the end of l"
  (loop [i (int 0) j (int (dec (count l)))]  ;; count is inlined, thus ret a primitive.
    (if (> i j) ; continue when i == j
      false
      (let [ mid (unchecked-divide (unchecked-add i j) 2) midv (l mid) ]
        (cond
          (= midv v)
            mid
          (> midv v)
            (recur i (unchecked-dec mid))
          :else
            (recur (unchecked-inc mid) j))))))

(bisect [1 3 5 6 8 9] 3)

;; use java collections binarySearch directly
(defn java-binsearch [xs x]
  (java.util.Collections/binarySearch xs x compare))

(java-binsearch [1 3 5 6 8 9] 3)

; recursive version of bi-sect, insert just before entry that is bigger than val
; arg is map-indexed vector l, [idx val]
(defn recur-bisect [l v]
  (if (empty? l)
    0   ; ret immediately upon empty list, we ensured never recur with empty list.
    (let [len (count l) mid (quot len 2) midv (second (nth l mid))
          lo (subvec (vec l) 0 mid) hi (subvec (vec l) (inc mid))]
      (if (>= v midv)     ; recur until the first one bigger than val
        (if (empty? hi)  ; hi subvec explored, insert after mid
          (inc (first (nth l mid)))
          (recur hi v))        ; never recur with empty list
        (if (empty? lo)       ; lo subvec explored, insert before mid
          (first (nth l mid))
          (recur lo v))))))   ; never recur with empty list

(recur-bisect (map-indexed vector []) 3)
(recur-bisect (map-indexed vector [5]) 3)
(recur-bisect (map-indexed vector [1 2 3 4 5]) 3)
(recur-bisect (map-indexed vector [1 2 3 3]) 3)
(recur-bisect (map-indexed vector [1 2 3 3 5]) 3)
(recur-bisect (map-indexed vector [1 2 3 3 5]) 8)


; for list, use header iteration when need to apply fn to each element in list.
; for tree, can use branch DP to explore
(defn permutation [text]
  (letfn [(inject-each-pos [hd subw]   ; ret a list of strings
            (if (empty? subw)
              hd   ; bottom, ret hd string
              (let [sz (inc (count subw))
                    splits (map #(split-at % subw) (range sz))
                    injected-splits (map #(concat (first %) (vec hd) (second %)) splits)]
                (map #(apply str %) injected-splits))))]

    (if (empty? text)
      []
      (let [ hd (subs text 0 1)
             subp (permutation (subs text 1))]
        (if (empty? subp)
          [hd]
          (mapcat #(inject-each-pos hd %) subp))))))

;
; mutual recursion is idea for state machine transition

; trampoline(fn & args) change recur fn to  recur #(fn) to achieve TCO
; you give trampoline a fn, trampoline will recur the fn without stack overflow.

(defn my-even? [n]
  (letfn [(e? [n]
            (if (zero? n)
              true
              #(o? (dec (Math/abs n)))))
          (o? [n]
            (if (zero? n)
              false
              #(e? (dec (Math/abs n)))))]
          (trampoline e? n)))

(defn my-odd? [n]
  (not (my-even? n)))


; recursive build a list, recursive destructure a list
(defn- coll-or-scalar [x & _] (if (coll? x) :collection :scalar))  ; dispatch 
(defmulti replace-symbol coll-or-scalar)
(defmethod replace-symbol :collection [coll oldsym newsym]
  (lazy-seq   ; invoke the body only when needed, ret empty seq at bottom
    (when (seq coll)
      ; apply the same repalce-symbo to the first ele, be it scalar or a seq, 
      ; and to rest list. replace-symbo polymorphy by dispatch on the ele type.
      (cons (replace-symbol (first coll) oldsym newsym)
            (replace-symbol (rest coll) oldsym newsym)))))
; after dispatching, the first arg is exact
(defmethod replace-symbol :scalar [obj odlsym newsym]
  (if (= obj oldsym)
    newsym
    oldsym))

; given a list, continuously delete every other ele until one left
; just transform the list recursive
(defn filterlist [l]
  (if (= (count l) 1)   ; base, only one, ret ele in the list
    (first l)
    (let [sz (count l)  ; or if next point to itself, only one left.
          keep-even-l (keep-indexed #(if (even? %1) %2) l)] ; keep ele with even idx, drop odd index ele in the list
      (if (even? sz)   ; if len is even, the last ele is dropped, no need to adjust head for next recursion
        (recur keep-even-l)
        (recur (next keep-even-l))))))  ; odd is size, next recursion need to adjust head

