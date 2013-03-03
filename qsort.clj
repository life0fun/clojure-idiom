;; lazy qsort
;; (load-file "qsort.clj")

(ns qsort
  (:import [java.util.collections])
  (:use clojure.set))

;; take n from a random seq
(defn nom [n] (take n (repeatedly #(rand-int n))))

;;
;; qsort, divide and conquer, pull apart work list, destructuring.
;; pull work apart as two lists, for each list, again pull apart as pivot(head) and rest
;;

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

;;
;; test
(qsort [2 1 4 3])

;;
;; bisect, if not found, insert to the end.
;; To make this really fast you will want to use int throughout, though,
;; and unchecked arithmetic:
;;
(defn bisect [l v]
  "binary search for value in l, if not found, insert v at the end of l"
  (loop [i (int 0) j (int (dec (count l)))]  ;; count is inlined, thus ret a primitive.
    (if (> i j)
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


