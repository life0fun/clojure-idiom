;; java collection
;; (load-file "java-array.clj")

;; first, import the java package.
(ns java-array
  (:import [java.text SimpleDateFormat]
           [java.util Calendar TimeZone])
  (:use clojure.contrib.io
        clojure.contrib.seq-utils)

;;
;; list logic operation, (and list1 list2)
(def data [[:x :e :e] [:o :x :e] [:o :e :x]])
(for [x [0 1 2]] (nth (nth data x) x))        ;; get diagonal
(for [x [0 1 2]] (nth (nth data x) (- 3 x)))  ;; reverse diagonal

;; use java array as a mutable container for intermediate result.
 here we are talking about JVM arrays: a mutable container 
;; (alength tokens) (aget tokens 2) (aset tokens 2 "actionable")

;; split a string into java array
(def tokens (.split "clojure.in.action" "\\."))
;; use amap to prn array
(def z (amap tokens idx ret (aset ret idx (.concat "xx" (aget ret idx)))))
(amap z idx ret (prn (aget ret idx)))

(defn asum [#^floats xs]
  (areduce xs i ret (float 0) (+ ret (aget xs i))))

(asum (float-array [1 2 3]))

;;
;; types of array : primitive and reference
;; primitive array : boolean-array byte-array char-array double-array float-array int-array long-array object-array short-array
;; use make-array and into-array functions to create primitive arrays:
(doto (StringBuilder. "abc")
  (.append (char-array [\x \y \z])))

(let [ary (make-array Integer/TYPE 3 3)]  ;; obtain Class objects for the primitive types
  (dotimes [i 3]
    (dotimes [j 3]
      (aset ary i j (+ i j))))
        (map seq ary))

(into-array Integer/TYPE [1 2 3])

;; reference array : Character [], array with boxed primitive.
;; into-array ret reference array of boxed primitive
(into-array ["a" "b" "c"])

(doto (StringBuilder. "abc")
  (.append (into-array [\x \y \z])))
  ;=> #<StringBuilder abc[Ljava.lang.Character;@65efb4be>]

(into-array [(java.util.Date.) (java.sql.Time. 0)])

;; To create a heterogeneous array of java.lang.Object, use the to-array or to-array-2d 
(to-array-2d [[1 2 3] [4 5 6]])  ;;=> #<Object[][] [[Ljava.lang.Object;@bdccedd>
(to-array ["a" 1M #(%) (proxy [Object] [])]) ;;=> #<Object[] [Ljava.lang.Object;@18987a33>
(to-array [1 (int 2)])  ;;=> #<Object[] [Ljava.lang.Object;@6ad3c65d>]]]]

;; variadic method/constructor calls
(String/format "An int %d and a String %s" (to-array [99, "luftballons"]))

;; clojure collections are map, set, sequential(list, vector)
;; clojure types are atom, array
;; Clojure object are boxed types.


;; JVM array are mutale. use aset to change ary content.
(def ary  (into-array [1 2 3]))
(def sary (seq ary))
(aset ary 0 42)

;; Be cautious when sharing arrays from one function to the next, and especially across threads.
;; amap maps an expr across the ary.
(defn asum-sq [xs]
  (let [dbl (amap xs i ret
              (* (aget xs i)
                 (aget xs i)))]
    (areduce dbl i ret 0
      (+ ret (aget dbl i)))))

(asum-sq (float-array [1 2 3 4 5]))

;; using forName to determine
(defmulti what-is class)
(defmethod what-is (Class/forName "[Ljava.lang.String;""]") [a] "1d String")
(defmethod what-is (Class/forName "[[Ljava.lang.Object;""]]") [a] "2d Object")
(defmethod what-is (Class/forName "[[[[I""]]]]") [a] "Primitive 4d int")
(defmethod what-is (Class/forName "[[D""]]") [a] "Primitive 2d double")
(defmethod what-is (Class/forName "[Lclojure.lang.PersistentVector;""]") [a])

;; use java util collections static methods, i.e., comparators, sort, etc.
(import '[java.util Comparator Collections ArrayList]')
(defn gimme [] (ArrayList. [1 3 4 8 2]))
(doto (gimme)
  (Collections/sort (Collections/reverseOrder)))

(doto (gimme) (Collections/sort #(compare %2 %1)))
(doto (gimme) (Collections/sort >))
(doto (gimme) (Collections/sort <))
(doto (gimme) (Collections/sort (complement <)))

;; create a thread execute a runnable
(doto (Thread.
  #(do (Thread/sleep 5000)
       (println "haikeeba!")))
  .start)

;; callable with future
;; FutureTask<V> ft = new FutureTask<V>(new Callable<V>(){public V call()});
;; how do we pass args ?
(import '[java.util.concurrent FutureTask]')
(let [f (FutureTask. #(do (Thread/sleep 5000) 42))]
  (.start (Thread. #(.run f)))
  (.get f))  ;; will block until done.

;; java.util.list conformance for sequence and seq.
;; no generic consideration due to erasure.
;; Clojure doesn't handle generics anyway type info does not exist at runtime.
(.get '[a b c] 1)
(.containsAll '[a b c] '[b c])
(.add '[a b c] 'd)              ;; sequence not mutable

;; java.lang.comparable and comparator
;; vector is the only collection that impls Comparable IF.
;;
(.compareTo [:a] [:a])
(.compareTo [:a :b] [:a])
(.compareTo [:a :b] [:a :b :c])
(sort [[:a :b :c] [:a] [:a :b]])

;; java.util.Collection
;; idiom : use a Clojure sequence as a model to build a mutable sequence to use Java Collections API

(defn shuffle [coll]
  (seq (doto (java.util.ArrayList. coll)
    java.util.Collections/shuffle)))

(shuffle (range 10))

;; java.util.map, you can operate on JVM objects directly.
(doto (java.util.HashMap.) (.put :a "xxx"))

(java.util.Collections/unmodifiableMap
  (doto (java.util.HashMap.) (.put :a 1)))

(into {} (doto (java.util.HashMap.) (.put :a 1)))

;;
;; When writing Clojure code, use errors to mean can’t continue and 
;; exceptions to mean can or might continue.
;; Clojure's take on checked exception.
;; By default, declare that all functions throw the root Exception or RuntimeException.




;;
;; use clojure for data process
;;
;; parse a string with java.lang.String
(defn parse-line [line]
  (let [tokens (.split (.toLowerCase line) " ")]
    (map #(vector % 1) tokens)))

(parse-line "Twas brillig and the slithy toves")

;; combine a seq of key value pairs, group by reduce to a map.
(defn combine [mapped]
  (->> (apply concat mapped)
       (group-by first)
       (map (fn [[k v]]
              {k (map second v)}))
       (apply merge-with conj)))

(use 'clojure.contrib.io')
(combine (map parse-line (read-lines "/Users/e51141/tmp/x")))

;; sum the tally count of a vec of value.
(defn sum [[k v]]
  {k (apply + v)})

;; sum the val vector for each key, then merge keys
(defn reduce-parsed-lines [collected-values]
  (apply merge (map sum collected-values)))

;; integrated solution
(defn word-frequency [filename]
  (->> (read-lines filename)
    (map parse-line)
      (combine)
        (reduce-parsed-lines)))




