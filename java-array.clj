;; java collection
;; (load-file "java-array.clj")

;; here we are talking about JVM arrays: a mutable container 
;; (alength tokens) (aget tokens 2) (aset tokens 2 "actionable")
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
