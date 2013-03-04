;; java collection
;; (load-file "java-array.clj")

;; clojure collections are map, set, sequential(list, vector)
;; clojure types are atom, array
;; Clojure numbers are of the boxed variety.

(doto (StringBuilder. "abc")
  (.append (char-array [\x \y \z])))

;; use the make-array and into-array functions to create primitive arrays
(let [ary (make-array Integer/TYPE 3 3)]
  (dotimes [i 3]
    (dotimes [j 3]
      (aset ary i j (+ i j))))
  (map seq ary))

;; Ary mutability
(def ary  (into-array [1 2 3]))
(def sary (seq ary))
(aset ary 0 42)

;; variadic method/constructor calls
(String/format "An int %d and a String %s" (to-array [99, "luftballons"]))

;; To create an array of a particular reference type, into-array passing in a sequence of objects:

(into-array ["a" "b" "c"])
(into-array [(java.util.Date.) (java.sql.Time. 0)])

;; using forName to determine
(defmulti what-is class)
(defmethod what-is (Class/forName "[Ljava.lang.String;""]") [a] "1d String")
(defmethod what-is (Class/forName "[[Ljava.lang.Object;""]]") [a] "2d Object")
(defmethod what-is (Class/forName "[[[[I""]]]]") [a] "Primitive 4d int")
(defmethod what-is (Class/forName "[[D""]]") [a] "Primitive 2d double")
(defmethod what-is (Class/forName "[Lclojure.lang.PersistentVector;""]") [a])

;; comparator
(import '[java.util Comparator Collections ArrayList]')
(defn gimme [] (ArrayList. [1 3 4 8 2]))
(doto (gimme)
  (Collections/sort (Collections/reverseOrder)))

(doto (gimme) (Collections/sort #(compare %2 %1)))
(doto (gimme) (Collections/sort >))
(doto (gimme) (Collections/sort <))
(doto (gimme) (Collections/sort (complement <)))

;; Runnable
(doto (Thread. #(do (Thread/sleep 5000)
  (println "haikeeba!")))
  .start)

;; callable with future
;; FutureTask<V> ft = new FutureTask<V>(new Callable<V>(){public V call()});
;;
(import '[java.util.concurrent FutureTask]')
(let [f (FutureTask. #(do (Thread/sleep 5000) 42))]
  (.start (Thread. #(.run f)))
  (.get f))


;; java.util.list
;;
(.get '[a b c] 1')
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

;; java.util.map
(java.util.Collections/unmodifiableMap
  (doto (java.util.HashMap.) (.put :a 1)))

(into {} (doto (java.util.HashMap.) (.put :a 1)))

;;
;; When writing Clojure code, use errors to mean can’t continue and 
;; exceptions to mean can or might continue.
;; Clojure's take on checked exception.
;; By default, declare that all functions throw the root Exception or RuntimeException.
