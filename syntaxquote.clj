;; clarify syntax quote unquote
;; (load-file "syntaxquote.clj")

;;
;; syntax quote unquote demythfied.
;;  http://www.learningclojure.com/2010/11/syntax-quote-kata-for-confused.html
;;
(def x '(* 3 5))
(def y (* 3 5))
x
y
(list 'println x (eval x) y)
(list `println x (eval x) y)
`(list println x (eval x) y)
`(println x (eval x) y)
`(println ~x (eval x) y)
`(println ~x ~(eval x) y)
`(println ~x ~(eval x) ~y)
`(println ~x ~(eval x) ~y ~@x)


;;
;; defmacro diffs from defn as it wont eval the argument.
;; so you use arg is unevaluated form.
;; to eval form, unquote it inside syntax quote.

;; unquoting inside syntax quote: `( ~x )
;; bounce outside the syntax-quoted form and evaluate the form in that context, 
;; inserting the result back where the tilde was.

;; quote unquoting `( '~x ), just unquoting the form, do not eval it.

;; splicing unquote `(max ~@(shuffle (range 10) )) unroll/flatten a colletion into multiple exprs.
;; except that it allows multiple forms to be inserted in the place of a single unquote-splicing form:
;; like apply, splicing "unroll" a collection into multiple expressions.

;; unquote operations are meaningless outside of syntax-quote forms

(defmacro dbg-1 [s]
  (let [x# s]
    `(println '~s ~s ~x#)))

(dbg-1 (* 3 4))

(defmacro dbg-2 [s]
  (let [x# s]
    (do
      (println "dbg-2 uneval form: " x# s)
      `(println "unquote inside syntax-quote " '~s ~s ~x#)
    )))

(dbg-2 (* 3 4))

;; quote unquote, `( '~x)  vs `( ~'x)
`[:a ~(+ 1 1) c]    ;; #user/c
`[:a ~(+ 1 1) 'c]   ;; (quote user/c)
;`[:a ~(+ 1 1) ~c]   ;; unqote tries to eval c, unable to resolve symbol c
;`[:a ~(+ 1 1) '~c]  ;; quote unqote, still trying to find c, take its form, unevaled list.
`[:a ~(+ 1 1) ~'c]  ;; unqote symbol c, get symbol c
`[:a ~(+ 1 1) '~'c] ;; quote unquote, get the raw form of 'c, full qualify ' to (quote c) Vs. (quote user/c)
`[:a (+ 1 1) '~'c]  ;; quote unquote, get the raw form of 'c, full qualify ' to (quote c) Vs. (quote user/c)
`{:a 1 :b '~(list 1 2)}  ;; unquote (1 2) => (quote (1 2))
`{:a 1 :b '~@(list 1 2)} ;; unquote and unroll list, (quote 1 2)

(defmacro dbg-3 [s]
  `(let [x# ~s y# '~s]   ;; unquoting vs quote unquoting
    (do
      (println "dbg-3" 'x# x# y# '~s ~s )
    )))

(dbg-3 (* 3 4))

(defmacro forloop [[i end] & code]
  `(let [finish# ~end]
    (loop [~i 0]
      (when (< ~i finish#)
        ~@code
       (recur (inc ~i))))))

(forloop [i 2]
  (println i)
    (println (* i i)))

;;
;; def source to store func body inside function var's :source meta.
;;
(defmacro defsource
  "Similar to clojure.core/defn, but saves the function's definition in the var's :source meta-data."
  {:arglists (:arglists (meta (var defn)))}
    [fn-name & defn-body]
      `(do (defn ~fn-name ~@defn-body)
           (alter-meta! (var ~fn-name) assoc :source (drop 1 (quote ~&form)))
           (var ~fn-name)))

(defsource foo [a b] (println "foo : " (+ a b)))
(foo 3 5)
(:source (meta #'foo)) ;; => (foo [a b] (+ a b))

;;
;; clojure static scoping and dynamic scoping.
;; with let shadowing.
;;

(def x 1)   # root binding, global to all threads.

(defn dummy-fn2[]
  (println "x from dummy-fn2:" x)
  (+ x 1))  ;; return x+1, did not re-assign to x

(defn dummy-fn []
  (println "entering function:" x)   ;; 1 static scoping 
  (println "var x:" @#'x)   ;; = 1  @#'x ref to root binding.
  (dummy-fn2)               ;; = 1 static scoping
  (println "---")
  (let [x 100]          ;; let closure shadows x with 100, final constant;
    (println "after let:" x);; = 100
    (println "var x:" @#'x) ;; = 1
    (dummy-fn2)             ;; let is local static closure
    (println "---")
    (let [x (dummy-fn2)]    ;; nested let closure x=2
      (println "after let and dummy-fn2:" x)  ;; = 2
      (println "var x:" @#'x)  ;; = 1
      (dummy-fn2)              ;; = 1
      (println "---")
      (binding [x 888]         ;; dynamic change root binding. no affect local closure vars.
        (println "var x:" @#'x) ;; = 888 now global x in this thread changed by binding.
        (println "after binding:" x)  ;; = 2 however, can not change let closure
        (dummy-fn2)       ;; = 888
        (println "---")
        (let [x (dummy-fn2)]    ;; x=888 another local nest closure
          (println "after binding and dummy2:" x)  ;; = 888
          (println "var x:" @#'x)  ;; = 888
          (dummy-fn2)  ;; = 888
          (println "---"))))))

(dummy-fn)

;;
;; calling java from clojure and convert java collection into clojure data structure.
;; To call java method chain, use (doto x & form) : (doto (java.util.HashMap.) (.put :a 1) (.put :b 2))
;; 
;; use (seq coll) or (into #{} coll) to convert java collection into clojure data structure.
;; use (clojure.set/intersection x y) to get set intersection
;; 
(defn stringMatch [s, keywords]   ;; s is a string, not a list.
  (let [ words (.split s " ")
         wordset (into #{} words)
     sect (clojure.set/intersection wordset (set keywords))
       ]

     (if (empty? sect)
       (if (not-empty sect)
         (prn 'not-empty)
     false)
       s)))

(stringMatch "this is the world" ["that" "is"])  ;; note the diff with double quote ['that ...]

;; Named Var : thread local dynamic bindings
    (def a 1)
    (binding [a 10] (.start (Thread. (fn [] (println a)))))
    (binding [a 10] (.start (Thread. (bound-fn [] (println a)))))

;;
;; destructuring : use [] to recv []/() destructuring. use {} for {} destructuring
    (def book {:name "SICP" :details {:pages 657 :isbn-10 "0262011530"}})
    (let [{{pages :pages} :details} book] (prn pages))
;; map slicing : :keys [] to select keys :as a new map.
    (let [{:keys [name age] :as new-sliced-map} {:name "n" :gender "f" :age 18}] )

;; word count
    (reduce #(assoc %1 %2 (inc (get %1 %2 0))) {} “aabcdcdcd”)
    (apply merge-with + (map (fn [c] {c 1}) “abcdaabccc”))

;;
;; -> macro Vs. doto
;; -> thread values down into each form as pipe
;; doto : calling a sequence of forms on the very first input arg.
;;
(-> (Math/sqrt 25) int list)
(doto (new java.util.HashMap) (.put "a" 1) (.put "b" 2))
(-> (new java.util.HashMap) (.put "a" 1) (.put "b" 2))


;;
;; create function on the fly with (comp f g h)
;; ((partial + 5) 10 20) isnt curry (#(apply + 5 %& ) 10 20)
;;
(def plays [{:band "Burial",     :plays 979,  :loved 9}
            {:band "Eno",        :plays 2333, :loved 15}
            {:band "Bill Evans", :plays 979,  :loved 9}
            {:band "Magma",      :plays 2665, :loved 31}])

(sort-by :band plays)
(def sort-by-loved-ratio (partial sort-by #(/ (:plays %) (:loved %))))

(defn columns [column-names]
  (fn [row]
    (vec (map row column-names))))
(sort-by (columns [:plays :loved :band]) plays)
