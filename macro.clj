;; clarify syntax quote unquote
;; (load-file "syntaxquote.clj")

(ns clojure-test
  ;; refer only in 1.4 (:require clojure.string :refer [ join ] :as string))
  (:require clojure.string))


; fn return evaluated data(value), or object, (fn objects).
; macro returns clojure data structure, to be evaluated by compiler.
; clj data structure: (map fn coll), (f args), (list map fn coll), the same as (eval (read-string "(* 3 4)))
; syntax quote fn, takes a form, ret a list template data structure (list f args) to be evaled by compiler.
; you do not need syntax quote if you can verbosely keep (list '+ 2 4).
; syntax quote gives you a list form template, `(+ 2 3), so you can avoid (list ...) for each form in the chain.
; because syntax quoto will quote symbols in current ns, `(x) => (user/x), inside macro, you want your param not 
; be quoted, but eval-ed in-place. so you unquote ~x your param, effectly eval your symbol x in-place. 

; syntax quote/unquote are template to create list form to return from macro.
; the list form reted from template is evaluated when reted.
; the form returned from macroexpand is the one to be evaluated. 

; '(2 3) is quote, so we can write list with (3 4), () viewed as list, not fn.
; `(2 3) is syntax quoting. fully qualify all eles in the list. 
;  `(a :b 3 "x") => (user/a :b 3 "x")  primitive keep the same. var expand to fully qualified name.
; ~x syntax unqote, so do not expand var, must use within the scope of syntax quote.
(def x (* 3 4))
'x -> (quote x)
`(x) -> (user/x)  ; quote in current ns.
`(~x) -> ((* 3 5))
`(~@x) -> (* 3 5)

`(1 2 (list 3 4))    =>  (1 2 (clojure.core/list 3 4))
`(1 2 ~(list 3 4))   =>  (1 2 (3 4))
`(1 2 ~@(list 3 4))  =>  (1 2 3 4)

;;
;; syntax quote unquote demythfied.
;;  http://www.learningclojure.com/2010/11/syntax-quote-kata-for-confused.html
;;
(def x '(* 3 5))
(def y (* 3 5))
x
y
(list 'println x (eval x) y)  ==  (println (* 3 5) 15 15) ; ' <- raw form
(list `println x (eval x) y)  ==  (clojure.core/println (* 3 5) 15 15)
`(list println x (eval x) y)  ==  (clojure.core/list clojure.core/println user/x (clojure.core/eval user/x) user/y)
`(println x (eval x) y)       ==  (clojure.core/println user/x (clojure.core/eval user/x) user/y)
`(println ~x (eval x) y)      ==  (clojure.core/println (* 3 5) (clojure.core/eval user/x) user/y)
`(println ~x ~(eval x) y)     ==  (clojure.core/println (* 3 5) 15 user/y)
`(println ~x ~(eval x) ~y)    ==  (clojure.core/println (* 3 5) 15 15)
`(println ~x ~(eval x) ~y ~@x) == (clojure.core/println (* 3 5) 15 15 * 3 5)


;;
;; defmacro diffs from defn as it wont eval the argument.
;; so you use arg is unevaluated form.
;; to eval form, unquote it inside syntax quote.

;; quote unquoting `( '~s  ==> ~s ) (quote (* 2 3)) ==> 6 

(defmacro dbg-1 [s]  (let [x# s z :z]   `(prn ~z '~s ~s ~x#)))      ; if x# not in quote, unquote it inside quote. 
(defmacro dbg-1 [s] `(let [x# ~s ~'z :z] (prn ~'z '~s ~s x#))) ; if x# inside quote, unquote args when assign to it.

(clojure.core/println :z (quote (* 2 3)) (* 2 3) (* 2 3))
(dbg-1 (* 2 3)  ; ==> :z (* 2 3) 6 6

(let* [x__9440__auto__ (* 2 3) z :z] (clojure.core/prn z (quote (* 2 3)) (* 2 3) x__9440__auto__))


;; splicing unquote `(max ~@(shuffle (range 10) )) unroll/flatten a colletion into multiple exprs.
;; except that it allows multiple forms to be inserted in the place of a single unquote-splicing form:
;; like apply, splicing "unroll" a collection into multiple expressions.


; quote unquote, `( '~x)  vs `( ~'x)
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
;; clojure static scoping and dynamic scoping.
;; with let shadowing.
;;

(def xx 1)   ;; root binding, global to all threads.

(defn dummy-fn2 []
  (println "xx from dummy-fn2:" xx)
  (+ xx 1))  ;; return x+1, did not re-assign to x

(defn dummy-fn []
  (println "entering function:" xx)   ;; 1 static scoping 
  (println "var xx:" @#'xx)   ;; = 1  @#'x ref to root binding.
  (dummy-fn2)               ;; = 1 static scoping
  (println "---")
  (let [xx 100]          ;; let closure shadows x with 100, final constant;
    (println "after let:" xx);; = 100
    (println "var xx:" @#'xx) ;; = 1
    (dummy-fn2)             ;; let is local static closure
    (println "---")
    (let [xx (dummy-fn2)]    ;; nested let closure x=2
      (println "after let and dummy-fn2:" xx)  ;; = 2
      (println "var xx:" @#'xx)  ;; = 1
      (dummy-fn2)              ;; = 1
      (println "---")
      (binding [xx 888]         ;; dynamic change root binding. no affect local closure vars.
        (println "var xx:" @#'xx) ;; = 888 now global x in this thread changed by binding.
        (println "after binding:" xx)  ;; = 2 however, can not change let closure
        (dummy-fn2)       ;; = 888
        (println "---")
        (let [xx (dummy-fn2)]    ;; x=888 another local nest closure
          (println "after binding and dummy2:" xx)  ;; = 888
          (println "var xx:" @#'xx)  ;; = 888
          (dummy-fn2)  ;; = 888
          (println "---"))))))

(dummy-fn)

;; macro used for resource management.
;; (with-open [page stream]  (body) will close stream in try/catch/finally clause.
;; generic resource manage that delegate resource release fn to close-fn that passed in as an argument.
(defmacro with-resource [binding close-fn & body]   ;; take resource, resource-release-fn, and exec body.
  `(let ~binding
    (try
      (do ~@body)
      (finally
        (~close-fn ~(binding 0))))))

(let [stream (joc-www)]
  (with-resource [page stream]
    #(.close %)
    (.readLine page)))

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

;; map reduce : reduce to a Atom tot on top of init. Intermediate result is in tot.
    (apply merge-with + (map (fn [c] {c 1}) "abcdabccc"))
    (map (fn [m] (hash-map (-> m str keyword) 1)) "abcdabc")
    (reduce (fn [t c] (assoc t (-> c str keyword) (inc (get t (-> c str keyword) 0)))) {} "abcdabc")
    (reduce #(assoc %1 %2 (inc (get %1 %2 0))) {} "abcdcdcd")

;;
;; -> threading marco Vs. doto
;; -> symbol : thread symbol down into each form to eval as pipe
;; doto obj : calling a sequence of forms on the object
;;
(-> (Math/sqrt 25) int list)
;; create a java obj and call a seq of funcs
(doto (new java.util.HashMap) (.put "a" 1) (.put "b" 2))
(-> (new java.util.HashMap) (.put "a" 1) (.put "b" 2))

(map #(doto % keyword prn) ['a 'b])   ;; wrong (a b)
(map #(-> % keyword prn) ['a 'b])     ;; [:a :b]


;;
;; with macro to manage resource in try/catch/finally block.
;;
(defmacro with-rabbit [[mq-host mq-username mq-password] & exprs]
  `(with-open [connection# (new-connection ~mq-host ~mq-username ~mq-password)]
     (binding [*rabbit-connection* connection#]
       (do ~@exprs))))

(defn send-message [routing-key message-object]
  (with-open [channel (.createChannel *rabbit-connection*)]
      (.basicPublish channel "" routing-key nil
                     (.getBytes (str message-object)))))

(with-rabbit ["localhost" "guest" "guest"]
  (send-message "chapter14-test" "chapter 14 test method"))

;
; syntax quote/unquote are form template. 
; args to defmacro are not evaluated. does not matter whether inside template or not.
; macro expand with fully qualified name to avoid name capture.
; use auto-gensym to create local var inside let form.
; to really capture this name, use ~'this pattern.
; the data structure reted from macro expand will be evaluated.
; so wrap with do if you need to evaluate the reted list.

(defmacro unless [test & then]
  `(if (not ~test) 
    (do ~@then)))

(defmacro log-fn [fn & arg] 
  `(let [now# (System/currentTimeMillis)] 
    (do (prn now#) 
      (~fn ~@arg))))

(defmacro log-fn [fn & arg]
  `(let [now# (System/currentTimeMillis)]
     (do (prn now#) '(apply ~fn ~arg) (prn "time-is " (- (System/currentTimeMillis) now#)))))

(defn timeit [f & args]
  (let [st (System/currentTimeMillis)]
    (try
      (apply f args)
      (finally
        (println "Run time: " (- (System/currentTimeMillis) start) "ms")))))


; ret a list of (def foo), and wrap by do form to execute.
(defmacro my-declare [& symbols]
  `(do
     ~@(map #(list 'def %) symbols)))

; macro expand to ret a form to be evaluated.
(defmacro my-and 
  ([] true)
  ([x] x)
  ([x & next]
    `(let [and# ~x]
       (if and# (my-and ~@next) and#))))

; foreach macro
; (foreach [x (range 10)] (prn x))
(defmacro foreach [[symb coll] & body]
  `(loop [coll# ~coll]
    (when-let [[~symb & xs#] (seq coll#)]
      ~@body
      (recur xs#))))

; (as-> {:a 1} ch (assoc ch :b 2) (update-in ch [:c] 3))
; interleave put x as the binding of next form. 
; (macroexpand '(as-> {:a 1} x (prn x) (assoc x :b 2)))
;   (let* [x {:a 1} x (prn x) x (assoc x :b 2)] x)
(defmacro as->
  "Binds name to expr, evaluates the first form in the lexical context
  of that binding, then binds name to that result, repeating for each
  successive form, returning the result of the last form."
  {:added "1.5"}
  [expr name & forms]
  `(let [~name ~expr
         ~@(interleave (repeat name) forms)]
     ~name))
