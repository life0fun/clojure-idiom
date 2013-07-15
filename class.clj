;; A class object system for clojure
;; (load-file "class.clj")

;; (:use mixed in all fns, symbols, and mappings from other _NameSpace_.)
;; (:import [pkg1] [pkg2 fn1 ...]) import Java packages
;; (:require [namespace_1 :ref local_namespace_])
;; when defining ns, include only the references that are used.
;; :exclude, :only, :as, :refer-clojure, :import, :use, :load, and :require.
;; ;use naked could corrupt the namespace.  (:use :only)
;; :import working with java deftype defrecord

(ns my-class
  (:refer-clojure :exclude [defstruct])
  (:use [clojure.test :only (are is)])
  (:require (clojure [zip :as z]))
  (:import (java.util.Collection)))


; method spec destruct spec expression into name and body
(defn method-spec [sexpr]
  (let [name (keyword (second sexpr))
    body (next sexpr)]
    [name (conj body 'fn)]))

(method-spec '(method age [] (* 2 10)))

; a list of method specs
(defn method-specs [sexprs]
  (->> sexprs
       (filter #(= 'method (first %)))
       (mapcat method-spec)
       (apply hash-map)))

(method-specs '((method age []
                  (* 2 10))
                (method greet [visitor]
                  (str "Hello there, " visitor))))

; a new object is just a closure with instance state
(declare this)    ; give a dynamic var so that fn can be executed under the bindings of this.

(defn new-object [klass]
  (let [state (ref {})]
    (fn thiz [command & args]  ; give closure an explicit name(thiz), so we can bind it to this.
      (condp = command
        :class klass
        :class-name (klass :name)
        :set! (let [[k v] args]
                (dosync (alter state assoc k v))
                nil)
        :get (let [[key] args]
               (key @state))

        (let [method (klass :method command)]
          (if-not method
            (throw (RuntimeException.
              (str "Unable to respond to " command))))
          (binding [this thiz]
            (apply method args)))))))

; a new class is a closure on methods
(defn find-method [method-name instance-methods]
  (instance-methods method-name))

(defn new-class [class-name methods]
  (fn klass [command & args]
    (condp = command
      :name (name class-name)
      :new (new-object klass)
      :method (let [[method-name] args]
                (find-method method-name methods)))))

; use def inside macro to force evaluation of fns fn map {:fname (fn [] (* 2 4))}
;
(defmacro defclass [class-name & specs]
  (let [fns (or (method-specs specs) {})]
    `(def ~class-name (new-class '~class-name ~fns))))

(defclass Person
        (method age []
          (* 2 10))
        (method greet [visitor]
          (str "Hello there, " visitor))
        (method about [diff]                ; invoke :age method in the same closure by binding to this.
          (str "I was born about " (+ diff (this :age)) " years ago")))

;
; after parsing method specs, we got fns
;  {:age (fn age [] (* 2 10)), :greet (fn greet [visitor] (str "Hello there, " visitor))}
; to force evaluate fns, put it through def
;   (eval fns) -> produce {:age #<user$age user$age@681e731c>
;   (def efns {:age (fn age [] (* 2 8))})) -> {:age #<user$age user$age@6165e7a5>}
;   (eval `(def efns ~fns))
;
; (apply (efns :age) [])
;
(Person :method :age)

(def shelly (Person :new))
(shelly :age)
(shelly :greet "Nancy")

; macro is ask compiler to generate code for you.
; quote unquote only affect variables, no effect on other keywords, including ' quote.
; quote unquoting `( '~x ), ' symbol is quote, return quoted substituded x value
;
; `(quote) to skip evaluation. quote entire expr inside () to avoid quote each item individually.
; ~(unquote) to substitude the value. similar to string intrapolate. 
; if var is a form, (* 3 4), unquote it will cause it being evaluated.
; ~() unquote entire () to avoid unquote each one. ~(eval ~x)
; 
; `( '~var-name) intrapolate form, but do not eval.
; ~@var-name (unquote-splicing):  remove the list ().
;
(defmacro dbg [fn-name args & body]
  `(defn ~fn-name ~args
    (println "dbg ...")
    ~@body))

; when passing '(* 2 3) to macro, it will not be evalued b/c macro syntax quote.
; however, put it inside let bindings inside macro will force it evaluate.
(defn gen-map [nm spec] {(keyword nm) spec})
(defmacro fnmacro [name] (let [m (gen-map name '(fn [n] (* 2 n)))] `(prn ~m) `~m))
(fnmacro age)

; arg is task name and spec, not in list data struture, pass to macro and return a 
; task map where key is fn name and val is fn closure
(defmacro fnmacro [name spec] (let [m (gen-map name spec)] `(prn ~m) `~m))
(apply ((fnmacro foo (fn [n] (* 2 n))) :foo) [4])

; if fn body is in quoted list data structure, directly eval the data structure.
(def spec '(age [n] (* 2 n)))
(defn fnw [sexpr]
  (eval (conj spec 'fn)))
(apply (fnw spec) [4])

; if fn body is code, not list data structure, pass to defmacro to wrap it.
(defmacro fn-wrapper [fname args & body]
  `(defn ~fname ~args
      (prn "calling " '~fname '~args '~@body) ; when prn, use substituded val, quote to non-evaluated form.
      ~@body))
(fn-wrapper foo [n] (* 2 n))
(foo 4)

; dbg macro take a form, and prn it and its evaluated result.
; because (eval "x") eval to itself, we can always add eval without side effect, so
; the macro can take both (quote (* 2 3)) as well as (* 2 3)
(defmacro dbg [sexpr]
  (prn sexpr)
  `~sexpr)

(dbg (* 2 4))
(dbg '(* 2 4))

(defmacro dbg-ev [sexpr]
  (prn sexpr)
  `~(eval sexpr))

(dbg-ev (* 2 4))
(dbg '(* 2 4))
(let [f (dbg-ev '(fn [n] (* 2 n)))] (f 3))

; fn composition with defmacro defn
(defmacro fn-wrapper [name arg alg data-list]
  (let [bd (conj data-list alg)]
    `(defn ~name ~arg ~bd)))

; create a fn that map algebra to a list of data
(fn-wrapper double [n] * (2 n))


; macro examples
(defmacro declare [ & names]
  `(do
    ~@(map #(list 'def %) names)))
(macroexpand-1  '(declare add multiply subtract divide))

; and is just another macro
(defmacro my-and 
  ([] true)
  ([x] x)
  ([x & next]
    `(if ~x
      (and ~@next)
      ~x)))

(defmacro my-and 
  ([] true)
  ([x] x)
  ([x & next]
    `(let [and# ~x]
      (if and#
        (and ~@next)
        and#))))

; time (* 1234 12345)
(defmacro time [expr]
  `(let [start# (System/nanotime)
         ret# ~expr]
    (prn
      (str "Elapsed time :"
        (/ (double (- (System/nanotime) start#)) 1000000.0)
        " msecs"))
    ret#))




