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
(defn new-object [klass]
  (let [state (ref {})]
    (fn [command & args]
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
          (apply method args))))))

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
          (str "Hello there, " visitor)))

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


