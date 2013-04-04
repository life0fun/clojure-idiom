;; the main to entry to use 
;;
;; require or use other namespace's code as though its yours.
;; require can now take a :refer option. :refer takes a list of symbols to refer 
;; from the namespace or :all to bring in all public vars."
;;
;; gen-class gen a java class that delegate java class .method to clj :prefix func. (.main to -main)
;; clojure in action java interop has great depth in gen-class options.
;;
;; for mutables, only single field available called state.
;; : with or without :gen-class, clj with gen a set of classes for each clj function.
;; args: :name, :init, :constructor :state,
;; http://kotka.de/blog/2010/02/gen-class_how_it_works_and_how_to_use_it.html 
;;
(ns cljaction-test.core
  (:require [clojure.string :as str])
  (:require [clojure.java.jdbc :as sql])
  (:import [java.io FileReader]
           [java.util Map Map$Entry List ArrayList Collection Iterator HashMap]
           )
  (:use [cljaction-test.util])   ;; use util namespace without fully qualified name.
  (:use [cljaction-test.chapter14-worker])  ;; use chapter14_worker
  ;;(:use [cljaction-test.chapter14-worker-usage])
  (:use [cljaction-test.chapter14-sender])
  (:use [cljaction-test.chapter14-receiver])
  (:gen-class :main true))


;; main entry, refered from prj.clj and lein run will execute.
(defn -main []
  (prn "hello world")
  (debug)
  (receiver-recv))
  ;;(sender-single-msg))
