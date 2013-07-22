(ns trident-clj.prn-filter
  (:import [java.io FileReader]
           [java.util Map Map$Entry List ArrayList Collection Iterator HashMap])
  (:import [storm.trident.operation TridentCollector Function]
           [backtype.storm.tuple Values])
  (:require [clojure.string :as str]
            [clojure.tools.logging :as log])
  (:gen-class
    :name com.colorcloud.trident.PrintFilter  ; namespace to 
    :implements [storm.trident.operation.Filter]))  ; implement filter interface


(defn -prepare      ; gen-class method prefix by -
  " called once, better for init global var and db conn "
  [this conf ^storm.trident.operation.TridentOperationContext context]
  (prn " --- PrintFilter prepare ---"))

(defn -cleanup
  "called once to release resource upon tear down"
  []
  (prn " --- PrintFilter cleanup ----"))

(defn -isKeep  ; 
  "process each tuple, predicate to decide whether to keep"
  [this ^storm.trident.tuple.TridentTuple tuple]
  (prn " <tuple> " tuple)
  true)
    