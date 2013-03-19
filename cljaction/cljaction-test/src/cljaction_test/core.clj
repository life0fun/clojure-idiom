;; the main to entry to use 
;;
;; require or use other namespace's code as though its yours.
;; require can now take a :refer option. :refer takes a list of symbols to refer 
;; from the namespace or :all to bring in all public vars."
;;
(ns cljaction-test.core
;;  (:use chapter14_worker)
  (:import [java.io FileReader]
           [java.util Map Map$Entry List ArrayList Collection Iterator HashMap]
           )
  (:use [cljaction-test.util])   ;; use util namespace's code
  (:gen-class :main true))

;; main entry
(defn -main []
  (prn "hello world")
  (debug))
