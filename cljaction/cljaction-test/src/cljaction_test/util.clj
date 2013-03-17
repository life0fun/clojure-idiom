;; util module.
;; require can now take a :refer option. :refer takes a list of symbols to refer 
;; from the namespace or :all to bring in all public vars."
;;

(ns cljaction-test.util
;;  (:use chapter14_worker)
  (:gen-class))

(defn debug []
  (prn "util debug print hello world"))
