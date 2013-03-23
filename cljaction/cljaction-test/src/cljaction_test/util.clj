;; util module.
;; require can now take a :refer option. :refer takes a list of symbols to refer 
;; from the namespace or :all to bring in all public vars."
;;

(ns cljaction-test.util
  (:use [cljaction-test.chapter14-worker])  ;; use chapter14_worker
  )

(defn debug []
  (prn "util debug print hello world"))
