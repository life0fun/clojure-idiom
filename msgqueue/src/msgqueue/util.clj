;; util module.
;; require can now take a :refer option. :refer takes a list of symbols to refer 
;; from the namespace or :all to bring in all public vars."
;;

(ns msgqueue.util)

(defn debug []
  (prn " ... msg queue util debug print hello world ..."))
