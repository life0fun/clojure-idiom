(def user-1 {:login "rob" :referrer :mint.com :salary 100000 :rating ::gold})
(def user-2 {:login "kyle" :referrer :google.com :salary 90000 :rating ::platinum})
(def user-3 {:login "celeste" :referrer :yahoo.com :salary 70000 :rating ::bronze})

(defn fee-amount [percentage user]
  (float (* 0.01 percentage (:salary user))))

(defn affiliate-fee-cond [user]
  (cond 
    (= :google.com (:referrer user)) (fee-amount 0.01 user)
    (= :mint.com (:referrer user)) (fee-amount 0.03 user)
    :default (fee-amount 0.02 user)))
    
(defmulti affiliate-fee :referrer :default :else)
(defmethod affiliate-fee :mint.com [user]
  (fee-amount 0.03 user))
(defmethod affiliate-fee :google.com [user]
  (fee-amount 0.01 user))
(defmethod affiliate-fee :else [user]
  (fee-amount 0.02 user))

(defn profit-rating [user]
 (let [ratings [::bronze ::silver ::gold ::platinum]]
   (nth ratings (rand-int (count ratings)))))

(defn fee-category [user]
;  [(:referrer user) (profit-rating user)])
  [(:referrer user) (:rating user)])

(defmulti profit-based-affiliate-fee fee-category)
(defmethod profit-based-affiliate-fee [:mint.com ::bronze] [user]
  (fee-amount 0.03 user))
(defmethod profit-based-affiliate-fee [:mint.com ::silver] [user]
  (fee-amount 0.04 user))
(defmethod profit-based-affiliate-fee [:mint.com ::gold] [user]
  (fee-amount 0.05 user))
(defmethod profit-based-affiliate-fee [:mint.com ::platinum] [user]
  (fee-amount 0.05 user))
(defmethod profit-based-affiliate-fee [:google.com ::gold] [user]
  (fee-amount 0.03 user))
(defmethod profit-based-affiliate-fee [:google.com ::platinum] [user]
  (fee-amount 0.03 user))
(defmethod profit-based-affiliate-fee :default [user]
  (fee-amount 0.02 user))

(derive ::bronze ::basic)
(derive ::silver ::basic)
(derive ::gold ::premier)
(derive ::platinum ::premier)

(defmulti affiliate-fee-for-hierarchy fee-category)
(defmethod affiliate-fee-for-hierarchy [:mint.com ::bronze] [user]
  (fee-amount 0.03 user))
(defmethod affiliate-fee-for-hierarchy [:mint.com ::silver] [user]
  (fee-amount 0.04 user))
(defmethod affiliate-fee-for-hierarchy [:mint.com ::premier] [user]
  (fee-amount 0.05 user))
(defmethod affiliate-fee-for-hierarchy [:google.com ::premier] [user]
  (fee-amount 0.03 user))
(defmethod affiliate-fee-for-hierarchy :default [user]
  (fee-amount 0.02 user))

;;;; visitor revisited

(def assignment-node {:type :assignment :expression "assignment"})
(def variableref-node {:type :variable-ref :expression "variableref"})

(defmulti checkValidity :type)
(defmethod checkValidity :assignment [node]
  (println "checking :assignment, expression is" (:expression node)))
(defmethod checkValidity :variable-ref [node]
  (println "checking :variable-ref, expression is" (:expression node)))

(defmulti generateASM :type)
(defmethod generateASM :assignment [node]
  (println "generating ASM for :assignment, expression is" (:expression node)))
(defmethod generateASM :variable-ref [node]
  (println "generating ASM for :variable-ref, expression is" (:expression node)))

