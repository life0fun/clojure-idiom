(ns chapter08.expense-finders-spec
  (:use chapter08.expense-finders
        clojure.test
        chapter08.mock-stub))

(def all-expenses [(struct-map expense :amount 10.0 :date "2010-02-28")
                   (struct-map expense :amount 20.0 :date "2010-02-25")
                   (struct-map expense :amount 30.0 :date "2010-02-21")])

(deftest test-fetch-expenses-greater-than
  (stubbing [fetch-all-expenses all-expenses]
    (let [filtered (fetch-expenses-greater-than "" "" "" 15.0)]
      (is (= (count filtered) 2))
      (is (= (:amount (first filtered)) 20.0))
      (is (= (:amount (last filtered)) 30.0)))))

(comment - see the version using mocking below
         (deftest test-filter-greater-than
           (let [filtered (expenses-greater-than all-expenses 15.0)]
             (is (= (count filtered) 2))
             (is (= (:amount (first filtered)) 20.0))
             (is (= (:amount (last filtered)) 30.0)))))

(comment - see the version with verify below 
         (deftest test-filter-greater-than
           (mocking [log-call]
             (let [filtered (expenses-greater-than all-expenses 15.0)]
               (is (= (count filtered) 2))
               (is (= (:amount (first filtered)) 20.0))
               (is (= (:amount (last filtered)) 30.0))))))

(deftest test-filter-greater-than
  (mocking [log-call]
    (let [filtered (expenses-greater-than all-expenses 15.0)]
      (is (= (count filtered) 2))
      (is (= (:amount (first filtered)) 20.0))
      (is (= (:amount (last filtered)) 30.0))))
  (verify-call-times-for log-call 1)
  (verify-first-call-args-for log-call "expenses-greater-than" 15.0)
  (verify-nth-call-args-for 1 log-call "expenses-greater-than" 15.0))