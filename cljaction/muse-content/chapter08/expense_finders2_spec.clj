(ns chapter08.expense-finders2-spec
  (:use chapter08.expense-finders
        clojure.test
        chapter08.mock-stub2))

(def all-expenses [(struct-map expense :amount 10.0 :date "2010-02-28")
                   (struct-map expense :amount 20.0 :date "2010-02-25")
                   (struct-map expense :amount 30.0 :date "2010-02-21")])

(comment - now look at the one below using are
  (defmocktest test-fetch-expenses-greater-than
    (stubbing [fetch-all-expenses all-expenses]
      (let [filtered (fetch-expenses-greater-than "" "" "" 15.0)]
        (is (= (count filtered) 2))
        (is (= (:amount (first filtered)) 20.0))
        (is (= (:amount (last filtered)) 30.0))))))

(defmocktest test-fetch-expenses-greater-than
  (stubbing [fetch-all-expenses all-expenses]
    (let [filtered (fetch-expenses-greater-than "" "" "" 15.0)]
      (are [x y] [= x y]
        (= (count filtered) 2)
        (= (:amount (first filtered)) 20.0)
        (= (:amount (last filtered)) 30.0)))))

(defmocktest test-filter-greater-than
  (mocking [log-call]
    (let [filtered (expenses-greater-than all-expenses 15.0)]
      (testing "the filtering itself works as expected"
          (is (= (count filtered) 2))
          (is (= (:amount (first filtered)) 20.0))
          (is (= (:amount (last filtered)) 30.0))))
    (testing "Auditing via log-call works correctly"
      (verify-call-times-for log-call 1)
      (verify-first-call-args-for log-call "expenses-greater-than" 15.0))))