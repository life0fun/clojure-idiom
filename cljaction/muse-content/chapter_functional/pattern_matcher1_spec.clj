(ns chapter-functional.pattern-matcher1-spec
  (:use chapter-functional.pattern-matcher1)
  (:use clojure.test))

(deftest test-match
  (is (match 'a 'a))
  (is (match '? 'a))
  (is (not (match 'a '(?))))
  (is (match '(a ?) '(a b)))
  (is (match '(a ? c) '(a b c)))
  (is (match '(a ? c (d ? f)) '(a b c (d e f))))
  (is (match '(a ? c (d ? f (g ? i))) '(a b c (d e f (g h i)))))
  (is (match '(? ? ?) '(a b c)))
  
  (is (not (match 'a 'b)))
  (is (not (match '(a b) '(a c))))
  (is (not (match '? '(a ?))))
  (is (not (match '(a ?) '(a))))
)