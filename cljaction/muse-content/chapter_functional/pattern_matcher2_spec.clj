(ns chapter-functional.pattern-matcher2-spec
  (:use chapter-functional.pattern-matcher2)
  (:use clojure.test))

(deftest test-matches?
  (let [[m bindings] (matches? '?x 'a)]
    (is m)
    (is (= 'a (bindings '?x))))
  (let [[m bindings] (matches? '(a ?x) '(a b))] 
    (is m)
    (is (= 'b (bindings '?x))))
  (let [[m bindings] (matches? '(?x b ?y) '(a b a))]
    (is m)
    (is (= 'a (bindings '?x)))
    (is (= 'a (bindings '?y))))
  (let [[m bindings] (matches? '(?x b ?x) '(a b a))] 
    (is m)
    (is (= 'a (bindings '?x))))
  (let [[m bindings] (matches? '(?x (a b) ?x) '(a (a b) a))] 
    (is m)
    (is (= 'a (bindings '?x))))
  (let [[m bindings] (matches? '(?x (a b) ?x) '(a (a b) a))] 
    (is m)
    (is (= 'a (bindings '?x))))
  (let [[m bindings] (matches? '(?x (e ?x)) '(a (e a)))] 
    (is m)
    (is (= 'a (bindings '?x))))
  (let [[m bindings] (matches? '(?x (e ?x)) '(a (e a)))] 
    (is m)
    (is (= 'a (bindings '?x))))
  (let [[m bindings] (matches? '(?x (e ?x (f ?x))) '(a (e a (f a))))] 
    (is m)
    (is (= 'a (bindings '?x))))
  (let [[m bindings] (matches? '(?x (e ?x (f ?x))) '(a (e b (f a))))] 
    (is (not m))
    (is (= 'a (bindings '?x))))
  (let [[m bindings] (matches? '(?x (e ?y (f ?y))) '(a (e g (f g))))] 
    (is m)
    (is (= 'a (bindings '?x)))
    (is (= 'g (bindings '?y))))
  (let [[m bindings] (matches? '(?x (e ?y (f ?y))) '(a (e b (f g))))] 
    (is (not m))
    (is (= 'a (bindings '?x)))
    (is (= 'g (bindings '?y)))))

(def rules-db '((parent amit adi)
                (parent deepthi adi)
                (parent ds amit)
                (parent kiran amit)
                (parent bharth deepthi)))

(deftest test-simple-query-processor
  (let [[m bindings] (simple-query-processor '(parent ?x adi) rules-db {})]
    (is m)
    (is ( = '(amit deepthi) (bindings '?x))))
  (let [[m bindings] (simple-query-processor '(parent ?x aditya) rules-db {})]
    (is (not m)))
) 