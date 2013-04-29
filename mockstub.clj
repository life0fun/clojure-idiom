(ns mockstub.core
  (:use clojure.test))


; to upper fn 
(defn to-upper [s]
  (.toUpperCase (str s)))

; test to upper fn with a set of is assertion
(deftest test-to-upcase
  (is (= "RATHORE" (to-upper "rathore")))
  (is (= "1" (to-upper 1)))
  (is (= "AMIT" (to-upper "AMIT"))))

; or use are assertion
(deftest test-to-upcase
  (are [l u] (= u (to-upper l))
    "RATHORE" "RATHORE"
    "1" "1"
    "amit" "AMIT"))

; a stub ret a canned val predefined.
; a mock records the fact that it was called with a specific set of args so we can verify api is called properly later from mock log.

; dynamic binding for test driven
; fns are dynamic vars, you can bind it with stub or mock values, elegant.
(defn cal [x y] (prn "real cal " x y) [x y])
(cal x)
(binding [cal (constantly ["mock-val1" "mock-val2"])] (cal "berkeley" "ucla"))

; stub take a list of pairs of [fn-name mock-form] and dynamic binding mock to fn-name.
(defmacro stubbing [stub-forms & body]
  (let [stub-pairs (partition 2 stub-forms)
        returns (map last stub-pairs)
        stub-fns (map #(list 'constantly %) returns)
        real-fns (map first stub-pairs)]
    `(binding [~@(interleave real-fns stub-fns)]
       ~@body)))

(defn calc-x [x1 x2]
  (* x1 x2))
(defn calc-y [y1 y2]
   (/ y2 y1))
(defn some-client []
  (println (calc-x 2 3) (calc-y 3 4)))

(stubbing [calc-x 1 calc-y 2]
  (some-client))

; common stub-fn ret passed in val no matter what args
(defn stub-fn [return-value]
  (fn [& args]         
    return-value))

; stub out the fn with fixed rets, i.e. 200 OK.        
(defmacro stubbing [stub-forms & body]
  (let [stub-pairs (partition 2 stub-forms)
        returns (map last stub-pairs)
        stub-fns (map #(list 'stub-fn %) returns)  ; use stub fn               
        real-fns (map first stub-pairs)]
    `(binding [~@(interleave real-fns stub-fns)]
               ~@body)))

; a mock records when and how API called(time, count, args) so we can verify calls later.
(def mock-calls (atom {}))
 
(defn stub-fn [the-function return-value]
  (swap! mock-calls assoc the-function [])
  (fn [& args]
    (swap! mock-calls update-in [the-function] conj args)
    return-value))
 
; mock-fn does not provide a ret val, while stub ret a canned ret val
(defn mock-fn [the-function]
  (stub-fn the-function nil))   ; ret nil when mocked out, or ret 200 OK.
 
; mock out a list of fn-names with mock-fn 
(defmacro mocking [fn-names & body]
  (let [mocks (map #(list 'mock-fn (keyword %)) fn-names)]
    `(binding [~@(interleave fn-names mocks)]
       ~@body)))
  
(defmacro stubbing [stub-forms & body]
  (let [stub-pairs (partition 2 stub-forms)
        real-fns (map first stub-pairs)
        returns (map last stub-pairs)
        stub-fns (map #(list 'stub-fn (keyword %1) %2) real-fns returns)]
    `(binding [~@(interleave real-fns stub-fns)]
       ~@body)))

(defmacro verify-call-times-for [fn-name number]
  `(is (= ~number (count (@mock-calls ~(keyword fn-name))))))
 
;(defmacro verify-first-call-args-for [fn-name & args]
;  `(is (= '~args (first (@mock-calls ~(keyword fn-name))))));

(defmacro verify-first-call-args-for [fn-name & args]
  `(verify-nth-call-args-for 1 ~fn-name ~@args)) 

(defmacro verify-nth-call-args-for [n fn-name & args]
  `(is (= '~args (nth (@mock-calls ~(keyword fn-name)) (dec ~n)))))
 
(defn clear-calls []
  (reset! mock-calls {}))


; mock test : give test name and test body
(defmacro defmocktest [test-name & body]
  `(deftest ~test-name 
     (binding [mock-calls (atom {})]
       (do ~@body))))

; test body first stubbing out dependent fns, and focus on just fn of this module.
(defmocktest test-fetch-expenses-greater-than
  (stubbing [fetch-all-expenses all-expenses]
    (let [filtered (fetch-expenses-greater-than "" "" "" 15.0)]
      (is (= (count filtered) 2))
      (is (= (:amount (first filtered)) 20.0))
      (is (= (:amount (last filtered)) 30.0)))))

; mock dependent API out and verify call logs.
; use the testing macro to group these according to those goals:
(defmocktest test-filter-greater-than
  (mocking [log-call]
    (let [filtered (expenses-greater-than all-expenses 15.0)]
      (testing "the filtering itself works as expected"   ; use testing macro to group tests
          (is (= (count filtered) 2))
          (is (= (:amount (first filtered)) 20.0))
          (is (= (:amount (last filtered)) 30.0))))
      (testing "Auditing via log-call works correctly"  ; use testing macro to group tests
        (verify-call-times-for log-call 2)
        (verify-first-call-args-for log-call "expenses-greater-than" 15.0))))


; For testing private functions, you need to use the following macro (courtesy of chouser),
(defmacro with-private-fns [[ns fns] & tests]
  "Refers private fns from ns and runs tests in context."
  `(let ~(reduce #(conj %1 %2 `(ns-resolve '~ns '~%2)) [] fns)
     ~@tests))


; use fixture to set-up test
(defn my-fixture [f]
  ;;Perform setup, establish bindings, whatever.
  (f)  ;;Then call the function we were passed.
  ;;Tear-down / clean-up code here.
 )

