(defmacro sync-set [r v]
  (list 'dosync 
        (list 'ref-set r v)))


(defn exhibits-oddity? [x]
        (if (odd? x)
          (println "Very odd!")))

(defmacro unless [test then]
  (list 'if
        (list 'not test)
        then))

(defn unless [test then]
  (if (not test)
    then))

(defn unless [test then-thunk]
  (if (not test)
    (then-thunk)))

(defn exhibits-oddity? [x]
  (unless (even? x)
          #(println "Rather odd!")))

(defmacro unless [test then]
  `(if (not ~test)
     ~then))

(defn exhibits-oddity? [x]
  (unless (even? x)
          (println "Very odd, indeed!")))

(defn exhibits-oddity? [x]
  (unless (even? x)
          (println "Odd!")
          (println "Very odd!")))

(defn exhibits-oddity? [x]
  (unless (even? x)
          (do
            (println "Odd!")
            (println "Very odd!"))))

(defmacro unless [test & exprs]
  `(if (not ~test)
     (do ~exprs)))

(defmacro unless [test & exprs]
  `(if (not ~test)
     (do ~@exprs)))

(defmacro randomly-2 [& exprs]
  (nth exprs (rand-int (count exprs))))

(defmacro randomly [& exprs]
  `(let [index# (rand-int (count '~exprs))
         expr#      (nth '~exprs index#)]
     (eval expr#)))


(defmacro randomly [& exprs]
  (let [len (count exprs)
        index (rand-int len)
        conditions (map #(list '= index %) (range len))]
     `(cond ~@(interleave conditions exprs))))

(defn check-credentials [username password]
  true)

(defn login-user [request]
  (let [username (:username request)
        password (:password request)]
    (if (check-credentials username password)
      (str "Welcome back, " username ", " password " is correct!")
      (str "Login failed!"))))

(defmacro defwebmethod [name args & exprs]
  `(defn ~name [{:keys ~args}]
     ~@exprs))

(defwebmethod login-user [username password]
  (if (check-credentials username password)
    (str "Welcome, " username ", " password " is still correct!")
    (str "Login failed!")))

(defmacro assert-equals [test-expr expected]
  `(let [lhsv# ~test-expr
         rhsv# ~expected]
     (if-not (= lhsv# rhsv#)
       (throw (RuntimeException. (str '~test-expr " did not evaluate to " rhsv#)))
       true)))

(defmacro assert-true [test-expr]
  (let [[operator lhs rhs] test-expr]
    `(let [lhsv# ~lhs
           rhsv# ~rhs
           ret# ~test-expr]
       (if-not ret#
         (throw (RuntimeException. (str '~lhs " is not " '~operator " " rhsv#)))
         true))))

(defmacro assert-true [test-expr]
  (if-not (= 3 (count test-expr))
    (throw (RuntimeException. "Argument must be of the form (operator test-expr expected-expr)")))
  (if-not (some #{(first test-expr)} '(< > <= >= = not=))
    (throw (RuntimeException. "operator must be one of < > <= >= = not=")))
  (let [[operator lhs rhs] test-expr]
    `(let [lhsv# ~lhs
           rhsv# ~rhs
           ret# ~test-expr]
       (if-not ret#
         (throw (RuntimeException. (str '~lhs " is not " '~operator " " rhsv#)))
         true))))

(defmacro infix [expr]
  (let [[left op right] expr]
    (list op left right)))

(defmacro <- [expr]
  (let [[left op right] expr]
    (list op left right)))

(defmacro print-infixes [& exprs]
  (let [fix (fn [[left op right]]
              (list 'println (list op left right)))
        prefixed (map fix exprs)]
    `(do  ~@prefixed)))


(defmacro def-logged-fn [fn-name args & body]
  `(defn ~fn-name ~args
     (let [now (System/currentTimeMillis)]
       (println "[" now "] Call to" (str (var ~fn-name)))
       ~@body)))

(defmacro def-logged-fn [fn-name args & body]
  `(defn ~fn-name ~args
     (let [now# (System/currentTimeMillis)]
       (println "[" now# "] Call to" (str (var ~fn-name)))
       ~@body)))


(declare daily-report)

(let [start "2009-10-22"]
  (shadow-time (daily-report start)))

(defmacro extend-fn [name args & body]
  `(let [old-fn# (var-get (var ~name))
         new-fn# (fn [& p#] 
                   (let [~args p#] 
                     (do ~@body)))
         wrapper# (fn [& params#]
                    (if (= ~(count args) (count params#))
                      (apply new-fn# params#)
                      (apply old-fn# params#)))] 
     (alter-var-root (var ~name) (constantly wrapper#))))