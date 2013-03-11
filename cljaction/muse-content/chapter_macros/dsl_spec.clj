(ns chapter-macros.dsl-spec
  (:use clojure.test
        chapter-macros.engine
        chapter-macros.dsl-store
        chapter-macros.session))

(def dsl-code (str
  '(defsegment googling-clojurians
     (and
      (> (count $search-terms) 0)
      (matches? $url-referrer "google")))
  
  '(defsegment loyal-safari
     (and
      (empty? $url-referrer)
      (= :safari $user-agent)))

    '(defsegment googling-clojurians-chrome-1
     (and
      (googling-clojurians)
      (= :chrome $user-agent)))))

(def abc-session {:consumer-id "abc"
                  :url-referrer "http://www.google.com/search?q=clojure+programmers"
                  :search-terms ["clojure" "programmers"]
                  :ip-address "192.168.0.10"
                  :tz-offset 480
                  :user-agent :safari})

(defn setup-test []
  (redis/flushdb)
  (load-code dsl-code)
  (save-session abc-session))

(deftest test-googling-clojurians
  (setup-test)
  (in-session "abc"
    (is ((segment-named :googling-clojurians)))
    (is (= :googling-clojurians (first (classify))))))

(deftest test-not-googling-clojurians
  (setup-test)
  (save-session (dissoc abc-session :url-referrer))
  (in-session "abc"
    (is (not ((segment-named :googling-clojurians))))))

(deftest test-loyal-safari
  (setup-test)
  (save-session (dissoc abc-session :url-referrer))  
  (in-session "abc"
    (is ((segment-named :loyal-safari)))
    (is (= :loyal-safari (first (classify))))))

(deftest test-googling-clojurians-chrome-1
  (setup-test)
  (save-session (assoc abc-session :user-agent :chrome))
  (in-session "abc"
    (is (= :googling-clojurians-chrome-1 (first (classify))))))