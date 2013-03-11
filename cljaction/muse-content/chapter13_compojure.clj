(ns chapter13-compojure
  (:use compojure
        ))

(defroutes hello
  (GET "/"
    (html [:h1 "Hello, world!"]))

  (GET "/echo"
    (html [:h2 "You said " (params :message)]))

  (GET "/stat"
    404)

  (GET "/set-session"
    [(session-assoc :some-key (params :name)) "Set!"])

  (GET "/get-session"
    (html [:h3 "some-key is" (session :some-key)]))

  (ANY "*"
       [404 "Page Not Found!"]))

(run-server {:port 8080}
            "/*" (servlet (with-session hello)))