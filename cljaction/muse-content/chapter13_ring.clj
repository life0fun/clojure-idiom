(ns chapter13-ring
  (:use ring.adapter.jetty)
  (:use ring.middleware.params))

(defn hello [req]
  {:body "hello world!"})

(defn echo [req]
  (let [qs (:query-string req)
        params (apply hash-map (.split qs "="))]
    {:body (params "echo")}))

(defn new-echo [req]
  (println "new")
  {:body (get-in req [:query-params "echo"])})

(def echo-app (wrap-params new-echo))

(run-jetty echo-app {:port 8080})