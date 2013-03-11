(ns chapter13-webbing-cookies
  (:import (com.sun.grizzly.http.embed GrizzlyWebServer)
           (com.sun.grizzly.tcp.http11 GrizzlyAdapter)
           (com.sun.grizzly.util.http Cookie))
  (:require (org.danlarkin [json :as json])))

(declare *the-request* *the-response*)

(defn cookie-hash []
  (let [cookies (.getCookies *the-request*)
	kv (fn [c] {(.getName c) (.getValue c)})]
    (apply merge (map kv cookies))))

(defn add-cookie [name value]
  (let [c (Cookie. name value)]
    (.setPath c "/")
    (.addCookie *the-response* c)))

(defn read-cookie [name]
  (let [cookies (cookie-hash)]
    (if-not (empty? cookies)
      (cookies name))))

;;;;; jsonp server

(defn route-for [request handlers]
  (let [registered (keys handlers)
	uri-string (.getRequestURI request)]
    (first (filter #(.startsWith uri-string %) registered))))

(defn handler-for [request handlers]
  (handlers (route-for request handlers)))

(defn singularize-values [a-map]
  (let [kv (fn [[k v]] {k (aget v 0)})]
    (reduce merge {} (map kv a-map))))

(defn params-map-from [request]
  (singularize-values (into {} (.getParameterMap request))))

(defn only-jsonp-param? [params-map]
  (and (= 1 (count params-map))
       (= "jsonp" (first (keys params-map)))))

(defn without-query-string? [request]
  (let [params-map (params-map-from request)]
    (or (empty? params-map) 
	(only-jsonp-param? params-map))))

(defn parsed-params-from-uri [request handlers]
  (let [uri-string (.getRequestURI request)
	requested-route (route-for request handlers)
	params-string (.substring uri-string (count requested-route))]
    (rest (.split params-string "/"))))

(defn params-for [request handlers]
   (if (without-query-string? request)
     (parsed-params-from-uri request handlers)
     (params-map-from request)))

(defn jsonp-callback [request]
  ((params-map-from request) "jsonp"))

(defn prepare-response [response-text request]
  (if (jsonp-callback request)
    (str (jsonp-callback request)  "(" (json/encode-to-str response-text) ")")
    response-text))

(defn response-from [handler params without-query-string]
  (try
   (if without-query-string
     (apply handler params)
     (handler params))
  (catch Exception e
    (println "Error! Unable to process, reason -")
    (println (.printStackTrace e)))))

(defn service-http-request [handler-functions request response]
  (binding [*the-request* request *the-response* response]
      (let [requested-route (route-for request handler-functions)
            handler (handler-for request handler-functions)]
        (if handler
          (let [params (params-for request handler-functions)
                without-query-string (without-query-string? request)
                response-text (response-from handler params without-query-string)]
            (println "Responding to" requested-route "with params:" params)
            (.println (.getWriter response) (prepare-response response-text request)))
          (println "Unable to respond to" (.getRequestURI request))))))

(defn grizzly-adapter-for [handler-functions-as-route-map]
  (proxy [GrizzlyAdapter] []
    (service [req res]
      (service-http-request handler-functions-as-route-map req res))))

(defn boot-web-server [handler-functions-as-route-map port]
  (let [gws (GrizzlyWebServer. port)]
    (.addGrizzlyAdapter gws (grizzly-adapter-for handler-functions-as-route-map))
    (println "Started http-gateway on port" port)
    (.start gws)))

;;;;;;;; example

(defn greet [name]
  (add-cookie "greeting" "hello")
  (str "hello, " name))

(defn judge-credentials [{u "username" p "password"}]
  (println "greeting cookie:" (read-cookie "greeting"))
  (str u "/" p " is a good combo!"))

(def routes {
  "/test/greet" greet
  "/judge/creds" judge-credentials
})

(boot-web-server routes 10000)