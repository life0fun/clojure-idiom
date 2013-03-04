(ns joy.web
  (:import (com.sun.net.httpserver HttpHandler HttpExchange HttpServer)
           (java.net InetSocketAddress HttpURLConnection)
           (java.io IOException FilterOutputStream)
           (java.util Arrays)))
           
(defn new-server [port path handler]
  (doto (HttpServer/create (InetSocketAddress. port) 0)
    (.createContext path handler)
    (.setExecutor nil)
    (.start)))

;; default handle is a proxy on top of http request handler.
(defn default-handler [txt]
  (proxy [HttpHandler] []
    (handle [exchange]
      (.sendResponseHeaders exchange HttpURLConnection/HTTP_OK 0)
      (doto (.getResponseBody exchange)
          (.write (.getBytes txt))
          (.close)))))    ;; Close over txt
          
(def server 
  (new-server 3001 "/joy/hello" (default-handler "Hello Cleveland")))
  

;; bind the ret of default-handle to a var, and pass to server
(.stop server 0)
(def p (default-handler
          "There's no problem that can't be solved with another level of indirection"))
(def server (new-server 8123 "/joy/hello" p))

;;
;; now make default handler ret a fn
(defn make-handler-fn [fltr txt]
  (fn [this exchange]             ;; this captures the caller, which is proxy of http handler.
    (let [b (.getBytes txt)]
      (-> exchange
          .getResponseHeaders
          (.set "Content-Type" "text/html"))
      (.sendResponseHeaders exchange
                            HttpURLConnection/HTTP_OK
                            0)
      (doto (fltr (.getResponseBody exchange)) 
        (.write b)
        (.close)))))

;; bind the ret fn of make-handler-fn
;; update-proxy the p proxy with the mapping of handle
(defn change-message
  "Convenience method to change a proxy's output message"
  ([p txt] (change-message p identity txt))
  ([p fltr txt]
    (update-proxy p {"handle" (make-handler-fn fltr txt)})))
    

;;
;; now we can use
(change-message p "Hello Dynamic!")

