;; java proxy
;; (load-file "java-proxy.clj")

;; Clojure can do java bettern than java. Proxy is meant strictly for interoperability.
;; Proxy extend concrete class while reify abstracts type, protocol and interface.
;; extend only those instances where interoperability demands it.
;; instance reted from proxy is a proper proxy that does method dispatch based on look up map.


;; java object member method is not high order fn. use macro memfn to convert.
(map #(.getBytes %) ["amit" "rob" "kyle"])
(map (memfn getBytes) ["amit" "rob" "kyle"])

(.subSequence "Clojure" 2 5)
((memfn subSequence start end) "Clojure" 2 5)

;; clojure generates a proxy for a class in which each method looks up the function implementing a method in a map.
;; Based on the method name, the corresponding function is retrieved from a map and invoked with the this reference and the argument(s)
;; proxy just intercepts method calls and wraps it with extra info or dispatch it to delegates.

;; (proxy [baseclass-name] [baseclass-constructor-parameters]
;;   (method-redefinition-name [parameters]
;;       method-body))
;;
(defn streaming-filter [o]
  (proxy [FilterOutputStream] [o]
    (write [b]
      (proxy-super write (.getBytes (str "<strong>" (.toUpperCase (String .b)) "</strong>"))))))

;;
;; proxy java object's toString
;;
(defn toStringProxy
  [msg]
  (proxy [Object] []
    (toString [] (conj "tag-" msg (proxy-super toString)))))

(.toString (toStringProxy "hello world !"))

;;
;; Proxy a java interface
;;
(defn IDerefProxy
  [msg]
    (let [state (atom msg)]
      (proxy [clojure.lang.IDeref]
        (toString [] @state)
        (deref [] state))))

(def o (IDerefProxy "hello world"))
(.toString o)
(reset! @o "world hello")
(.toString o)

;;
;; Proxy with multiple Arities in base class
;;
(proxy [Example] []
  (someMethod [x]
    (condp instance? x
      String  (.doSomethingWithString this x)
      Integer (.doSomethingWithInteger this x)))
  (diffArgs
    ([x] (proxy-super diffArgs x)
    ([x y] (.doMoreStuff this, x, y)))))


;; gen-class, need aot compiler to gen java class before hand.
;; (compile 'joy.gui.DynaFrame')
;; clojure namespace as package name.  specify whatever you
;; We have only on single ﬁeld available called – surprise! – state.
;;
(ns some.namespace
  (:gen-class
    :name joy.gui.DynaFrame
    :extends javax.swing.JFrame
    :implements [clojure.lang.IMeta]
    :prefix df-
    :state state    ;; deﬁnes a method which will return the object's state.
    :init init
    :constructors {[String] [String]}
    :methods [[display [java.awt.Container] void]    ;; public method
              ^{:static true} [version [] String]])  ;; static method

  (:import (javax.swing JFrame JPanel)
           (java.awt BorderLayout Container)))

(in-ns 'joy.gui.DynaFrame')
(defn df-init [title]
  [[title] (atom {::title title})])

;;
;; override toString fn with first arg as this.
(defn -toString [this]
  "Hello, World!")

;;
;; the difference between gen-class and proxy
;; gen-class creates a named class while proxy wraps an existing concrete java class.
;; gen-class taks an object as first arg, proxy capture this symbol the java way. Do not rebind this symbol.
;;

;;
;; an http server example with http proxy handler.
;;
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
  (proxy [HttpHandler] []      ;; proxy gen bytecode for actual class on demand.
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
;; update-proxy updates the mapped functions within a proxy at runtime.
;;
(defn change-message
  "Convenience method to change a proxy's output message"
  ([p txt] (change-message p identity txt))
  ([p fltr txt]
    (update-proxy p {"handle" (make-handler-fn fltr txt)})))

;;
;; now we can change display text dynamically.
(change-message p "Hello Dynamic!")



