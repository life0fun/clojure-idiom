;; java proxy
;; (load-file "java-proxy.clj")
;; http://kotka.de/blog/2010/03/proxy_gen-class_little_brother.html

;; first, import the java package.
(ns java-proxy
  (:import [java.text SimpleDateFormat]
           [java.util Calendar TimeZone]
           [java.io OutputStream FileOutputStream]))

;; Clojure can do java bettern than java. Proxy is meant strictly for interoperability.
;; Proxy extend concrete class while reify abstracts type, protocol and interface.
;; extend only those instances where interoperability demands it.
;; instance reted from proxy is a proper proxy that does method dispatch based on look up map.

;; To extend a class, create a proxy stub of the class, and then wrap the proxy stub inside the clj fn. The clj fn rets the proxy stub object.
;; The object instantiated by proxy gets stubs for the methods which just look up usual clojure functions in a map.
;; This means proxy-methods are normal (though anonymous) clojure functions and in particular they close over their environment.
;; If a method is not found in the proxy's map the stub throws an UnsupportedOperationException or calls the super's method.

;; java object member function is not high order fn. use macro memfn to convert.
(map #(.getBytes %) ["amit" "rob" "kyle"])
(map (memfn getBytes) ["amit" "rob" "kyle"])

(.subSequence "Clojure" 2 5)
((memfn subSequence start end) "Clojure" 2 5)

; clojure generates a proxy for a class in which each method looks up the function implementing a method in a map.
; Based on the method name, the corresponding function is retrieved from a map and invoked with the this reference and the argument(s)
; proxy just intercepts method calls and wraps it with extra info or dispatch it to delegates.

; proxy macro used to gen obj that impl IF and extend base class on the fly.
; (proxy [baseclass-name] [baseclass-constructor-parameters]
;   (method-redefinition-name [parameters]
;       method-body))
(defn streaming-filter [os]
  (proxy [FilterOutputStream] [os]  ; args or base class ctor args
    (write [b]
      (proxy-super write (.getBytes (str "<strong>" (.toUpperCase (String .b)) "</strong>"))))))

(def f (streaming-filter (FileOutputStream. "./java-proxy.clj")))
(.write f "xxxx")

; proxy java object's toString
(defn toStringProxy
  [msg]
  (proxy [Object] []  ; proxy Object class, ctor takes no args
    (toString [] (conj "tag-" msg (proxy-super toString)))))

(.toString (toStringProxy "hello world !"))


; Proxy a java interface
(defn IDerefProxy
  [msg]
    (let [state (atom msg)]
      (proxy [clojure.lang.IDeref]  ; proxy a IF, not ctor args
        (toString [] @state)
        (deref [] state))))

(def o (IDerefProxy "hello world"))
(.toString o)
(reset! @o "world hello")
(.toString o)


; Proxy with multiple Arities in base class
(proxy [Example] []
  (someMethod [x]
    (condp instance? x
      String  (.doSomethingWithString this x)
      Integer (.doSomethingWithInteger this x)))
  (diffArgs
    ([x] (proxy-super diffArgs x)
    ([x y] (.doMoreStuff this, x, y)))))

; the magic this for dispatch. Do not redef this inside proxy-method
; While the methods for gen-class take the object as first argument (and can thus it can be named whatever you like) 
; proxy captures the symbol this in a similar way Java does. So in a proxy method this will always refer to the instance at hand.
(proxy [Object] []
  (toString []
    (let [this "huh?"]   ;; do NOT re-def this inside proxy-method
      (proxy-super toString))))

(def myr 
  (proxy [Runnable][]
    (run [] 
      (prn "running proxy"))))

(def myz 
  (reify Runnable 
    (run [this] 
      (prn "running reify"))))

(.start (Thread. myr))
(.start (Thread. myz))

(import 'java.util.concurrent.Executors)
(.submit (Executors/newSingleThreadExecutor) myr)

(import [java.util.concurrent Executors])
(.submit (Executors/newSingleThreadExecutor) myr)


; gen-class, need AOT compiler to gen java class before hand.
; (compile 'joy.gui.DynaFrame')
; clojure namespace as package name.  specify whatever you
; We have only on single ﬁeld available called – surprise! – state.
(ns some.namespace
  (:gen-class   ; convert a clojure ns to a java class, with private state, init, getter/setter
    :name joy.gui.DynaFrame      ; class name of generated class
    :extends javax.swing.JFrame  ; 
    :implements [clojure.lang.IMeta]
    :prefix df-     ; find the correct fn to compile to class
    :state state    ; deﬁnes a method which will return the object's state.
    :init init
    :constructors {[String] [String]}
    :methods [[display [java.awt.Container] void]    ;; public method
              ^{:static true} [version [] String]])  ;; static method

(:import (javax.swing JFrame JPanel)
         (java.awt BorderLayout Container)))

(in-ns 'joy.gui.DynaFrame')   ; class name specified by gen-class :name prop
(defn df-init [title]
  [[title] (atom {::title title})])


; override toString fn with first arg as this.
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

; proxy create an obj that impls a IF or extends a concrete class.
; involke the method on the object reted by (proxy ) macro.
(defn default-handler [txt]    ; ret a proxy object
  (proxy [HttpHandler] []      ; proxy gen bytecode for actual class on demand.
    (handle [exchange]         ; proxy methods can be changed by update-proxy at run-time
      (.sendResponseHeaders exchange HttpURLConnection/HTTP_OK 0)
      (doto (.getResponseBody exchange)
            (.write (.getBytes txt))
            (.close)))))    ;; Close over txt

(def server
  (new-server 3001 "/joy/hello" (default-handler "Hello Cleveland"))) ; default-handler ret a proxy obj


;; bind the ret of default-handle to a var, and pass to server
(.stop server 0)
(def p (default-handler  ; default-handler ret a proxy object
         "There's no problem that can't be solved with another level of indirection"))
(def server (new-server 8123 "/joy/hello" p))  ; now p is a proxy object

; ret a closure
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

; bind the ret fn of make-handler-fn
; update-proxy updates the mapped functions within a proxy at runtime.
(defn change-message
  "Convenience method to change a proxy's output message"
  ([p txt] (change-message p identity txt))
  ([p fltr txt]
    ; update-proxy proxy mappings : change the fn name string mapping to fn object
    (update-proxy p {"handle" (make-handler-fn fltr txt)})))

; create a closure that 
(change-message p "Hello Dynamic!")



