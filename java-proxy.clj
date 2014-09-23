;; java repl using lein repl

lein repl

; can not import entire package, has to import one class by one class
(import '(java.util HashMap HashSet))
(import '(java.util.regex Pattern Matcher))

(.. System (getProperties) (get "os.name"))
(-> (System/getProperties) (.get "os.name"))
(doto (new java.util.HashMap) (.put "a" 1) (.put "b" 2))
(doto (java.util.HashMap.) (.put "a" 1) (.put "b" 2))


; compile a pattern, and use pattern to match against a string, catpure matcher groups.
(let [p (Pattern/compile "hello (\\S+)")
      m (.matcher p "hello world")]
  (if (.find m)
    (prn (.group m 1))))

; simple string match
(.matches "hello world" "hello \\S+")


;; java proxy
;; (load-file "java-proxy.clj")
;; http://kotka.de/blog/2010/03/proxy_gen-class_little_brother.html

;; first, import the java package.
(ns java-proxy
  (:import [java.text SimpleDateFormat]
           [java.util Calendar TimeZone]
           [java.io OutputStream FileOutputStream]))

; Where Proxy shines ? whenever you have to extend concrete class in the framework.
; proxy extends concrete class dynamically while reify abstracts type, protocol and interface.

; To extend a class, create a proxy stub with a spec map of the class, and wrap it inside proxy macro.
; The object instantiated by proxy is an instance of extended class and delegate fn based on spec map.
; This means proxy-methods are lambda function closures closed over their context.
; If a method is not found in the proxy's map the stub throws an UnsupportedOperationException or calls the super's method.

; (proxy [baseclass-name] [baseclass-constructor-parameters]
;   (method-redefinition-name [parameters]
;       method-body))

(def d (proxy [Date] [] 
    (toString [] "Proxy-date-to-string-changed-to-hello")))

(.toString d) ; <= "Proxy-date-to-string-changed-to-hello"

; clojure proxy extends a class with a spec map. Proxy macro gen bytecode impl IF to extend base class on demand on the fly.
; Based on the method name, the corresponding function is retrieved from a map and invoked with the this reference and the argument(s)
; proxy just intercepts method calls and wraps it with extra info or dispatch it to delegates.
; you can use (update-prox proxy mappings) to dynamically assoc spec maps to proxy. 
; so dynamically create closure to extend any class at runtime.

(update-proxy d {"toString" (fn [this] "update-proxy-date-to-string")})
(.toString d) ; <= "update-proxy-date-to-string"


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


; ns defines a namespace, with gen-class, also defines a class, that simply contains stubs to 
; map java class methods calls to ns functions.
; gen-class, need AOT compiler in project.clj to gen java class before hand. AOT tie gened files to clj version.
; (compile 'joy.gui.DynaFrame')
; gen-class creates a class that's a delegate for the vars (prefixed fn), contains state.
; clj gen a set of classes for each fn in a namespace at location classpath.
; java class has many fields, here we only have single class field, called state. use (atom {}) as field map.
; use require to resolve class dependency.
(ns some.namespace
  (:gen-class   ; convert a clojure ns to a java class, with private state, init, getter/setter
    :name com.colorcloud.MyClass      ; this ns impls this class.
    :extends javax.swing.JFrame  ; 
    :implements [clojure.lang.IMeta]
    :prefix df-     ; method prefix with df- is class method, first arg is this pointer.
                    ; (defn df-foo ...), called by (.foo class-instance-object)
    :state state    ; define a method that return the object's state. called as (swap ! (.state this) conj (.getValue ))
    :init init      ; called when obj initiation. ret a [], first arg is a vec of args for super class. 
                    ; second is object's state. init ret the state of object and called when obj instantiate.
    :constructors {[String] [String]} ; map the args of Klz constructor to the args to super klz constructor.
                                      ; to determine which constructor to call.
    :methods [ [display [java.awt.Container] void]     ; public method
               #^{:static true} [version [] String]])  ; class static method

(:import (javax.swing JFrame JPanel)
         (java.awt BorderLayout Container)))

(in-ns 'joy.gui.DynaFrame')   ; class name specified by gen-class :name prop
; init called when obj instantiate, :constructors decide when super klz constructor to call, and init object state.
; the first element is an arg vector to super klz to determine which super constructor to call.
; The second element of the vector is the state for the instance. use (atom {}) as concurmap storing mutable state.
(defn df-init [title]
  [[title] (atom {:title title})])

; to access object state, use (.state this)
(swap! @(.state this) merge {:test "test"})


; another example
(ns  #^{:doc "A simple class with instance vars"
    :author "David G. Durand"}
   com.tizra.example)

(gen-class
  :name com.tizra.example.Demo
  :state state
  :init init
  :prefix "-"
  :main false
  :methods [[setLocation [String] void]
            [getLocation [] String]]
)

(defn -init []
  "store our fields as a hash"
  [[] (atom {:location "default"})])

(defmacro setfield
  [this key value]
  `(swap! (.state ~this) into {~key ~value}))

(defmacro getfield
  [this key]
  `(@(.state ~this) ~key))

(defn -setLocation [this ^java.lang.String loc]
  (setfield this :location loc))

(defn ^String -getLocation
  [this]
  (getfield this :location))

=> (com.tizra.example.Demo.)
#<Demo com.tizra.example.Demo@673a95af>

=> (def ex (com.tizra.example.Demo.))
#'user/ex

=> (.getLocation ex)
"default"

=> (.setLocation ex "time")
nil

=> (.getLocation ex)
"time"

;;
;; the difference between gen-class and proxy
;; gen-class creates a named class while proxy extends an existing concrete java class.
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



;; java object member function is not high order fn. use macro memfn to convert.
(map #(.getBytes %) ["amit" "rob" "kyle"])
(map (memfn getBytes) ["amit" "rob" "kyle"])

(.subSequence "Clojure" 2 5)
((memfn subSequence start end) "Clojure" 2 5)
