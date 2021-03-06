;; Types, protocols, and records
;; (load-file "types.clj")

;; (:use mixed in all fns, symbols, and mappings from other _NameSpace_.)
;; (:import [pkg1] [pkg2 fn1 ...]) import Java packages
;; (:require [namespace_1 :ref local_namespace_])
;; when defining ns, include only the references that are used.
;; :exclude, :only, :as, :refer-clojure, :import, :use, :load, and :require.
;; ;use naked could corrupt the namespace.  (:use :only)
;; :import working with java deftype defrecord

; (ns my-ns
;   (:refer-clojure :exclude [defstruct])
;   (:use (clojure set xml))  ;; mixed-in other namespace without namespace qualification.
;   (:use [clojure.test :only (are is)])
;   (:require (clojure [zip :as z]))
;   (:import (java.util.Collection)))


; partial and curry
; Although both partial and curry return functions, a partially applied function is always 
; ready to run when it’s applied to the remaining arguments. If given insufficient 
; arguments, a partially applied function will throw an exception complaining about 
; that. Curried functions return functions that are further curried if they’re applied to insufficient arguments.
(defn curried-fn [func args-len]
  (fn [& args]
    (let [remaining (- args-len (count args))]
      (if (zero? remaining)
        (apply func args)
        (curried-fn (apply partial func args) remaining)))))

; code as data, is closure object a data object, or a function ?
; The line is blurred when closure object can ret the props and perform fn call in the same form.
; (object message-name & arguments)
(defn new-user [login password email]
  (fn [a]
    (condp = a
      :login login
      :password password
      :email email
      :authenticate (= password (first args)))))

(def tom (new-user "tom" "tompasswd" "tom@tom.com"))
(tom :login)
(tom :email)
(tom :authenticate)


;; prototype inheritance
;; the flexibility of UDP comes from each map contains a reference to a prototype 
;; map used as a parent link to inherited fields.

;; beget, set prototype inheritate chain of cur object to the passed in map.
(defn beget [o p] (assoc o ::prototype p))
;; get, lookup a key along the prototype chain.
(defn get [m k]
  (when m
    (if-let [[_ v] (find m k)]
      v
      (recur (::prototype m) k))))  ;; recursive along prototype chain.

(def cat {:likes-dogs true, :ocd-bathing true})
(def morris (beget {:likes-9lives true} cat))
(def post-traumatic-morris (beget {:likes-dogs nil} morris))

(get cat :likes-dogs)    ;; true
(get morris :likes-dogs)    ;; true
(get post-traumatic-morris :likes-dogs)    ;; nil

;; Multimethods provide a way to perform function polymorphism based on the result 
;; of an arbitrary dispatch function.
;; multimethods impl polymorphism based on arbitrary dispatch functions. multimethods are sometimes less than ideal.
;; Dispatch is the runtime procedure for looking up which function to call based on the parameters given.
;; single dispatch uses runtime implicit self parameter to lookup.
;; multimethod can use any number of parameters for dispatch.

;; when multimethod called with prototype map, map is queried using key.
(defmulti  compiler :os)        ;; dispatch on :os key
(defmethod compiler ::unix [m] (get m :c-compiler))
(defmethod compiler ::osx  [m] (get m :c-compiler))

(def clone (partial beget {}))
(def unix   {:os ::unix, :c-compiler "cc", :home "/home", :dev "/dev"})
(def osx  (-> (clone unix)
              (put :os ::osx)
              (put :c-compiler "gcc")
              (put :home "/Users")))

(compiler unix) ;=> "cc"
(compiler osx) ;=> "gcc"

;; true Multimethods on multiple keys
;; without Multimethods dispatch, you will need a messy (cond ) form.
;; this is the same as messy switch in OO and you need use subtype to encapsulate.

(defn fee-amount [percentage user]
  (float (* 0.01 percentage (:salary user))))
(defn affiliate-fee-cond [user]
  (cond                         ;; messy cond switch to different fns.
    (= :google.com (:referrer user)) (fee-amount 0.01 user)
    (= :mint.com (:referrer user)) (fee-amount 0.03 user)
    :default (fee-amount 0.02 user)))

;; In FP, use multimethod dispatch to encapsulate polymorphism by dispatching to different fns.
;; juxt take a set of fn and apply args to each fn, ret a vector of result.
(def each-math (juxt + * - /))
(each-math 2 3)

;; The dispatch values for the new compile-cmd methods are vectors composed of the
;; results of looking up the :os key and calling the compiler function defined
(defmulti  compile-cmd  (juxt :os compiler))
(defmethod compile-cmd [::osx "gcc"] [m] (str "/usr/bin/" (get m :c-compiler)))
(defmethod compile-cmd :default [m] (str "Unsure where to locate " (get m :c-compiler)))

(compile-cmd osx)   ;;=> "/usr/bin/gcc"
(compile-cmd unix)  ;;=> "Unsure where to"

;; Using multimethods and the UDP is an interesting way to build abstractions.
;; Multi- methods allowing for polymorphic dispatch based on arbitrary functions.
;; Clojure also provides a simpler model for creating abstractions and gaining 
;; the benefits of polymorphism types, protocols, and records

;; Clojure provides facilities for creating logically grouped polymorphic functions types, records, and protocols.
;; Clojure polymorphism lives in the protocol functions, not in the classes, as compare to Monkey patch and Wrappers.
;; polymorphism and recur. Fn impl using recur is not polymorphism.
;;

(ns ns-type-protocol-records
  (:require clojure.string))

;; defrecord creates a JVM class.  (defrecord name [fields*] options* specs*)
;; record types are concrete classes
;; look up keys in records is more quickly than the equivalent array map or hash map 
;; less mem than boxed objects(Byte, Integer, Long)
;; defrecord deprecates defstruct.

(defrecord InfiniteConstant [i]
  clojure.lang.ISeq             ;; interface name to be impl by record type.
    (seq [this]
      (lazy-seq (cons i (seq this)))))

;; this create a JVM class and import to current ns implicitly.
(defrecord TreeNode [val l r])
(TreeNode. 5 nil nil)   ;; TreeNode now is JVS class and use new Klz to instantiate.

(defn xconj [t v]
  (cond
    (nil? t)       (TreeNode. v nil nil)
    (< v (:val t)) (TreeNode. (:val t) (xconj (:l t) v) (:r t))
    :else (TreeNode. (:val t) (:l t) (xconj (:r t) v))))

(defn xseq [t]
  (when t
    (concat (xseq (:l t)) [(:val t)] (xseq (:r t)))))

(def sample-tree (reduce xconj nil [3 5 2 4 6]))
(xseq sample-tree)

;;
;; protocol, Interface, a set of fn signatures.
;; the first parameter to a protocol function corresponds to the target object, object.if-method
;;
(defprotocol FIXO
  (fixo-push [fixo value])
  (fixo-pop [fixo])
  (fixo-peek [fixo]))

;;
; Protocol is clojure style Mixin for OO style polymorphism. All protocol fn get 
; first args as this ref to target. (defrecord, deftype, map, java obj) are OO.
; Protocols are implemented using extend forms: extend, extend-type, extend-protocol.
; extend-type : to impl inside JVM type.
; extend class-name protocol-name mixin-fn-map
; extend protocol to multiple types.
; extend type to multiple protocols.
; Protocol extends to class/type/IF with single dispatch of first arg, and extendible multiple dispatches.
; this is exactly as javascript augment object by adding fns in prototype object chain.
; Types extended to protocols.
; concrete type and protocol can be from 3rd party and we can extend without any adapters, wrappers, monkey patching
;
; extension happens in the protocol fns, not in the types.
; Clojure polymorphism lives in the protocol(interface) functions, not in the classes, as compare to Monkey patch and Wrappers.
; Monkey patch, not polymorphism, namespace collision. 
; Wrapper: need to define beforeHandle.
; Monkey patch: TreeNode.fixo-push = function() {}
; Wrapper : class TreeWrapper { private TreeNode, public fixo-push()}
; Protocol enables run-time in-line polymorphism that can integrate 3rd lib easily.
;;

;; so we just simply import a JVM type, and start to extend it.
;; for static compiling java, everything must be done at the time of definition. 
(extend-type TreeNode    ;; extend-type to impl certain protocol.
  FIXO
  (fixo-push [node value]
    (xconj node value)))

(xseq (fixo-push sample-tree 5/2))
;;
;; we can even extend built-in class or interface.
;;
(extend-type clojure.lang.IPersistentVector
  FIXO
  (fixo-push [vector value]
        (conj vector value)))
        (fixo-push [2 3 4 5 6] 5/2)

;;
;; Clojure-style mixins vs JS Mixins  Only possible in prototype map chain.
;; mixin are way to share code between unrelated classes. One object can borrow fns from another into it prototype chain.
;; (full blown fn, object map with extend, and functional mixin asCircle.call(object-to-be-extended.prototype))
;; Mixins in Clojure refer to the creation of discrete maps containing protocol function implementations 
;;
(use 'clojure.string')
(defprotocol StringOps (rev [s]) (upp [s]))

(extend-type String
  StringOps
  (rev [s] (clojure.string/reverse s)))

(rev "Works")  ;; => "skroW"

; Mixin to borrow fns from other objects with extend ([atype & proto+mmaps])
(def rev-mixin {:rev clojure.string/reverse})
(def upp-mixin {:upp (fn [this] (.toUpperCase this))})
(def fully-mixed (merge upp-mixin rev-mixin))
(extend String StringOps fully-mixed)   ;; extend mixin

(-> "Works" upp rev)  ;; SKROW

;
; clojure does not encourage inheritance. Use object map literal mix-in with extend function.
; You can use a map with name maps to fn, and each Type extend the protocol impl map.
; def a map with name to fn mapping, the same as JS mixins with object literal.
; The first args to protocol is the target fn itself, the fn object that recvd the impl
(def tree-node-fixo
  {:fixo-push (fn [node value]   ; [this value]
                (xconj node value))
   :fixo-peek (fn [node]   ; [this]
                (if (:l node)
                  (recur (:l node)) (:val node)))
   :fixo-pop (fn [node]   ; [this]
                (if (:l node)
                  (TreeNode. (:val node) (fixo-pop (:l node)) (:r node)) (:r node)))})

;; extend JVM class with a mixin map that contains all fns in protocols.
(extend TreeNode FIXO tree-node-fixo)   

(xseq (fixo-into (TreeNode. 5 nil nil) [2 4 6 7]))

;;
; reify, like deftype and defrecord, instantiate an unamed type that impls 1+ protocols spec.
; unlike deftype and defrecord, it does not take a name or a vector of fields;
; datatype created by reify do not have explicit fields, relying instead on closures.
;
(let [x 42
     r (reify AProtocol
        (foo [this b] (prn "reify bar"))
        (bar [this _] (prn "reify bar " x)))]
  (bar r))   ; will print x using 42

(defn fixed-fixo
  ([limit] (fixed-fixo limit []))
  ([limit vector]
    (reify FIXO
      (fixo-push [this value]
        (if (< (count vector) limit)
          (fixed-fixo limit (conj vector value))
          this))
      (fixo-peek [_] (peek vector))
      (fixo-pop [_] (pop vector)))))


;;
;; diff between protocol and java interface
;; protocol is multimethod dispatch, java IF is OO IF for duck-typing, decoupling.
;; java is hard object, java IF must be defined and implemented at the time it’s defined.
;; clojure and Js are soft objects. You can augment/extend object type at runtime !!!
;;
;; methods of the protocol itself are namespaced in a way that Java and C++ interfaces aren’t.
;; In Clojure, the methods always use the same namespace as the protocol itself, ns closure. which means
;; a record or type can extend (via extend, extend-type, and so on) identically named methods
;; of two different protocols without any ambiguity. This allows you to avoid a whole category 
;; of issues that can come up when trying to combine third-party libraries into a single codebase.


;;
;; definterface Vs defprotocol
;; definterface produces an interface .class that java code can implament inorder to create classes suitable to pass to your Clojure functions.
;; Protocols are, in short, a faster and more focused way of doing dispatch than multimethods.
;; you actually have running code in a protocol that is used by other clojure code.
;; use definterface sparingly and prefer protocols unless absolutely necessary.

;; protocols and interfaces can be extended to record types using the extend forms, 
;; Protocol and interface method implementations can be written directly inside a defrecord form
;; Putting method definitions inside the defrecord form also allows you to implement 
;; Java interfaces and extend java.lang.Object, which isn’t possible using any extend form.
;; Because interface methods can accept and return primitive values as well as boxed objects, 
;; implementations of these in defrecord can also support primitives. This is important for 
;; interoperability and can provide ultimate performance parity with Java code.

;; record types are maps and implement everything maps should.
;; deftype is lower level similar to defrecord but doesn’t implement anything 
;; thus re-impl methods wont conflict existing ones.
;;
(defrecord InfiniteConstant [i]
  clojure.lang.ISeq             ;; interface name to be impl by record type.
    (seq [this]
      (lazy-seq (cons i (seq this)))))

(deftype InfiniteConstant [i]
  clojure.lang.ISeq
    (seq [this]
      (lazy-seq (cons i (seq this)))))

(take 3 (InfiniteConstant. 5))

;;
;; final implementation of TreeNode using deftype, we can override ISeq and IPersistentStack 
;;
(deftype TreeNode [val l r]
  FIXO
    (fixo-push [_ v] (TreeNode. val (fixo-push l v) r))
    (fixo-peek [_] (fixo-peek l))
    (fixo-pop [_] (TreeNode. val (fixo-pop l) r))
  clojure.lang.IPersistentStack
    (cons [this v] (fixo-push this v))
    (peek [this] (fixo-peek this))
    (pop [this] (fixo-pop this))
  clojure.lang.Seqable
    (seq [t] (concat (seq l) [val] (seq r))))

(def sample-tree2 (into (TreeNode. 3 nil nil) [5 2 4 6]))
(seq sample-tree2)


;; chess mover
;; 1. move statement
{:from "e7", :to "e8", :castle? false, :promotion \Q}

(defn build-move [& pieces]
  (apply hash-map pieces))

(build-move :from "e7" :to "e8" :promotion \Q)

(defrecord Move [from to castle? promotion]
  Object
  (toString [this]
    (str "Move " (:from this)
         " to " (:to this)
         (if (:castle? this) " castle"
         (if-let [p (:promotion this)]
           (str " promote to " p)
            "")))))

(defn build-move [& {:keys [from to castle? promotion]}]
  {:pre [from to]}
    (Move. from to castle? promotion))

(str (build-move :from "e2" :to "e4")) ;=> "Move e2 to e4"


;; OO single dispatch problem: not work with more than one hierarchy.
;; public interface People {
;;    void pet(Dog d);
;;    void pet(Cat c);
;; }
;; now if you have dog and cat inherits from animal and you call p.pet(new Animal("dog"))
;; it wont work because single dispatch only dispatch this, not the first arguments.

;; Visitor pattern for double dispatching.
;; visitor pattern double dispatch on the first argument.
;; the types of the receiver(this, self) and the first argument would be used to find the method 
;; visitor pattern allows additional operations to be separated from the data object hierarchy.
;; public void accept(NodeVisitor visitor) {
;;     visitor.visitAssignment(this); }   ;; dispatch on the first argument, visitor, and pass the self.

;; A template for handling a functional composition in OOP.
;; OOP wants to group code by classes, too many object, object explosion.
;; We want code grouped by functions
;; This makes it easier to add operations at a later time.
;; Relies on Double Dispatch!!!

;; Dispatch based on (VisitorType, ValueType) pairs.
;; Often used to compute over AST’s (abstract syntax trees)
;; Heavily used in compilers Remember visitPostOrder



;; redis protocol with using multimethod dispatching.
;; the first char is control char the direct which type the response is.

(defmulti parse-reply reply-type :default :unknown)
(defmethod parse-reply :unknown
  [#^BufferedReader reader]
    (throw (Exception. (str "Unknown reply type:"))))
(defmethod parse-reply \-
  [#^BufferedReader reader]
    (let [error (read-line-crlf reader)]
      (throw (Exception. (str "Server error: " error)))))
(defmethod parse-reply \+
  [#^BufferedReader reader]
  (read-line-crlf reader))
(defmethod parse-reply \$
  [#^BufferedReader reader]
  (let [line (read-line-crlf reader)
        length (parse-int line)]
    (if (< length 0)
      nil
      (let [#^chars cbuf (char-array length)]
        (do
          (do-read reader cbuf 0 length)
          (read-crlf reader) ;; CRLF
          (String. cbuf))))))
(defmethod parse-reply \*
  [#^BufferedReader reader]
  (let [line (read-line-crlf reader)
        count (parse-int line)]
    (if (< count 0)
      nil
      (loop [i count
        replies []]
        (if (zero? i)
          replies
          (recur (dec i) (conj replies (read-reply reader))))))))
(defmethod parse-reply \:
  [#^BufferedReader reader]
  (let [line (trim (read-line-crlf reader))
        int (parse-int line)]
    int))


;; spinable seq
;; 1. a protocol(IF) spec about the behavior, this is a seq impl multiple specs, seq and spinable.
;; 2. a fn reify the IF specs.
;; 3. fn args is a state !!!
(defprotocol Spinnable
  (spin [this] "Return a seq walking the opposite direction as this"))

(defn iter-bi [x f b]
  (reify
    Spinnable
    (spin [_] (iter-bi x b f))
    clojure.lang.ISeq
      (first [_] x)
      (more [_] (iter-bi (f x) f b))
      (next [this] (rest this))   ; same as rest since all iter-bi's are infinite seqs
      (seq [this] this)
      (equiv [_ _] false)))       ; cheater!

(extend-type clojure.lang.LazySeq
  Spinnable
  (spin [this] (spin (seq this))))

(->> (iter-bi 0 inc dec)
  (drop 4)
  spin
  (take 10))

;=> (5 4 3 2 1 0 -1 -2 -3 -4)


;
; deftype creates a java class and bring into current ns.
; (deftype name [& fields] & opts+specs)
; when using deftype to encapsulate, remember to return instantiated original object.
;
(deftype CloseableSeq [delegate-seq close-fn]
  clojure.lang.ISeq
    ; next ret a IPersistentCollection
    (next [this]
      (if-let [n (next delegate-seq)]
        (CloseableSeq. n close-fn)
        (.close this)))
    (first [this] (if-let [f (first delegate-seq)] f (.close this)))
    (more [this] (if-let [n (next this)] n '()))
    ; cons also ret the IPersistentCollection, hence, you need to call constructor.
    (cons [this obj] (CloseableSeq. (cons obj delegate-seq) close-fn))
    (count [this] (count delegate-seq))
    (empty [this] (CloseableSeq. '() close-fn))
    (equiv [this obj] (= delegate-seq obj))
  clojure.lang.Seqable 
    (seq [this] this)
  java.io.Closeable
    (close [this] (close-fn)))

;
; =======================================
; expression problem : test the expressiveness of the lang.
; define a datatype by cases, do two things, without recompiling, while retain static type safety
;   add new cases to datatype, and add new fn over datatype, 
;
; for fn lang with pattern match, we can easily add fn without recompiling, as pattern match fn knows all patterns. hard to add new matches.
; for oo with subtype polymorphism, we can easily add types as each subtype knows all function.  hard to add new fn.
; danile spiewak http://vimeo.com/user18356272/review/66548717/3531875329
;
; example, for an algebra s-expression, we can add various fn to it based on pattern match.
; each fn knows all the cases(+. -)
(defmulti value :op)  ; pattern match fn is get map's :op attribute
(defmulti value :+ [expr] (+ (get expr :left-op) (get expr :right-op)))
(defmulti value :- [expr] (- (get expr :left-op) (get expr :right-op)))
;
; To add new fn, we just need to write new multi fn because pattern match fn knows all pattern match. Note that the same dispatch function.
; dispatch function knows all sub functions for dispatching.
(defmulti pprint :op)
(defmulti pprint :+ [expr] (prn "pprint +" expr))
(defmulti pprint :- [expr] (prn "pprint -" expr))
; however, it is hard to add new patterns (dispatch match type). because need to change pattern match function in all multimethods.
; and if we'd extend dispatch fn, we need modify all multimethod where dispatch fn is used.
; If we want to add multiplication, we need modify both value fn and pprint fn.
;
; on the other hand, for subtype polymorphism, each type knows all the fns, so you can add types.
; if you'd add fn, need to change all types to know that function.
;
; OO model algebra, add/sub is a type of algebra. so + goes from operator in c, subtype of algebra, to fn in clojure.
; class Add // Sub  // Multi  <-- easy to add subtype
;   private int l, r
;   pub int value() {return l+r;} // return l-r 
;   pub void pprint() {}  <== hard to add new fn.
;
; it is easy to add new type multiplication, as each type knows all fns.
; however hard to add new fn, to add pprint fn, we need to extend all subtypes to know the new function.

; In clojure, typed class(deftype, defrecord, reify) is a special OO class which stores fields in PersistentMap.
; OO encapsulation, can not take generic approach to information processing. (class specific getter/setter)
; defrecord put fields into map, you can map reduce, and extend protocol to type to achieve type-driven polymorphism.
;
; protocol is a way to enable add fns to subtype by extend type to protocol impl easily in fn lang.
; you can keep extending tyoe to new protocols _dynamically_ without modifying existing code. Yeah !
; The first args to protocol is the target fn itself, the fn object that recvd the impl
(defprotocol Eval   ; Eval protocol spec only have value fn.
  (value [this]))
; Add is a subtype of algebra, extend the protocol of Eval to subtype, so extend subtypes to knows all fns.
(deftype Add [e1 e2]
  (Eval 
    (value [this] (+ (value e1) (value e2)))))

(deftype Sub [e1 e2]
  (Eval
    (value [this] (- (value e1) (value e2)))))

; to make type knows another fn, just extend-type with protocol.
(defprotocol Pprint
  (pprint [this]))
(def add-pprint {:pprint (fn [this] (prn "add-pprint"))})
(def sub-pprint {:pprint (fn [this] (prn "sub-pprint"))})
; now easily extend the type to a new protocol of fns.
; now you can just keep extending types to new protocol, without need to modify existing types.
(extend Add Pprint add-pprint)
(extend Sub Pprint sub-pprint)
; =======================================
