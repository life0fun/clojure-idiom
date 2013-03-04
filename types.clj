;; Types, protocols, and records
;; (load-file "types.clj")


;;
;; Clojure multimethods impl polymorphism based on arbitrary dispatch functions. multimethods are sometimes less than ideal.
;; Clojure provides facilities for creating logically grouped polymorphic functions types, records, and protocols.
;; Clojure polymorphism lives in the protocol functions, not in the classes, as compare to Monkey patch and Wrappers.
;; polymorphism and recur. Fn impl using recur is not polymorphism.
;;

(ns ns-type-protocol-records
  (:require clojure.string))


;; defrecord creates a TYPE.  (defrecord name [fields*] options* specs*)
;; record types are concrete classes
;; look up keys in records is more quickly than the equivalent array map or hash map 
;; less mem than boxed objects(Byte, Integer, Long)
;; defrecord deprecates defstruct.

(defrecord InfiniteConstant [i]
  clojure.lang.ISeq             ;; interface name to be impl by record type.
    (seq [this]
      (lazy-seq (cons i (seq this)))))

(defrecord TreeNode [val l r])
(TreeNode. 5 nil nil)

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
;; protocol, Interface
;; the first parameter to a protocol function corresponds to the target object, object.if-method
;;
(defprotocol FIXO
  (fixo-push [fixo value])
  (fixo-pop [fixo])
  (fixo-peek [fixo]))

;;
;; Protocols are implemented using extend forms: extend, extend-type, extend-protocol.
;; this is exactly as javascript augment object by adding fns in prototype object chain.
;; Types extended to protocols.
;; concrete type and protocol can be from 3rd party and we can extend without any adapters, wrappers, monkey patching
;;
;; Clojure polymorphism lives in the protocol(interface) functions, not in the classes, as compare to Monkey patch and Wrappers.
;;  Monkey patch: TreeNode.fixo-push = function() {}
;;  Wrapper : class TreeWrapper { private TreeNode, public fixo-push()}
;;
(extend-type TreeNode
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
;; Clojure-style mixins  vs JS Mixins 
;; (full blown fn, object map with extend, and functional mixin asCircle.call(object-to-be-extended.prototype))
;; Mixins in Clojure refer to the creation of discrete maps containing protocol function implementations 
;;
(use 'clojure.string')
(defprotocol StringOps (rev [s]) (upp [s]))

(extend-type String
  StringOps
  (rev [s] (clojure.string/reverse s)))

(rev "Works")  ;; => "skroW"

;; clojure does not encourage inheritance. Use object map literal mix-in with extend function.
;; You can use a map with name maps to fn, and each Type extend the protocol impl map.
;; def a map with name to fn mapping, the same as JS mixins with object literal.
;;
(def tree-node-fixo
  {:fixo-push (fn [node value]
                (xconj node value))
   :fixo-peek (fn [node]
                (if (:l node)
                  (recur (:l node)) (:val node)))
   :fixo-pop (fn [node]
                (if (:l node)
                  (TreeNode. (:val node) (fixo-pop (:l node)) (:r node)) (:r node)))})

(extend TreeNode FIXO tree-node-fixo)   ;; extend protocol using map.

(xseq (fixo-into (TreeNode. 5 nil nil) [2 4 6 7]))


;;
;; reify macro brings together function closures and protocol extend into a single form.
;; reify realizes a single instance of type, protocol, or interface, = abstractions.
;; reify method arglists include the object itself. It’s idiomatic to use this to refer to object and _ to ignore its.
;;
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
