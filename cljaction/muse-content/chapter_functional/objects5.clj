;;;; method invocation
(defn new-object [klass]
  (let [state (ref {})]
    (fn [command & args]
      (condp = command
        :class klass
        :class-name (klass :name)
        :set! (let [[k v] args] 
                (dosync (alter state assoc k v))
                nil)
        :get (let [[key] args]
               (key @state))
        (let [method (klass :method command)]
          (if-not method 
            (throw (RuntimeException. (str "Unable to respond to" method))))
          (apply method args))))))

(defn find-method [method-name instance-methods]
  (instance-methods method-name))

(defn new-class [class-name methods]
  (fn klass [command & args]
    (condp = command
      :name (name class-name)
      :new (new-object klass)
      :method (let [[method-name] args] 
                (find-method method-name methods)))))

(defn method-spec [sexpr]
  (let [name (keyword (second sexpr))
	body (next sexpr)]
    [name (conj body 'fn)]))

(defn method-specs [sexprs]
  (->> sexprs
       (filter #(= 'method (first %)))
       (mapcat method-spec)
       (apply hash-map)))

(defmacro defclass [class-name & specs]
  (let [fns (or (method-specs specs) {})]
    `(def ~class-name (new-class '~class-name ~fns))))

(comment

  (defclass Person
    (method age []
            (* 2 10))
    (method greet [visitor]
            (str "Hello there, " visitor)))

  (def shelly (Person :new))
  (shelly :age)
  (shelly :greet "Nancy")
)




