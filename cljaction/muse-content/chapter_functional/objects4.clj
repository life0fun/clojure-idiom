;;;; method definition
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
               (key @state))))))

(defn new-class [class-name methods]
  (fn klass [command & args]
    (condp = command
      :name (name class-name)
      :new (new-object klass))))

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
            (str "Hello there," visitor)))

  (def shelly (Person :new))
  (shelly :set! :name "Shelly Johnson")
  (shelly :get :name)

)




