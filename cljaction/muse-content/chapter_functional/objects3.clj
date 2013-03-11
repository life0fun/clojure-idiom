;; state
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

(defn new-class [class-name]
  (fn klass [command & args]
    (condp = command
      :name (name class-name)
      :new (new-object klass))))

(defmacro defclass [class-name]
  `(def ~class-name (new-class '~class-name)))



(comment

  (defclass Person)

  (Person :name)

  (def nancy (Person :new))

  (nancy :set! :name "Nancy")

  (nancy :get :name)
)


