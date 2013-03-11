;; instantiation
(defn new-object [klass]
  (fn [command & args]
    (condp = command
      :class klass
      :class-name (klass :name))))

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

  (def nancy (Person :new)))

