; data store connection

; using clj-record as ORM to store models
; to define a model, you define a namespace. 

; Requiring clj-record.boot pulls in several different namespaces, 
; including core, associations, and validation. 
; you only need to call the init-model function from the clj-record.core namespace.
(ns com.colorcloud.model.user 
	(:require clj-record.boot)
	(:use [clojure.contrib.sql]))  ; (use 'clojure.contrib.sql)  

; require the clj-record.boot namespace. Internally, the library is made up of several 
; different namespaces that contain code related to things like 
; associations, validations, callbacks, and serialization.

(def db    ; mysql db configuration using jdbc driver
  {:classname "com.mysql.jdbc.Driver"
   :subprotocol "mysql"
   :user "root"
   :password "password"
   :subname "//localhost/damages_dev"})

; now call init-model to create a model store with validations, etc
(clj-record.core/init-model
	(:associations (has-many charges)))

; to use the model, import it and start to create records model/create, etc
(require '(com.colorcloud.model [user :as user]))
(user/create {:login "rob"
              :first_name "Robert"
              :last_name "Berger"
              :password "secret"
              :email_address "rob@runa.com"})

; get record by id 
(user/get-record 1)

; search record by find like mongo json query
(user/find-records {:first_name "robert"})

; update a record
(user/update {:login "stevenson" :id 2})

(user/destroy-record {:id 2})

; association : charge model fk point to user model
(ns com.colorcloud.model.charge
	(:require clj-record.boot))

; init-model with association and validation, etc
(clj-record.core/init-model
	(:associations (belongs-to user))
	(:validation  ; validate input data
  	(:amount_dollars "Must be positive!" #(>= % 0))
  	(:amount_cents "Must be positive!" #(>= % 0)))
	
	(:callbacks   ; data change handler, before-save, before-update, and after-load.
  	(:before-save (fn [record]
         	 	        (if-not (:category record)
            	        (assoc record :category "uncategorized")
              	      record)))))

; validate input data
(let [errors (charge/validate {:amount_dollars 0
                               :amount_cents -10
                               :date "2010-01-10"
															 :vendor_name "amazon"})]
	(println errors))

