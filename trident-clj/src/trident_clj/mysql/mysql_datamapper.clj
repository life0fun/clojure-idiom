
(ns dbconn.mysql.mysql-datamapper
  (:use [clojure.contrib.str-utils])
  (:require [clojure.data.json :as json])
  (:require [clojure.java.jdbc :as jdbc])
  (:use [korma.core])
  (:use [korma.db])
  (:require [clojure.string :as str]))

; the author of clj-record do not recommend the use of it.
; http://elhumidor.blogspot.com/2012/11/why-not-to-use-my-library-clj-record.html
; use http://sqlkorma.com/, it is saner.
; All the parts of a query in Korma can be composed at will. 

; use mysql help with connection spec map to create connection pool.
(def mys (mysql {:classname "com.mysql.jdbc.Driver" 
                 :subprotocol "mysql" 
                 :subname "//localhost:3306/test"   ; dbname is test
                 :user "root"}))

(def pg (postgres {:db "korma"
                   :user "korma"
                   :password "kormapass"
                   ;; optional keys
                   :host "myhost"
                   :port "4567"
                   :delimiters ""}))


; create db connection based on mys configuration.
(defdb sparkle-db mys)

; def entity models
(declare users email address state account posts)

; create table manually, entity only represent the table. Need create table first!
; create table account (
;   id INT NOT NULL AUTO_INCREMENT PRIMARY KEY,
;   users_id INT NOT NULL,
;   balance Decimal(10,4)
; )ENGINE=InnoDB DEFAULT CHARSET=utf8;

; create table users (
;   id INT NOT NULL AUTO_INCREMENT PRIMARY KEY,
;   first VARCHAR(100),
;   last VARCHAR(100),
;   account_id INT,
;   active BOOL
; )ENGINE=InnoDB DEFAULT CHARSET=utf8;
; alter table users add foreign key(account_id) references account(id) on delete cascade;

; create table email (
;   id INT NOT NULL AUTO_INCREMENT PRIMARY KEY,
;   users_id INT NOT NULL,
;   email VARCHAR(100),
;   FOREIGN KEY (users_id) REFERENCES users(id) ON DELETE CASCADE
; )ENGINE=InnoDB DEFAULT CHARSET=utf8;

; create table state_st (
;   id INT NOT NULL AUTO_INCREMENT PRIMARY KEY,
;   name VARCHAR(100)
; )ENGINE=InnoDB DEFAULT CHARSET=utf8;

; create table address (
;   id INT NOT NULL AUTO_INCREMENT PRIMARY KEY,
;   users_id INT NOT NULL,
;   id_state INT NOT NULL,
;   FOREIGN KEY (users_id) REFERENCES users(id) ON DELETE CASCADE,
;   FOREIGN KEY (id_state) REFERENCES state_st(id) ON DELETE CASCADE
; )ENGINE=InnoDB DEFAULT CHARSET=utf8;

; create table posts (
;   id INT NOT NULL AUTO_INCREMENT PRIMARY KEY,
;   content VARCHAR(100)
; )ENGINE=InnoDB DEFAULT CHARSET=utf8;

; create table users_posts (
;   posts_id INT NOT NULL,
;   users_id INT NOT NULL,
;   FOREIGN KEY (posts_id) REFERENCES posts(id) ON DELETE CASCADE,
;   FOREIGN KEY (users_id) REFERENCES users(id) ON DELETE CASCADE
; )ENGINE=InnoDB DEFAULT CHARSET=utf8;


(defentity users
  ;; Basic configuration
  (pk :id) ;; by default "id". This line is unnecessary.
           ;; it's used for relationships joins.
  (table :users) ;; by default the name of the symbol.
                 ;; The line above is also unecessary.
  (database sparkle-db) ;; if none is specified the last defdb
                        ;; will be used. Also unnecessary.
  (entity-fields :first :last) ;; default fields for selects

  ;; Mutations

  ; apply a function before records/value going into the db
  (prepare (fn [{last :last :as v}]
             (if last   ; if has last col, chg to upper case
               (assoc v :last (str/upper-case last)) 
               v)))  
  
  ; apply a function to all query results coming from database.
  (transform (fn [{first :first :as v}]
                 (if first   ; note how first keyword get shadowed.
                   (assoc v :first (str/capitalize first)) 
                   v)))
  
  ; Relationships
  ; fk on address table with format table_id. address.users_id = users.id
  (has-one address)
  
  ; ent to subent. fk is on sub-entity with table_id format. 
  ; email.users_id = users.id
  (has-many email)

  ; sub-ent to ent. current entity has fk with format tablename_id:
  ; belongs-to, has-one need to be paired.
  ; users.account_id = account.id
  (belongs-to account)
  
  ; a join table users_posts with columns user_id and post_id
  ; like has-many, also gets the results in a second
  ; query for each element 
  (many-to-many posts :users_posts))
  
;; a entity as the result of subselect, give tablename
; set the table name for the entity  
(defentity subselect-example
  (table (subselect users (where {:active true}))
         :activeUsers))

; email entity model users.id = emails.users_id, one user to many email
(defentity email
  (belongs-to users))

; address entity, fk ref back to users, state
(defentity address
  (pk :my_pk) ; sets the primary key to "my_pk"
  (belongs-to users)
  (belongs-to state {:fk :id_state})) ; fk state.id = address.id_state

(defentity state
  (table :state_st) ; sets the table to "state_st"
  (has-many address))

(defentity account
  (has-one users))

(defentity posts
  (many-to-many users :users_posts))

; test with raw 
(defn select-test-tbl []
  (jdbc/with-connection {:classname "com.mysql.jdbc.Driver" 
                         :subprotocol "mysql" 
                         :subname "//localhost:3306/test"   ; dbname is test
                         :user "root"}
    (jdbc/with-query-results rs ["select * from test"]
      (prn rs))))

; populate all tables
(defn populate-accounts []
  (-> (insert* "account")
      (values [{:users_id 1 :balance 0}
               {:users_id 1 :balance 1}
               {:users_id 2 :balance 0}
               {:users_id 3 :balance 0}])
      (insert)))

; populate users
(defn populate-users []
  (-> (insert* "users")
      (values [
          {:first "mike" :last "jackson" :account_id 1}   ; auto id starts from 1
          {:first "bon"  :last "jovi" :account_id 2}
          {:first "kenny"  :last "G" :account_id 3}])
      (insert)))

(defn populate-email []
  (-> (insert* "email")
      (values [
        {:users_id 7 :email "user_0@test.com"}
        {:users_id 7 :email "user_0_1@test.com"}
        {:users_id 8 :email "user_1@test.com"}
        {:users_id 9 :email "user_2@test.com"}])
      (insert)))

(defn populate-db []
  (populate-accounts)
  (populate-users)
  (populate-email))

;
; select and subselect
;
(defn get-user [name]
  (select users
    (fields :first :last :email.email)  ; :table.filed
    (where {:last [like name]})
    (join email (= :email.users_id :id))
    (where {:email.id [in (subselect email
                            (fields :id)
                            (where {:email [like "%@test%"]}))]})))
    ; with only applicable for one-one mapping, otherwise, duplicated key error.
    ; (with email
    ;   (fields :email)
    ;   (where {:email [like "%@test%" ]}))))
