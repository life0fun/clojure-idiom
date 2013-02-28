;;
;; data structure transforming between Atom, vector, list, array and map.
;; (load-file "map-idiom.clj")

(ns clojure-test
  (:require clojure.string :refer [join] :as string))


;; function programming : focus on result, no precedure.
;; what is the input, A map or a list of map,
;; what is the result, reduce to A map, or return a vector of map.
;; Intermediate result is stored in lazy data structure.

;;
;; calling java from clojure and convert java collection into clojure data structure.
;; To call java method chain, use (doto x & form) : (doto (java.util.HashMap.) (.put :a 1) (.put :b 2))
;;
;; use (seq coll) or (into #{} coll) to convert java collection into clojure data structure.
;; use (clojure.set/intersection x y) to get set intersection
;; 
(defn stringMatch [s, keywords]   ;; s is a string, not a list.
  (let [ words (.split s " ")
         wordset (into #{} words)
     sect (clojure.set/intersection wordset (set keywords))
       ]

     (if (empty? sect)
       (if (not-empty sect)
         (prn 'not-empty)
     false)
       s)))

;; note the diff with double quote ['that ...]
(stringMatch "this is the world" ["that" "is"])  

;; Named Var : thread local dynamic bindings
(def a 1)
(binding [a 10] (.start (Thread. (fn [] (println a)))))
(binding [a 10] (.start (Thread. (bound-fn [] (println a)))))

;;
;; destructuring : use [] to recv []/() destructuring. use {} for {} destructuring
(def book {:name "SICP" :details {:pages 657 :isbn-10 "0262011530"}})
(let [{{pages :pages} :details} book] (prn pages))

;;
;; map slicing : assoc/dissoc map/merge-with and update-in
;; input is Atom map, expand to a list of map during interim, then merge map.
;;
(apply merge-with + (map #(apply hash-map %) {:a 1 :b 2 :c 1 :d 2 :e 5}))

(let [{:keys [name age] :as new-sliced-map} {:name "n" :gender "f" :age 18}] )

;; map reduce : reduce to a Atom tot on top of init. Intermediate result is in tot.
;; trans string to intermediate  map list and using merge-with to get Atom map.
;; init a map, and reduce a list of char onto the init map.
(apply merge-with + (map (fn [c] {c 1}) "abcdabccc"))
(map (fn [m] (hash-map (-> m str keyword) 1)) "abcdabc")
(reduce (fn [t c] (assoc t (-> c str keyword) (inc (get t (-> c str keyword) 0)))) {} "abcdabc")
(reduce #(assoc %1 %2 (inc (get %1 %2 0))) {} "abcdcdcd")

;;
;; -> macro Vs. doto
;; -> thread values down into each form as pipe
;; doto : calling a sequence of forms on the very first input arg.
;;
(-> (Math/sqrt 25) int list)
(doto (new java.util.HashMap) (.put "a" 1) (.put "b" 2))
(-> (new java.util.HashMap) (.put "a" 1) (.put "b" 2))

;;
;; create function on the fly with (comp f g h)
;; ((partial + 5) 10 20) isnt curry (#(apply + 5 %& ) 10 20)
;;
(def plays [{:band "Burial",     :plays 979,  :loved 9}
            {:band "Eno",        :plays 2333, :loved 15}
            {:band "Bill Evans", :plays 979,  :loved 9}
            {:band "Magma",      :plays 2665, :loved 31}])

(select-keys plays [:band :loved])
(sort-by :band plays)
(def sort-by-loved-ratio (partial sort-by #(/ (:plays %) (:loved %))))

(sort-by (columns [:plays :loved :band]) plays)

(map #(update-in (select-keys % [:band]) [:rate] {}) plays)
(map #( -> % (dissoc :play) (assoc :rate 10)) plays)

;;
;; idiomatic way to change value in nested map using update-in
;; the fn that applied to each map entry is used for crunch map entry value.
;;
(def person { :name {:first-name "John" :middle-name "Michael" :last-name "Smith" }})
(update-in person [:name] assoc :first-name "Bob" :last-name "Doe")
(update-in person [:name] merge {:first-name "Bob" :last-name "Doe"})
(update-in person [:name] into {:first-name "Bob" :last-name "Doe"})
(-> person
    (assoc-in [:name :first-name] "Bob")
    (assoc-in [:name :last-name]  "Doe"))

(def foo {:bar {:baz {:quux 123}}})
(update-in foo [:bar :baz :quux] inc)  ;; {:bar {:baz {:quux 124}}}

(defn update-map-entry [m k args] 
   (update-in m [(keyword k)] (fn [oldvalue] (if-not (nil? oldvalue) (hash-map) (clojure.string/join args)))))

;;
;; merge-with to A map using (f val-in-result val-in-later) e.g. conj to crunch atom into set
;; the fn that applied to each map entry is used for crunch map entry value.
;;
(def data [{:a 1 :b "a"}
           {:a 2 :b "b"}
           {:a 3 :b "c"}
           {:a 4 :b "a"}])

(let [base {:a #{} :b #{}}]
  (apply merge-with conj base data))   ;; the same key, conj the latter val into result val

;;
;; filter map entry with certain column.
;; filter to get rid of columns, get a list, apply merge-with to reduce.
;;
(def m {:a "x" :b "y" :c "z" :d "w"})
(def z (filter (fn [[k v]] (not (= k :c))) m))
(apply merge-with str (map (fn [[k v]] {k v}) z))
(map #( -> % (dissoc :c) (assoc :e 10)) z)
(select-keys m [:a :c])

;;
;; create map
;;
(#(zipmap (map :categoryname %) %)
 [{:categoryid 1, :categoryname "foo" } 
  {:categoryid 2, :categoryname "bar" } 
  {:categoryid 3, :categoryname "baz" }])
 
;;
;; timeline
(def events
  [
   [1509 :marry   "Catherine of Aragon"]
   [1527 :unmarry "Catherine of Aragon"]
   [1533 :marry   "Anne Boleyn"]
   [1536 :unmarry "Anne Boleyn"]
   [1536 :marry   "Jane Seymour"]
   [1537 :unmarry "Jane Seymour"]
   [1540 :marry   "Anne of Cleves"]
   [1540 :unmarry "Anne of Cleves"]
   [1540 :marry   "Catherine Howard"]
   [1542 :unmarry "Catherine Howard"]
   [1543 :marry   "Catherine Parr"]])

(def timeline
  (let [events-by-year (group-by first events)]
     (map #(map next (events-by-year %))
       (iterate inc (reduce min (keys events-by-year))))))

(take 30 timeline)

;;
;; group by firm and sum the Val and PE col
;;
(def a [{:firm "MSFT" :Val 10  :PE 3 }
        {:firm "MSFT" :Val 15  :PE 4}
        {:firm "GOG" :Val 15 :PE 3}
        {:firm "YAH" :Val 8 :PE 1}])

(for [m (group-by :firm a)]
   (assoc (apply merge-with + (map #(dissoc % :firm) (val m))) :firm (key m)))


(defn addv [e] 
  (apply merge-with (fn [rslt latter] 
    (if-not (= rslt latter) (+ rslt latter) (name rslt))) e))
(for [m g] (addv (val m)))


;;
;; group by map keys
;;
(defn combine-by-coords
  [values]
  (let [grouped-by-x (group-by :x values)]
     (persistent!
     (reduce (fn [res x-val]
               (assoc! res x-val (group-by :y (x-val grouped-by-x))))
             (transient {})
             (keys grouped-by-x)))))

;;
;; merge a list of maps into one
;;
(def data [{:humor :happy} {:humor :sad} {:humor :happy} {:weather :sunny}])
(reduce (fn [t c] (reduce (fn [t1 c1] (prn t1 c1)) t c))
  {} data)

(reduce (fn [m1 m2]         ;; reduce on a list of maps
    (reduce (fn [m [k v]]   ;; this reduce on each entry inside one map.
        (update-in m [k] (fnil conj []) v)) m1, m2))  ;; closure to ref to final atom
    {} data)

(reduce (fn [t c] (reduce (fn [t1 c1] (update-in t [(first c1)] (fnil conj []) (second c1) )) t c)) {} data)
(apply merge-with into (map #(hash-map (first (keys %)) (vec (vals %))) data))
