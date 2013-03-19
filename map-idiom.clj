;;
;; data structure transforming between Atom, vector, list, array and map.
;; (load-file "map-idiom.clj")
;;

;; :require ;use load code from other namespace.
;; :import java package
(ns ns-map-idiom
  ;;(:require clojure.string :refer [join] :as string))
  (:require clojure.string))

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
(apply merge-with + (map (fn [c] {c 1}) "abcdabccc"))        ;; merge a map, same key + value.
(map (fn [m] (hash-map (-> m str keyword) 1)) "abcdabc")
(reduce (fn [ret this] (assoc ret (-> this str keyword) (inc (get ret (-> this str keyword) 0)))) {} "abcdabc")
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

(sort-by #(key %) {:a 1 :b 20 :c 4})   ;; sort-by seq a map and sort by key
(sort-by #(val %) {:a 1 :b 20 :c 4})   ;; sort-by seq a map and sort by val

(def sort-by-loved-ratio (partial sort-by #(/ (:plays %) (:loved %))))

;; sort-by column sequence, like prim key, second key, etc
(defn columns [colname]
  (fn [row] (vec (map row colnames))))   ;; data structure as function. ({:a 1} :a) ([1 2] 1)

(sort-by (columns [:plays :loved :band]) plays)

(map #(update-in (select-keys % [:band]) [:rate] {}) plays)
(map #( -> % (dissoc :play) (assoc :rate 10)) plays)

;;
;; find the min/max of certain key
;;
(reduce (fn [t this] (let [cp (:plays this)] (if (< cp t) cp t) )) 99999 plays)
(reduce (fn [t this] (let [cp (:plays this)] (min cp t) )) 99999 plays)

(defn min-by [f coll]
  (when (seq coll)
    (reduce
      (fn [min this]
        (if (< (f this) (f min))
          this
          min)) coll)))
(min-by :loved plays)


;;
;; apply a fn to a set of keys in a map
;;
(defn keys-apply [f ks m]
  "Takes a function, a set of keys, and a map and applies the function
   to the map on the given keys.  A new map of the results of the function
   applied to the keyed entries is returned."
  (let [only (select-keys m ks)]
    (zipmap (keys only) (map f (vals only)))))

(keys-apply #(.toUpperCase %) #{:band :plays} (plays 0))

;;
;; apply a fn to a set of keys, return the original map with modified key
;;
(defn manip-map [f ks m]
  "Takes a function, a set of keys, and a map and applies
   the function to the map on the given keys.  A modified
   version of the original map is returned with the results
   of the function applied to each keyed entry."
  (conj m (keys-apply f ks m)))

(manip-map #(int (/ % 2)) #{:plays :loved} (plays 0)) ;; => {:band "Burial", :plays 489, :loved 4}

;; use pure func
(defn halve! [ks]
  (map (partial manip-map #(int (/ % 2)) ks) plays))

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
;; map diff, return mapping in m2 that's not m1
(defn map-diff [m1 m2]
  (into {}
    (filter
      (fn [[k v]]
        (not= (get m1 k) v))
      m2)))

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

;; update-in fnil, wrap a fn, when nil args passed to fn, replace with empty collection or 0
(reduce (fn [m1 m2]         ;; reduce on a list of maps
    (reduce (fn [m [k v]]   ;; this reduce on each entry inside one map.
        (update-in m [k] (fnil conj []) v)) m1, m2))  ;; closure to ref to final atom
    {} data)

(reduce (fn [t c] (reduce (fn [t1 c1] (update-in t [(first c1)] (fnil conj []) (second c1) )) t c)) {} data)
(apply merge-with into (map #(hash-map (first (keys %)) (vec (vals %))) data))

;; reverse map
(defn reverse-map
  "{:a 1 :b 1 :c 2} -> {1 [:a :b] 2 :c}"
  [amap]
  (reduce (fn [m [k v]] 
    (let [existing (get m v [])]
      (assoc m v (conj existing k))))
    {} amap))


;; any intersection among a seq of sets
;; first, convert sets into a list, then find items with cnt > 2
(defn any-intersection [& sets]
  (let [k->cnt (mapred (apply concat sets)]
    (-> (filter (fn [[k v]] (> v 1)) k->cnt)
        keys)))

;; first, get all keys in a list, then for each key, merge values.
(defn join-maps [& maps]
  (let [all-keys (apply set/union (for [m maps] (-> m keys set)))]
    (into {}
      (for [k all-keys]
        [k (for [m maps] (m k))]
        ))))

;; apply fn for each item in the list
(defn fast-list-map [afn alist]
  (let [ret (ArrayList.)]
    (fast-list-iter [e alist]
      (.add ret (afn e)))
    ret ))

;; iter over map with type hint.
;;
(defmacro fast-map-iter [[bind amap] & body]
  `(let [iter# (map-iter ~amap)]
    (while (iter-has-next? iter#)
      (let [entry# (iter-next iter#)
            ~bind (convert-entry entry#)]
        ~@body
        ))))


;; list comprehension with grp by, ret a mapping key->List.
(defn fast-group-by [afn alist]
  (let [ret (HashMap.)]
    (fast-list-iter [e alist]
      (let [key (afn e)
            ^List curr (get-with-default ret key (ArrayList.))]
        (.add curr e)))
    ret ))


;;
;; thread-last ->> and thread-first ->
;; thread-first an obj into a series of simple fns.
;; thread-last a collection to map/red/filter fns as they take collection as last arguments.

;; list users in ny, group by employer
(def jay {:name "jay fields" :employer "drw.com" :current-city "new york"})
(def john {:name "john dydo" :employer "drw.com" :current-city "new york"})
(def mike {:name "mike ward" :employer "drw.com" :current-city "chicago"})
(def chris {:name "chris george" :employer "thoughtworks.com" :current-city "new york"})

;; update-in take coll as the first arg, need to use anonym fn to wrap update-in
;; the anonym fn take coll as its only arg, implicitly the last arg, and pass the coll to update-in as first arg.
(->> [jay john mike chris]
  (filter (comp (partial = "new york") :current-city))
  (group-by :employer)     ;; group-by ret a map with key col and a val vector
  (#(update-in % ["drw.com"] (partial map :name))))

;; use thread-first to cater for update-in take coll as first arg.
(-> [jay john mike chris]
  (->> (filter (comp (partial = "new york") :current-city))
       (group-by :employer))
  (update-in ["drw.com"] (partial map :name)))

(-> [jay john mike chris]
  (->> (filter (comp (partial = "new york") :current-city)))
  (->> (group-by :employer))
  (update-in ["drw.com"] (partial map :name)))

;; use let to sequentially assign intermediate variables.
(let [people [jay john mike chris]
      new-yorkers (filter #(= "new york" (:current-city %)) people)
      by-employer (group-by :employer new-yorkers)]   

(assoc by-employer "drw.com" (map :name (get by-employer "drw.com"))))

;; myflatten without flatten
(defn myflatten [l]
  (reduce (fn [ret this]
      (if-not (coll? this)
        (concat ret [this])
        (concat ret (myflatten this)))) [] l)))

;;
;; Astar algorithm
(defn astar [start-yx step-est cell-costs]
  (let [size (count cell-costs)]
    (loop [steps 0
           routes (vec (replicate size (vec (replicate size nil))))
           work-todo (sorted-set [0 start-yx])] 
      (if (empty? work-todo)
        [(peek (peek routes)) :steps steps]
        (let [[_ yx :as work-item] 
              (first work-todo)
              rest-work-todo (disj work-todo work-item)
              nbr-yxs (neighbors size yx)
              cheapest-nbr (min-by :cost (keep #(get-in routes %) nbr-yxs))
              newcost (path-cost (get-in cell-costs yx) cheapest-nbr)
              oldcost (:cost (get-in routes yx))]

          (if (and oldcost (>= newcost oldcost))
            (recur (inc steps) routes rest-work-todo)
            (recur (inc steps)
                   (assoc-in routes yx {:cost newcost :yxs (conj (:yxs cheapest-nbr []) yx)})
                   (into rest-work-todo
                     (map (fn [w] (let [[y x] w] [(total-cost newcost step-est size y x) w]))
                           nbr-yxs)))))))))
