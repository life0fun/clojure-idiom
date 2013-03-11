(ns chapter12.datamapper
  (:import (org.apache.hadoop.hbase HBaseConfiguration)
           (org.apache.hadoop.hbase.client Put Get HTable)
           (org.apache.hadoop.hbase.util Bytes)))

(def *single-column-family?*) 
(def *primary-keys-config*)
(def *hbase-single-column-family*)

(defstruct key-config :qualifier :functor)

(defn config-for [key-name qualifier functor]
  {key-name (struct key-config qualifier functor)})

(defn config-keys [& encoders]
  (apply merge encoders))

(defn column-name-delimiter []
  (if *single-column-family?* "__" ":"))

(defn single-column-prefix []
  (str *hbase-single-column-family* ":"))

(defn encoding-keys []
  (*primary-keys-config* :encode))

(defn qualifier-for [key-name]
  (((encoding-keys) (keyword key-name)) :qualifier))

(defn encoding-functor-for [key-name]
  (((encoding-keys) (keyword key-name)) :functor))

(defn symbol-name [prefix]
  (if (keyword? prefix) 
    (name prefix)
    (str prefix)))

(defn new-key [part1 separator part2]
  (str (symbol-name part1) separator (symbol-name part2)))

(defn prepend-to-keys [prefix separator hash-map]
  (reduce (fn [ret key] 
            (assoc ret 
              (new-key prefix separator key)
              (hash-map key)))
          {} (keys hash-map)))

(defn process-map [initial-prefix final-prefix single-map]
  (let [all-keys (to-array (keys single-map))]
    (areduce all-keys idx ret {}
      (assoc ret
        (str initial-prefix "_" (symbol-name (aget all-keys idx)) (column-name-delimiter) final-prefix)
        (single-map (aget all-keys idx))))))

(defn process-maps [key maps]
  (let [qualifier (qualifier-for key)
	encoding-functor (encoding-functor-for key)]
    (apply merge (map 
		  (fn [single-map]
		    (process-map (symbol-name key) (encoding-functor single-map) (dissoc single-map qualifier)))
		  maps))))

(defn process-strings [key strings] 
  (reduce (fn [ret the-string] 
            (assoc ret (new-key key (column-name-delimiter) the-string) the-string))  
          {} strings))

(defn process-multiple [key values]
  (if (map? (first values)) 
    (process-maps key values)
    (process-strings key values)))

(defn process-key-value [key value]
  (cond
    (map? value) (prepend-to-keys key (column-name-delimiter) value)
    (vector? value) (process-multiple key value)
    :else {(new-key key (column-name-delimiter) "") value}))

(defn prepend-keys-for-single-column-family [flattened]
  (if-not *single-column-family?*
    flattened
    (let [prefix (single-column-prefix)
          key-prepender (fn [[key value]] 
                          {(str prefix key) value})]
      (apply merge (map key-prepender flattened)))))

(defn flatten [bloated-object]
  (let [f (apply merge (map 
			(fn [[k v]] 
			  (process-key-value k v))
			bloated-object))]
    (prepend-keys-for-single-column-family f)))


;;;;;;;;;;;;;;; into hbase ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn hbase-table [table-name]
  (HTable. (HBaseConfiguration.) table-name))

(defn add-to-insert-batch [put flattened-list]
  (doseq [[column value] flattened-list]
    (let [[family qualifier] (.split column ":")]
      (.add put (Bytes/toBytes family) (Bytes/toBytes (or qualifier "")) (Bytes/toBytes (str value))))))

(defn insert-into-hbase [object-to-save hbase-table-name row-id]
  (let [table (hbase-table hbase-table-name)
        put (Put. (Bytes/toBytes row-id))
        flattened (flatten object-to-save)]
    (add-to-insert-batch put flattened)
    (.put table put)))

;;;;;;;;;;;;;;;; from hbase to hash  ;;;;;;;;;;;;;;;;;;

(defn hbase-object-as-hash [hbase-result]
  (let [extractor (fn [kv]
                    {(String. (.getColumn kv)) (String. (.getValue kv))})
        key-values-objects (.list hbase-result)]
    (apply merge (map extractor key-values-objects))))

(defn read-row [hbase-table-name row-id]
  (let [table (hbase-table hbase-table-name)
        hbase-get-row-id (Get. (Bytes/toBytes row-id))]
    (.get table hbase-get-row-id)))

(defn get-flattened-map [hbase-table-name row-id]
  (hbase-object-as-hash (read-row hbase-table-name row-id)))

;;;;;;;;;;;;;;;; hydrating ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn decoding-keys []
  (*primary-keys-config* :decode))

(defn all-primary-keys []
  (map #(symbol-name %) (keys (encoding-keys))))

(defn primary-key [column-family]
  (first (filter #(.startsWith column-family (str %)) (all-primary-keys))))

(defn decoding-functor-for [key-name]
  (((decoding-keys) (keyword key-name)) :functor))

(defn decode-with-key [key-name value]
  ((decoding-functor-for key-name) value))

(defn symbolize [a-string]
  (keyword a-string))

(defn column-name-empty? [key-name]
  (= 1 (count (.split key-name (column-name-delimiter)))))

(defn is-from-primary-keys [key-name]
  (let [key-name-str (symbol-name key-name)]
    (some #(.startsWith key-name-str %) (all-primary-keys))))

(defn tokenize-column-name [full-column-name]
  (seq (.split full-column-name (column-name-delimiter))))

(defn strip-prefixes [flattened-and-prepended]
  (if-not *single-column-family?*
    flattened-and-prepended
    (let [prefix-length (count (single-column-prefix))
          prefix-stripper (fn [[key value]]
                            {(.substring key prefix-length) value})]
      (apply merge (map prefix-stripper flattened-and-prepended)))))

(defn has-one-string-hydration [hydrated #^String column-family #^String value]
  (assoc hydrated (symbolize column-family) value))

(defn has-one-object-hydration [hydrated #^String column-family #^String column-name #^String value]
  (let [value-map (or (hydrated column-family) {})]
    (assoc-in hydrated [column-family (symbolize column-name)] value)))

(defn has-many-strings-hydration [hydrated  column-family  value]
  (let [column-key (symbolize column-family)
        old-value (hydrated column-key)]
    (if (nil? old-value) 
      (assoc hydrated column-key [value])
      (assoc hydrated column-key (conj old-value value)))))

(defn has-many-objects-hydration [hydrated  column-family  column-name  value]
  (let [ outer-key (primary-key column-family)
         inner-key (.substring column-family (+ 1 (count outer-key)) (count column-family))
        primary-key-name (qualifier-for outer-key)
        inner-map (or (hydrated outer-key) {})
        inner-object (or (inner-map column-name) 
                         {(symbolize (symbol-name primary-key-name)) (decode-with-key outer-key column-name)})]
    (assoc hydrated outer-key 
	    (assoc inner-map column-name
		    (assoc inner-object (symbolize inner-key) value)))))

(defn collapse-for-hydration [mostly-hydrated]
  (reduce (fn [ret key]
            (let [primary-key (symbol-name key)
                 inner-map (ret primary-key)
                 inner-values (apply vector (vals inner-map))]
              (assoc ret primary-key inner-values)))
          mostly-hydrated (all-primary-keys)))

(defn hydrate-pair [ key-name flattened hydrated]
  (let [ value (.trim (str (flattened key-name)))
        [ column-family  column-name] (tokenize-column-name key-name)]
    (cond
      (= column-name value) (has-many-strings-hydration hydrated column-family value)
      (is-from-primary-keys column-family) (has-many-objects-hydration hydrated column-family column-name value)
      (column-name-empty? key-name) (has-one-string-hydration hydrated column-family value)
      :else (has-one-object-hydration hydrated column-family column-name value))))

(defn hydrate [flattened-and-prepended]
  (let [flattened-object (strip-prefixes flattened-and-prepended)
        flat-keys (keys flattened-object)
        mostly-hydrated (reduce (fn [ret key]
                                   (hydrate-pair key flattened-object ret))
                                {} flat-keys)
        pair-symbolizer (fn [[key value]] 
                          {(symbolize key) value})]
    (apply merge (map pair-symbolizer (collapse-for-hydration mostly-hydrated)))))

(defn read-as-hydrated [hbase-table-name row-id]
  (hydrate (get-flattened-map hbase-table-name row-id)))