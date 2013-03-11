(ns chapter12
  (:import (java.util Set)
          (org.apache.hadoop.hbase HBaseConfiguration)
          (org.apache.hadoop.hbase.client Put Get HTable)
          (org.apache.hadoop.hbase.util Bytes)))

(require '(org.rathore.amit [capjure :as cap]))

(defn hbase-table [table-name]
  (HTable. (HBaseConfiguration.) table-name))

(defn add-to-put [p object column-family]
  (let [name-of (fn [x] (if (keyword? x) (name x) (str x)))]
    (doseq [[k v] object]
      (.add p (Bytes/toBytes column-family) (Bytes/toBytes (name-of k)) (Bytes/toBytes (str v))))))

(defn put-in-table [object table-name column-family row-id]
  (let [table (hbase-table table-name)
        p (Put. (Bytes/toBytes row-id))]
    (add-to-put p object column-family) 
    (.put table p)))

(defn print-from-table [table-name row-id column-family]
  (let [table (hbase-table table-name)
        g (Get. (Bytes/toBytes row-id))
        r (.get table g)
        nm (.getFamilyMap r (Bytes/toBytes column-family))]
    (doseq [[k v] nm]
      (println (String. k) ":" (String. v)))))


(def painting 
     {:trees ["cedar", "maple", "oak"],
      :houses 4,
      :cars [{:make "honda", :model "fit", :license  "ah12001"},
             {:make  "toyota", :model  "yaris", :license  "xb34544"}],
      :road {:name "101N" :speed 65}
})

