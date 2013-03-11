(ns chapter08.stubbing)

(defn stub-fn [return-value]
  (fn [& args]
    return-value))
 
(defmacro stubbing [stub-forms & body]
  (let [stub-pairs (partition 2 stub-forms)
        returns (map last stub-pairs)
        stub-fns (map #(list 'stub-fn %) returns)
        real-fns (map first stub-pairs)]
    `(binding [~@(interleave real-fns stub-fns)]
       ~@body)))