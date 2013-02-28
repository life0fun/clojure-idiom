(defn normalize-fns [body]
    (for [[name args & impl] body             
        :let [args (-> "this"               
                        gensym               
                       (cons args)          
                       vec)]]                                                                                                                               
      (concat [name args] impl)
      ))        

(defmacro defspout [name output-spec & [opts & impl :as all]]
  (println opts " : impl= " impl " all= ", all)
  (if-not (map? opts)
    `(defspout ~name ~output-spec {} ~@all)
    (let [worker-name (symbol (str name "__"))
          conf-fn-name (symbol (str name "__conf__"))
          params (:params opts)
          conf-code (:conf opts)
          prepare? (:prepare opts)
          prepare? (if (nil? prepare?) true prepare?)
          fn-body (if prepare?
                    (cons 'fn impl)
                    (let [[args & impl-body] impl
                          coll-sym (first args)
                          prepargs [(gensym "conf") (gensym "context") coll-sym]]
                      `(fn ~prepargs (spout (~'nextTuple [] ~@impl-body)))))
          definer (if params
                    `(defn ~name [& args#]
                       (println ~output-spec ~worker-name ~conf-fn-name args#))
                    `(def ~name
                       (println ~output-spec ~worker-name ~conf-fn-name []))
                    )
          ]
      (println "name=" name)
      (println "params=" params)
      (println "output-spec=" output-spec)
      (println "impl=" impl)
      (println "fn-body=" fn-body)
      (println "conf-fn-name=" conf-fn-name)
      (println "=------" )
      ;(println definer )
      `(do
         (defn ~conf-fn-name ~(if params params []) 
           ~conf-code                 
           )                          
         (defn ~worker-name ~(if params params []) 
           ~fn-body                   
           )                          
         ~definer                     
         ))))                 

(defmacro spout [& body]
  (let [[spout-fns other-fns] (split-with #(not (symbol? %)) body)
        fns (normalize-fns spout-fns)]
    `(reify ISpout
        ~@fns
        ~@other-fns)))



(defspout sentence-spout ["sentence"]
  [conf context collector]
  (let [sentences ["a little brown dog"
                   "the man petted the dog"
                   "four score and seven years ago"
                   "an apple a day keeps the doctor away"]]
     (println sentences)
  ))

(defspout sentence-spout-parameterized ["word"] {:params [sentences] :prepare false}
    [collector]
    (Thread/sleep 500)
    (println collector [(rand-nth sentences)]))
