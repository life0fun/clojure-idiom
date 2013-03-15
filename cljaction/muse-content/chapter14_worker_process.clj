(ns chapter14-worker-process
  (:use chapter14-rabbitmq chapter14-worker))

(defn response-for [worker-handler worker-args]
  (try
   (let [value (apply worker-handler worker-args)]
     {:value value :status :success})
   (catch Exception e 
     {:status :error})))

(defn process-request [worker-handler worker-args return-q]
  (future 
    (with-rabbit ["localhost" "guest" "guest"]
      (let [response-envelope (response-for worker-handler worker-args)]
        (if return-q (send-message return-q response-envelope))))))

;;
;; worker fn is defn inside defworker
;;   (alter workers assoc worker-name# (fn ~args (do ~@exprs))))
;;
(defn handle-request-message [req-str]
  (try
   (let [req (read-string req-str)
         worker-name (req :worker-name) worker-args (req :worker-args) return-q (req :return-q)
         worker-handler (@workers worker-name)]  ;; where does ref to workers pased in ??
     (if (not (nil? worker-handler))
       (do
         (println "Processing:" worker-name "with args:" worker-args)
         (process-request worker-handler worker-args return-q))))
   (catch Exception e)))

(defn start-handler-process []
  (println "Serving up" (count @workers) "workers.")
  (doseq [request-message (message-seq WORKER-QUEUE)]
    (handle-request-message request-message)))

