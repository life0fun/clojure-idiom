;; clojure io
;; (load-file "io.clj")

;; when defining ns, include only the references that are used.
;;:exclude, :only, :as, :refer-clojure, :import, :use, :load, and :require.
;; ;use naked could corrupt the namespace.  (:use :only)
;; :import working with java deftype defrecord
;; :refer
;; (ns my-ns
;;   (:refer-clojure :exclude [defstruct])
;;   (:use (clojure set xml))  ;; use other namespace without namespace qualification.
;;   (:use [clojure.java.io])
;;   (:use [clojure.test :only (are is)])
;;   (:require (clojure [zip :as z]))
;;   (:import (java.util.Collection)))


;; get the current directory
(System/getProperty "user.dir")

(def filename "/Users/e51141/macsrc/clj/io.clj")

;; slurp a file into mem
(slurp "/Users/e51141/macsrc/clj/io.clj")

;; read a file line by line
(with-open [rdr (reader filename)]
  (doseq [line (line-seq rdr)]
      (println line)))

(with-open [wrtr (writer "/tmp/test.txt")]
  (.write wrtr "Line to be written"))

(with-open [wrtr (writer "/tmp/test.txt" :append true)]
  (.write wrtr "Line to be appended"))

(defn fetch-url[address]
  (with-open [stream (.openStream (java.net.URL. address))]
    (let [buf (java.io.BufferedReader.
          (java.io.InputStreamReader. stream))]
      (apply str (line-seq buf)))))

(fetch-url "http://google.com")


;;
;; fetch binary data
(defn fetch-data [url]
  (let [con    (-> url java.net.URL. .openConnection)
        fields (reduce (fn [h v] (assoc h (.getKey v) (into [] (.getValue v)))) {} (.getHeaderFields con))
        size   (first (fields "Content-Length"))
        in     (java.io.BufferedInputStream. (.getInputStream con))
        out    (java.io.BufferedOutputStream. (java.io.FileOutputStream. "out.file"))
        buffer (make-array Byte/TYPE 1024)]
    (loop [g (.read in buffer) r 0]
      (if-not (= g -1)
        (do
          (println r "/" size)
          (.write out buffer 0 g)
          (recur (.read in buffer) (+ r g)))))
    (.close in)
    (.close out)
    (.disconnect con)))

(fetch-data "http://google.com")


;; deal with socket directly
(defn socket [host port]
  (let [socket (java.net.Socket. host port)
        in (java.io.BufferedReader. (java.io.InputStreamReader. (.getInputStream socket)))
        out (java.io.PrintWriter. (.getOutputStream socket))]
    {:in in :out out}))

(def conn (socket "irc.freenode.net" 6667))
(println (.readLine (:in conn)))
