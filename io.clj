; clojure io
; (load-file "io.clj")

; when defining ns, include only the references that are used.
;:exclude, :only, :as, :refer-clojure, :import, :use, :load, and :require.
; ;use naked could corrupt the namespace.  (:use :only)
; :import working with java deftype defrecord
; :refer
; (ns my-ns
;   (:refer-clojure :exclude [defstruct])
;   (:use (clojure set xml))  ;; use other namespace without namespace qualification.
;   (:use [clojure.java.io])
;   (:use [clojure.contrib.io])  ; (use 'clojure.contrib.io)
;   (:use [clojure.test :only (are is)])
;   (:require [clojure [zip :as z]])
;   (:import [java.util.Collection]))


; get the current directory
(System/getProperty "user.dir")

(def filename "/Users/e51141/macsrc/clj/io.clj")

; open a file, spit content to it, with option, using clojure.java.io/write, then close it.
(spit "/tmp/x" "1. log-1\n" :append true)
(spit "/tmp/x" "2. log-2\n" :append true)

; slurp a file into mem
(slurp "/Users/e51141/macsrc/clj/io.clj")

; transform file as line seq using clojure.java.io/reader
(with-open [rdr (clojure.java.io/reader "/Users/e51141/tmp/x")]
  (printf "%s\n" (clojure.string/join "\n" (line-seq rdr))))


; output stream convert a file to byte output stream.
(:use [clojure.java.io :only [output-stream]])
(defn use-output-stream []
  (with-open [o (output-stream "test.txt")]
    (.write o 65))) ; Writes 'A'

(defn parse-line [line]
  (let [tokens (.split (.toLowerCase line) " ")]
    (map #(vector % 1) tokens)))

(defn sum [[k v]]
  {k (apply + v)})

(defn reduce-parsed-lines [collected-values]
  (apply merge (map sum collected-values)))

(defn combine [mapped]
  (->> (apply concat mapped)
       (group-by first)
       (map (fn [[k v]]
              {k (map second v)}))
       (apply merge-with conj)))

(defn word-frequency [filename]
  (->> (read-lines filename)
       (map parse-line)
       (combine)
       (reduce-parsed-lines)))


;; read a file line by line
(use '[clojure.java.io :only (reader)])
(with-open [rdr (reader filename)]
  (doseq [line (line-seq rdr)]
      (println line)))

(with-open [wrtr (writer "/tmp/test.txt")]
  (.write wrtr "Line to be written"))

(with-open [wrtr (writer "/tmp/test.txt" :append true)]
  (.write wrtr "Line to be appended"))

(defn fetch-url [url]
  (with-open [stream (.openStream (java.net.URL. url))]
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
        in (java.io.java. (BufferedReader.io.InputStreamReader. (.getInputStream socket)))
        out (java.io.PrintWriter. (.getOutputStream socket))]
    {:in in :out out}))

(def conn (socket "irc.freenode.net" 6667))
(println (.readLine (:in conn)))


; parsing logs
(defn request-seq [filename]
  (->> (read-lines filename)
       (drop 2)               ; drop head 2 lines
       (lazy-request-seq)))
cons
;  the result of head (next-log-record hd) to result seq of the rest 
(defn lazy-request-seq [log-lines]
  (lazy-seq
    (let [record (next-log-record log-lines)]
      (if (empty? record)
        nil
        (cons (remove empty? record)
              (lazy-request-seq (drop (count record) log-lines)))))))

(defn next-log-record [log-lines]
  (let [head (first log-lines)
        body (take-while (complement record-start?) (rest log-lines))]
    (remove nil? (conj body head))))



; named arguments by destructure the rest argument with a map
(defn foo [& {:keys [k1 k2 k3]}]
  (prn "calling foo : " (str k1 k2 k3)))
(foo :a :k1 "k" :k2 "b2" :k3 "c3")

(defn blah [& {:keys [key1 key2 key3] :or {key3 10}}] 
  (str key1 key2 key3))

; unamed argument with underscore _
; the underscore is used idiomatically indicates that the argument is not used.


; execute shell command 
(use '[clojure.java.shell :only [sh]])
(sh "ls" "-la")
(ns-unmap 'user 'sh)

(use '[clojure.contrib.shell-out])
(sh "ls" "-la")

; execute shell with java runtime
(import 'java.lang.Runtime)
(let [p (.exec (Runtime/getRuntime) "ls -la")
      br (java.io.BufferedReader. (java.io.InputStreamReader. (.getInputStream p)))]
  ;(map prn (line-seq br)))
  (for [l (line-seq br)]
    (prn l)))


