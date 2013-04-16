(defproject wordcount "1.0.0-SNAPSHOT"
  :description "first project for word count"
  :source-path "src"
  :aot :all
  :repositories {"local" ~(str (.toURI (java.io.File. "local_jars")))}
  :dependencies [ [org.clojure/clojure "1.4.0"]
                 [commons-collections/commons-collections "3.2.1"] ]
  :dev-dependencies [[storm "0.8.2"]
                     [org.clojure/clojure-contrib "1.2.0"]]
  :main wordcount.core)
