(defproject wordcount "1.0.0-SNAPSHOT"
  :description "first project for word count"
  :source-path "src"
  :aot :all
  :repositories { }
  :dependencies [
                 [org.clojure/clojure "1.4.0"]
                 [commons-collections/commons-collections "3.2.1"]
                ]
  :dev-dependencies [
                      [storm "0.8.2"]
                    ]
  )
