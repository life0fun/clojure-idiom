(defproject trident-clj "1.0.0-SNAPSHOT"
  :description "storm trident with redis storage in clojure"
  :repositories {"local" ~(str (.toURI (java.io.File. "local_jars")))}
  :source-paths ["src"]  ; where the namespace directory starts
  :test-paths ["test"]
  :dependencies [       ; lein2, all deps is under maven repo, $HOME/.m2/repository
    [org.clojure/tools.logging "0.2.6"]     ; logging
    [korma "0.3.0-RC5"]    ; awesome korm
    [clj-redis "0.0.12"]   ;
    [org.clojure/data.json "0.2.2"]    ; json package
  ]
  ; activate profiles to config projects. settings in certain profile is not propagated
  ; downstream to projects that deps on your proj.
  ; use with-profile to select certain profile to run on.
  :profiles {:dev 
              {:dependencies [[storm "0.9.0-wip15"]
                      [org.clojure/clojure "1.4.0"]]}}
  ; to run lein do clean, with-profile dev compile, with-profile dev run.
  :main trident-clj.core
  :warn-on-reflection true
  ; may just :aot :all
  :aot [trident-clj.core      
        trident-clj.tweet-spout
        trident-clj.persister
        trident-clj.prn-filter]
  :min-lein-version "2.0.0")