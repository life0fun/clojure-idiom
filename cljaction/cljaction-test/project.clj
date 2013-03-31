;;
;; http://www.unexpected-vortices.com/clojure/brief-beginners-guide/libs-management-and-use.html
;; http://www.pgrs.net/2011/10/30/using-local-jars-with-leiningen/
;;
;; path to resolve lib. Lib format: group-id/artifact-id, version string.
;;  central (http://repo1.maven.org/maven2),
;;  local (file:$HOME/macsrc/clj/cljaction/cljaction-test/local_jars/),
;;  clojars (http://clojars.org/repo/)
;;
;; add lib coordinates (artifact-id and (if differing) group-id) go into your project.clj’s :dependencies vector.
;; require the package into ns macro’s :require list. (ns package.namespace) = (src/package/namespace)
;;
(defproject cljaction-test "1.0.0-SNAPSHOT"
  :description "executing clojure in action source code"
  :repositories {"local" ~(str (.toURI (java.io.File. "local_jars")))}
  :dependencies [[org.clojure/clojure "1.3.0"]
                 [org.clojure/java.jdbc "0.2.3"]
                 [org.clojure/clojure-contrib "1.2.0"]
                 [clojure-rabbitmq "0.2.1"]
                ]
  :main cljaction-test.core)

