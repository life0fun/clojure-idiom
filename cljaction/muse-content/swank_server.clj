;Start Swank server
(require 'swank.swank)
(swank.swank/ignore-protocol-version "2009-03-09")
(swank.swank/ignore-protocol-version "2009-09-28")
(swank.swank/start-server "/tmp/swank.port" :port 4005 :dont-close true)

(println "Started on port" 4005)