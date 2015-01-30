(ns clojure-programming-wilson.core
  (:require [clojure-programming-wilson.wilson :as wilson]))

(defn -main [& args]
  (if args
    (wilson/run (read-string (first args)))
    (println "Usage: lein run USEWILSON
              
                  USEWILSON = true  (Wilson's algorithm)
                              false (Clojure Programming variant)")))
