(defproject lcug2 "0.1.0-SNAPSHOT"
  :description "Lyon Clojure User Group #2 : Transducers"
  :url "https://github.com/leonoel/lcug2"
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/core.async "0.2.395"]
                 [clj-http "3.4.1"]
                 [enlive "1.1.6"]]
  :profiles
  {:dev
   {:dependencies [[ring/ring-core "1.5.0"]
                   [ring/ring-jetty-adapter "1.5.0"]]}})
