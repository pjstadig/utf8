(defproject pjstadig/utf8 "0.2.0-SNAPSHOT"
  :description "Byte vector backed, utf8 strings for Clojure."
  :url "http://github.com/pjstadig/utf8/"
  :license {:name "Mozilla Public License, v. 2.0"
            :url "http://mozilla.org/MPL/2.0/"}
  :global-vars {*warn-on-reflection* true}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [nio "1.0.1"]]
  :profiles {:dev {:dependencies [[criterium "0.4.2"]]}})
