(defproject comp-bio "0.1.0-SNAPSHOT"
  :description "comp-bio under lein"
  :url ""
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
  				[org.clojure/math.combinatorics "0.0.3"]]
  :main ^:skip-aot comp-bio.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
