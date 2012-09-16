(defproject crux "0.1.0-SNAPSHOT"
  :description "Focus on the crux of the matter."
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [org.clojure/test.generative "0.1.4"]
                 [clj-time/clj-time "0.4.3"]
                 [slingshot "0.10.3"]
                 [ring-mock "0.1.1"]
                 [savant-core "0.1.0-SNAPSHOT"]
                 [dalap "0.0.1-SNAPSHOT"]
                 [birdseye "1.0.0-SNAPSHOT"]]
  :plugins [[s3-wagon-private "1.1.2"]
            [sjl/lein2-generative "0.1.4.2"]]
  :repositories {"snapshots" {:url "s3p://lein-snapshots/snapshots"}})
