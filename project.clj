(defproject fractal "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/tools.logging "0.3.0"]
                 [org.openjfx/javafx-base "11.0.2"]
                 [org.openjfx/javafx-controls "11.0.2"]
                 [org.openjfx/javafx-graphics "11.0.2"]
                 [org.openjfx/javafx-swing "11.0.2"]
                 [com.taoensso/timbre "3.2.1"]]
  :main ^:skip-aot fractal.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
