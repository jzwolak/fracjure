(defproject fractal "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/tools.logging "0.3.0"]
                 [com.oracle/javafx-runtime "2.2.45"]
                 [com.taoensso/timbre "3.2.1"]]
  :main ^:skip-aot fractal.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
