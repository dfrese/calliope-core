(defproject de.active-group/calliope-core "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies [[org.clojure/clojure "1.8.0" :scope "provided"]
                 [org.clojure/clojurescript "1.9.229" :scope "provided"]
                 [de.active-group/edonus "0.1.0-SNAPSHOT"]
                 [de.active-group/orpheus-core "0.1.0-SNAPSHOT"]]

  :plugins [[lein-cljsbuild "1.1.4"]
            [lein-doo "0.1.7"]
            [lein-codox "0.10.1"]]

  :cljsbuild
  {:builds {:test
            {:source-paths ["src" "test"]
             :compiler {:output-to "target/test.js"
                        :output-dir "target"
                        :optimizations :none
                        :main calliope.runner}}}}

  :doo {:build "test"}

  :codox {:language :clojurescript
          :metadata {:doc/format :markdown}}
  )
