(defproject htm-mnist "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.nfrac/comportex "0.0.14-SNAPSHOT"]
                 [org.numenta/sanity "0.0.14-SNAPSHOT"]
                 [org.clojure/data.priority-map "0.0.7"]
                 [org.clojure/data.codec "0.1.0"]
                 [hiccup "1.0.5"]
                 ]
  :source-paths ["src/clojure"]
  :java-source-paths ["src/java"]
  :repl-options {;:nrepl-middleware [cemerick.piggieback/wrap-cljs-repl]
                 :init-ns org.numenta.sanity.comportex.launchpad}
  )
