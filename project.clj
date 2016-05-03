(defproject mocko "0.2.0"
  :description "A simple mocking library."
  :url "https://github.com/codahale/mocko"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]]
  :deploy-repositories [["releases" :clojars]
                        ["snapshots" :clojars]]
  :profiles {:dev [:project/dev :profiles/dev]
             :test [:project/test :profiles/test]
             :profiles/dev {}
             :profiles/test {}
             :project/dev {:source-paths ["dev"]
                           :repl-options {:init-ns user}}
             :project/test {:dependencies []}})
