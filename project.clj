(defproject nanfott "0.1.0-SNAPSHOT"
  :description "A ntc programming language for Nan"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :repositories {"sonatype-staging"
                 "https://oss.sonatype.org/content/groups/staging/"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/clojurescript "0.0-1847"]
                 [org.clojure/core.async "0.1.0-SNAPSHOT"]
                 [com.cemerick/piggieback "0.0.5"]
                 ;[org.clojure/core.match "0.2.0-rc5"]
                 ; Attempts to get repl working correctly.  
                 ;[org.clojure/google-closure-library-third-party "0.0-2029"]
                 ;[org.clojure/google-closure-library "0.0-2029-2"]
                 ;[org.clojure/google-closure-library "0.0-20130212-95c19e7f0f5f"]
                 [compojure "1.1.5"]
                 [ring "1.2.0"]
                 ;[ring/ring-core "1.2.0"]
                 ;[ring/ring-devel "1.2.0"]
                 ;[http-kit "2.1.8"]
                 ]

  :repl-options {:nrepl-middleware [cemerick.piggieback/wrap-cljs-repl]}
  :injections [(require '[cljs.repl.browser :as brepl]
                        '[cemerick.piggieback :as pb])
               (defn browser-repl []
                 (pb/cljs-repl :repl-env
                               (doto (brepl/repl-env :port 9000)
                                      cljs.repl/-setup)))]


  :plugins [[lein-cljsbuild "0.3.2"]
            [lein-ring "0.8.5"]]

  :ring {:handler nanfott.core/handle}


  ;:main zzzzzz

  :source-paths ["src/clj"]

  :cljsbuild 
  {:builds
   [{:id "nanfott"
     :source-paths ["src/cljs"]
     :compiler {:optimizations :whitespace
                :pretty-print true
                :output-dir "out" 
                :output-to "resources/public/js/main.js"
                }}]}
)

; lein cljsbuild once
;                auto
;                clean
; lein trampoline cljsbuild repl-rhino
