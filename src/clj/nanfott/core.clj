(ns nanfott.core
  (:require [cemerick.austin.repls :refer (browser-connected-repl-js)]
            [net.cgrand.enlive-html :as enlive]
            [compojure.route :refer (resources)]
            [compojure.core :refer (GET defroutes)]  
            ring.adapter.jetty
            [clojure.java.io :as io]))                                 

(enlive/deftemplate page
  (io/resource "public/nanfott.html")
  []
  ; inject repl connection code
  [:body] (enlive/append
            (enlive/html [:script (browser-connected-repl-js)])))

(defroutes site
  (resources "/") 
  (GET "/*" req (page)))

(defn run-server
  []
  (defonce ^:private server
    (ring.adapter.jetty/run-jetty #'site {:port 8080 :join? false}))
  server)

(def repl-env (reset! cemerick.austin.repls/browser-repl-env
                      (cemerick.austin/repl-env)))

(defn connect-repl []
  (cemerick.austin.repls/cljs-repl repl-env))


(println "(run-server) to start ring; (connect-repl) to switch to cljs repl")
