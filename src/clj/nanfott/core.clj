(ns nanfott.core
  (:use compojure.core)
  (:require  [compojure.handler :as handler]
             [compojure.route :as route])
)

(defroutes app-routes
  ; to serve document root address
  (GET "/" [] "<p>Hello from compojure</p>")
  ; to serve static pages saved in resources/public directory
  (route/resources "/")
  ; if page is not found
  (route/not-found "Page not found"))

(def handle
  (handler/site app-routes))
