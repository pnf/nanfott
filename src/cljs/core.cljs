(ns nanfott.core
  (:require ;[cljs.core.async :refer [>! <! chan put! take! timeout close!]]
            ;[goog.dom :as dom]
            ;[clojure.browser.dom :as dom]
            [clojure.browser.repl :as repl])
  ;(:require-macros [cljs.core.async.macros :refer [go alt!]])
)

(.write js/document "\nAbout to connect to localhost:9000\n")

(def ret (repl/connect "http://localhost:9000/repl"))


(.write js/document (str "I am supposed to have connected:" ret))
