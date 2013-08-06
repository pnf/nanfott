(ns nanfott.repl
 (:require [clojure.browser.repl :as repl]))

(.write js/document "\nAbout to connect to localhost:9000\n")
(def ret (repl/connect "http://localhost:9000/repl"))
(.write js/document (str "I am supposed to have connected:" ret))
