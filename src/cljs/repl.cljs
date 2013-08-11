(ns nanfott.repl
 (:require [clojure.browser.repl :as repl]))
(def ret (repl/connect "http://localhost:9000/repl"))
