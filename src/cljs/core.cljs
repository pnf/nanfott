(ns nanfott.core
  (:require ;[cljs.core.async :refer [>! <! chan put! take! timeout close!]]
            [goog.dom :as dom]
            ;[clojure.browser.dom :as dom]
            [dommy.utils :as utils]
            [dommy.core :as dmy]
            [dommy.attrs :as attrs]
            [clojure.browser.repl :as repl])
  (:use-macros
   [dommy.macros :only [sel sel1 node]]
)
  ;(:require-macros [cljs.core.async.macros :refer [go alt!]])
)

#_ (
(def x (dmy/set-style! (node [:div {:id "dot"}]) "z-index" "1" "-webkit-transition" "left 2s ease, top 2s ease"   "width" "10px" "height" "10px" "background-color" "red" "position" "absolute" "left" "30px" "top" "30px"))
(dommy/append! (sel1 :body) x)

(def x (dmy/set-style! x "left" "30px" "top" "30px"))

;triangle
(def x (dmy/set-style! x "width" "0" "height" "0" "border-bottom" "120px solid green" "border-left" "60px solid transparent" "border-right" "60px solid transparent"))

; TODO: write abstract object so I don't have to query style.  We'll diff to determine
; what style fields to change.  (..-element) will return [map, element]
; 


(.-value (aget (.-attributes x) 1))

(.-value (aget (.-attributes (second (dmy/ancestor-nodes x))) 0))

(sel1 :#outerBox)
(sel1 "#outerBox")
(sel1 :#text )

)



(defrecord Attr [x y size rotation color])

(def default-shape (Attr. 100 100 100 0.0 "red"))

(def box (sel1 "#outerBox"))

(defprotocol Shape
  "Maintain pretty shapes"
  (build [shape])
  (update [shape attr])
)

(defrecord Triangle [attr element]
  Shape
  (build [shape]
   (let [{:keys [element attr]} shape
        {:keys [x y size rotation color]} attr
        element  (or element (node [:div {:id (str (gensym))}]))
        style    (concat ["width"   "0"
                          "height"  "0"
                          "left"    (str x)
                          "bottom"  (str y)
                          "border-bottom" (str size "px solid " color)
                          "border-left"   (str (/ size 2) "px solid transparent")
                          "border-right" (str (/ size 2) "px solid transparent")
                          "position" "absolute"
                          "-webkit-transition" " left 2s ease, top 2s ease"
                          "-webkit-transform" (str "rotate(" rotation "deg) ")
])
        element           (apply (partial dmy/set-style! element) style)]
    (println "hello" element)
    (->Triangle attr element)))
  (update [shape attr]
    (build (->Triangle attr (:element shape))))
)

(defn update-shape [shape & kvs]
  (let [{:keys [element attr]} shape
        attr                   (apply (partial assoc attr) kvs)]
    (println attr)
    (update shape attr)
    )
  )

(defn add-shape [fcty] 
  (let [s (build  (fcty default-shape nil)) ]
    (dmy/append! box (:element s))
    s))
