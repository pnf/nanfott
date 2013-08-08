(ns nanfott.core
  (:require ;[cljs.core.async :refer [>! <! chan put! take! timeout close!]]
            [goog.dom :as dom]
            ;[clojure.browser.dom :as dom]
            [dommy.utils :as utils]
            [dommy.core :as dmy]
            [dommy.attrs :as attrs]
            [clojure.string :as str]
            [clojure.browser.repl :as repl])
  (:use-macros
   [dommy.macros :only [sel sel1 node]]
   )
  ;(:require-macros [cljs.core.async.macros :refer [go alt!]])
)

;; (def x (dmy/set-style! (node [:div {:id "dot"}]) "z-index" "1" "-webkit-transition" "left 2s ease, top 2s ease"   "width" "10px" "height" "10px" "background-color" "red" "position" "absolute" "left" "30px" "top" "30px"))
;; (dommy/append! (sel1 :body) x)
;; (def x (dmy/set-style! x "left" "30px" "top" "30px"))
;;                                         ;triangle
;; (def x (dmy/set-style! x "width" "0" "height" "0" "border-bottom" "120px solid green" "border-left" "60px solid transparent" "border-right" "60px solid transparent"))
;; (.-value (aget (.-attributes x) 1))
;; (.-value (aget (.-attributes (second (dmy/ancestor-nodes x))) 0))
;; (sel1 :#outerBox)
;; (sel1 "#outerBox")
;; (sel1 :#text )

(defrecord Attr [x y size rotation color])

(def default-shape (Attr. 100 100 100 0.0 "red"))

(def box (dommy.macros/sel1 "#outerBox"))

(defprotocol Shape
  "Maintain pretty shapes"
  (build [shape])
  (update [shape attr])
  (archname [shape]))

(defrecord Triangle [attr element]
  Shape
  (build [shape]
   (let [{:keys [element attr]} shape
        {:keys [x y size rotation color]} attr
        element  (or element (dommy.macros/node [:div {:id (str (gensym))}]))
        style    (concat ["width"   "0"
                          "height"  "0"
                          "left"    (str x)
                          "bottom"  (str y)
                          "border-bottom" (str size "px solid " color)
                          "border-left"   (str (/ size 2) "px solid transparent")
                          "border-right" (str (/ size 2) "px solid transparent")
                          "position" "absolute"
                          "-webkit-transition" " left 2s ease, top 2s ease"
                          "-webkit-transform" (str "rotate(" rotation "deg) ")])
        element           (apply (partial dmy/set-style! element) style)]
    (->Triangle attr element)))
  (update [shape attr]
    (build (->Triangle attr (:element shape))))
  (archname [shape] "triangle")
)

(def triangle ->Triangle)

(def items (atom {}))

; like merge-with, which doesn't work for records
(defn adjust-attr [a fkvs]
  "a is an Attr, fkvs is a sequence of triples function, key, delta"
  (reduce (fn [acc [f k v]]
            (if-let [v0 (get acc k)]
            (assoc acc k (f v0 v))
            (assoc acc k v)))
          a
          fkvs))

(defn adjust-shape [k & fkvs]
  "k = key in items map; fkv = [f k v] per adjust-attr"
  (let [shape                  (get @items k)
        {:keys [element attr]} shape
        attr                   (adjust-attr attr fkvs)
        shape                  (update shape attr)]
    (swap! items #(assoc % k shape "it" shape))
    shape))

;(defn update-shape [shape & fvs] (adjust-shape shape identity fvs))

(def archetypes {"triangle" ->Triangle})
(defn make [fctyp] 
  "fctyp is either a factory function or a string representing an archetype"
  (let [fcty  (if (fn? fctyp) fctyp (archetypes fctyp))
        empty (fcty default-shape nil)
        shape (build  empty)
        arch  (archname shape)]
        (dmy/append! box (:element shape))
    (swap! items #(assoc % "it" shape arch shape))
    shape))

(defn rename [old new]
  "move shape from items register under old name to new name"
  (swap! items (fn [m] (let [s (m old)] (dissoc m old) (assoc m new s)))))

(def it "it")

(def up "up")
(def down "down")
(def right "right")
(def left "left")
(def dirs {"up" [+ :y 100] "down" [- :y 100] "left" [- :x 100] "right" [+ :x 100]})
(defn move [kshape dir]  (adjust-shape kshape (dirs dir)))

(def rotns {:right [+ :rotation 22.5] :left [- :rotation 22.5]})
(defn turn [shape dir] (adjust-shape shape (rotns dir)))

(defn newv [a b] b)

(def red "red")
(def blue "blue")
(def green "green")
(def yellow "yellow")
(defn color [shape c] (adjust-shape shape [newv :color c]))

(def verbs {"make" make "move" move "turn" turn})

(defn do-something [line] 
  "sugar for people who don't like to type parentheses.  Check only that
the first string is a known verb, and the second exists."
  (let [line (str/lower-case line)
        [verb arg & moreargs] (str/split line #"\s+")
        fn   (get verbs verb)]
    (cond
     (nil? fn) (str "We don't know how to " verb ".")
     (nil? arg) (str verb " what?")
     :else (apply fn arg moreargs)
     )
    ))
