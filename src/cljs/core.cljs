(ns nanfott.core
  (:require [cljs.core.async :refer [>! <! chan put! take! timeout close! dropping-buffer]]
            [cljs.core.match]
            [goog.dom :as dom]
            ;[clojure.browser.dom :as dom]
            [dommy.utils :as utils]
            [dommy.core :as dmy]
            [dommy.attrs :as attrs]
            [clojure.string :as str]
            [clojure.browser.repl :as repl])
  (:use-macros
   [dommy.macros :only [sel sel1 node]]
   [cljs.core.match.macros :only [match]]
   )
  (:require-macros [cljs.core.async.macros :refer [go alt!]])
)

(defrecord Attr [x y z size rotation color])

(def default-shape (Attr. 100 100 0 100 0.0 "red"))

(def box (dommy.macros/sel1 "#outerBox"))

(defn getById [id] (or  (and id (dommy.macros/sel1 (str "#" id)))
                        (let [el (dommy.macros/node [:div {:id (str id)}])]
                          (dmy/append! box el)
                          el)))

(def dom-mutations (chan (dropping-buffer 1000)))

(go (while true (let [job (<! dom-mutations)]
                  (.log js/console (str "Got " job))
                  (condp = (:task job)
                    :set-style (let [element (getById (:element job))
                                     style (:style job)]
                                 (.log js/console "Setting style")
                                 (apply (partial dmy/set-style! element) style))
                    :pause     (let [t (:msec job)]
                                 (.log js/console (str "Pausing for " t " msec"))
                                 (<! (timeout t))
                                 (.log js/console "Paused")
                                 )))))


(defprotocol Shape
  "Maintain pretty shapes"
  (build [shape])
  (update [shape attr])
  (archname [shape]))


(defrecord Triangle [attr element]
  Shape
  (build [shape]
   (let [{:keys [element attr]} shape
        {:keys [x y z size rotation color]} attr
        style    (concat ["width"   "0"
                          "height"  "0"
                          "left"    (str x)
                          "bottom"  (str y)
                          "z-index" (str z)
                          "border-bottom" (str size "px solid " color)
                          "border-left"   (str (/ size 2) "px solid transparent")
                          "border-right" (str (/ size 2) "px solid transparent")
                          "position" "absolute"
                          "-webkit-transition" " width 0.5s ease, height 0.5s ease"
                          "-webkit-transform" (str "rotate(" rotation "deg) ")])
        element  (or element (gensym))]
     (go (>! dom-mutations {:task :set-style :element element :style style}))
     (->Triangle attr element)))
  (update [shape attr]
    (build (->Triangle attr (:element shape))))
  (archname [shape] "triangle")
)


(def triangle ->Triangle)

(defrecord Circle [attr element]
  Shape
  (build [shape]
   (let [{:keys [element attr]} shape
        {:keys [x y z size rotation color]} attr
        style    (concat ["width"   (str size)
                          "height"  (str size)
                          "z-index" (str z)
                          "background" color
                          "-webkit-border-radius" (str (/ size 2))
                          "moz-border-radius" (str (/ size 2))
                          "border-radius" (str (/ size 2))
                          "position" "absolute"
                          "left"    (str x)
                          "bottom"  (str y)
                          "-webkit-transition" " width 0.5s ease, height 0.5s ease"
                          "-webkit-transform" (str "rotate(" rotation "deg) ")])
        element  (or element (gensym))]
     (go (>! dom-mutations {:task :set-style :element element :style style}))
     (->Circle attr element)))
  (update [shape attr]
    (build (->Circle attr (:element shape))))
  (archname [shape] "circle")
)
(def circle ->Circle)


(defrecord Square [attr element]
  Shape
  (build [shape]
   (let [{:keys [element attr]} shape
        {:keys [x y z size rotation color]} attr
        style    (concat ["width"   (str size)
                          "height"  (str size)
                          "z-index" (str z)
                          "background" color
                          "position" "absolute"
                          "left"    (str x)
                          "bottom"  (str y)
                          "-webkit-transition" " width 0.5s ease, height 0.5s ease"
                          "-webkit-transform" (str "rotate(" rotation "deg) ")])
        element  (or element (gensym))]
     (go (>! dom-mutations {:task :set-style :element element :style style}))
     (->Square attr element)))
  (update [shape attr]
    (build (->Square attr (:element shape))))
  (archname [shape] "square")
)
(def square ->Square)

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

(def archetypes {"triangle" ->Triangle "circle" ->Circle "square" ->Square} )
(defn make [fctyp] 
  "fctyp is either a factory function or a string representing an archetype"
  (if-let [fcty  (if (fn? fctyp) fctyp (archetypes fctyp))]
    (let  [empty (fcty default-shape nil)
           shape (build  empty)
           arch  (archname shape)]
      (swap! items #(assoc % "it" shape arch shape))
      (str "We made a " fctyp))
    "We don't know how to make that"))

(defn rename [old new]
  "move shape from items register under old name to new name"
  (swap! items (fn [m] (let [s (m old)] (dissoc m old) (assoc m new s))))
  (str old " is now named " new))

(def it "it")

(defn sgn [i] (cond (> i 0) 1 (< i 0) -1 :else 0))

(defn parseNumber [amt]
  (let [x (js/parseFloat amt)]
    (if (js/isNaN x) (throw (js/Error. "not a number!")) x)))

(defn adjust-amt [[f k v] [amt] scale]
  "If amt is specified, change v to signum(v)*int(amt)"
  (if (nil? amt) [f k v]
      (try (let [x (int (* scale (sgn v) (parseNumber amt)))] [f k x])
           (catch js/Object e (throw (js/Error. (str amt " is not a number:" e)))))))

(defn maybeParseNumber [amt] (try (parseNumber amt) (catch js/Object e amt)))

(def up "up")
(def down "down")
(def right "right")
(def left "left")
(def dirs {"up" [+ :y 100] "down" [- :y 100] "left" [- :x 100] "right" [+ :x 100]
           "closer" [+ :z 1] "further" [- :z 1] "back" [- :z 1]})

(defn move [kshape dir & amt]  (adjust-shape kshape (adjust-amt (dirs dir) amt 10)))

(def rotns {"right" [+ :rotation 22.5] "left" [- :rotation 22.5]})

(defn turn [kshape dir & amt]
  (if-let [d (rotns dir)]
    (adjust-shape kshape (adjust-amt (rotns dir) amt 1.0))
    (throw (js/Error. (str "We don't know how to turn " dir)))))

(defn resize [kshape multiple pwr]
  (let [pwr      (if (nil? pwr) 1.0 (parseNumber pwr))
        multiple (Math/pow multiple pwr)]
    (adjust-shape kshape [* :size multiple])))

(defn grow [kshape & [pwr]] (resize kshape 1.1 pwr))
(defn shrink [kshape & [pwr]] (resize kshape 0.9 pwr))

(defn newv [a b] b)

(def red "red")
(def blue "blue")
(def green "green")
(def yellow "yellow")

(defn color [shape c & x] 
  (if (seq x) (throw (js/Error. (str "What do you mean by " x)))
      (adjust-shape shape [newv :color c])))

(defn pause [msec]
  (let [sec (parseNumber msec)]
    (go (>! dom-mutations {:task :pause :msec msec}))))


(def verbs {"make" make
            "move" move
            "turn" turn
            "name" rename
            "call" rename
            "paint" color 
            "color" color
            "grow" grow
            "shrink" shrink
            "pause" pause
            })

(defn do-something [line] 
  "sugar for people who don't like to type parentheses.  Check only that
the first string is a known verb, and the second exists."
  (if (= (first line) "(") (parse line)
      (let [line (str/lower-case line)
            [verb arg & moreargs] (str/split line #"\s+")
            fn   (get verbs verb)]
        (cond
         (nil? fn) (str "We don't know how to " verb ".")
         (nil? arg) (str verb " what?")
         :else (apply fn arg moreargs)))))

(def input (sel1 :#input))
(def output (sel1 :#output))

(defn events [el type]
  (let [out (chan)]
    (dmy/listen! el type (fn [e] (put! out e)))
    out))

(defn eval-input []
  (let [line (.-value input)
        cur  (.-value output)
        res  (if (> (count line) 0)
               (try  (do (.log js/console "About to process: " line)
                         (do-something line) )
                     (catch js/Object e (str e)))
               "Can I help you?")]
                                        ;(.log js/console (str "Status: " cur " " res))
    (set! (.-value output) (str cur "\n" line "\n  " res))
    (set! (.-value input) "")
    (set! (.-scrollTop output) (.-scrollHeight output))
    )
  )

(defn start []
  (.log js/console "Starting")
  (let [c (events input :keydown)]
    (go (while true
          (let [v (<! c)
                v (.-keyCode v)]
            (if (= v 13) (eval-input)))))))


(defn tokenize [line] 
  (filter #(> (count %) 0) (str/split
                            (str/replace line #"\s*([\(\)])\s*" " $1 ")
                            #"\s+")))

(defn recursive-eval [x]
  (if (seq? x) (apply (first x) (map recursive-eval (rest x)))
      x))

(defn make-function [& x] 
  (fn [] (last (map recursive-eval x))))

(def known-tokens (merge verbs {"+" + "-" - "*" * "/" / "str" str "fn" make-function
                                "repeatedly" repeatedly}))

(defn read-tokens [acc tokens in-func]
  (loop [acc            acc
         [token & rest] tokens]
    (condp  = token 
      nil      acc
      ")"      [(reverse acc) rest]
      "("      (let [[[verb & other] rest] (read-tokens '() rest (= "fn" (first rest)))
                     sub         (if in-func (conj other verb) (apply verb other))
                     acc         (conj acc sub)]
                 (recur acc rest))
      (recur (conj acc (or (known-tokens token) (maybeParseNumber token))) rest))))

(defn parse [s] (read-tokens () (tokenize s) false))

;looping
;(go (while true (let [x (<! c)] (.log js/console x) (if (> x 0) (go (>! c (dec x)))))))

