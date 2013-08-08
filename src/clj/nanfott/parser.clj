(ns nanfott.parser 
  (:require [instaparse.core :as insta]))


;; Very silly parser.  All expressions are of the form

;(def )

;(def )


(def my-parse (insta/parser "L = VERB <SP> OBJ *
SP = #'\\s+'
VERB = 'make' | 'paint'
OBJ = SHAPE / NAME
SHAPE = 'triangle' | 'square'
NAME = #'\\w+'
"))
