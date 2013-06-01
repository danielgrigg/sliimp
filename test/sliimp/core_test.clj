(ns sliimp.core-test
  (:use midje.sweet
        sliimp.core))

(fact "`clip` clips two rectangles"
      (clip (rect :x 23 :y 47 :width 34 :height 57)
            (rect :x 23 :y 47 :width 34 :height 57)) 
      => (rect :x 23 :y 47 :width 34 :height 57))