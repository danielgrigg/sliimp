(ns sliimp.jimp
    (:use sliimp.core)
    (:use sliimp.film))

(defn test1 [w h]
  (spit-film 
   (image-process 
    (film {:bounds (rect {:width w :height h})}) 
    (fn [x y] (pixel [(Math/sin (* 0.01 x 0.01 y))
                      (Math/sin (* 0.03 x (* 0.02 (rand)) y))
                    0; (Math/sin (* 0.03 x 0.03 y))
                    ] 1))) 
   "/tmp/foo.exr"))

;(def P (float-array (* 2048 2048 4) (float 0)))
;(doseq [y (range 2048) x (range 2048)];
;		    (aset-float P (+ (* 4 y 2048) (* 4 x) 0) (float (Math/sin (* 0.003 0.003 x y))))
;		    (aset-float P (+ (* 4 y 2048) (* 4 x) 1) (float 0))
;		    (aset-float P (+ (* 4 y 2048) (* 4 x) 2) (float 0))
;		    (aset-float P (+ (* 4 y 2048) (* 4 x) 3) (float 1)))
		    