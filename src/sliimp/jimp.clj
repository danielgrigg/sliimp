;; (ns sliimp.jimp
;;   (:gen-class)
;;     (:use sliimp.core)
;;     (:use sliimp.film))
;; 
;; (set! *warn-on-reflection* true)
;; 
;; (defn test1 [w h]
;;   (spit-film 
;;    (image-process 
;;     (film {:bounds (rect {:width w :height h})}) 
;;      (fn [x y] (pixel [(Math/sin (* 0.01 x 0.01 y))
;;                        (Math/sin (* 0.03 x (* 0.02 (rand)) y))
;;                        0; (Math/sin (* 0.03 x 0.03 y))
;;                        ] 1))) 
;;     "/tmp/foo.exr"))
;; 
;; (defn bench1 [n]
;;   (let [w 512
;;         h 512
;;         P (float-array (* w h 4) (float 0))]
;;     (dotimes [i n]
;;       (doseq [y (range h) x (range w)]
;;         (let [pidx (int (+ (* 4 w y) (* 4 x)))]
;;           (aset-float P pidx (float (Math/sin (* 0.03 0.03 x y))))
;;           (aset-float P (int (+ pidx 1)) (float 0))
;;           (aset-float P (int (+ pidx 2)) (float 0))
;;           (aset-float P (int (+ pidx 3)) (float 1)))))
;;     (write-exr {:path "/tmp/bench.exr" :width w :height h :pixels P})))
;;                 
;; 
;; (defn -main [& args]
;;   (let [n (read-string (or (first args) "10" ))
;;         f (film {:bounds (rect {:width 512 :height 512})})
;;         boxesfn (fn [x y] (pixel [(Math/sin (* 0.03 0.03 x y)) 0.0 0.0] 1.0))]
;;     (do
;;       (println "Benching" n "times")
;;       ;      (bench1 n))))
;;       (dotimes [i n]
;;         (image-process f boxesfn)))))
;; 
;; 
