 (ns sliimp.film
   (:gen-class)
   (:use slimath.core)
   (:use sliimp.core)
   (:require slicna.core)
   (:import sliimp.core.Rect))


(defrecord Pixel [^float x ^float y ^float z ^float w])

(defn pixel 
   ([^double x ^double y ^double z]
      (Pixel. x y z 1.0))
   ([^double x ^double y ^double z ^double weight]
      (Pixel. x y z weight)))

;; Capture the type-hinting name for an array of class c
(defmacro array-type [cname & args]
  `(-> (new ~cname ~@args) list into-array class .getName)) 

(defrecord Film [^Rect bounds #^"[Lsliimp.film.Pixel;" pixels])

(defn funky [w h]
  (let [#^"[Lsliimp.film.Pixel;" ps (make-array Pixel (* w h))] 
    (amap ps idx ret (pixel 0.0 0.0 0.0 0.0))))

(defn film [& {:keys [bounds clear-colour]}]
   "Create a film, using a blank pixel array if pixels is missing."
   (let [#^"[Lsliimp.film.Pixel;" ps (make-array Pixel (* (height bounds) (width bounds)))
         ^Pixel p clear-colour] 
     (Film. bounds (amap ps idx ret p))))


 
;; (defn pixel-black [] (Pixel. [0.0 0.0 0.0] 1.0))
;; 
;; (defn add-pixel [^Pixel p ^Pixel q]
;;   (Pixel. (v3add (:xyz p) (:xyz q)) (+ (:weight p) (:weight q))))
;; 
;; (defn pixel-colour [^Pixel p]
;;   (v3muls (:xyz p) (:weight p)))
;; 
;; (defn normalize [^Pixel p]
;;   (if (< (:weight p) 0.0001)
;;     (pixel-black)
;;     (Pixel. (v3muls (:xyz p) (/ (:weight p))) 1.0)))
;; 

;; 

;; 
;; (defn pixel-idx [^Film f ^long x ^long y]
;;   (+ (* y (int (width (:bounds f)))) x))
;; 
;; (defn get-pixel [^Film f ^long x ^long y]
;;   ((:pixels f) (+ (* y (int (width (:bounds f)))) x)))
;; 
;; (defn set-pixel [^Film f ^Pixel pxl ^long x ^long y]
;;    (assoc f :pixels 
;;           (assoc (:pixels f) (+ (* y (int (width (:bounds f)))) x) pxl)))
;; 
 (defn write-exr [{:keys [path width height pixels]}]
   (slicna.core/invoke :exru 
                       "write_rgba"
                       Integer
                       width
                       height
                       path
                       pixels))

 (defn spit-film [^Film f path]
   (write-exr {:path path
               :width (int (width (:bounds f)))
               :height (int (height (:bounds f)))
               :pixels (float-array
                        (reduce (fn [acc x] 
                                  (conj acc (:x x) (:y x) (:z x) 1.0)) [] (:pixels f)))}))

(defn test-spit [w h pxl ]
  (spit-film (film :bounds (rect {:width w :height h}) 
                   :clear-colour pxl)
             "/tmp/funk.exr"))

 (defn -main [& args]
   (test-spit 4096 4096 (pixel (rand) (rand) (rand) 1.0)))
                  

;; 
;; (defn spit-film [^Film f path]
;;   (write-exr {:path path
;;               :width (int (width (:bounds f)))
;;               :height (int (height (:bounds f)))
;;               :pixels (float-array 
;;                        (reduce (partial apply conj) []
;;                                (map (fn [p] (conj (:xyz (normalize p)) 1.0)) 
;;                                     (:pixels f))))}))
;; 
;; (defn image-process [image kernel-fn]
;; "Apply kernel-fn to all pixels.  kernel-fn must be a function of 2 arguments, x and y."
;;   (reduce 
;;    (fn [image' [x y]] (set-pixel image' (kernel-fn x y) x y)) 
;;    image
;;    (rect-seq (:bounds image))))
;; 
