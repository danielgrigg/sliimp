 (ns sliimp.film
   (:gen-class)
   (:use slimath.core)
   (:use sliimp.core)
   (:require slicna.core)
   (:import sliimp.core.Rect))
;   (:import sliimp.core.Bounded2))


(defrecord Pixel [^float x ^float y ^float z ^float w])

(defn ^Pixel pixel 
   ([^double x ^double y ^double z]
      (Pixel. x y z 1.0))
   ([^double x ^double y ^double z ^double weight]
      (Pixel. x y z weight)))

;; Capture the type-hinting name for an array of class c
(defmacro array-type [cname & args]
  `(-> (new ~cname ~@args) list into-array class .getName)) 

(defrecord Film [^Rect bounds #^"[Lsliimp.film.Pixel;" pixels]
  Bounded2
    (width [this] (int (width bounds)))
    (height [this] (int (height bounds))))


(defn ^Film film [& {:keys [bounds clear-color]}]
   "Create a film, using a blank pixel array if pixels is missing."
   (let [#^"[Lsliimp.film.Pixel;" ps (make-array 
                                      Pixel 
                                      (* (height bounds) (width bounds)))
         ^Pixel p clear-color] 
     (Film. bounds (amap ps idx ret p))))


 (defn ^Pixel add-pixel "Add two pixels" [^Pixel p ^Pixel q]
   (Pixel. (+ (:x p) (:x q))
           (+ (:y p) (:y q))
           (+ (:z p) (:z q))
           (+ (:w p) (:w q))))

 
 (defn ^Pixel normalize "Normalize a pixel" [^Pixel p]
   (if (< (:w p) 0.0001)
     (pixel 0.0 0.0 0.0 0.0)
     (let [inv-w (float (/ (:w p)))]
       (Pixel. (* (:x p) inv-w) 
               (* (:y p) inv-w)
               (* (:z p) inv-w)
               1.0))))

 
 (defn ^long pixel-idx [^long stride ^long x ^long y]
   (+ (* y stride x)))
 
 (defn ^Pixel get-pixel [^Film f ^long x ^long y]
   (aget (:pixels f) (int (+ (* y (int (width (:bounds f)))) x))))
 
 (defn set-pixel [^Film f ^Pixel p ^long x ^long y]
   (aset (:pixels f) (int (+ (* y (int (width (:bounds f)))) x)) p))


 (defn- write-exr [{:keys [path width height pixels]}]
   (slicna.core/invoke :exru 
                       "write_rgba"
                       Integer
                       width
                       height
                       path
                       pixels))


 (defn spit-film [^Film f path]
   (let [w (int (width (:bounds f)))
         h (int (height (:bounds f)))
         l (int (* w h))
         #^"[Lsliimp.film.Pixel;" ps (:pixels f)
         ^floats fs (float-array (* l 4))]
     (doseq [n (range l)]
       (let [n' (int n)
             ^Pixel p (normalize (aget ps n'))
             y (int (* n' 4))]
               (aset fs (int (+ y 0)) (float (:x p)))
               (aset fs (int (+ y 1)) (float (:y p)))
               (aset fs (int (+ y 2)) (float (:z p)))
               (aset fs (int (+ y 3)) (float 1.0))))
     
     (write-exr {:path path
                 :width w
                 :height h
                 :pixels fs})))

(defn test-spit [w h pxl ]
  (spit-film (film :bounds (rect {:width w :height h}) 
                   :clear-color pxl)
             "/tmp/funk.exr"))

(defn pixel-rand [] (pixel (rand) (rand) (rand) 1.0))
(defn pixel-black [] (pixel 0.0 0.0 0.0 1.0))
(defn pixel-white [] (pixel 1.0 1.0 1.0 1.0))
(defn pixel-red [] (pixel 1.0 0.0 0.0 1.0))
(defn pixel-green [] (pixel 0.0 1.0 0.0 1.0))
(defn pixel-blue [] (pixel 0.0 0.0 1.0 1.0))

(defn -main [& args]
  (test-spit 1024 1024 (pixel-rand)))
                  
(defn image-process [^Film f kernel-fn]
 "Apply kernel-fn to all pixels.  kernel-fn must be a function of 2 arguments, x and y."
 
   (doseq [y (range (height f))
           x (range (width f))]
     (set-pixel f (kernel-fn x y) x y)))
