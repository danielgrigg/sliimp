 (ns sliimp.film
   (:use [slicna core]
         [slimath core vec matrix]
         [sligeom core bounding transform]
         [sliimp core sampler filter])
   (:import [java.util.concurrent ArrayBlockingQueue]
            [sliimp core.Rect sampler.Sample filter.Filter sampler.Sampler]
            [sligeom.transform Transform]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(defrecord Pixel [^float X ^float Y ^float Z ^float weight])

(defn ^Pixel pixel 
  "Construct a pixel"
   ([^double x ^double y ^double z]
      (Pixel. x y z 1.0))
   ([^double x ^double y ^double z ^double weight]
      (Pixel. x y z weight)))

;; Capture the type-hinting name for an array of class c
(defmacro array-type [cname & args]
  `(-> (new ~cname ~@args) list into-array class .getName)) 

(defrecord Film [^Rect bounds 
                 #^"[Lsliimp.film.Pixel;" pixels 
                 ^String name
                 ^Filter filter
                 sampler-f
                 ^long samples-per-pixel
                 ^ArrayBlockingQueue requests
                 finished-f
                 ^Thread worker]
  Bounding
    (width [this] (int (width bounds)))
    (height [this] (int (height bounds)))
    (depth [this] 1))

(defn ^Pixel add-sample 
  "Add a weighted-sample to a pixel"
  [^Pixel q ^Sample s ^double weight]
  (Pixel. (+ (* (:X s) weight) (:X q))
          (+ (* (:Y s) weight) (:Y q))
          (+ (* (:Z s) weight) (:Z q))
          (+ (:weight q) weight)))

;; splat! implementation
(defn- splat' [^Film film ^Sample s]
  (let [#^"[Lsliimp.film.Pixel;" ps (:pixels film)
        ^Filter filter' (:filter film)
        filter-width (double (:width filter'))]

    (doseq [[dx dy] (clipped-coverage-seq (:bounds film)
                                          (:x-film s) 
                                          (:y-film s) 
                                          filter-width)]
      (let [idx (int (+ (* dy (int (width film))) dx))
            xf (float (- (:x-film s) dx 0.5))
            yf (float (- (:y-film s) dy 0.5))
            weight (float (.filterAt filter' xf yf))
            ^Pixel q (aget ps idx)]
          (aset ps idx (add-sample q s weight))))))

(defn splat! 
  "Splat a sample to a film. 
The sample is mixed into all pixels that contains in their filter extent."
  [^Film film ^Sample s]
  (let [^ArrayBlockingQueue splat-queue' (:requests film)]
    (.put splat-queue' [s])))

(defn finish-film! "Finish using a film" [^Film film]
; Queue'ing nil explicity raises a null exception on the queue,
; so package our request.
  (let [^ArrayBlockingQueue splat-queue' (:requests film)]
    (.put splat-queue' [nil])))

(defn ^Film film 
  "Create a film, cleared with a clear-color pixel. finished-f is 
invoked after a finish-film! has been processed." 
  [& {:keys [bounds clear-color queue-size filter 
             sampler-f samples-per-pixel name finished-f]
      :or {name (str (gensym))
           filter (tent)
           sampler-f stratified-seq2
           samples-per-pixel 1
           queue-size 128 
           clear-color (pixel 0.0 0.0 0.0) 
           finished-f identity }}]

  (let [npixels (* (height bounds) (width bounds))
        #^"[Lsliimp.film.Pixel;" ps (make-array Pixel npixels)
         requests (ArrayBlockingQueue. queue-size)
         ^Film film' (Film. bounds 
                        (amap ps idx ret clear-color)
                        name
                        filter
                        sampler-f 
                        samples-per-pixel
                        requests
                        finished-f
                        nil)

         worker (Thread.
                 (fn []
                   (loop [nsplat 1]
                     (when-let [[^Sample s]  (.take requests)]
                       (when s
                         (splat' film' s)
                         (recur (inc nsplat)))))
                   (finished-f film')))]
     (.start worker)
     (assoc film' :worker worker)))

 (defn ^Pixel normalize "Normalize a pixel" [^Pixel p]
   (if (< (:weight p) 0.0001)
     (pixel 0.0 0.0 0.0 0.0)
     (let [inv-w (float (/ (:weight p)))]
       (Pixel. (* (:X p) inv-w) 
               (* (:Y p) inv-w)
               (* (:Z p) inv-w)
               1.0))))
 
(defn- ^long pixel-idx 
  "index of pixel at (x,y)" 
  [^long stride ^long x ^long y]
   (+ (* y stride x)))
 
(defn ^Pixel get-pixel 
  "Get pixel at (x,y)" 
  [^Film f ^long x ^long y]
  (let [#^"[Lsliimp.film.Pixel;" ps (:pixels f)] 
   (aget ps (int (+ (* y (int (width (:bounds f)))) x)))))
 
(defn set-pixel! 
  "Set pixel at (x,y) to p"
  [^Film f ^Pixel p ^long x ^long y]
  (let [#^"[Lsliimp.film.Pixel;" ps (:pixels f)]
    (aset ps (int (+ (* y (int (width (:bounds f)))) x)) p)))

(defn channels 
  "Get interleaved channels."
  ([^Film f c]
     (let [#^"[Lsliimp.film.Pixel;" ps (:pixels f)]
       (map c ps)))
  ([^Film f c & cs] 
     (let [#^"[Lsliimp.film.Pixel;" ps (:pixels f)]
       (map #(vec (for [c' (cons c cs)] (c' %))) ps))))

 (defn- write-exr! 
   "Write an EXR image"
   [{:keys [path width height pixels]}]
   (slicna.core/invoke :exru 
                       "write_rgba"
                       Integer
                       width
                       height
                       path
                       pixels))

 (defn spit-film! 
   "Write a film"
   [^Film f path]
   (let [w (int (width (:bounds f)))
         h (int (height (:bounds f)))
         l (int (* w h))
         #^"[Lsliimp.film.Pixel;" ps (:pixels f)
         ^floats fs (float-array (* l 4))]
     (doseq [n (range l)]
       (let [n' (int n)
             ^Pixel p (normalize (aget ps n'))
             y (int (* n' 4))]
               (aset fs (int (+ y 0)) (float (:X p)))
               (aset fs (int (+ y 1)) (float (:Y p)))
               (aset fs (int (+ y 2)) (float (:Z p)))
               (aset fs (int (+ y 3)) (float 1.0))))
     
     (write-exr! {:path path
                 :width w
                 :height h
                 :pixels fs})))

(defn image-process [^Film f kernel-fn]
 "Apply kernel-fn to all pixels.  
kernel-fn must be a function of 2 arguments, x and y."
 (let [[x0 y0 x1 y1] (rect-vec (:bounds f))
       sf (partial (:sampler-f f) (:samples-per-pixel f))]
   (doseq [y (range y0 y1) x (range x0 x1)]
     (let [^Sampler ss (sf x y)]
       (doseq [^Sample s (:samples ss)]        
         (splat! f (kernel-fn s)))))))

(defn ^Transform screen-transform 
"NDC to screen coordinates. (0,0) in screen-space is the top-left."
[^double w ^double h]
  (compose (translate 0 h 0)
           (scale w (- h) 1) 
           (scale 0.5 0.5 1.0) 
           (translate 1 1 0)))

(defn sampling-fn "Get sampling fn for a film" [^Film f]
  (partial (:sampler-f f) (:samples-per-pixel f)))

(defn ^Sampler sampler-film [^Film film ^long x ^long y]
  ((:sampler-f film) (:samples-per-pixel film) x y))
