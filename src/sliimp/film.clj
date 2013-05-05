 (ns sliimp.film
   (:require slicna.core)
   (:use slimath.core)
   (:use (sliimp core sampler filter))
   (:import (java.util.concurrent ArrayBlockingQueue TimeUnit))
   (:import (sliimp core.Rect sampler.Sample filter.Filter)))

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
                 ^ArrayBlockingQueue requests
                 ^Thread worker
                 finished-f]
  Bounded2
    (width [this] (int (width bounds)))
    (height [this] (int (height bounds))))

(defn ^Pixel add-sample-pixel 
  "Add a weighted-sample to a pixel"
  [^Pixel q ^Sample s ^double weight]
  (Pixel. (+ (* (:X s) weight) (:X q))
          (+ (* (:Y s) weight) (:Y q))
          (+ (* (:Z s) weight) (:Z q))
          (+ (:weight q) weight)))

;; splat! implementation
(defn- splat' [^Film film ^Sample s]
  (let [x-max (get-in film [:bounds :x1])
        y-max (get-in film [:bounds :y1])
        coverage-seq (fn [^double x ^double y ^double w] 
                    (-> (coverage x y w)
                        (clip (:bounds film))
                        rect-seq))
        filter-width (double (get-in film [:filter :width]))]

    (doseq [[dx dy] (coverage-seq (:x-film s) (:y-film s) filter-width)]
      (let [idx (int (+ (* dy (int (width (:bounds film)))) dx))
            xf (float (- (:x-film s) dx 0.5))
            yf (float (- (:y-film s) dy 0.5))
            weight (float (.filterAt (:filter film) xf yf))
            ^Pixel q (aget (:pixels film) idx)]
          (aset (:pixels film) idx (add-sample-pixel q s weight))))))


(defn splat! 
  "Splat a sample to a film. The sample is mixed into all pixels that contain
s in their filter extent."
  [^Film film ^Sample s]
  (.put (:requests film) [s]))

(defn finish-film! "Finish using a film" [^Film f]
; Queue'ing nil explicity raises a null exception on the queue,
; so package our request.
  (.put (:requests f) [nil]))

(defn ^Film film 
  "Create a film, cleared with a clear-color pixel. finished-f is 
invoked after a finish-film! has been processed." 
  [& {:keys [bounds clear-color queue-size filter name finished-f] 
      :or {name (str (gensym)) 
           queue-size 100 
           clear-color (pixel 0.0 0.0 0.0) 
           finished-f identity}}]
  (let [#^"[Lsliimp.film.Pixel;" ps (make-array 
                                      Pixel 
                                      (* (height bounds) (width bounds)))
         requests (ArrayBlockingQueue. queue-size)
         ^Film film' (Film. bounds 
                        (amap ps idx ret clear-color)
                        name
                        filter
                        requests
                        nil
                        finished-f)

         worker (Thread.
                 (fn []
                   (loop [nsplat 1]
                     (when-let [[^Sample s]  (.take requests)]
                       (when s
;                         (println (:name film') "splat(" nsplat "):" s)
                         (splat' film' s)
                         (recur (inc nsplat)))))
                   (finished-f film')))]
     (.start worker)
     (assoc film' :worker worker)))
                                                                                          

; (defn ^Pixel add-pixel "Add two pixels" [^Pixel p ^Pixel q]
;   (Pixel. (+ (:x p) (:x q))
;           (+ (:y p) (:y q))
;           (+ (:z p) (:z q))
;           (+ (:w p) (:w q))))

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
   (aget (:pixels f) (int (+ (* y (int (width (:bounds f)))) x))))
 
(defn set-pixel! 
  "Set pixel at (x,y) to p"
  [^Film f ^Pixel p ^long x ^long y]
  (aset (:pixels f) (int (+ (* y (int (width (:bounds f)))) x)) p))

(defn channels 
  "Get interleaved channels."
  ([^Film f c]
     (map c (:pixels f)))
  ([^Film f c & cs] 
     (map #(vec (for [c' (cons c cs)] (c' %))) (:pixels f))))

 (defn- write-exr! 
   "Write an EXR file"
   [{:keys [path width height pixels]}]
   (slicna.core/invoke :exru 
                       "write_rgba"
                       Integer
                       width
                       height
                       path
                       pixels))

 (defn spit-film! [^Film f path]
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
(defn- test-spit! []
  (do
    (let [f (film :bounds (rect :width 512 :height 512) 
                  :clear-color (pixel (rand) (rand) (rand))
                  :finished-f #(spit-film! % "/tmp/test-spit.exr"))]
          
    (spit-film! f "/tmp/test-spit.exr")
    (println "spit done!")
    (Thread/sleep 2000)
    (finish-film! f))))

(defn pixel-rand [] (pixel (rand) (rand) (rand) 1.0))
(defn pixel-black [] (pixel 0.0 0.0 0.0 1.0))
(defn pixel-white [] (pixel 1.0 1.0 1.0 1.0))
(defn pixel-red [] (pixel 1.0 0.0 0.0 1.0))
(defn pixel-green [] (pixel 0.0 1.0 0.0 1.0))
(defn pixel-blue [] (pixel 0.0 0.0 1.0 1.0))
                  

(defn poll-film! [^Film f path n s]
  "Poll the film and write it to disk n times over n seconds"
  (dotimes [n' n]
    (spit-film! f path)
    (Thread/sleep s))
  (println "poll-film done"))

(defn demo-splat [& {:keys [w h n path] 
                     :or {w 1024 h 512 n 5 path "/tmp/demo-splat.exr"}}]
  (let [w' (double w)
        F  (film :bounds (rect :width w :height h) 
                :filter (gaussian :width w' :alpha (/ 1.0 w' 2.0))
                :finished-f #(do 
                               (println (:name %) "finished") 
                               (spit-film! % path)))]
    (do
      (doseq [y (range n) x (range n)] 
        (splat! F 
                (sample (rand-int w) 
                        (rand-int h) 
                        [(* 1.0 (rand)) (* 1.0 (rand)) (* 1.0 (rand))])))
      (poll-film! F path (* n n) 4000)
      (finish-film! F))))