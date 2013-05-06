(ns sliimp.sampler)

(defrecord Sample [^float x-film 
                   ^float y-film 
                   ^float X 
                   ^float Y 
                   ^float Z])

(defn sample 
  ([^double film-x ^double film-y] 
     (Sample. film-x film-y 0.0 0.0 0.0))
  ([^Sample s ^double x ^double y ^double z]
     (Sample. (:x-film s) (:y-film s) x y z)))
;  ([ film-x  film-y x y z]
;     (Sample. film-x film-y x y z)))


(defn stratify1 [^long w]
  (let [inv-w (double (/ w))]
    (for [x (range w)] (* (+ x (rand)) inv-w))))

(defn stratify2 [^long w ^long h]
  (let [inv-w (double (/ w)) inv-h (double (/ h))]
    (for [x (range w) y (range h)]
      [ (* (+ x (rand)) inv-w) (* (+ y (rand)) inv-h)])))

(defn stratified-sampler2 [^long n-samples]
  (let [w (int (Math/ceil (Math/sqrt n-samples)))]
    (stratify2 w w)))

(defn uniform1 [^long w]
  (let [inv-w (double (/ w))]
    (for [x (range w)] (* (+ x 0.5) inv-w))))

(defn uniform2 [^long w ^long h]
  (let [inv-w (double (/ w)) inv-h (double (/ h))]
    (for [x (range w) y (range h)]
      [ (* (+ x 0.5) inv-w) (* (+ y 0.5) inv-h)])))

(defn uniform-sampler2 [^long n-samples]
  (let [w (int (Math/ceil (Math/sqrt n-samples)))]
    (uniform2 w w)))
          
(defn sample-seq2 
  "Generate a seq of 2d samples using sampler2-f"
[sampler2-f nsamples [dx dy]]
  (map (fn [[sx sy]] [(+ dx sx) (+ dy sy)]) (sampler2-f nsamples)))

;; sliimp.film> (time (count (sample-seq2 stratified-sampler2 (* 1000 10000) [2 2])))
;; "Elapsed time: 5201.102 msecs"
;; 10004569


(defrecord Sampler [^long nx 
                    ^long ny
                    #^"[Lsliimp.sampler.Sample;" samples])

(defn stratified-seq2 [^long nsamples ^long pxl-x ^long pxl-y] 
  (let [w (int (Math/ceil (Math/sqrt nsamples)))
        n (int (* w w))
        #^"[Lsliimp.sampler.Sample;" ss (make-array Sample n)
        inv-w (double (/ w))]
    (do
      (doseq [y (range w) x (range w)]
        (let [film-x (double (+ pxl-x (* (+ x (rand)) inv-w)))
              film-y (double (+ pxl-y (* (+ y (rand)) inv-w)))
              idx (int (+ x (* y w)))]
          (aset ss idx (Sample. film-x film-y 0.0 0.0 0.0))))
        (Sampler. w w ss))))

(defn uniform-seq2 [^long nsamples ^long pxl-x ^long pxl-y] 
  (let [w (int (Math/ceil (Math/sqrt nsamples)))
        n (int (* w w))
        #^"[Lsliimp.sampler.Sample;" ss (make-array Sample n)
        inv-w (double (/ w))]
    (do
      (doseq [y (range w) x (range w)]
        (let [fx (double (+ pxl-x (* (+ x 0.5) inv-w))) 
              fy (double (+ pxl-y (* (+ y 0.5) inv-w)))
              idx (int (+ x (* y w)))]
          (aset ss idx (Sample. fx fy 0.0 0.0 0.0))))
        (Sampler. w w ss))))

