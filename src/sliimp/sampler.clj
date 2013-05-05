(ns sliimp.sampler)

(defrecord Sample [^double x-film 
                   ^double y-film 
                   ^double X 
                   ^double Y 
                   ^double Z])

(defn sample 
  ([film-x film-y] 
     (Sample. film-x film-y 0.0 0.0 0.0))
  ([film-x film-y [x y z]]
     (Sample. film-x film-y x y z)))

(defn stratify1 [^long w]
  (let [inv-w (double (/ w))]
    (for [x (range w)] (* (+ x (rand)) inv-w))))

(defn stratify2 [^long w ^long h]
  (let [inv-w (double (/ w)) inv-h (double (/ h))]
    (for [x (range w) y (range h)]
      [ (* (+ x (rand)) inv-w) (* (+ y (rand)) inv-h)])))

(defn stratified-sampler2 [^long n-samples]
  (let [w (int (Math/ceil (Math/sqrt n-samples)))]
    (stratify2 n-samples n-samples)))

    ;(partial stratify2 w w)))

(defn uniform1 [^long w]
  (let [inv-w (double (/ w))]
    (for [x (range w)] (* (+ x 0.5) inv-w))))

(defn uniform2 [^long w ^long h]
  (let [inv-w (double (/ w)) inv-h (double (/ h))]
    (for [x (range w) y (range h)]
      [ (* (+ x 0.5) inv-w) (* (+ y 0.5) inv-h)])))

(defn uniform-sampler2 [^long n-samples]
  (let [w (long (Math/ceil (Math/sqrt n-samples)))]
    (uniform2 w w)))
;    (partial uniform2 w w)))
          
(defn sample-seq2 
  "Generate a seq of 2d samples using sampler2-f"
[sampler2-f nsamples [dx dy]]
  (map (fn [[sx sy]] [(+ dx sx) (+ dy sy)]) (sampler2-f nsamples)))
