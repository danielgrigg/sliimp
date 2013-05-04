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

(defn sample-radiance [^Sample s [x y z]]
  (Sample. (.x-film s) (.y-film s) x y z))

(defn stratify1 [^long w]
  (let [inv-w (double (/ w))]
    (for [x (range w)] (* (+ x (rand)) inv-w))))

(defn stratify2 [^long w ^long h]
  (let [inv-w (double (/ w)) inv-h (double (/ h))]
    (for [x (range w) y (range h)]
      [ (* (+ x (rand)) inv-w) (* (+ y (rand)) inv-h)])))

(defn sampler-stratify [^long n-samples]
  (let [w (long (Math/ceil (Math/sqrt n-samples)))]
    (partial stratify2 w w)))

(defn uniform1 [^long w]
  (let [inv-w (double (/ w))]
    (for [x (range w)] (* (+ x 0.5) inv-w))))

(defn uniform2 [^long w ^long h]
  (let [inv-w (double (/ w)) inv-h (double (/ h))]
    (for [x (range w) y (range h)]
      [ (* (+ x 0.5) inv-w) (* (+ y 0.5) inv-h)])))

(defn sampler-uniform [^long n-samples]
  (let [w (long (Math/ceil (Math/sqrt n-samples)))]
    (partial uniform2 w w)))
          
