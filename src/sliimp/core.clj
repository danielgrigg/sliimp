(ns sliimp.core)

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

;; 0-*-1-*-2-*-3
;; | 0 | 1 | 2 |
;; In continuous coordinates, pixel 1 is at 1.5.
;; Likewise, 1.7 would be pixel 1. 
(defn continuous "Discrete to continuous" [d] (+ d 0.5))
(defn discrete "Continuous to discrete" [c] (Math/floor c))

(defprotocol Bounded2 
  (width [this])
  (height [this]))

(defrecord Rect [x0 y0 x1 y1]
  Bounded2
  (width [this]
    (- x1 x0))
  (height [this]
    (- y1 y0))
)

(defn rect
  "Create a rect"
 [& {:keys [x y width height] :or {x 0 y 0}}]
   (Rect. x y (+ x width) (+ y height)))

(defn rect-vec [^Rect r]
  [(:x0 r) (:y0 r) (:x1 r) (:y1 r)])

(defn widen-rect [^Rect r ^long dx ^long dy]
  (Rect. (:x0 r) 
        (:y0 r) 
        (+ (:x1 r) dx)
        (+ (:y1 r) dy)))

(defn clip
  "Clip two rectangles"
 [^Rect this ^Rect other]
  (Rect. (max (:x0 this) (:x0 other))
         (max (:y0 this) (:y0 other))
         (min (:x1 this) (:x1 other))
         (min (:y1 this) (:y1 other))))

(defn rect-discrete [^Rect r]
  "Create a discrete rect from a continuous one"
  (Rect. (int (discrete (:x0 r))) (int (discrete (:y0 r)))
         (int (discrete (:x1 r))) (int (discrete (:y1 r)))))
                            
(defn expand 
  "Expand a discrete Rect by a continous distance d, in all directions"   
  [^Rect r ^double d]
  (Rect. (discrete (- (continuous (:x0 r)) d))
         (discrete (- (continuous (:y0 r)) d))
         (discrete (+ (continuous (:x1 r)) d))
         (discrete (+ (continuous (:y1 r)) d))))


(defn coverage "Coverage of [x y] with radius. The coverage is the bounding-box of a circle with radius r, centered at [x y]." 
  [^double x ^double y ^double radius]

  (let [dx (- x 0.5) dy (- y 0.5)]
   (Rect. (int (Math/ceil (- dx radius))) 
          (int (Math/ceil (- dy radius)))
          (int (Math/floor (+ dx radius)))
          (int (Math/floor (+ dy radius))))))

;; Everyone says mixing test and production code is bad, but why? 
;; It's 'close' to the code, so a reader can peruse the test to understand
;; the code and it forces you to maintain your tests...
(defn test-coverage []
  (= (coverage 2.4 2.4 1.5) (Rect. 1 1 3 3)))

(defn rect-seq-inclusive 
  "Inclusive seq of all r coordinates, clipped to (x-max,y-max)."
  ([^Rect r]
     (rect-seq-inclusive r (:x1 r) (:y1 r)))
  ([^Rect r ^double x-max ^double y-max]
     (let [x0 (int (:x0 r))
           y0 (int (:y0 r))
           x1 (int (:x1 r))
           y1 (int (:y1 r))]
       (for [y (range y0 (min y-max (inc y1))) 
             x (range x0 (min x-max (inc x1)))] [x y]))))

(defn rect-seq "Seq of all r coordinates" [^Rect r]
  (let [x0 (int (:x0 r))
        y0 (int (:y0 r))
        x1 (int (:x1 r))
        y1 (int (:y1 r))]
    (for [y (range y0 y1) x (range x0 x1)] [x y])))