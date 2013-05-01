(ns sliimp.core)

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(defn continuous "Discrete to continuous" [d] (+ ~d 0.5))
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
 [{:keys [x y width height] :or {x 0 y 0}}]
   (Rect. x y (+ x width) (+ y height)))


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

(defn coverage [^double x ^double y ^double radius]
"Continuous bounds of the area covered by a circle at (x,y)"
   (Rect. (- x radius -0.5)
          (- y radius -0.5)
          (+ x radius +0.5)
          (+ y radius +0.5)))

(defn rect-seq [^Rect r]
  (let [x0 (int (:x0 r))
        y0 (int (:y0 r))
        x1 (int (:x1 r))
        y1 (int (:y1 r))]
    (for [y (range y0 y1) x (range x0 x1)] [x y])))
