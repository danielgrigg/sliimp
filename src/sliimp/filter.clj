(ns sliimp.filter)

(definterface FilterOp
  (^double filterAt [^double x ^double y]))

(defrecord Filter [^double width f]
  FilterOp
  (filterAt [this x y]
    (f x y)))

(defn box
"A box filter"
  ([] (box 1.0))
  ([^double width] (Filter. width (constantly 1.0))))

(defn tent
"A linear tent filter"
  ([]
     (tent 1.0))
  ([^double width]
     (Filter. width
              (fn ^double [^double x ^double y]
                (* (- width (Math/abs x)) (- width (Math/abs y)))))))

(defn- ^double gaussian1 [^double x ^double a ^double exp-w]
  (- (Math/exp (- (* a x x ))) exp-w))

     
(defn gaussian
  ([& {:keys [width alpha] :or { width 2.0 alpha 2.0}}]
     (let [w (double width)
           a (double alpha)
           exp-w (double (Math/exp (- (* a w w))))]
       (Filter. w
                (fn [^double x ^double y]
                  (* (gaussian1 x a exp-w) (gaussian1 y a exp-w)))))))


(defn- mitchell1 [^double B ^double C ^double x]
  (let [x (double (Math/abs (* 2.0 x)))]
    (if (< x 1.0)
      (* (+ (* (-  12.0 (* 9.0 B) (* 6.0 C)) x x x)
         (* (+ -18.0 (* 12.0 B) (* 6.0 C)) x x)
         (- 6.0 (* 2.0 B))) (/ 6.0))

      (* (+ (* (- (+ B (* 6.0 C))) x x x)
            (* (+ (* 6.0 B) (* 30.0 C)) x x)
            (* (- (+ (* 12.0 B) (* 48.0 C))) x)
            (+ (* 8.0 B) (* 24.0 C))) (/ 6.0)))))

(defn mitchell
  ([]
     (mitchell 2.0 (/ 3.0) (/ 3.0)))  

  ([^double width ^double B ^double C]
     (Filter. width
              (fn [^double x ^double y]
                (* (mitchell1 B C (/ x width)) 
                   (mitchell1 B C (/ y width)))))))
