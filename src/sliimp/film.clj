(ns sliimp.film
  (:use slimath.core)
  (:use sliimp.core)
  (:require slicna.core)
  (:import sliimp.core.Rect))

(defrecord Pixel [xyz ^double weight])

(defn pixel 
  ([[^double x ^double y ^double z :as xyz]]
     (pixel xyz 1.0))
  ([[^double x ^double y ^double z :as xyz] weight]
     (Pixel. xyz weight)))

(defn weight [^Pixel p]
  (v3muls (:xyz p) (:weight p)))

(defn normalize [^Pixel p]
  (if (< (:weight p) 0.0001)
    (pixel-black)
    (Pixel. (v3muls (:xyz p) (/ (:weight p))) 1.0)))

(defn add-pixel [^Pixel p ^Pixel q]
  (Pixel. (v3add (:xyz p) (weighted q)) (+ (:weight p) (:weight q))))

(defn get-pixel [^Film f ^long x ^long y]
  ((:pixels f) (+ (* y (int (width (:bounds f)))) x)))

(defn pixel-black [] (Pixel. [0.0 0.0 0.0] 1.0))

(defrecord Film [^Rect bounds pixels])

(defn film [{:keys [bounds pixels]}]
  "Create a film, using a blank pixel array if pixels is missing."
  (Film. bounds (or pixels
                     (vec (repeatedly
                           (* (height bounds) (width bounds))
                          pixel-black)))))

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
                       (reduce (partial apply conj) []
                               (map (fn [p] (conj (:xyz (normalize p)) 1.0)) 
                                    (:pixels f))))}))

