(ns sliimp.film-demo
  (:use [sliimp core sampler filter film])
  (:import [sliimp core.Rect sampler.Sample]))

(defn poll-film! [f path n s]
  "Poll the film and write it to disk n times over n seconds"
  (dotimes [n' n]
    (spit-film! f path)
    (Thread/sleep s))
  (println "poll-film done"))


(defn demo-splat [& {:keys [w h n] 
                      :or {w 1280 h 720 n 8}}]
  (let [path-prefix (str "/tmp/demo-splat" (rand-int 1000))
        F  (film :bounds (rect :width w :height h) 
                 :filter (gaussian w (/ 1.0 w))
                 :finished-f #(do 
                                (println (:name %) "finished" path-prefix) 
                                (spit-film! % 
                                            (str path-prefix "-final.exr"))))]
    (do
      (doseq [y (range n) x (range n)](splat! F (sample-random w h)))
      (finish-film! F))))

(defn demo-image-process [n]
  (let [path (str "/tmp/demo-image-process" (rand-int 1000) ".exr")
        f (film :bounds (rect :width 512 :height 512) 
                :filter (gaussian 4.0 (/ 1.0 4.0))
                :finished-f #(spit-film! % path)
                :sampler-f stratified-seq2
                :samples-per-pixel n)
        kf2 (fn [^Sample s] 
              (let [w (* (Math/sin (* (:x-film s) (:y-film s) 0.09 0.09)))] 
                (sample s w 0.0 0.0)))]
    (do
      (image-process f kf2)
      (finish-film! f))))

