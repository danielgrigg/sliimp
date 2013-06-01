(defproject sliimp "0.1.10"
  :description "An image processing library"
  :url "http://sliplanesoftware.com/"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "LATEST"]
                 [slimath "LATEST"]
                 [sligeom "LATEST"]
                 [slicna "LATEST"]]
  :profiles {:dev {:dependencies [[midje "1.5.0"]]}})


