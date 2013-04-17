(ns sliimp.core)

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(defmacro continuous "Discrete to continuous" [d] `(+ ~d 0.5))
(defmacro discrete "Continuous to discrete" [c] `(Math/floor ~c))


