;;;; astronomical-symbols.lisp — example astronomical-symbols
;;;;
;;;; Planet, moon, node and zodiac symbols as markers, in Hershey
;;;; "astrology" (default) and a system font, and the planets placed on a
;;;; log-log plot of mass against distance.
;;;;
;;;; Uses: plot-text, plot-point, make-viewport,
;;;;       draw-box

(in-package :cl-cairoplot-examples)

(defparameter *symbol-font* #+darwin "Apple Symbols" #-darwin "DejaVu Sans"
  "A system font with astronomical symbols.")

(define-example astronomical-symbols (d :aspect 0.95)
  "Planet, moon, node and zodiac symbols as markers, in Hershey
   \"astrology\" (default) and a system font, and the planets placed on
   a log-log plot of mass against distance."
  (page-title d "Astronomical symbols")
  (let ((symbols *astronomical-symbols*))
    (plot-text d 0.05 0.925 "Hershey \"astrology\" (default)" :coords :device :size 11)
    (plot-text d 0.55 0.925 (format nil "system font (:font ~S)" *symbol-font*)
               :coords :device :size 11)
    (loop for name in symbols
          for i from 0
          for col = (floor i 14)
          for x = (+ 0.05 (* 0.21 col))
          for y = (- 0.88 (* 0.032 (mod i 14)))
          do (plot-point d x y :marker name :coords :device :size 5)
             (plot-point d (+ x 0.5) y :marker name :coords :device :size 5
                                       :font *symbol-font*)
             (dolist (dx '(0 0.5))
               (plot-text d (+ x dx 0.03) y (format nil "~(~S~)" name)
                          :coords :device :valign :center :size 10))))
  ;; the planets: log mass (Earth = 1) against log semimajor axis (AU)
  (let ((vp (make-viewport d 0.14 0.92 0.1 0.4 :window '(-0.6 1.8 -3.2 3))))
    (loop for (planet a mass) in '((:mercury 0.387 0.0553) (:venus 0.723 0.815)
                                   (:earth 1.0 1.0) (:mars 1.524 0.107)
                                   (:jupiter 5.20 317.8) (:saturn 9.58 95.2)
                                   (:uranus 19.2 14.5) (:neptune 30.05 17.1)
                                   (:pluto 39.5 0.0022))
          do (plot-point vp (log a 10) (log mass 10) :marker planet :size 7))
    (draw-box vp :x-log t :y-log t :x-label "semimajor axis (AU)"
                 :y-label '("mass (M" ("⊕" :scale 0.7 :rise -0.25 :font :astrology) ")"))))
