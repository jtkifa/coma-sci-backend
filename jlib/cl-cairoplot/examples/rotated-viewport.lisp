;;;; rotated-viewport.lisp — example rotated-viewport
;;;;
;;;; The same plot unrotated and in a viewport rotated 37 degrees: frame,
;;;; ticks, text, grid and clipping all turn with it.
;;;;
;;;; Uses: make-viewport :angle, with-style, plot-polyline, with-color,
;;;;       plot-circle, draw-box

(in-package :cl-cairoplot-examples)

(define-example rotated-viewport (d :size 800 :aspect 0.6)
  "The same plot unrotated and in a viewport rotated 37 degrees: frame,
   ticks, text, grid and clipping all turn with it."
  (page-title d "A plot in a rotated viewport")
  (flet ((panel (vp title)
           (multiple-value-bind (xs ys) (samples (lambda (x) (* 1.4 (sin x))) 0 10)
             (with-style (vp :color :blue :line-width 1.5) (plot-polyline vp xs ys)))
           (with-color (vp '(1 0 0 0.4)) (plot-circle vp 5 0 0.6 :round t :fill t))
           (draw-box vp :grid t :x-label "x (radians)" :y-label "1.4 sin x"
                        :title title)))
    (panel (make-viewport d 0.08 0.4 0.15 0.8 :window '(0 10 -1.5 1.5)) "unrotated")
    (panel (make-viewport d 0.56 0.88 0.2 0.72 :window '(0 10 -1.5 1.5) :angle 37)
           "rotated 37°")))
