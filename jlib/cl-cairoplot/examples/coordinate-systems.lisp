;;;; coordinate-systems.lisp — example coordinate-systems
;;;;
;;;; :DEVICE, :WORLD and :PLOT coordinates, a reversed window, and
;;;; SET-WINDOW-EQUAL-SCALES making a round circle.
;;;;
;;;; Uses: with-dash, plot-rectangle, device-aspect, plot-text,
;;;;       make-viewport, with-style, plot-polyline,
;;;;       draw-box, set-window-equal-scales, with-color, plot-circle

(in-package :cl-cairoplot-examples)

(define-example coordinate-systems (d :aspect 0.6)
  ":DEVICE, :WORLD and :PLOT coordinates, a reversed window, and
   SET-WINDOW-EQUAL-SCALES making a round circle."
  (page-title d "Coordinate systems")
  (with-dash (d '(4 3))
    (plot-rectangle d 0.01 0.01 0.99 (- (device-aspect d) 0.01) :coords :world))
  (plot-text d 0.02 0.03 ":world border" :coords :device :size 10)
  (let ((vp (make-viewport d 0.1 0.45 0.15 0.85 :window '(10 0 -1.5 1.5))))
    (multiple-value-bind (xs ys) (samples (lambda (x) (* 2 (sin x))) -1 11)
      (with-style (vp :color :blue :line-width 1.5) (plot-polyline vp xs ys)))
    (draw-box vp :x-label "x (reversed window 10..0)" :title "clipped to viewport"))
  (let ((vp (make-viewport d 0.58 0.95 0.15 0.85)))
    (set-window-equal-scales vp -2 2 -2 2)
    (with-color (vp :red) (plot-circle vp 0 0 1.5))
    (draw-box vp :title "equal scales")))
