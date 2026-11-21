;;;; minimal-plot.lisp — example minimal-plot
;;;;
;;;; The shortest plot.  A device starts with a viewport selected (made
;;;; from *DEFAULT-VIEWPORT*, leaving room for numbers and labels), and
;;;; plotting functions given the device draw in it, so the whole
;;;; program is:
;;;;
;;;;   (with-device (d :pdf "plot.pdf")
;;;;     (set-window d 0 10 -1.2 1.2)
;;;;     (plot-polyline d xs ys)
;;;;     (draw-box d :x-label "time (s)" :y-label "amplitude"))
;;;;
;;;; Uses: *default-viewport*, set-window, plot-polyline, plot-points,
;;;;       draw-box (all given the device)

(in-package :cl-cairoplot-examples)

(define-example minimal-plot (d)
  "The shortest plot: the device's default viewport with its window
   set, a curve, some points and a box, with no MAKE-VIEWPORT or
   WITH-VIEWPORT."
  (set-window d 0 10 -1.2 1.2)
  (multiple-value-bind (xs ys) (samples (lambda (x) (* (exp (/ x -4)) (cos (* 3 x)))) 0 10)
    (plot-polyline d xs ys))
  (let ((xs (loop for i from 0 below 21 collect (* 0.5 i))))
    (plot-points d xs (loop for x in xs for i from 0
                            collect (+ (* (exp (/ x -4)) (cos (* 3 x)))
                                       (* 0.08 (gaussian-noise (+ 50 i)))))
                 :marker :circle))
  (draw-box d :x-label "time (s)" :y-label "amplitude" :title "The default viewport"))
