;;;; basic-plot.lisp — example basic-plot
;;;;
;;;; Two curves with a frame, ticks, numbers, labels and title, drawn in
;;;; the device's default viewport.
;;;;
;;;; Uses: set-window, with-style, plot-polyline, draw-box

(in-package :cl-cairoplot-examples)

(define-example basic-plot (d)
  "Two curves with a frame, ticks, numbers, labels and title."
  (set-window d 0 10 -1.2 1.2)
  (multiple-value-bind (xs ys) (samples (lambda (x) (* (exp (- (/ x 6))) (sin (* 2 x)))) 0 10)
    (with-style (d :color :blue :line-width 1.5)
      (plot-polyline d xs ys)))
  (multiple-value-bind (xs ys) (samples (lambda (x) (exp (- (/ x 6)))) 0 10)
    (with-style (d :color :red :dash '(6 4))
      (plot-polyline d xs ys)))
  (draw-box d :x-label "time (s)" :y-label "amplitude"
              :title "Damped oscillation" :zero-axes t))
