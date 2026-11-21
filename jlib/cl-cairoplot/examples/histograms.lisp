;;;; histograms.lisp — example histograms
;;;;
;;;; Histograms: step, filled and bar styles; a density histogram with
;;;; the normal curve over it; a cumulative one; and two translucent
;;;; distributions overlaid with a legend.
;;;;
;;;; Uses: make-viewport, plot-histogram, with-color,
;;;;       with-style, plot-polyline, draw-box, draw-legend, histogram

(in-package :cl-cairoplot-examples)

(define-example histograms (d :size 900 :aspect 0.62)
  "Histogram styles (step, filled, bars), density with a normal curve,
   cumulative, and two overlaid distributions with a legend."
  (page-title d "Histograms")
  (let ((sample (loop for k below 2000 collect (gaussian-noise k)))
        (other (loop for k from 5000 below 6500 collect (+ 1.5 (* 0.7 (gaussian-noise k))))))
    (flet ((panel (x1 y1 title &rest window)
             (let ((vp (make-viewport d x1 (+ x1 0.27) y1 (+ y1 0.33) :window window)))
               (values vp title))))
      ;; step
      (multiple-value-bind (vp title) (panel 0.06 0.52 "style :step" -4 4 0 260)
        (with-style (vp :color :blue :line-width 1.2)
          (plot-histogram vp sample :bins 32 :range '(-4 4)))
        (draw-box vp :title title :y-label "count"))
      ;; filled, outlined
      (multiple-value-bind (vp title) (panel 0.385 0.52 "style :filled, outlined" -4 4 0 260)
        (with-color (vp '(0.2 0.6 0.3 0.5))
          (plot-histogram vp sample :bins 32 :range '(-4 4) :style :filled))
        (with-color (vp '(0.1 0.4 0.2))
          (plot-histogram vp sample :bins 32 :range '(-4 4)))
        (draw-box vp :title title))
      ;; bars
      (multiple-value-bind (vp title) (panel 0.71 0.52 "style :bars" -4 4 0 520)
        (with-color (vp :orange)
          (plot-histogram vp sample :bins 16 :range '(-4 4) :style :bars))
        (draw-box vp :title title))
      ;; density with the normal curve
      (multiple-value-bind (vp title) (panel 0.06 0.08 "normalize :density" -4 4 0 0.45)
        (with-color (vp '(0.5 0.5 0.8 0.5))
          (plot-histogram vp sample :range '(-4 4) :style :filled :normalize :density))
        (multiple-value-bind (xs ys)
            (samples (lambda (x) (/ (exp (* -0.5 x x)) (sqrt (* 2 pi)))) -4 4)
          (with-style (vp :color :red :line-width 1.5) (plot-polyline vp xs ys)))
        (draw-box vp :title title :x-label "x" :y-label "density"))
      ;; cumulative fraction
      (multiple-value-bind (vp title) (panel 0.385 0.08 "cumulative, :fraction" -4 4 0 1.05)
        (with-style (vp :color :purple :line-width 1.2)
          (plot-histogram vp sample :bins 64 :range '(-4 4) :cumulative t
                                    :normalize :fraction))
        (draw-box vp :title title :x-label "x"))
      ;; two overlaid distributions
      (multiple-value-bind (vp title) (panel 0.71 0.08 "overlaid, with a legend" -4 4 0 0.65)
        (with-color (vp '(0 0.3 1 0.35))
          (plot-histogram vp sample :bins 40 :range '(-4 4) :style :filled :normalize :density))
        (with-color (vp '(1 0.2 0 0.35))
          (plot-histogram vp other :bins 40 :range '(-4 4) :style :filled :normalize :density))
        (draw-box vp :title title :x-label "x")
        (draw-legend vp '(("A: mean 0" :fill (0 0.3 1 0.35))
                          ("B: mean 1.5" :fill (1 0.2 0 0.35)))
                     :position :upper-left)))))
