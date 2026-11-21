;;;; axis-styles.lisp — example axis-styles
;;;;
;;;; Four DRAW-BOX styles: grid, log-log with superscripts, a reversed
;;;; magnitude axis with numbers on all sides, and large/small numbers.
;;;;
;;;; Uses: make-viewport, with-style, plot-polyline,
;;;;       draw-box, plot-line, plot-points

(in-package :cl-cairoplot-examples)

(define-example axis-styles (d :size 800)
  "Four DRAW-BOX styles: grid, log-log with superscripts, a reversed
   magnitude axis with numbers on all sides, and large/small numbers."
  (page-title d "Axis styles")
  (let ((vp (make-viewport d 0.09 0.44 0.56 0.9 :window '(0 10 -1.2 1.2))))
    (multiple-value-bind (xs ys) (samples #'sin 0 10)
      (with-style (vp :color :blue :line-width 1.5) (plot-polyline vp xs ys)))
    (draw-box vp :grid t :x-label "x" :y-label "sin x" :title "Grid"))
  (let ((vp (make-viewport d 0.6 0.95 0.56 0.9 :window '(-1 4 -3 3))))
    (with-style (vp :color :red :line-width 1.5)
      ;; a power law y = x^-1.2, in log10 units
      (plot-line vp -1 1.2 4 -4.8))
    (draw-box vp :x-log t :y-log t :grid :minor :tick-direction :out
                 :x-label "frequency ν (Hz)"
                 :y-label '("flux (W m" ("-2" :scale 0.6 :rise 0.5) ")")
                 :title "Log-log, ticks out"))
  ;; a variable star's light curve: magnitudes run downward, so brighter
  ;; is up (window y from 18.6 down to 16.4)
  (let ((vp (make-viewport d 0.09 0.4 0.1 0.42 :window '(0 1 18.6 16.4))))
    (flet ((light-curve (phase)           ; slow fade, fast rise
             (if (< phase 0.8)
                 (+ 16.9 (* 1.3 (/ phase 0.8)))
                 (- 18.2 (* 1.3 (/ (- phase 0.8) 0.2))))))
      (multiple-value-bind (xs ys) (samples #'light-curve 0 1 200)
        (with-style (vp :color :red :line-width 1) (plot-polyline vp xs ys)))
      (let ((phases (loop for i below 60 collect (/ (+ i (* 0.5 (noise i))) 60))))
        (plot-points vp phases
                     (loop for p in phases for i from 0
                            collect (+ (light-curve p) (* 0.25 (noise (+ i 100)))))
                     :marker :filled-circle :size 2.5)))
    (draw-box vp :numbers '(:bottom :left :top :right) :y-numbers :vertical
                 :tick-direction :both :font :roman
                 :x-label "phase" :y-label "magnitude"))
  (let ((vp (make-viewport d 0.62 0.95 0.1 0.42 :window '(0 3e6 -4e-5 6e-5))))
    (flet ((rate (n) (* 5.5e-5 (exp (/ n -1.5e6)) (cos (/ (* 2 pi n) 1.2e6)))))
      (multiple-value-bind (xs ys) (samples #'rate 0 3e6)
        (with-style (vp :color :blue :line-width 1.5) (plot-polyline vp xs ys)))
      (let ((ns (loop for n from 1e5 below 3e6 by 2e5 collect n)))
        (plot-points vp ns (mapcar #'rate ns) :marker :circle :size 3)))
    ;; a 10^6 step keeps the wide mantissa×10^n labels apart
    (draw-box vp :font :sans :x-label "N" :y-label "rate" :x-step 1e6
                 :title "Exponents (Helvetica)")))
