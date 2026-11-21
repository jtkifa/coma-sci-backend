;;;; filled-contours-and-density.lisp — example filled-contours-and-density
;;;;
;;;; Filled contours (PLOT-FILLED-CONTOURS) with contour lines over them
;;;; and a colorbar of discrete bands; and a 2D histogram
;;;; (PLOT-HISTOGRAM2D) of correlated points, with contours of the
;;;; counts.
;;;;
;;;; Uses: make-viewport, set-window-equal-scales,
;;;;       plot-filled-contours, with-style, plot-contours, draw-box,
;;;;       draw-colorbar, colorbar-viewport, plot-histogram2d

(in-package :cl-cairoplot-examples)

(define-example filled-contours-and-density (d :size 900 :aspect 0.5)
  "Filled contours with lines and a banded colorbar; a 2D histogram of
   correlated points with contours of its counts."
  (page-title d "Filled contours and 2D histograms")
  ;; terrain as filled bands
  (let ((vp (make-viewport d 0.06 0.44 0.12 0.88 :window '(0 10 0 10)))
        (terrain (terrain-image)))
    (let ((levels (prog1 (plot-filled-contours vp terrain :x-range '(0 10) :y-range '(0 10)
                                                          :levels '(100 200 300 400 500 600 700
                                                                    800 900 1000)
                                                          :colormap :viridis)
                    (with-style (vp :color :black :line-width 0.5)
                      (plot-contours vp terrain :x-range '(0 10) :y-range '(0 10)
                                                :levels '(200 300 400 500 600 700 800 900))))))
      (draw-box vp :x-label "km" :y-label "km" :title "filled contours" :tick-direction :out)
      (draw-colorbar (colorbar-viewport vp) :levels levels :colormap :viridis
                                            :label "height (m)" :size 10)))
  ;; a 2D histogram of correlated points
  (let* ((n 20000)
         (xs (loop for k below n collect (gaussian-noise k)))
         (ys (loop for k below n for x in xs collect (+ (* 0.6 x) (* 0.8 (gaussian-noise (+ k n))))))
         (vp (make-viewport d 0.56 0.92 0.12 0.88)))
    (set-window-equal-scales vp -4 4 -4 4)
    (multiple-value-bind (lo hi counts)
        (multiple-value-prog1
            (plot-histogram2d vp xs ys :bins 50 :x-range '(-4 4) :y-range '(-4 4)
                                       :colormap :inferno :scale :sqrt)
          )
      (with-style (vp :color :white :line-width 0.6)
        (plot-contours vp counts :x-range '(-4 4) :y-range '(-4 4) :levels '(5 20 50 100)))
      (draw-box vp :x-label "x" :y-label "y" :title "2D histogram, 20000 points"
                   :tick-direction :out)
      (draw-colorbar (colorbar-viewport vp) :colormap :inferno :scale :sqrt :vmin lo :vmax hi
                                            :label "count" :size 10))))
