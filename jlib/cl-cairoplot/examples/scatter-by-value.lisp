;;;; scatter-by-value.lisp — example scatter-by-value
;;;;
;;;; Points colored by a third quantity (VALUES-TO-COLORS) and sized by
;;;; a fourth (VALUES-TO-SIZES), with a colorbar and a legend for the
;;;; sizes, placed automatically where it covers fewest points; and the
;;;; same colors with halos over a dark background.
;;;;
;;;; Uses: values-to-colors, values-to-sizes, make-viewport,
;;;;       plot-points, draw-box, colorbar-viewport, draw-colorbar,
;;;;       draw-legend (:position :best, the default), with-color,
;;;;       plot-rectangle

(in-package :cl-cairoplot-examples)

(define-example scatter-by-value (d :size 900 :aspect 0.55)
  "A scatter plot colored by one quantity and sized by another, with a
   colorbar and a size legend; and colored points with halos on dark."
  (page-title d "Points colored and sized by value")
  (let* ((n 300)
         (color-index (loop for k below n collect (+ 0.6 (* 0.25 (gaussian-noise k)))))
         (redshift (loop for k below n for c in color-index
                         collect (max 0.01 (+ (* 1.5 (- c 0.4)) (* 0.15 (gaussian-noise (+ k 999)))))))
         (magnitude (loop for k below n for z in redshift
                          collect (+ 19 (* 4 z) (* 0.8 (gaussian-noise (+ k 3333))))))
         (mass (loop for k below n collect (+ 10 (* 0.5 (gaussian-noise (+ k 7777)))))))
    (multiple-value-bind (colors lo hi) (values-to-colors redshift :colormap :plasma :vmin 0 :vmax 1.2)
      (let ((sizes (values-to-sizes mass :min-size 1.2 :max-size 7 :vmin 9 :vmax 11))
            (vp (make-viewport d 0.08 0.52 0.12 0.88 :window '(0 1.3 25 17))))
        (plot-points vp color-index magnitude :marker :filled-circle :colors colors :sizes sizes)
        (draw-box vp :x-label "color index" :y-label "magnitude" :title "colored by z, sized by mass")
        (draw-colorbar (colorbar-viewport vp) :colormap :plasma :vmin lo :vmax hi
                                              :label "redshift" :size 11)
        (draw-legend vp (loop for m in '(9 10 11)
                              collect (list (format nil "log M = ~D" m) :marker :circle
                                            :size (first (values-to-sizes (list m) :min-size 1.2
                                                                         :max-size 7 :vmin 9 :vmax 11))))))
      ;; the same colors over a dark background, with halos
      (let ((vp (make-viewport d 0.64 0.97 0.12 0.88 :window '(0 1.3 25 17))))
        (with-color (vp '(0.1 0.1 0.15)) (plot-rectangle vp 0 25 1.3 17 :fill t))
        (plot-points vp color-index magnitude :marker :filled-circle :colors colors :size 2.5
                                              :halo (make-halo :opacity 0.5))
        (draw-box vp :x-label "color index" :title "on dark, with halos")))))
