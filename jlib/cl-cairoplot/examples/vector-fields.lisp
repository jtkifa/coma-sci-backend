;;;; vector-fields.lisp — example vector-fields
;;;;
;;;; Vector fields: arrows of a flow (a pair of vortices) on a grid,
;;;; colored by speed with a colorbar; streamlines of the same flow; a
;;;; polarization map, headless vectors centered on their points over an
;;;; image; and proper motions of stars, arrows from each star colored
;;;; by membership in a moving cluster.
;;;;
;;;; Uses: plot-vector-field (:colormap), plot-streamlines,
;;;;       plot-vectors (:heads nil, :pivot :middle, :colors),
;;;;       draw-colorbar, colorbar-viewport, set-window-equal-scales

(in-package :cl-cairoplot-examples)

(defun vortex-pair (x y)
  "Velocity at X, Y of two opposite vortices at (-1, 0) and (1, 0) with
   soft cores, in a weak uniform stream."
  (let ((u 0.15) (v 0))
    (loop for (x0 gamma) in '((-1 1) (1 -1))
          do (let* ((dx (- x x0)) (dy y) (r2 (+ (* dx dx) (* dy dy) 0.15)))
               (incf u (/ (* gamma (- dy)) r2))
               (incf v (/ (* gamma dx) r2))))
    (values u v)))

(define-example vector-fields (d :size 900 :aspect 0.72)
  "Arrows of a vortex flow colored by speed, its streamlines, a
   polarization map over an image, and stellar proper motions."
  (page-title d "Vector fields")
  (let ((panels (make-panel-grid d 2 2 :left 0.07 :right 0.9 :bottom 0.08 :top 0.9
                                       :hspace 0.12 :wspace 0.1)))
    ;; arrows on a grid, colored by speed
    (let ((vp (aref panels 0 0)))
      (set-window-equal-scales vp -3 3 -2 2)
      (multiple-value-bind (lo hi)
          (plot-vector-field vp #'vortex-pair '(-3 3) '(-2 2) :n 18 :colormap :viridis)
        (draw-colorbar (colorbar-viewport vp :gap 0.01) :colormap :viridis :vmin lo :vmax hi
                                                        :label "speed" :size 9))
      (draw-box vp :title "plot-vector-field, :colormap"))
    ;; streamlines of the same flow
    (let ((vp (aref panels 0 1)))
      (set-window-equal-scales vp -3 3 -2 2)
      (with-style (vp :color "#2a78d6" :line-width 0.8)
        (plot-streamlines vp #'vortex-pair '(-3 3) '(-2 2) :density 1.2))
      (with-color (vp "#eb6834")
        (plot-points vp '(-1 1) '(0 0) :marker :filled-circle :size 4))
      (draw-box vp :title "plot-streamlines"))
    ;; polarization around the ring: headless, centered on each point
    (let ((vp (aref panels 1 0)) (sky (sky-image))
          (xs '()) (ys '()) (us '()) (vs '()))
      (set-window-equal-scales vp -0.5 159.5 -0.5 119.5)
      (plot-image vp sky :colormap :gray :scale :asinh :vmin 0 :vmax 300 :reverse t)
      (loop for y from 4 below 120 by 7
            do (loop for x from 4 below 160 by 7
                     do (let* ((dx (- x 100)) (dy (- y 80)) (r (sqrt (+ (* dx dx) (* dy dy))))
                               ;; tangential, strongest on the ring
                               (p (* 0.2 (exp (- (expt (/ (- r 22) 7) 2))))))
                          (when (> p 0.02)
                            (push x xs) (push y ys)
                            (push (/ (* p (- dy)) r) us) (push (/ (* p dx) r) vs)))))
      (with-style (vp :color "#e34948" :line-width 1.4)
        (plot-vectors vp xs ys us vs :scale 40 :heads nil :pivot :middle))
      (draw-box vp :title "polarization: :heads nil :pivot :middle"))
    ;; proper motions: a moving cluster among field stars
    (let ((vp (aref panels 1 1))
          (xs '()) (ys '()) (us '()) (vs '()) (colors '()))
      (set-window-equal-scales vp -10 10 -7 7)
      (dotimes (k 60)
        (let ((member (< k 25)))
          (push (if member (* 2 (gaussian-noise (+ 100 k))) (* 18 (noise (+ 200 k)))) xs)
          (push (if member (* 2 (gaussian-noise (+ 300 k))) (* 13 (noise (+ 400 k)))) ys)
          (push (if member (+ 4 (* 0.4 (gaussian-noise (+ 500 k)))) (* 3 (gaussian-noise (+ 600 k)))) us)
          (push (if member (+ -3 (* 0.4 (gaussian-noise (+ 700 k)))) (* 3 (gaussian-noise (+ 800 k)))) vs)
          (push (if member "#eb6834" "#2a78d6") colors)))
      (plot-points vp xs ys :marker :filled-circle :size 2)
      (plot-vectors vp xs ys us vs :scale 0.3 :colors colors :head-size 5)
      (draw-legend vp '(("cluster" :line t :color "#eb6834") ("field" :line t :color "#2a78d6"))
                   :position :lower-left :size 9)
      (draw-box vp :title "proper motions: plot-vectors :colors"
                   :x-label "Δx (arcmin)" :y-label "Δy (arcmin)"))))
