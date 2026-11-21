;;;; skewed-coordinates.lisp — example skewed-coordinates
;;;;
;;;; SET-TRANSFORM with shear: shapes ignore the skew by default; with
;;;; :FOLLOW-SKEW they are laid out in the unskewed plot and skewed with
;;;; it. Clipping regions match the drawn shapes.
;;;;
;;;; Uses: make-viewport, viewport-rectangle, set-transform,
;;;;       with-style, plot-line, draw-frame, plot-text,
;;;;       with-color, plot-ellipse, plot-rectangle, with-clipping,
;;;;       ellipse-region, circle-region, plot-circle

(in-package :cl-cairoplot-examples)

(define-example skewed-coordinates (d :aspect 0.5)
  "SET-TRANSFORM with shear: shapes ignore the skew by default; with
   :FOLLOW-SKEW they are laid out in the unskewed plot and skewed with
   it.  Clipping regions match the drawn shapes."
  (page-title d "Skewed coordinates and :follow-skew")
  (flet ((skewed (x1 x2 caption)
           (let ((vp (make-viewport d x1 x2 0.1 0.85)))
             (multiple-value-bind (wx1 wx2 wy1 wy2) (viewport-rectangle vp :world)
               (set-transform vp 0.065 0.04 0 0.05 (/ (+ wx1 wx2) 2) (/ (+ wy1 wy2) 2)))
             (with-style (vp :color :light-gray :line-width 0.5)
               (loop for g from -2 to 2 do (plot-line vp g -2 g 2) (plot-line vp -2 g 2 g)))
             (draw-frame vp)
             (plot-text d (/ (+ x1 x2) 2) 0.05 caption :coords :device
                                                       :halign :center :size 11)
             vp)))
    (let ((vp (skewed 0.02 0.32 "ellipses: dashed default, filled :follow-skew")))
      (with-color (vp '(0 0 1 0.35)) (plot-ellipse vp -1 1 0.8 0.5 :fill t :follow-skew t))
      (with-style (vp :dash '(3 2)) (plot-ellipse vp -1 1 0.8 0.5))
      (with-color (vp '(0 0 1 0.35)) (plot-ellipse vp 1 -1 0.8 0.5 :angle 40 :fill t
                                                                   :follow-skew t))
      (with-style (vp :dash '(3 2)) (plot-ellipse vp 1 -1 0.8 0.5 :angle 40)))
    (let ((vp (skewed 0.35 0.65 "rectangle at 30°: red default, blue :follow-skew")))
      (with-style (vp :dash '(3 2)) (plot-rectangle vp -1 -0.5 1 0.5))
      (with-color (vp '(1 0 0 0.4)) (plot-rectangle vp -1 -0.5 1 0.5 :angle 30 :fill t))
      (with-color (vp '(0 0 1 0.4)) (plot-rectangle vp -1 -0.5 1 0.5 :angle 30 :fill t
                                                                     :follow-skew t)))
    (let ((vp (skewed 0.68 0.98 "clip regions (fill) match outlines")))
      (with-clipping (vp (ellipse-region -1 1 0.8 0.5 :angle 40))
        (with-color (vp '(0 0.6 0 0.5)) (plot-rectangle vp -2 -2 2 2 :fill t)))
      (plot-ellipse vp -1 1 0.8 0.5 :angle 40)
      (with-clipping (vp (circle-region 1 -1 0.7 :follow-skew t))
        (with-color (vp '(1 0.5 0 0.6)) (plot-rectangle vp -2 -2 2 2 :fill t)))
      (plot-circle vp 1 -1 0.7 :follow-skew t))))
