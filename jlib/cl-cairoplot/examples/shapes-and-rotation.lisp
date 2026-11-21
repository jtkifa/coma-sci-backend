;;;; shapes-and-rotation.lisp — example shapes-and-rotation
;;;;
;;;; Rotated rectangles, polygons and ellipses keep their shape in a very
;;;; non-square window, also inside a rotated viewport; ROUND circles.
;;;;
;;;; Uses: with-style, plot-rectangle, with-color, plot-ellipse,
;;;;       plot-circle, plot-polygon, with-plot-rotation, plot-text,
;;;;       make-viewport, draw-box

(in-package :cl-cairoplot-examples)

(define-example shapes-and-rotation (d :aspect 0.55)
  "Rotated rectangles, polygons and ellipses keep their shape in a very
   non-square window, also inside a rotated viewport; ROUND circles."
  (page-title d "Shapes rotated on the page")
  (flet ((scene (vp)
           (with-style (vp :dash '(3 3)) (plot-rectangle vp 0.5 0.6 2.5 1.4))
           (with-color (vp '(1 0 0 0.4)) (plot-rectangle vp 0.5 0.6 2.5 1.4 :fill t :angle 30))
           (with-style (vp :dash '(3 3)) (plot-ellipse vp 4.5 1 1.2 0.15))
           (with-color (vp '(0 0 1 0.4)) (plot-ellipse vp 4.5 1 1.2 0.15 :angle 60 :fill t))
           (with-color (vp '(0 0.6 0 0.5))
             (plot-ellipse vp 7 1 1 0.5 :a-scale :x :b-scale :x :angle 45 :fill t))
           (plot-circle vp 7 1 1 :round t)
           (with-style (vp :dash '(3 3)) (plot-polygon vp '(8.5 9.5 9) '(0.3 0.3 0.9)))
           (with-color (vp '(1 0.5 0 0.6))
             (plot-polygon vp '(8.5 9.5 9) '(0.3 0.3 0.9) :angle 90 :fill t))
           (with-plot-rotation (vp -20 5 0.3)
             (plot-rectangle vp 4 0.15 6 0.45)
             (plot-text vp 5 0.3 "with-plot-rotation" :halign :center :valign :center
                                                       :size 10))))
    (let ((vp (make-viewport d 0.06 0.47 0.12 0.82 :window '(0 10 0 2))))
      (scene vp)
      (draw-box vp :title "window 0..10 × 0..2"))
    (let ((vp (make-viewport d 0.6 0.92 0.24 0.7 :window '(0 10 0 2) :angle 30)))
      (scene vp)
      (draw-box vp :title "same, viewport at 30°"))))
