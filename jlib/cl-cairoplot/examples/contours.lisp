;;;; contours.lisp — example contours
;;;;
;;;; Contours over an image at log-spaced levels; a signed field with
;;;; levels colored by a colormap, negative ones dashed, and labels; and
;;;; labeled contours in a rotated viewport.
;;;;
;;;; Uses: make-viewport, set-window-equal-scales,
;;;;       plot-image, with-style, plot-contours, draw-box,
;;;;       with-line-width, plot-point

(in-package :cl-cairoplot-examples)

(define-example contours (d :size 820 :aspect 0.8)
  "Contours over an image at log-spaced levels; a signed field with
   levels colored by a colormap, negative ones dashed, and labels; and
   labeled contours in a rotated viewport."
  (page-title d "Contours")
  (let ((vp (make-viewport d 0.07 0.45 0.52 0.9)))
    (set-window-equal-scales vp -0.5 159.5 -0.5 119.5)
    (let ((sky (sky-image)))
      (plot-image vp sky :colormap :gray :scale :log :vmin 5 :vmax 600 :reverse t)
      (with-style (vp :color :red :line-width 0.8)
        (plot-contours vp sky :levels '(20 40 80 160 320 640))))
    (draw-box vp :title "levels 20, 40 ... 640 over the image" :size 11))
  (let ((vp (make-viewport d 0.56 0.94 0.52 0.9)))
    (set-window-equal-scales vp -0.5 119.5 -0.5 119.5)
    (with-line-width (vp 1.2)
      (plot-contours vp (wave-image) :levels '(-0.8 -0.6 -0.4 -0.2 0.2 0.4 0.6 0.8)
                                     :colormap :coolwarm :negative-dash '(4 3)
                                     :labels t :label-spacing 120))
    (draw-box vp :title "colormap colors, negative dashed, labels" :size 11))
  (let ((vp (make-viewport d 0.16 0.6 0.1 0.4 :window '(0 10 0 10) :angle 15)))
    (plot-contours vp (terrain-image) :x-range '(0 10) :y-range '(0 10)
                                      :levels (loop for v from 200 to 1000 by 100 collect v)
                                      :labels t :label-spacing 150
                                      :label-format (lambda (v) (format nil "~D m" (round v))))
    (plot-point vp 3 6 :marker :filled-triangle :size 5)
    (draw-box vp :x-label "km" :y-label "km" :title "terrain, viewport at 15°" :size 11)))
