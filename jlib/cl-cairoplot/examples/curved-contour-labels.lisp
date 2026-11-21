;;;; curved-contour-labels.lisp — example curved-contour-labels
;;;;
;;;; Contour labels as straight text (LABELS T) and with each character
;;;; following the line (LABELS :CURVED), on tightly bent contours and on
;;;; terrain.
;;;;
;;;; Uses: make-viewport, set-window-equal-scales,
;;;;       plot-contours, draw-box, plot-point

(in-package :cl-cairoplot-examples)

(define-example curved-contour-labels (d :size 820 :aspect 0.8)
  "Contour labels as straight text (LABELS T) and with each character
   following the line (LABELS :CURVED), on tightly bent contours and on
   terrain."
  (page-title d "Straight and curved contour labels")
  (loop for (mode x1 title) in '((t 0.06 "labels t (straight)")
                                 (:curved 0.54 "labels :curved"))
        do (let ((vp (make-viewport d x1 (+ x1 0.4) 0.5 0.9)))
             (set-window-equal-scales vp -0.5 119.5 -0.5 119.5)
             (plot-contours vp (ring-image) :levels '(0.2 0.4 0.6 0.8 1.0)
                                            :colormap :viridis :labels mode
                                            :label-size 13 :label-spacing 110
                                            :label-format (lambda (v) (format nil "r = ~,1F" v)))
             (draw-box vp :title title :size 11)))
  (let ((vp (make-viewport d 0.18 0.62 0.1 0.38 :window '(0 10 0 10) :angle 15)))
    (plot-contours vp (terrain-image) :x-range '(0 10) :y-range '(0 10)
                                      :levels (loop for v from 200 to 1000 by 100 collect v)
                                      :labels :curved :label-spacing 150
                                      :label-format (lambda (v) (format nil "~D m" (round v))))
    (plot-point vp 3 6 :marker :filled-triangle :size 5)
    (draw-box vp :x-label "km" :y-label "km" :title "terrain, curved labels, viewport at 15°"
                 :size 11)))
