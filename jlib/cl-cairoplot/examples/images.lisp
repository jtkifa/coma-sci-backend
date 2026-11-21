;;;; images.lisp — example images
;;;;
;;;; One image four ways: gray linear, viridis sqrt, magma log, and a
;;;; signed field in the diverging coolwarm map, each with a colorbar.
;;;;
;;;; Uses: colormap, make-viewport, set-window-equal-scales,
;;;;       plot-image, draw-box, draw-colorbar,
;;;;       colorbar-viewport

(in-package :cl-cairoplot-examples)

(define-example images (d :size 820 :aspect 0.8)
  "One image four ways: gray linear, viridis sqrt, magma log, and a
   signed field in the diverging coolwarm map, each with a colorbar."
  (page-title d "Images and colormaps")
  (let ((sky (sky-image)))
    (loop for (colormap scale title x1 y1) in
          '((:gray :linear "gray, linear" 0.07 0.53)
            (:viridis :sqrt "viridis, sqrt" 0.55 0.53)
            (:magma :log "magma, log" 0.07 0.06))
          do (let ((vp (make-viewport d x1 (+ x1 0.33) y1 (+ y1 0.38))))
               (set-window-equal-scales vp -0.5 159.5 -0.5 119.5)
               (multiple-value-bind (lo hi)
                   (plot-image vp sky :colormap colormap :scale scale :vmin 5 :vmax 400)
                 (draw-box vp :title title :tick-direction :out :size 11)
                 (draw-colorbar (colorbar-viewport vp) :colormap colormap :scale scale
                                                       :vmin lo :vmax hi :size 10)))))
  (let ((vp (make-viewport d 0.55 0.88 0.06 0.44)))
    (set-window-equal-scales vp -0.5 119.5 -0.5 119.5)
    (plot-image vp (wave-image) :colormap :coolwarm :vmin -1 :vmax 1)
    (draw-box vp :title "coolwarm (diverging)" :tick-direction :out :size 11)
    (draw-colorbar (colorbar-viewport vp) :colormap :coolwarm :vmin -1 :vmax 1 :size 10)))
