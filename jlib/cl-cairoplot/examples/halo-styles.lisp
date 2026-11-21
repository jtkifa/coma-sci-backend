;;;; halo-styles.lisp — example halo-styles
;;;;
;;;; Halo objects: the named presets :DEFAULT (soft), :CRISP, :GLOW,
;;;; :DARK and :DARK-CRISP, and a custom MAKE-HALO, on the same overlay.
;;;;
;;;; Uses: make-halo, make-viewport, set-window-equal-scales,
;;;;       plot-image, with-halo, with-color,
;;;;       plot-contours, plot-text, plot-point, draw-frame,
;;;;       viewport-rectangle

(in-package :cl-cairoplot-examples)

(define-example halo-styles (d :size 900 :aspect 0.6)
  "Halo objects: the named presets :DEFAULT (soft), :CRISP, :GLOW, :DARK
   and :DARK-CRISP, and a custom MAKE-HALO, on the same overlay."
  (page-title d "Halo styles (make-halo and named halos)")
  (let ((sky (sky-image)))
    (loop for (spec title light) in
          `((:default ":halo :default (soft)" nil)
            (:crisp ":halo :crisp" nil)
            (:glow ":halo :glow" nil)
            (,(make-halo :color '(1 0.85 0.4) :opacity 0.7) "(make-halo :color '(1 .85 .4) :opacity .7)" nil)
            (:dark ":halo :dark" t)
            (:dark-crisp ":halo :dark-crisp" t))
          for i from 0
          for x1 = (+ 0.02 (* 0.33 (mod i 3)))
          for y1 = (if (< i 3) 0.49 0.03)
          do (let ((vp (make-viewport d x1 (+ x1 0.31) y1 (+ y1 0.38))))
               (set-window-equal-scales vp 45 115 35 80)
               (if light
                   (plot-image vp sky :colormap :gray :scale :asinh :vmin 0 :vmax 300 :reverse t)
                   (plot-image vp sky :colormap :viridis :scale :asinh :vmin 0 :vmax 300))
               (with-halo (spec)
                 (with-color (vp (if light :white :black))
                   (plot-contours vp sky :levels '(40 160) :labels t :label-size 12
                                         :label-format (lambda (v) (format nil "~D" (round v))))
                   (plot-text vp 66 70 "bright star ♃" :valign :center :size 18))
                 (with-color (vp :red) (plot-point vp 52 45 :marker :filled-star :size 9)))
               (draw-frame vp)
               (multiple-value-bind (vx1 vx2 vy1 vy2) (viewport-rectangle vp)
                 (declare (ignore vx2 vy1))
                 (plot-text d vx1 (+ vy2 0.008) title :coords :device :size 11))))))
