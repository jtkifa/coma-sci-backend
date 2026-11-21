;;;; image-placement.lisp — example image-placement
;;;;
;;;; An image in a rotated viewport with its colorbar, nearest versus
;;;; bilinear pixels, NaN masking, and every named colormap.
;;;;
;;;; Uses: make-viewport, set-window-equal-scales,
;;;;       plot-image, with-color, plot-circle, draw-box,
;;;;       draw-colorbar, colorbar-viewport, colormap-names,
;;;;       diverging-colormap, reverse-colormap, plot-text

(in-package :cl-cairoplot-examples)

(define-example image-placement (d :size 820 :aspect 0.75)
  "An image in a rotated viewport with its colorbar, nearest versus
   bilinear pixels, NaN masking, and every named colormap."
  (page-title d "Image placement, pixels and colormaps")
  ;; rotated viewport: image, contour-free overlay, colorbar turn together
  (let ((vp (make-viewport d 0.06 0.4 0.42 0.88 :angle 20)))
    (set-window-equal-scales vp -0.5 159.5 -0.5 119.5)
    (multiple-value-bind (lo hi)
        (multiple-value-prog1
            (plot-image vp (sky-image) :colormap :inferno :scale :asinh
                                          :vmin 5 :vmax 300)
          (with-color (vp :cyan) (plot-circle vp 100 80 22 :round t)))
      (draw-box vp :title "viewport at 20°, asinh" :size 11)
      (draw-colorbar (colorbar-viewport vp) :colormap :inferno :scale :asinh
                                            :vmin lo :vmax hi :size 10)))
  ;; 12 x 12 pixels: nearest vs bilinear, with NaNs masked
  (let ((small (make-array '(12 12) :element-type 'double-float)))
    (dotimes (j 12)
      (dotimes (i 12)
        (setf (aref small j i)
              (if (and (<= 4 i 6) (<= 7 j 9))
                  float-utils:+double-float-nan+
                  (float (+ (sin (/ i 2)) (cos (/ j 3))) 1d0)))))
    (loop for (interpolation x1) in '((:nearest 0.55) (:bilinear 0.77))
          do (let ((vp (make-viewport d x1 (+ x1 0.19) 0.6 0.86)))
               (set-window-equal-scales vp -0.5 11.5 -0.5 11.5)
               (plot-image vp small :colormap :viridis :interpolation interpolation
                                    :nan-color :red)
               (draw-box vp :title (format nil "~(~S~), NaN red" interpolation)
                            :size 10))))
  ;; every named colormap, and two made here
  (let ((ramp (make-array '(1 256) :element-type 'double-float)))
    (dotimes (i 256) (setf (aref ramp 0 i) (float i 1d0)))
    (loop for (spec label) in (append (mapcar (lambda (n) (list n (format nil "~(~S~)" n)))
                                              (colormap-names))
                                      (list (list (diverging-colormap :blue-cyan :orange)
                                                  "diverging blue-cyan / orange")
                                            (list '(:black :blue :white)
                                                  "gradient black / blue / white")
                                            (list (reverse-colormap :viridis)
                                                  "reversed viridis")))
          for y = 0.33 then (- y 0.029)
          do (plot-image d ramp :colormap spec :coords :device
                                :x-range '(0.3 0.8) :y-range (list y (+ y 0.022)))
             (plot-text d 0.29 (+ y 0.011) label :coords :device :halign :right
                                                  :valign :center :size 10))))
