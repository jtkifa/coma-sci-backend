;;;; annotated-image.lisp — example annotated-image
;;;;
;;;; The same annotations on an image without and with halos: curved
;;;; contour labels, named stars, a planet symbol and a colorbar. The
;;;; image's lower limit is set below the background so the sky is not
;;;; black and the black halos show.
;;;;
;;;; Uses: with-style, plot-contours, with-color, plot-points,
;;;;       plot-text, plot-point, halo, with-halo, make-viewport,
;;;;       set-window-equal-scales, plot-image,
;;;;       draw-box, draw-colorbar, colorbar-viewport

(in-package :cl-cairoplot-examples)

(define-example annotated-image (d :size 1000 :aspect 0.42)
  "The same annotations on an image without and with halos: curved
   contour labels, named stars, a planet symbol and a colorbar.  The
   image's lower limit is set below the background so the sky is not
   black and the black halos show."
  (page-title d "Annotated image: without and with halos")
  (let ((sky (sky-image)))
    (flet ((annotations (vp)
             (with-style (vp :color :white :line-width 0.8)
               (plot-contours vp sky :levels '(20 40 80 160 320) :labels :curved
                                     :label-size 10 :label-spacing 150
                                     :label-format (lambda (v) (format nil "~D" (round v)))))
             (with-color (vp :cyan)
               (plot-points vp '(60 120 30) '(70 40 25) :marker :circle :size 10)
               (plot-text vp 69 79 "α Lisp" :size 15)
               (plot-text vp 127 48 "β Lisp" :size 15)
               (plot-text vp 36 32 "γ Lisp (variable)" :size 15))
             (with-color (vp :yellow)
               (plot-point vp 100 80 :marker :jupiter :size 10)
               (plot-text vp 100 72 "♃ transiting" :halign :center :valign :top :size 12)
               (plot-text vp 124 104 "ring nebula" :size 15))))
      (loop for (halo x1 title) in
            '((nil 0.05 "without halo")
              (t 0.5 "with halo: (with-halo (:color :black) ...)"))
            do (let ((vp (make-viewport d x1 (+ x1 0.39) 0.1 0.88)))
                 (set-window-equal-scales vp -0.5 159.5 -0.5 119.5)
                 (multiple-value-bind (lo hi)
                     (multiple-value-prog1
                         ;; VMIN below the sky level lifts the background
                         ;; out of the colormap's black end
                         (plot-image vp sky :colormap :inferno :scale :asinh
                                            :vmin -35 :vmax 400)
                       (if halo
                           (with-halo (:color :black) (annotations vp))
                           (annotations vp)))
                   (draw-box vp :title title :tick-direction :out :size 11)
                   (when halo
                     (draw-colorbar (colorbar-viewport vp) :colormap :inferno :scale :asinh
                                                           :vmin lo :vmax hi :label "counts"
                                                           :size 10))))))))
