;;;; halos.lisp — example halos
;;;;
;;;; Text, markers, symbols and contours over a busy image, without halos
;;;; and inside WITH-HALO; and dark halos on a light image.
;;;;
;;;; Uses: with-color, plot-contours, plot-points, plot-point,
;;;;       plot-text, halo, with-halo, make-viewport,
;;;;       set-window-equal-scales, plot-image, draw-box

(in-package :cl-cairoplot-examples)

(define-example halos (d :size 820 :aspect 0.42)
  "Text, markers, symbols and contours over a busy image, without halos
   and inside WITH-HALO; and dark halos on a light image."
  (page-title d "Halos (outlines) over images")
  (let ((sky (sky-image)))
    (flet ((overlay (vp)
             (with-color (vp :black)
               (plot-contours vp sky :levels '(40 160) :labels t :label-size 10
                                     :label-format (lambda (v) (format nil "~D" (round v)))))
             (with-color (vp :red) (plot-points vp '(20 45 140) '(95 40 100) :marker :filled-star
                                                                            :size 6))
             (with-color (vp :yellow) (plot-point vp 120 40 :marker :saturn :size 9))
             (with-color (vp :black)
               (plot-text vp 64 70 "bright star" :valign :center :size 14)
               (plot-text vp 100 108 "ring nebula" :halign :center :size 14))))
      (loop for (halo x1 title) in '((nil 0.03 "no halo") (t 0.35 "(with-halo () ...)"))
            do (let ((vp (make-viewport d x1 (+ x1 0.3) 0.08 0.88)))
                 (set-window-equal-scales vp -0.5 159.5 -0.5 119.5)
                 (plot-image vp sky :colormap :viridis :scale :asinh :vmin 0 :vmax 300)
                 (if halo
                     (with-halo () (overlay vp))
                     (overlay vp))
                 (draw-box vp :title title :size 11))))
    ;; dark halos with white ink on a light image
    (let ((vp (make-viewport d 0.67 0.97 0.08 0.88)))
      (set-window-equal-scales vp -0.5 159.5 -0.5 119.5)
      (plot-image vp sky :colormap :gray :scale :asinh :vmin 0 :vmax 300 :reverse t)
      (with-halo (:color :black)
        (with-color (vp :white)
          (plot-contours vp sky :levels '(40 160) :labels :curved :label-size 10
                                :label-format (lambda (v) (format nil "~D" (round v))))
          (plot-points vp '(20 45 140) '(95 40 100) :marker :star :size 6)
          (plot-text vp 64 70 "bright star" :valign :center :size 14)))
      (draw-box vp :title "(with-halo (:color :black) ...)" :size 11))))
