;;;; clipping-regions.lisp — example clipping-regions
;;;;
;;;; Composite clipping: an annulus (intersection with an outside), and
;;;; the outside of a union.
;;;;
;;;; Uses: make-viewport :clipping-region nil, set-window-equal-scales,
;;;;       with-clipping, region-intersection, circle-region,
;;;;       region-outside, region-union, rectangle-region,
;;;;       polygon-region, with-color, plot-rectangle, plot-circle,
;;;;       plot-text, plot-line

(in-package :cl-cairoplot-examples)

(define-example clipping-regions (d :aspect 0.55)
  "Composite clipping: an annulus (intersection with an outside), and
   the outside of a union."
  (page-title d "Clipping regions")
  ;; :CLIPPING-REGION NIL: the viewports themselves don't clip, so the
  ;; captions below them show
  (let ((vp (make-viewport d 0.05 0.5 0.08 0.88 :clipping-region nil)))
    (set-window-equal-scales vp -2 2 -2 2)
    (with-clipping (vp (region-intersection
                        (circle-region 0 0 1.8)
                        (region-outside (circle-region 0 0 0.8))
                        (region-outside (region-union
                                         (rectangle-region -2 -0.1 2 0.1)
                                         (polygon-region '(-0.1 0.1 0.1 -0.1)
                                                         '(-2 -2 2 2))))))
      (with-color (vp :dark-gray) (plot-rectangle vp -2 -2 2 2 :fill t)))
    (with-color (vp :red) (plot-circle vp 0 0 1.9))
    (plot-text vp 0 -2 "annulus minus a cross" :halign :center :valign :top :size 11))
  (let ((vp (make-viewport d 0.52 0.97 0.08 0.88 :clipping-region nil)))
    (set-window-equal-scales vp -2 2 -2 2)
    (with-clipping (vp (region-outside (region-union (circle-region -0.7 0 0.6)
                                                     (circle-region 0.7 0 0.6))))
      (loop for y from -2 to 2 by 0.1
            do (with-color (vp (list 0 (/ (+ y 2) 4) 1)) (plot-line vp -2 y 2 y))))
    (plot-text vp 0 -2 "outside a union of two circles"
               :halign :center :valign :top :size 11)))
