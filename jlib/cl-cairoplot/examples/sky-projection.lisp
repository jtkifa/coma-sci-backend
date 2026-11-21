;;;; sky-projection.lisp — example sky-projection
;;;;
;;;; True TAN projections: a field around the north celestial pole, where
;;;; lines of constant RA radiate from the pole and lines of constant Dec
;;;; are circles, with catalog points placed by RA and Dec (:COORDS
;;;; :SKY); and an image placed by its WCS (rotated 25 degrees by its CD
;;;; matrix), with contours and a catalog position on it and the curved
;;;; RA/Dec grid.
;;;;
;;;; Uses: make-tan-projection, set-window-tan, draw-sky-grid, make-wcs,
;;;;       wcs-projection, wcs-pixel-to-sky, plot-image :wcs,
;;;;       plot-contours :wcs, plot-points, plot-text

(in-package :cl-cairoplot-examples)

(define-example sky-projection (d :size 900 :aspect 0.5)
  "TAN projections: a field around the celestial pole with curved
   RA/Dec grid lines and points placed by RA and Dec; an image placed by
   its rotated WCS, with contours, a catalog position and the grid."
  (page-title d "Sky projections (TAN) and WCS images")
  ;; around the pole: RA lines are spokes, Dec lines circles
  (let ((vp (make-viewport d 0.1 0.45 0.16 0.84)))
    (set-window-tan vp (make-tan-projection 60 87) 4.2)
    (with-color (vp :red)
      ;; a ring of catalog points at dec +84 (a grid line), every 20
      ;; degrees of RA
      (plot-points vp (loop for ra from 0 below 360 by 20 collect ra)
                   (make-list 18 :initial-element 84)
                   :coords :sky :marker :filled-circle :size 3))
    (plot-point vp 0 90 :coords :sky :marker :plus :size 7)
    (plot-text vp 0 90 " pole" :coords :sky :size 11 :valign :bottom)
    (draw-sky-grid vp :size 11)
    (plot-text d 0.275 0.89 "TAN about (4h, +87°): the pole inside" :coords :device
               :halign :center :size 12))
  ;; an image with a rotated WCS, placed exactly by its CD matrix
  (let* ((image (sky-image))
         (wcs (make-wcs :crval '(150 2.2) :crpix '(80.5 60.5)
                        :cdelt '(-0.0012 0.0012) :crota 25))
         (vp (make-viewport d 0.6 0.95 0.16 0.84)))
    (set-window-tan vp (wcs-projection wcs) 0.13)
    (plot-image vp image :wcs wcs :colormap :gray
                         :scale :asinh :vmin 0 :vmax 300 :reverse t)
    (with-style (vp :color :blue :line-width 0.6)
      (plot-contours vp image :wcs wcs :levels '(40 160 640)))
    ;; the bright star's catalog position (from its pixel, via the WCS)
    (multiple-value-bind (ra dec) (wcs-pixel-to-sky wcs 60 70)
      (with-color (vp :red)
        (plot-point vp ra dec :coords :sky :marker :circle :size 9)
        (plot-text vp ra dec (format nil "  ~,4F, ~,4F" ra dec) :coords :sky :size 10
                                                                 :valign :bottom :halo t)))
    (draw-sky-grid vp :size 11)
    (plot-text d 0.775 0.89 "image with a WCS rotated 25°" :coords :device
               :halign :center :size 12)))
