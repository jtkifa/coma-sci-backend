;;;; sky-coordinate-frames.lisp — example sky-coordinate-frames
;;;;
;;;; Galactic and ecliptic coordinates.  A projection can be set up in
;;;; any frame (its reference point and grid are then galactic or
;;;; ecliptic), while :COORDS :SKY and every WCS stay in RA and Dec, so
;;;; the same equatorial all-sky map reprojects onto each.  Grids in a
;;;; second frame overlay the first.  Top: the sky in galactic
;;;; Hammer-Aitoff (the Milky Way along the equator) with the equatorial
;;;; grid overlaid, and in ecliptic Mollweide with the galactic grid
;;;; overlaid.  Bottom: an equatorial TAN field around the Galactic
;;;; center labeled in RA/Dec on the bottom and left and in galactic
;;;; l/b on the top and right; and an image whose WCS is galactic
;;;; (CTYPE GLON-CAR), placed exactly in a galactic CAR plot.
;;;;
;;;; Uses: make-projection (frame), set-window-projection, draw-sky-grid
;;;;       (:coordinates :labels :lon-sides :lat-sides :frame),
;;;;       wcs-from-header (GLON-CAR), plot-image :wcs, plot-points,
;;;;       frame-to-equatorial

(in-package :cl-cairoplot-examples)

(define-example sky-coordinate-frames (d :size 1000 :aspect 0.72)
  "Galactic and ecliptic frames: projections in each, grids of one
   frame overlaid on another, frame labels on opposite sides, and an
   image with a galactic WCS placed exactly."
  (page-title d "Equatorial, galactic and ecliptic coordinates")
  (multiple-value-bind (map wcs) (mock-milky-way 360 180)
    (flet ((show-map (vp)
             (plot-image vp map :wcs wcs :colormap :inferno :scale :sqrt :vmin 0 :vmax 110
                                :interpolation :bilinear))
           (title (box text &optional (above 0.03))
             (destructuring-bind (x1 x2 y1 y2) box
               (declare (ignore y1))
               (plot-text d (/ (+ x1 x2) 2) (+ y2 above) text :coords :device
                                                               :halign :center :size 11))))
      ;; galactic Hammer-Aitoff, equatorial grid overlaid
      (let* ((box '(0.05 0.47 0.56 0.9)) (vp (apply #'make-viewport d box)))
        (set-window-projection vp (make-projection :ait 0 0 :galactic))
        (show-map vp)
        (draw-sky-grid vp :size 9 :grid-color '(1 1 1 0.4))
        (with-style (vp :line-width 0.6)
          (draw-sky-grid vp :coordinates :equatorial :labels nil :frame nil
                            :grid-color '(0.3 0.9 1 0.6)))
        (title box "galactic AIT, equatorial grid in cyan"))
      ;; ecliptic Mollweide, galactic grid overlaid
      (let* ((box '(0.55 0.97 0.56 0.9)) (vp (apply #'make-viewport d box)))
        (set-window-projection vp (make-projection :mol 180 0 :ecliptic))
        (show-map vp)
        (draw-sky-grid vp :size 9 :grid-color '(1 1 1 0.4))
        (with-style (vp :line-width 0.6)
          (draw-sky-grid vp :coordinates :galactic :labels nil :frame nil
                            :grid-color '(0.4 1 0.4 0.6)))
        (title box "ecliptic MOL, galactic grid in green"))
      ;; an equatorial field labeled in both frames
      (let* ((box '(0.1 0.42 0.1 0.42)) (vp (apply #'make-viewport d box)))
        (set-window-projection vp (make-projection :tan 266.4 -28.9) 9)
        (show-map vp)
        (with-color (vp :cyan)
          ;; the Galactic center, placed by its galactic coordinates
          (multiple-value-bind (ra dec) (frame-to-equatorial :galactic 0 0)
            (plot-point vp ra dec :coords :sky :marker :plus :size 10)))
        (draw-sky-grid vp :size 9 :grid-color '(1 1 1 0.45)
                          :lon-sides '(:bottom) :lat-sides '(:left))
        (with-style (vp :color :dark-gray)
          (draw-sky-grid vp :size 9 :coordinates :galactic :frame nil
                            :grid-color '(0.4 1 0.4 0.7)
                            :lon-sides '(:top) :lat-sides '(:right)))
        (title box "TAN about the Galactic center: RA/Dec and l/b" 0.07))
      ;; an image with a galactic WCS, exact in a galactic CAR plot
      (let* ((gwcs (wcs-from-header '(:ctype1 "GLON-CAR" :crval1 0 :crval2 0
                                      :crpix1 120.5 :crpix2 60.5
                                      :cdelt1 -0.25 :cdelt2 0.25)))
             (image (mock-sky-image gwcs 240 120))
             (box '(0.56 0.96 0.1 0.42)) (vp (apply #'make-viewport d box)))
        (set-window-projection vp (wcs-projection gwcs) 32 17)
        (plot-image vp image :wcs gwcs :colormap :inferno :scale :sqrt :vmin 0 :vmax 110)
        (draw-sky-grid vp :size 9 :grid-color '(1 1 1 0.45) :lon-sides '(:bottom)
                          :lat-sides '(:left))
        (with-style (vp :color :dark-gray)
          (draw-sky-grid vp :size 9 :coordinates :equatorial :frame nil
                            :grid-color '(0.3 0.9 1 0.7)
                            :lon-sides '(:top) :lat-sides '(:right)))
        (title box "GLON-CAR image (exact), RA/Dec on top and right" 0.07)))))
