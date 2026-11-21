;;;; all-sky-projections.lisp — example all-sky-projections
;;;;
;;;; One synthetic all-sky map (a Milky Way band brightest toward the
;;;; Galactic center, a bulge and the Magellanic Clouds), held as a plate
;;;; carree (CAR) image with its WCS, reprojected onto six projections:
;;;; the all-sky Hammer-Aitoff and Mollweide ellipses, whole-boundary
;;;; zenithal views (the northern hemisphere in SIN, the whole sky about
;;;; the south pole in ZEA), and a wide stereographic field framed by a
;;;; rectangle.  Grids are labeled around and inside each boundary, or
;;;; on the frame when the boundary is not all in view; drawing is
;;;; clipped to the boundary.
;;;;
;;;; Uses: make-projection, set-window-projection, make-wcs :projection,
;;;;       plot-image :wcs, draw-sky-grid, plot-points (:coords :sky),
;;;;       plot-text, equatorial-to-frame

(in-package :cl-cairoplot-examples)

(defun mock-sky-brightness (ra dec)
  "A synthetic sky at RA, DEC: a Milky Way band brightest toward the
   Galactic center, a bulge and the Magellanic Clouds."
  (multiple-value-bind (l b) (equatorial-to-frame :galactic ra dec)
    (let ((l (- (mod (+ l 180) 360) 180)))
      (flet ((blob (ra0 dec0 size)
               (let ((dra (* (- (mod (+ (- ra ra0) 180) 360) 180) (cos (* dec (/ pi 180))))))
                 (exp (- (/ (+ (* dra dra) (expt (- dec dec0) 2)) (* 2 size size)))))))
        (+ 2
           (* 60 (exp (- (/ (* b b) (* 2 7 7))))
              (+ 0.35 (* 0.65 (exp (- (/ (* l l) (* 2 70 70)))))))
           (* 50 (exp (- (/ (+ (* l l) (* b b)) (* 2 9 9)))))
           (* 25 (blob 80.9 -69.8 4))       ; LMC
           (* 15 (blob 13.2 -72.8 2.5)))))))  ; SMC

(defun mock-sky-image (wcs w h)
  "A W by H image of MOCK-SKY-BRIGHTNESS through WCS, with noise."
  (let ((a (make-array (list h w) :element-type 'double-float)))
    (dotimes (j h a)
      (dotimes (i w)
        (setf (aref a j i)
              (float (+ (multiple-value-call #'mock-sky-brightness (wcs-pixel-to-sky wcs i j))
                        (* 2 (noise (+ i (* 1000 j)))))
                     1d0))))))

(defun mock-milky-way (w h)
  "A W by H all-sky map in CAR (RA left to right decreasing, as a FITS
   image with CDELT1 < 0) and its WCS."
  (let ((wcs (make-wcs :projection :car :crval '(180 0)
                       :crpix (list (+ 0.5 (/ w 2)) (+ 0.5 (/ h 2)))
                       :cdelt (list (- (/ 360 w)) (/ 180 h)))))
    (values (mock-sky-image wcs w h) wcs)))

(defun sky-panel (d map wcs projection box title &key half-width (ra-target nil) (size 9))
  (destructuring-bind (x1 x2 y1 y2) box
    (let ((vp (make-viewport d x1 x2 y1 y2)))
      (set-window-projection vp projection half-width)
      (plot-image vp map :wcs wcs :colormap :inferno :scale :sqrt :vmin 0 :vmax 110)
      ;; bright stars by RA and Dec: Sirius, Canopus, Arcturus, Vega, Polaris
      (with-color (vp :cyan)
        (plot-points vp '(101.29 95.99 213.92 279.23 37.95) '(-16.72 -52.70 19.18 38.78 89.26)
                     :coords :sky :marker :star :size 6))
      (with-style (vp :line-width 0.8)
        (draw-sky-grid vp :size size :ra-target ra-target :grid-color '(1 1 1 0.35)))
      (plot-text d (/ (+ x1 x2) 2) (+ y2 0.025) title :coords :device :halign :center
                                                       :valign :bottom :size 11))))

(define-example all-sky-projections (d :size 1000 :aspect 0.68)
  "One all-sky map (a CAR image with its WCS) reprojected onto
   Hammer-Aitoff, Mollweide, SIN, ZEA and STG projections, with grids
   labeled around each boundary or on the frame."
  (page-title d "All-sky and zenithal projections of one map")
  (multiple-value-bind (map wcs) (mock-milky-way 360 180)
    (sky-panel d map wcs (make-projection :ait 0) '(0.06 0.47 0.56 0.9)
               "Hammer-Aitoff (AIT) about 0h")
    (sky-panel d map wcs (make-projection :mol 180) '(0.55 0.96 0.56 0.9)
               "Mollweide (MOL) about 12h")
    (sky-panel d map wcs (make-projection :sin 0 90) '(0.05 0.29 0.08 0.44)
               "SIN: the northern hemisphere")
    (sky-panel d map wcs (make-projection :zea 0 -90) '(0.38 0.62 0.08 0.44)
               "ZEA: the whole sky about the south pole")
    (sky-panel d map wcs (make-projection :stg 266.4 -29) '(0.73 0.97 0.08 0.44)
               "STG about the Galactic center" :half-width 50)))
