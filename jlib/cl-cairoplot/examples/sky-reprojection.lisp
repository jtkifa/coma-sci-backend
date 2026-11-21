;;;; sky-reprojection.lisp — example sky-reprojection
;;;;
;;;; Images whose WCS is not linear in the plot's projection, resampled
;;;; onto it.  Each frame is made by sampling one mock sky (a catalog of
;;;; stars and a galaxy) through the frame's own WCS, so when the
;;;; reprojection is right every star sits in its catalog circle.
;;;; Left: a mosaic of two rotated frames about other tangent points,
;;;; with contours mapped through each WCS.  Right: a frame with
;;;; pincushion distortion, from a WCS class defined here that only
;;;; implements the two protocol methods WCS-PIXEL-TO-SKY and
;;;; WCS-SKY-TO-PIXEL (as an adapter to a full WCS library would).
;;;;
;;;; Uses: make-wcs, wcs-pixel-to-sky, wcs-sky-to-pixel (methods),
;;;;       plot-image :wcs, plot-contours :wcs, make-tan-projection,
;;;;       set-window-tan, draw-sky-grid, plot-points, plot-polyline,
;;;;       plot-polygon (:coords :sky)

(in-package :cl-cairoplot-examples)

;;; A WCS with radial (pincushion) distortion about pixel CENTER: the
;;; pixel at offset r from it is really at r (1 + K r^2), in LINEAR.

(defclass distorted-wcs ()
  ((linear :initarg :linear :reader distorted-wcs-linear)
   (k :initarg :k :reader distorted-wcs-k)
   (center :initarg :center :reader distorted-wcs-center)))

(defmethod wcs-pixel-to-sky ((wcs distorted-wcs) i j)
  (destructuring-bind (cx cy) (distorted-wcs-center wcs)
    (let* ((u (- i cx)) (v (- j cy))
           (f (+ 1 (* (distorted-wcs-k wcs) (+ (* u u) (* v v))))))
      (wcs-pixel-to-sky (distorted-wcs-linear wcs) (+ cx (* u f)) (+ cy (* v f))))))

(defmethod wcs-sky-to-pixel ((wcs distorted-wcs) ra dec)
  (destructuring-bind (cx cy) (distorted-wcs-center wcs)
    (multiple-value-bind (p q) (wcs-sky-to-pixel (distorted-wcs-linear wcs) ra dec)
      (let* ((u (- p cx)) (v (- q cy))
             (s (sqrt (+ (* u u) (* v v))))
             (k (distorted-wcs-k wcs))
             (r s))
        ;; solve r (1 + k r^2) = s by Newton's method
        (dotimes (n 6)
          (setf r (- r (/ (- (* r (+ 1 (* k r r))) s) (+ 1 (* 3 k r r))))))
        (let ((scale (if (zerop s) 1 (/ r s))))
          (values (+ cx (* u scale)) (+ cy (* v scale))))))))

;;; The mock sky

(defun mock-catalog (ra0 dec0 radius n seed)
  "N stars (RA DEC FLUX) within about RADIUS degrees of RA0, DEC0."
  (loop for k below n
        collect (list (+ ra0 (/ (* 2 radius (noise (+ seed (* 3 k))))
                                (cos (* dec0 (/ pi 180)))))
                      (+ dec0 (* 2 radius (noise (+ seed (* 3 k) 1))))
                      (+ 40 (* 900 (expt (+ 0.5 (noise (+ seed (* 3 k) 2))) 3))))))

(defun mock-sky (catalog galaxy sigma)
  "The surface brightness at RA, DEC of stars in CATALOG (Gaussians of
   SIGMA degrees) and GALAXY (RA DEC PEAK SIZE), over a background."
  (lambda (ra dec)
    (let ((cosd (cos (* dec (/ pi 180)))))
      (flet ((r2 (ra* dec*) (+ (expt (* (- ra ra*) cosd) 2) (expt (- dec dec*) 2))))
        (destructuring-bind (gra gdec peak size) galaxy
          (+ 10
             (* peak (exp (- (sqrt (/ (r2 gra gdec) (* size size))))))
             (loop for (ra* dec* flux) in catalog
                   sum (* flux (exp (/ (r2 ra* dec*) (* -2 sigma sigma)))))))))))

(defun observe (sky wcs w h seed)
  "A W by H frame of SKY through WCS, with noise."
  (let ((a (make-array (list h w) :element-type 'double-float)))
    (dotimes (j h a)
      (dotimes (i w)
        (setf (aref a j i)
              (float (+ (multiple-value-call sky (wcs-pixel-to-sky wcs i j))
                        (* 3 (gaussian-noise (+ seed i (* 1000 j)))))
                     1d0))))))

(defun frame-outline (wcs w h &optional (n 24))
  "RA and Dec lists along the edges of a W by H frame with WCS."
  (let ((ras '()) (decs '()))
    (flet ((add (i j) (multiple-value-bind (ra dec) (wcs-pixel-to-sky wcs i j)
                        (push ra ras) (push dec decs))))
      (dotimes (k n) (add (- (* k (/ w n)) 0.5) -0.5))
      (dotimes (k n) (add (- w 0.5) (- (* k (/ h n)) 0.5)))
      (dotimes (k n) (add (- w 0.5 (* k (/ w n))) (- h 0.5)))
      (dotimes (k n) (add -0.5 (- h 0.5 (* k (/ h n))))))
    (values (nreverse ras) (nreverse decs))))

(defun mark-catalog (d catalog)
  (with-style (d :color :red :line-width 0.6)
    (plot-points d (mapcar #'first catalog) (mapcar #'second catalog)
                 :coords :sky :marker :circle :size 9)))

(define-example sky-reprojection (d :size 900 :aspect 0.5)
  "Reprojection: a mosaic of two frames about other tangent points, and
   a frame with a distorted WCS (a user-defined WCS class), each
   resampled onto the plot's TAN projection, with contours mapped
   through each WCS and catalog stars circled."
  (page-title d "Images reprojected through their WCS")
  ;; a mosaic: two rotated frames, each about its own tangent point
  (let* ((catalog (mock-catalog 150 30 0.3 30 11))
         (sky (mock-sky catalog '(150.02 30.04 120 0.02) 0.006))
         (frames (list (make-wcs :crval '(149.86 30.06) :crpix '(60.5 50.5)
                                 :cdelt '(-0.003 0.003) :crota 12)
                       (make-wcs :crval '(150.15 29.95) :crpix '(60.5 50.5)
                                 :cdelt '(-0.003 0.003) :crota -20)))
         (vp (make-viewport d 0.1 0.45 0.16 0.84)))
    (set-window-tan vp (make-tan-projection 150 30) 0.36)
    (loop for wcs in frames
          for seed from 1
          for image = (observe sky wcs 120 100 (* 100000 seed))
          do (plot-image vp image :wcs wcs :colormap :gray :scale :asinh
                                  :vmin 0 :vmax 400 :reverse t)
             (with-style (vp :color :blue :line-width 0.5)
               (plot-contours vp image :wcs wcs :levels '(60 240)))
             (multiple-value-bind (ras decs) (frame-outline wcs 120 100)
               (with-style (vp :color :teal :line-width 0.8)
                 (plot-polygon vp ras decs :coords :sky))))
    (mark-catalog vp catalog)
    (draw-sky-grid vp :size 11)
    (plot-text d 0.275 0.89 "two frames, rotated, about other tangent points"
               :coords :device :halign :center :size 12))
  ;; a frame with pincushion distortion, through a user-defined WCS
  (let* ((catalog (mock-catalog 210 60 0.17 25 23))
         (sky (mock-sky catalog '(209.95 60.03 150 0.015) 0.004))
         (wcs (make-instance 'distorted-wcs
                             :linear (make-wcs :crval '(210 60) :crpix '(80.5 80.5)
                                               :cdelt '(-0.002 0.002) :crota 0)
                             :center '(79.5 79.5) :k 1.5d-5))
         (image (observe sky wcs 160 160 7))
         (vp (make-viewport d 0.6 0.95 0.16 0.84)))
    (set-window-tan vp (make-tan-projection 210 60) 0.2)
    (plot-image vp image :wcs wcs :colormap :gray :scale :asinh
                         :vmin 0 :vmax 400 :reverse t :interpolation :bilinear)
    (with-style (vp :color :blue :line-width 0.5)
      (plot-contours vp image :wcs wcs :levels '(60 240)))
    (multiple-value-bind (ras decs) (frame-outline wcs 160 160)
      (with-style (vp :color :teal :line-width 0.8)
        (plot-polygon vp ras decs :coords :sky)))
    (mark-catalog vp catalog)
    (draw-sky-grid vp :size 11)
    (plot-text d 0.775 0.89 "pincushion distortion (user-defined WCS class)"
               :coords :device :halign :center :size 12)))
