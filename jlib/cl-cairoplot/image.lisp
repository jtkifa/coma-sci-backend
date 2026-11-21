;;;; image.lisp — images through colormaps, and colorbars
;;;;
;;;; (PLOT-IMAGE DEVICE ARRAY :COLORMAP :VIRIDIS) draws a 2D array of
;;;; numbers (as PGPLOT's PGIMAG/PGGRAY), (AREF ARRAY ROW COLUMN) with
;;;; columns along x and rows along y.  By default pixel (row j, column i)
;;;; is centered on the :PLOT point (i, j) and row 0 is at the bottom
;;;; (ORIGIN :LOWER, the astronomical convention); X-RANGE and Y-RANGE
;;;; place the image's outer edges anywhere instead.
;;;;
;;;; Values map to colors in three steps: clip to VMIN..VMAX (default the
;;;; data's range, or a central PERCENTILE of it), apply SCALE to the
;;;; resulting fraction, and look the result up in the COLORMAP.  NaNs
;;;; get NAN-COLOR (default transparent).
;;;;
;;;; The image is drawn through the full plot transform, so it rotates
;;;; and skews with the viewport, and is clipped like any other drawing.
;;;; With :WCS (sky.lisp) the image is placed on a sky plot by its world
;;;; coordinate system: exactly by an affine when the WCS is linear in
;;;; the plot's projection, else reprojected (REPROJECT-IMAGE).
;;;; DRAW-COLORBAR (as PGWEDG) draws the matching scale bar.

(in-package :cl-cairoplot)

(defun scale-function (scale)
  "The function of a fraction 0..1 named by SCALE: :LINEAR, :SQRT,
   :SQUARE, :LOG (log(1 + 1000 f) / log 1001, as DS9) or :ASINH
   (asinh(10 f) / asinh 10); or SCALE itself if a function."
  (if (functionp scale)
      scale
      (ecase scale
        (:linear #'identity)
        (:sqrt #'sqrt)
        (:square (lambda (f) (* f f)))
        (:log (let ((norm (log 1001d0))) (lambda (f) (/ (log (+ 1 (* 1000 f))) norm))))
        (:asinh (let ((norm (asinh 10d0))) (lambda (f) (/ (asinh (* 10 f)) norm)))))))

(defun nan-p (v)
  (and (floatp v) (float-utils:float-nan-p v)))

(defun image-limits (array &key percentile)
  "Values VMIN VMAX of the non-NaN elements of ARRAY: the minimum and
   maximum, or with PERCENTILE (e.g. 99) the limits of the central
   PERCENTILE percent of the values."
  (let ((values (make-array (array-total-size array) :element-type 'double-float
                                                     :fill-pointer 0)))
    (dotimes (k (array-total-size array))
      (let ((v (row-major-aref array k)))
        (unless (nan-p v) (vector-push (float v 1d0) values))))
    (when (zerop (length values))
      (error "~A has no non-NaN values." array))
    (if percentile
        (let* ((sorted (sort values #'<))
               (n (length sorted))
               (cut (/ (- 100 percentile) 200)))
          (values (aref sorted (floor (* cut (1- n))))
                  (aref sorted (ceiling (* (- 1 cut) (1- n))))))
        (values (reduce #'min values) (reduce #'max values)))))

(defun image-pixels (array vmin vmax &key (colormap :gray) (scale :linear)
                                          reverse nan-color)
  "ARRAY mapped to a (SIMPLE-ARRAY (UNSIGNED-BYTE 32) (H W)) of ARGB words."
  (let* ((lut (colormap-lut (if reverse (reverse-colormap colormap) colormap)))
         (top (1- (length lut)))
         (f (scale-function scale))
         (range (float (- vmax vmin) 1d0))
         (nan (if nan-color
                  (let ((c (parse-color nan-color)))
                    (argb-word (color-red c) (color-green c) (color-blue c)
                               (color-alpha c)))
                  0))
         (pixels (make-array (array-dimensions array)
                             :element-type '(unsigned-byte 32))))
    (when (zerop range) (setf range 1d0))
    (dotimes (k (array-total-size array) pixels)
      (let ((v (row-major-aref array k)))
        (setf (row-major-aref pixels k)
              (if (nan-p v)
                  nan
                  (let ((fraction (max 0d0 (min 1d0 (/ (- v vmin) range)))))
                    (aref lut (round (* top (funcall f fraction)))))))))))

(defplot plot-image (device array &key (colormap :gray) vmin vmax percentile
                                     (scale :linear) reverse nan-color
                                     x-range y-range (origin :lower) transform
                                     (interpolation :nearest) (coords :plot) wcs)
  "Draw the 2D ARRAY through COLORMAP (see the top of image.lisp).
   VMIN, VMAX: data values at the ends of the colormap (default from
   IMAGE-LIMITS with PERCENTILE).  SCALE: :LINEAR, :SQRT, :SQUARE, :LOG,
   :ASINH or a function of 0..1.  REVERSE flips the colormap.  X-RANGE,
   Y-RANGE: lists (X1 X2), (Y1 Y2) of the image's outer edges in COORDS
   (default pixel centers at integer coordinates).  ORIGIN :LOWER puts
   row 0 at Y1, :UPPER at Y2.  TRANSFORM, an affine (or list (A B C D
   E F)) taking pixel centers (column i, row j, 0-based) to COORDS as
   x = A i + B j + E, y = C i + D j + F, places the image instead,
   rotated or sheared if need be (e.g. by a WCS CD matrix, see
   WCS-IMAGE-TRANSFORM).  WCS, any object following the WCS protocol in
   sky.lisp, places the image on the current viewport's sky projection
   (COORDS :PLOT), reprojecting it if need be.  INTERPOLATION :NEAREST
   or :BILINEAR.  Returns the VMIN and VMAX used, for DRAW-COLORBAR."
  (when (functionp transform)
    (error "PLOT-IMAGE takes an affine :TRANSFORM; place an image by a WCS with :WCS."))
  (destructuring-bind (h w) (array-dimensions array)
    (multiple-value-bind (lo hi) (if (and vmin vmax)
                                     (values vmin vmax)
                                     (image-limits array :percentile percentile))
      (let ((vmin (or vmin lo)) (vmax (or vmax hi)))
        (when wcs
          (assert (eq coords :plot) () "An image placed by :WCS needs :COORDS :PLOT.")
          (let ((vp (current-viewport device)))
            (or (setf transform (wcs-plane-transform wcs (viewport-sky-projection vp)))
                ;; not linear in the plot's projection: resample onto the plot
                (multiple-value-bind (out x-range y-range)
                    (reproject-image array wcs vp :interpolation interpolation)
                  (when out
                    (plot-image device out :colormap colormap :vmin vmin :vmax vmax
                                           :scale scale :reverse reverse :nan-color nan-color
                                           :x-range x-range :y-range y-range
                                           :interpolation interpolation))
                  (return-from plot-image (values vmin vmax))))))
        (destructuring-bind (x1 x2) (or x-range (list -0.5 (- w 0.5)))
          (destructuring-bind (y1 y2) (or y-range (list -0.5 (- h 0.5)))
            (let* ((sy (/ (- y2 y1) h))
                   ;; image pixel space (column u, row v) -> COORDS
                   (to-coords
                     (if transform
                         ;; pixel (i,j) covers [i,i+1] x [j,j+1] in image space,
                         ;; so its center (i+1/2, j+1/2) must go to TRANSFORM(i,j)
                         (affine-compose (as-affine transform)
                                         (make-affine 1 0 0 1 -1/2 -1/2))
                         (ecase origin
                           (:lower (make-affine (/ (- x2 x1) w) 0 0 sy x1 y1))
                           (:upper (make-affine (/ (- x2 x1) w) 0 0 (- sy) x1 y2)))))
                   (m (affine-compose (coords-transform device coords) to-coords)))
              (with-saved-state (device)
                ;; TRANSFORM takes cairo's order: xx yx xy yy x0 y0
                (transform device (aref m 0) (aref m 2) (aref m 1) (aref m 3)
                           (aref m 4) (aref m 5))
                (draw-argb-image device
                                 (image-pixels array vmin vmax :colormap colormap
                                                               :scale scale
                                                               :reverse reverse
                                                               :nan-color nan-color)
                                 :filter interpolation)))))
        (values vmin vmax)))))

;;; ============================================================
;;; Colorbars
;;; ============================================================

(defplot colorbar-viewport (viewport &key (side :right) (width 0.025) (gap 0.02))
  "A viewport for a colorbar beside VIEWPORT on SIDE (:RIGHT, :LEFT,
   :TOP or :BOTTOM), WIDTH thick and GAP away (fractions of the page
   width), turned with VIEWPORT if it is rotated."
  (let ((device (viewport-device viewport)))
    (multiple-value-bind (x1 x2 y1 y2) (viewport-rectangle viewport :world)
      (multiple-value-bind (bx1 bx2 by1 by2)
          (ecase side
            (:right  (values (+ x2 gap) (+ x2 gap width) y1 y2))
            (:left   (values (- x1 gap width) (- x1 gap) y1 y2))
            (:top    (values x1 x2 (+ y2 gap) (+ y2 gap width)))
            (:bottom (values x1 x2 (- y1 gap width) (- y1 gap))))
        ;; a viewport turns about its own center: move the bar's center
        ;; by the parent's rotation about the parent's center (in :WORLD,
        ;; where units are equal in x and y) so the two turn together
        (let* ((angle (viewport-angle viewport))
               (r (* angle (/ pi 180)))
               (cx (/ (+ x1 x2) 2)) (cy (/ (+ y1 y2) 2))
               (bx (- (/ (+ bx1 bx2) 2) cx)) (by (- (/ (+ by1 by2) 2) cy))
               (dx (- (- (* bx (cos r)) (* by (sin r))) bx))
               (dy (- (+ (* bx (sin r)) (* by (cos r))) by)))
          (make-viewport device (+ bx1 dx) (+ bx2 dx) (+ by1 dy) (+ by2 dy)
                         :coords :world :angle angle))))))

(defplot draw-colorbar (viewport &key (colormap :gray) vmin vmax (scale :linear)
                                    reverse label (orientation :vertical) size
                                    levels colors)
  "Draw a colorbar for VMIN..VMAX through COLORMAP and SCALE (as given
   to PLOT-IMAGE) filling VIEWPORT, with numbers along it and LABEL.
   With LEVELS (as returned by PLOT-FILLED-CONTOURS), draw discrete
   bands between them instead, colored as there (COLORMAP or COLORS).
   ORIENTATION :VERTICAL puts the numbers on the right and LABEL on
   top; :HORIZONTAL puts both below."
  (when levels
    (let* ((levels (sort (copy-list levels) #'<))
           (lo (first levels)) (hi (car (last levels)))
           (vertical (eq orientation :vertical)))
      (if vertical (set-window viewport 0 1 lo hi) (set-window viewport lo hi 0 1))
      (with-viewport (viewport)
        (loop for (a b) on levels
              for color in (band-colors levels colormap colors)
              while b
              do (with-color ((viewport-device viewport) color)
                   (if vertical
                       (plot-rectangle (viewport-device viewport) 0 a 1 b :fill t)
                       (plot-rectangle (viewport-device viewport) a 0 b 1 :fill t)))))
      (if vertical
          (draw-box viewport :ticks '(:left :right) :numbers '(:right) :title label
                             :size size :minor nil
                             :y-ticks (lambda (lo hi)
                                        (declare (ignore lo hi))
                                        (values (mapcar (lambda (v) (list v (format-decimal
                                                                            v (level-step levels))))
                                                        levels)
                                                '())))
          (draw-box viewport :ticks '(:bottom :top) :numbers '(:bottom) :x-label label
                             :size size :minor nil
                             :x-ticks (lambda (lo hi)
                                        (declare (ignore lo hi))
                                        (values (mapcar (lambda (v) (list v (format-decimal
                                                                            v (level-step levels))))
                                                        levels)
                                                '()))))
      (return-from draw-colorbar viewport)))
  (let* ((n 256)
         (values (make-array (if (eq orientation :vertical) (list n 1) (list 1 n))
                             :element-type 'double-float)))
    (dotimes (i n)
      (setf (row-major-aref values i)
            (float (+ vmin (* (- vmax vmin) (/ (+ i 0.5) n))) 1d0)))
    (if (eq orientation :vertical)
        (set-window viewport 0 1 vmin vmax)
        (set-window viewport vmin vmax 0 1))
    (with-viewport (viewport)
      (plot-image (viewport-device viewport) values
                  :colormap colormap :vmin vmin :vmax vmax :scale scale
                  :reverse reverse
                  :x-range (if (eq orientation :vertical) '(0 1) (list vmin vmax))
                  :y-range (if (eq orientation :vertical) (list vmin vmax) '(0 1))))
    (if (eq orientation :vertical)
        (draw-box viewport :ticks '(:left :right) :numbers '(:right)
                           :title label :size size)
        (draw-box viewport :ticks '(:bottom :top) :numbers '(:bottom)
                           :x-label label :size size))
    viewport))

(defun level-step (levels)
  "The smallest gap between consecutive LEVELS, for formatting them."
  (let ((gaps (remove 0 (mapcar (lambda (a b) (abs (- b a))) levels (rest levels)))))
    (if gaps (reduce #'min gaps) 1)))
