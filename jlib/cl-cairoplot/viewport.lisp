;;;; viewport.lisp — viewports and :PLOT coordinates
;;;;
;;;; A VIEWPORT is a rectangle of a plot device, its own reference frame,
;;;; together with an affine transform from :PLOT coordinates into it.
;;;; Usually that is a WINDOW, the range of plot coordinates mapped onto
;;;; the rectangle (as PGPLOT's PGSVP and PGSWIN); a window may run
;;;; backward, e.g. X1 > X2 for right ascension or Y1 > Y2 for
;;;; magnitudes.  SET-TRANSFORM gives any affine transform instead,
;;;; including rotation or skew within the frame, e.g. an image's WCS.
;;;;
;;;; A viewport may be rotated: everything about it (rectangle, window,
;;;; transform, clipping) is laid out as if it were not, and then the
;;;; whole frame is turned by ANGLE degrees counterclockwise about the
;;;; rectangle's center.  Inside WITH-VIEWPORT, cairo's own transform
;;;; holds that rotation, so all drawing, including the frame, ticks and
;;;; text of DRAW-BOX, comes out rotated: a normal looking plot at an
;;;; angle on the page.
;;;;
;;;; WITH-VIEWPORT also selects the viewport on its device, so :PLOT
;;;; coordinates refer to it, and clips to its clipping region (by
;;;; default its rectangle).  A device always has a viewport selected,
;;;; and plotting functions accept a device or a viewport; see "Targets"
;;;; below.

(in-package :cl-cairoplot)

;;; ============================================================
;;; Affine transforms
;;; ============================================================
;;;
;;; An affine transform is a vector #(A B C D E F) mapping (X,Y) to
;;;   (A*X + B*Y + E,  C*X + D*Y + F)

(deftype affine () '(simple-array double-float (6)))

(defun make-affine (a b c d e f)
  (let ((m (make-array 6 :element-type 'double-float)))
    (setf (aref m 0) (float a 1d0) (aref m 1) (float b 1d0)
          (aref m 2) (float c 1d0) (aref m 3) (float d 1d0)
          (aref m 4) (float e 1d0) (aref m 5) (float f 1d0))
    m))

(declaim (inline affine-apply))
(defun affine-apply (m x y)
  (declare (type affine m))
  ;; Converting X and Y to doubles first lets the arithmetic below
  ;; compile to raw float operations even when X, Y come from a list or
  ;; SIMPLE-VECTOR, where their type is unknown; generic arithmetic
  ;; would allocate a boxed double for every intermediate result.
  (let ((x (float x 1d0))
        (y (float y 1d0)))
    (values (+ (* (aref m 0) x) (* (aref m 1) y) (aref m 4))
            (+ (* (aref m 2) x) (* (aref m 3) y) (aref m 5)))))

(defun affine-compose (m n)
  "The transform applying N, then M."
  (declare (type affine m n))
  (let ((a (aref m 0)) (b (aref m 1)) (c (aref m 2)) (d (aref m 3)))
    (multiple-value-bind (e f) (affine-apply m (aref n 4) (aref n 5))
      (make-affine (+ (* a (aref n 0)) (* b (aref n 2)))
                   (+ (* a (aref n 1)) (* b (aref n 3)))
                   (+ (* c (aref n 0)) (* d (aref n 2)))
                   (+ (* c (aref n 1)) (* d (aref n 3)))
                   e f))))

(defun as-affine (spec)
  "SPEC as an affine: an affine itself, or a list (A B C D E F)."
  (if (listp spec) (apply #'make-affine spec) spec))

(defun affine-determinant (m)
  (- (* (aref m 0) (aref m 3)) (* (aref m 1) (aref m 2))))

(defun affine-invert (m)
  (let ((det (affine-determinant m)))
    (when (< (abs det) 1d-300)
      (error "The transform ~A is singular." m))
    (let ((a (/ (aref m 3) det)) (b (/ (- (aref m 1)) det))
          (c (/ (- (aref m 2)) det)) (d (/ (aref m 0) det)))
      (make-affine a b c d
                   (- (+ (* a (aref m 4)) (* b (aref m 5))))
                   (- (+ (* c (aref m 4)) (* d (aref m 5))))))))

(defparameter +identity-affine+ (make-affine 1 0 0 1 0 0))

(defun affine-rotation (degrees &optional (x 0) (y 0))
  "Rotation of canvas coordinates (y downward) counterclockwise on the
   page by DEGREES about the canvas point (X,Y)."
  (let* ((r (* degrees (/ pi 180)))
         (cos (cos r)) (sin (sin r)))
    ;; translate (X,Y) to the origin, rotate, translate back
    (make-affine cos sin (- sin) cos
                 (- x (* cos x) (* sin y))
                 (- y (* (- sin) x) (* cos y)))))

;;; ============================================================
;;; Viewports
;;; ============================================================
;;;
;;; Three frames are involved:
;;;   page   the canvas, unrotated
;;;   local  the viewport's frame: canvas units, laid out as if the
;;;          viewport were unrotated (identical to page when ANGLE = 0)
;;;   plot   :PLOT coordinates
;;; FORWARD maps plot -> local and FRAME maps local -> page.

(defclass viewport ()
  ((device :initarg :device :reader viewport-device)
   ;; the rectangle, in :DEVICE coordinates, before rotation
   (dx1 :initarg :dx1) (dx2 :initarg :dx2)
   (dy1 :initarg :dy1) (dy2 :initarg :dy2)
   (angle :initarg :angle :initform 0 :reader viewport-angle
          :documentation "Degrees counterclockwise about the center.")
   (projection :initarg :projection :initform nil :accessor viewport-projection
               :documentation "A sky projection for :SKY coordinates (sky.lisp).")
   (polar :initform nil :accessor viewport-polar
          :documentation "The mapping for :POLAR coordinates (polar.lisp).")
   (footprint :initform nil :accessor viewport-footprint
              :documentation "What data has been drawn where, for placing
              legends (see \"Footprints\" below).")
   (clipping-region :initarg :clipping-region :initform :viewport
                    :accessor viewport-clipping-region
                    :documentation "A CLIPPING-REGION, :VIEWPORT for the
                    viewport's rectangle, or NIL for no clipping.")
   (window :initform nil
           :documentation "(X1 X2 Y1 Y2) as given to SET-WINDOW, kept
                           exact for tick placement.")
   (forward :type affine :reader viewport-forward
            :documentation "plot -> local")
   (inverse :type affine :reader viewport-inverse
            :documentation "local -> plot")
   (frame :type affine :reader viewport-frame
          :documentation "local -> page")
   (frame-inverse :type affine :reader viewport-frame-inverse
                  :documentation "page -> local"))
  (:documentation "A rectangle of a device's page with a window of :PLOT coordinates
   mapped onto it (see the top of viewport.lisp)."))

;;; ============================================================
;;; Footprints
;;; ============================================================
;;;
;;; While a viewport is entered with clipping (WITH-VIEWPORT, as every
;;; plotting function enters its viewport), what is drawn is noted in
;;; the viewport's FOOTPRINT, in its local canvas frame: line segments,
;;; marker centers, polygons (filled or not) and text boxes.  DRAW-LEGEND
;;; :BEST places a legend where it covers least of them.  Decorations
;;; (axes, legends, labels outside the plot) are drawn unclipped, so they
;;; are not noted; grids drawn clipped bind *RECORD-FOOTPRINT* to NIL, as
;;; does applying a clip region.

(defvar *record-footprint* t
  "NIL while drawing what legends need not avoid.")

(defmacro without-footprint (&body body)
  "Run BODY without noting what it draws in any footprint."
  `(let ((*record-footprint* nil)) ,@body))

(defstruct footprint
  (segments (make-array 64 :element-type 'double-float :adjustable t :fill-pointer 0))
  (points (make-array 64 :element-type 'double-float :adjustable t :fill-pointer 0))
  (polygons '())                        ; (XS . YS) double-float vectors
  (boxes '()))                          ; (X1 Y1 X2 Y2)

(defparameter +footprint-limit+ 200000
  "Most numbers kept per kind (segments or points) in a footprint.")

(defun recording-footprint (device)
  "The footprint drawing on DEVICE should be noted in, or NIL."
  (let ((vp (and *record-footprint* (device-entered-viewport device))))
    (when vp
      (or (viewport-footprint vp)
          (setf (viewport-footprint vp) (make-footprint))))))

(declaim (inline note-numbers))
(defun note-numbers (vector &rest numbers)
  (when (< (fill-pointer vector) +footprint-limit+)
    (dolist (v numbers) (vector-push-extend (float v 1d0) vector))))

(defun note-segment (footprint x1 y1 x2 y2)
  (note-numbers (footprint-segments footprint) x1 y1 x2 y2))

(defun note-point (footprint x y)
  (note-numbers (footprint-points footprint) x y))

(defun note-polygon (footprint xs ys)
  (push (cons (copy-seq xs) (copy-seq ys)) (footprint-polygons footprint)))

(defun note-box (footprint x1 y1 x2 y2)
  (push (list (min x1 x2) (min y1 y2) (max x1 x2) (max y1 y2)) (footprint-boxes footprint)))

;;; ============================================================
;;; Targets: a device or a viewport
;;; ============================================================
;;;
;;; A device always has a SELECTED viewport (DEVICE-VIEWPORT): the one
;;; :PLOT coordinates refer to.  It starts as one made from
;;; *DEFAULT-VIEWPORT*; WITH-VIEWPORT selects another for its body, and
;;; SETF of DEVICE-VIEWPORT selects one from then on.
;;;
;;; The ENTERED viewport (DEVICE-ENTERED-VIEWPORT) is the one cairo is
;;; set up for: its (possibly rotated) frame as cairo's transform and
;;; its clip.  Outside WITH-VIEWPORT nothing is entered, so drawing is
;;; on the page.
;;;
;;; Plotting functions, defined with DEFPLOT, take a TARGET first: a
;;; device or a viewport.  Given a viewport they draw in it; given a
;;; device they draw in its selected viewport when their COORDS are
;;; :PLOT or :SKY, and on the page (or in the entered viewport)
;;; otherwise.  Either way they enter the viewport for the call unless
;;; it is already entered, so (PLOT-POLYLINE D XS YS) needs no
;;; WITH-VIEWPORT.  Entering per call (a save, transform and clip) is
;;; cheap: 20000 single-point calls make the same size PDF, SVG and PNG
;;; outside WITH-VIEWPORT as inside, and take about 20% longer.

(defmethod as-canvas ((viewport viewport))
  (viewport-device viewport))

(defun target-device (target)
  "The device of TARGET, a device or a viewport."
  (if (typep target 'viewport) (viewport-device target) target))

(defun target-viewport (target)
  "TARGET if it is a viewport, else the device's selected viewport."
  (if (typep target 'viewport) target (device-viewport target)))

(defun viewport-entered-p (viewport)
  "True if VIEWPORT is both selected and entered on its device."
  (let ((device (viewport-device viewport)))
    (and (eq (device-entered-viewport device) viewport)
         (eq (device-viewport device) viewport))))

(defmacro with-target ((var &optional (coords :plot)) &body body)
  "Rebind the variable VAR, a device or a viewport, to its device and
   run BODY in the viewport VAR names or, for :PLOT or :SKY COORDS (NIL
   meaning :PLOT), the device's selected viewport, entering it for BODY
   unless it already is."
  (let ((vp (gensym "VIEWPORT")) (fn (gensym "BODY")) (c (gensym "COORDS")))
    `(let* ((,c (or ,coords :plot))
            (,vp (cond ((typep ,var 'viewport) ,var)
                       ((member ,c '(:plot :sky :polar)) (device-viewport ,var))))
            (,var (target-device ,var)))
       (declare (ignorable ,c))
       (flet ((,fn () ,@body))
         (multiple-value-prog1
             (if (or (null ,vp) (viewport-entered-p ,vp))
                 (,fn)
                 (with-viewport (,vp) (,fn)))
           (incf (device-changes ,var)))))))

(defmacro defplot (name (target &rest lambda-list) &body body)
  "DEFUN for a function whose first argument may be a device or a
   viewport.  If TARGET is named VIEWPORT, BODY sees a viewport (the
   device's selected one when given a device).  If it is named DEVICE,
   BODY sees the device, drawing as WITH-TARGET does with the function's
   COORDS parameter (or :PLOT if it has none)."
  (let* ((doc (when (and (stringp (first body)) (rest body)) (list (pop body))))
         (decls (loop while (and (consp (first body)) (eq (first (first body)) 'declare))
                      collect (pop body)))
         (coords (if (member "COORDS" lambda-list
                             :key (lambda (p) (symbol-name (if (consp p) (first p) p)))
                             :test #'string=)
                     (intern "COORDS" (symbol-package target))
                     :plot)))
    `(defun ,name (,target ,@lambda-list)
       ,@doc ,@decls
       ,(cond ((string= (symbol-name target) "VIEWPORT")
               `(let ((,target (target-viewport ,target))) ,@body))
              ((string= (symbol-name target) "DEVICE")
               `(with-target (,target ,coords) ,@body))
              (t (error "DEFPLOT ~A: the first parameter must be named DEVICE or VIEWPORT."
                        name))))))

(defun local-rectangle (viewport)
  "The rectangle in the local frame (canvas units, y down), as values
   LEFT RIGHT BOTTOM TOP."
  (with-slots (device dx1 dx2 dy1 dy2) viewport
    (let ((w (canvas-width device)) (h (canvas-height device)))
      (values (* w dx1) (* w dx2) (* h (- 1 dy1)) (* h (- 1 dy2))))))

(defun update-frame (viewport)
  "Recompute FRAME from the angle and the rectangle's center."
  (multiple-value-bind (left right bottom top) (local-rectangle viewport)
    (let ((frame (affine-rotation (viewport-angle viewport)
                                  (/ (+ left right) 2) (/ (+ bottom top) 2))))
      (setf (slot-value viewport 'frame) frame
            (slot-value viewport 'frame-inverse) (affine-invert frame))))
  viewport)

(defun (setf viewport-angle) (degrees viewport)
  (setf (slot-value viewport 'angle) degrees)
  (update-frame viewport)
  degrees)

(defun set-forward (viewport m &optional window)
  (setf (slot-value viewport 'inverse) (affine-invert m)
        (slot-value viewport 'forward) m
        (slot-value viewport 'window) window)
  viewport)

(defun make-viewport (device x1 x2 y1 y2 &key (coords :device)
                                              (window '(0 1 0 1)) (angle 0)
                                              (clipping-region :viewport))
  "Make a viewport of DEVICE spanning X1..X2, Y1..Y2 in COORDS (:DEVICE
   or :WORLD), with WINDOW (X1 X2 Y1 Y2) in :PLOT coordinates, the whole
   rotated by ANGLE degrees counterclockwise about its center."
  (check-type coords (member :device :world))
  (multiple-value-bind (dx1 dy1) (convert-coords device (min x1 x2) (min y1 y2)
                                                 coords :device)
    (multiple-value-bind (dx2 dy2) (convert-coords device (max x1 x2) (max y1 y2)
                                                   coords :device)
      (let ((viewport (make-instance 'viewport :device device
                                               :dx1 dx1 :dx2 dx2
                                               :dy1 dy1 :dy2 dy2
                                               :angle angle
                                               :clipping-region clipping-region)))
        (update-frame viewport)
        (push viewport (device-viewports device))
        (destructuring-bind (wx1 wx2 wy1 wy2) window
          (set-window viewport wx1 wx2 wy1 wy2))))))

(defplot viewport-rectangle (viewport &optional (coords :device))
  "The viewport's rectangle, before rotation, as values X1 X2 Y1 Y2 in
   COORDS (:DEVICE or :WORLD on the page, or :CANVAS for its own frame)."
  (if (eq coords :canvas)
      (local-rectangle viewport)
      (with-slots (device dx1 dx2 dy1 dy2) viewport
        (multiple-value-bind (x1 y1) (convert-coords device dx1 dy1 :device coords)
          (multiple-value-bind (x2 y2) (convert-coords device dx2 dy2 :device coords)
            (values x1 x2 y1 y2))))))

(defplot set-window (viewport x1 x2 y1 y2)
  "Map :PLOT coordinates X1..X2, Y1..Y2 onto VIEWPORT's rectangle."
  (when (or (= x1 x2) (= y1 y2))
    (error "Window ~A ~A ~A ~A has zero width or height." x1 x2 y1 y2))
  (multiple-value-bind (left right bottom top) (local-rectangle viewport)
    (let* ((sx (/ (- right left) (- x2 x1)))
           (sy (/ (- top bottom) (- y2 y1))))   ; negative: canvas y is down
      (set-forward viewport
                   (make-affine sx 0 0 sy (- left (* sx x1)) (- bottom (* sy y1)))
                   (list x1 x2 y1 y2)))))

(defun coords-affine (device coords)
  "The transform from COORDS (not :PLOT) to canvas units on the page."
  (let ((w (canvas-width device)) (h (canvas-height device)))
    (ecase coords
      (:world  (make-affine w 0 0 (- w) 0 h))
      (:device (make-affine w 0 0 (- h) 0 h))
      (:canvas +identity-affine+))))

(defplot set-transform (viewport a b c d e f &key (coords :world))
  "Make :PLOT coordinates (X,Y) map to (A*X + B*Y + E, C*X + D*Y + F) in
   COORDS (:WORLD, :DEVICE or :CANVAS), measured as if the viewport were
   unrotated (its ANGLE then turns the result).  Use :WORLD, whose units
   are equal in x and y, for rotations without skew."
  (set-forward viewport
               (affine-compose (coords-affine (viewport-device viewport) coords)
                               (make-affine a b c d e f))))

(defplot viewport-transform (viewport &optional (coords :world))
  "The transform from :PLOT coordinates to COORDS, as values A B C D E F
   (see SET-TRANSFORM)."
  (let ((m (affine-compose
            (affine-invert (coords-affine (viewport-device viewport) coords))
            (viewport-forward viewport))))
    (values-list (coerce m 'list))))

(defun viewport-axis-aligned-p (viewport)
  "True if plot x runs along the viewport's horizontal edges and plot y
   along its vertical ones (whatever the viewport's own ANGLE)."
  (let ((m (viewport-forward viewport)))
    (and (< (abs (aref m 1)) (* 1d-12 (abs (aref m 0))))
         (< (abs (aref m 2)) (* 1d-12 (abs (aref m 3)))))))

(defplot viewport-window (viewport)
  "The :PLOT coordinates at the viewport's edges, as values X1 X2 Y1 Y2:
   x at the left and right edges and y at the bottom and top.  If the
   plot is rotated or skewed within the viewport, the bounding box
   (X1 < X2, Y1 < Y2) of the plot coordinates in the rectangle instead."
  (when (slot-value viewport 'window)
    (return-from viewport-window (values-list (slot-value viewport 'window))))
  (multiple-value-bind (left right bottom top) (local-rectangle viewport)
    (let ((inverse (viewport-inverse viewport)))
      (if (viewport-axis-aligned-p viewport)
          (values (affine-apply inverse left bottom)
                  (affine-apply inverse right bottom)
                  (nth-value 1 (affine-apply inverse left bottom))
                  (nth-value 1 (affine-apply inverse left top)))
          (let ((xs '()) (ys '()))
            (loop for (cx cy) in (list (list left bottom) (list right bottom)
                                       (list right top) (list left top))
                  do (multiple-value-bind (x y) (affine-apply inverse cx cy)
                       (push x xs) (push y ys)))
            (values (reduce #'min xs) (reduce #'max xs)
                    (reduce #'min ys) (reduce #'max ys)))))))

(defplot set-window-equal-scales (viewport x1 x2 y1 y2 &key (x-scale 1))
  "Set the window to X1..X2, Y1..Y2 and shrink the viewport about its
   center so one :PLOT unit is the same length in x and y (as PGWNAD),
   or, with X-SCALE, so one x unit is X-SCALE times as long as one y
   unit (e.g. cos(dec) for right ascension, see SET-WINDOW-SKY)."
  (with-slots (device dx1 dx2 dy1 dy2) viewport
    (let* ((w (* (canvas-width device) (- dx2 dx1)))   ; canvas units
           (h (* (canvas-height device) (- dy2 dy1)))
           ;; canvas units per y unit
           (scale (min (/ w (* x-scale (abs (- x2 x1)))) (/ h (abs (- y2 y1)))))
           (fx (/ (* scale x-scale (abs (- x2 x1))) w)) ; fraction kept
           (fy (/ (* scale (abs (- y2 y1))) h))
           (xc (/ (+ dx1 dx2) 2))
           (yc (/ (+ dy1 dy2) 2)))
      (setf dx1 (- xc (* fx (- xc dx1))) dx2 (+ xc (* fx (- dx2 xc)))
            dy1 (- yc (* fy (- yc dy1))) dy2 (+ yc (* fy (- dy2 yc))))))
  ;; the center is unchanged, so FRAME still holds
  (set-window viewport x1 x2 y1 y2))

(defplot set-window-sky (viewport ra dec half-width &optional (half-height half-width))
  "Set VIEWPORT's window to a patch of sky centered on RA, DEC (degrees),
   HALF-WIDTH by HALF-HEIGHT degrees on the sky, with right ascension
   increasing to the left (east) and scales equal on the sky at DEC: an
   RA degree is cos(DEC) as long as a Dec degree.  This is exact only
   at DEC; use it for fields small enough that RA scale varies little
   across them.  Returns the RA half-range, HALF-WIDTH / cos(DEC)."
  (let* ((k (cos (* dec (/ pi 180))))
         (dra (/ half-width k)))
    (set-window-equal-scales viewport (+ ra dra) (- ra dra)
                             (- dec half-height) (+ dec half-height) :x-scale k)
    dra))

(declaim (inline plot-to-canvas canvas-to-plot))

(defun plot-to-canvas (viewport x y)
  "Plot coordinates to the viewport's local frame (the drawing space
   inside WITH-VIEWPORT)."
  (affine-apply (viewport-forward viewport) x y))

(defun canvas-to-plot (viewport cx cy)
  "The :PLOT point of the point (CX,CY) of VIEWPORT's local frame (the
   inverse of PLOT-TO-CANVAS)."
  (affine-apply (viewport-inverse viewport) cx cy))

(defun viewport-region (viewport)
  "The viewport's own rectangle as a clipping region, in its local frame."
  (multiple-value-bind (left right bottom top) (local-rectangle viewport)
    (rectangle-region left bottom right top :coords :canvas)))

(defun enter-frame (viewport)
  "Set cairo's transform to VIEWPORT's frame (replacing, not composing,
   so viewports are always placed on the page)."
  (let ((device (viewport-device viewport))
        (m (viewport-frame viewport)))
    (reset-transform device)
    ;; TRANSFORM takes cairo's order: xx yx xy yy x0 y0
    (transform device (aref m 0) (aref m 2) (aref m 1) (aref m 3)
               (aref m 4) (aref m 5))))

(defmacro with-viewport ((viewport &key (clip t)) &body body)
  "Run BODY with VIEWPORT (or a device's selected viewport) selected and
   entered on its device: drawing in its (possibly rotated) frame and,
   if CLIP, clipped to its clipping region.  Restores the previous
   viewports, transform, clip and drawing state afterward."
  (let ((vp (gensym "VIEWPORT")) (dev (gensym "DEVICE")) (old (gensym "OLD"))
        (old-entered (gensym "OLD-ENTERED")))
    `(let* ((,vp (target-viewport ,viewport))
            (,dev (viewport-device ,vp))
            (,old (device-viewport ,dev))
            (,old-entered (device-entered-viewport ,dev)))
       (with-saved-state (,dev)
         (setf (device-viewport ,dev) ,vp
               (device-entered-viewport ,dev) ,vp)
         (unwind-protect
              (let ((*record-footprint* (and ,clip *record-footprint*)))
                (enter-frame ,vp)
                (when ,clip
                  (let ((region (viewport-clipping-region ,vp)))
                    (apply-clip ,dev (if (eq region :viewport)
                                         (viewport-region ,vp)
                                         region))))
                ,@body)
           (setf (device-viewport ,dev) ,old
                 (device-entered-viewport ,dev) ,old-entered)
           (incf (device-changes ,dev)))))))

(defplot erase-viewport (viewport &optional (color nil color-p))
  "Clear VIEWPORT's rectangle (turned with it, if it is rotated) to
   COLOR, by default its device's background (NIL: transparent), and
   forget what was drawn in it, so legends are placed afresh.  The
   numbers and labels DRAW-BOX drew outside the rectangle stay, but
   ticks inside it and the inner half of its frame line go: call
   DRAW-BOX again after drawing the new contents."
  (let* ((device (viewport-device viewport))
         (color (if color-p color (canvas-background device))))
    (with-viewport (viewport :clip nil)
      (multiple-value-bind (left right bottom top) (local-rectangle viewport)
        (with-saved-state (device)
          (with-backend-floats
            (if color
                (progn (set-color device color)
                       (cl-cairo2:set-operator :source (%context device)))
                (cl-cairo2:set-operator :clear (%context device))))
          (draw-rectangle device left top (- right left) (- bottom top) :fill t))))
    (setf (viewport-footprint viewport) nil)
    viewport))

;;; ============================================================
;;; Points on the page
;;; ============================================================

(defun viewport-contains-p (viewport cx cy)
  "True if the page canvas point (CX,CY) is inside VIEWPORT's (possibly
   rotated) rectangle."
  (multiple-value-bind (lx ly) (affine-apply (viewport-frame-inverse viewport) cx cy)
    (multiple-value-bind (left right bottom top) (local-rectangle viewport)
      (and (<= (min left right) lx (max left right))
           (<= (min bottom top) ly (max bottom top))))))

(defun locate-point (device cx cy &optional viewport)
  "The page canvas point (CX,CY) of DEVICE as values X Y in the :PLOT
   coordinates of VIEWPORT, by default the most recently made viewport
   containing it (else the device's selected viewport), and that
   viewport."
  (let ((viewport (or viewport
                      (find-if (lambda (vp) (viewport-contains-p vp cx cy))
                               (device-viewports device))
                      (device-viewport device))))
    (multiple-value-bind (lx ly) (affine-apply (viewport-frame-inverse viewport) cx cy)
      (multiple-value-bind (x y) (canvas-to-plot viewport lx ly)
        (values x y viewport)))))
