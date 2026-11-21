;;;; plot-device.lisp — plot device and its coordinate systems
;;;;
;;;; A PLOT-DEVICE is a CANVAS (so every canvas function, WITH-COLOR,
;;;; WITH-FONT, PUT-TEXT, ... works on it) with an aspect ratio R =
;;;; height/width and these coordinate systems, all with y increasing
;;;; upward from the bottom left corner:
;;;;
;;;;   :DEVICE  0..1 across, 0..1 up        (normalized, not isotropic)
;;;;   :WORLD   0..1 across, 0..R up        (isotropic: equal units in x, y)
;;;;   :PLOT    the window of the selected VIEWPORT (see viewport.lisp)
;;;;   :CANVAS  the current drawing space: canvas units, y downward
;;;;
;;;; :DEVICE and :WORLD are fixed to the page.  :CANVAS is the page too,
;;;; except inside WITH-VIEWPORT of a rotated viewport, where it is the
;;;; viewport's rotated frame (cairo's user space), so that text and
;;;; shapes drawn there rotate with the viewport.
;;;;
;;;; Everything is drawn by converting to :CANVAS coordinates with
;;;; TO-CANVAS, so line widths and text are never distorted by unequal
;;;; x and y scales.

(in-package :cl-cairoplot)

(defclass plot-device (canvas)
  ((aspect   :initarg :aspect :reader device-aspect
             :documentation "Height / width.")
   (viewport :initform nil :accessor device-viewport
             :documentation "The selected VIEWPORT, for :PLOT coordinates.")
   (entered :initform nil :accessor device-entered-viewport
            :documentation "The VIEWPORT cairo is set up for, or NIL for
                            the page (see viewport.lisp).")
   (viewports :initform '() :accessor device-viewports
              :documentation "The viewports made on the device, so ERASE-DEVICE
                              can forget what was drawn in them.")
   (changes :initform 0 :accessor device-changes
            :documentation "Counts plotting calls, so a device shown on
                            screen knows when to show it again."))
  (:documentation "A canvas with an aspect ratio, coordinate systems and
   viewports to plot in: what OPEN-DEVICE returns."))

(defparameter *default-viewport* '(0.12 0.95 0.11 0.9)
  "MAKE-VIEWPORT arguments for the viewport a device starts with
   selected: room at the sides for DRAW-BOX's numbers, labels and title.")

(defparameter *output-type-systems* '((:gtk . "cl-cairoplot-gtk"))
  "Output types other systems provide, with the system to load for each.")

(defgeneric output-type-buffer (output-type destination width height &key resolution)
  (:documentation "For an OUTPUT-TYPE OPEN-CANVAS does not draw itself,
   such as :GTK (a window, from system cl-cairoplot-gtk): values the
   PIXEL-BUFFER that the device draws in, the device's class (a subclass
   of PLOT-DEVICE) and more initargs for it.  The device is WIDTH x
   HEIGHT units; the buffer has RESOLUTION pixels per unit, or with
   RESOLUTION NIL as many as the output type chooses (e.g. 2 on a
   high-resolution screen), the same in x and y.")
  (:method (output-type destination width height &key resolution)
    (declare (ignore destination width height resolution))
    (let ((system (cdr (assoc output-type *output-type-systems*))))
      (error "Unknown output type ~S.~@[  Load it with (asdf:load-system ~S).~]"
             output-type system))))

(defgeneric device-opened (device)
  (:documentation "Called on a device OPEN-DEVICE has just opened; returns it.")
  (:method ((device plot-device)) device))

(defun open-device (output-type destination &key (size 600) (aspect 1)
                                                 (background :white)
                                                 (font *default-font*)
                                                 resolution)
  "Open a plot device; OUTPUT-TYPE and DESTINATION are as for OPEN-CANVAS,
   or another output type a loaded system provides (see
   OUTPUT-TYPE-BUFFER), such as :GTK with a window title.  SIZE is the
   width in points (pixels for :PNG and on screen) and ASPECT is
   height/width.  RESOLUTION is pixels per unit of SIZE for a :PNG
   image (default 1, see OPEN-CANVAS) or a :BUFFER device, whose size
   is its buffer's divided by it; for other output types it defaults to
   what suits them, e.g. the screen's scale factor for :GTK.  The device
   starts with a viewport made from *DEFAULT-VIEWPORT* selected."
  (let ((height (float (* size aspect) 1d0))
        (class 'plot-device) (initargs '()))
    (case output-type
      ((:pdf :ps :svg))
      (:png (setf height (round height)))
      (:buffer (setf resolution (or resolution 1)
                     size (/ (pixel-buffer-width destination) resolution)
                     height (/ (pixel-buffer-height destination) resolution)))
      (t (setf size (round size) height (round height))
         (multiple-value-setq (destination class initargs)
           (output-type-buffer output-type destination size height
                               :resolution resolution))
         ;; the pixels per unit the output type chose
         (setf output-type :buffer
               resolution (/ (pixel-buffer-width destination) size))))
    (let ((device (open-canvas output-type destination
                               :width size :height height :background background
                               :font font :class class :resolution (or resolution 1)
                               :initargs (list* :aspect (/ height size) initargs))))
      (setf (device-viewport device) (apply #'make-viewport device *default-viewport*))
      (device-opened device))))

(defun erase-device (device &optional (color (canvas-background device)))
  "Clear DEVICE's whole page to COLOR (default its background; NIL makes
   a PNG or buffer transparent), as PGPLOT's PGERAS.  Viewports keep
   their windows, and forget what was drawn in them, so legends are
   placed afresh.  A window shows the cleared page at once.  (On PDF and
   PostScript this paints over the page; NEXT-PAGE starts a new one.)"
  (erase-canvas device color)
  (dolist (viewport (device-viewports device))
    (setf (viewport-footprint viewport) nil))
  (incf (device-changes device))
  device)

(defgeneric read-cursor (device &key viewport mode anchor timeout)
  (:documentation "Wait for a click or a key press in DEVICE's window and
   return values X, Y (in the :PLOT coordinates of VIEWPORT, by default
   the viewport under the pointer), the KEY (:LEFT, :MIDDLE or :RIGHT
   for a mouse button, else the key's character or a keyword such as
   :ESCAPE) and the viewport, as PGPLOT's PGCURS and PGBAND.  MODE draws
   a rubber band while waiting, from ANCHOR, a list (X Y) of plot
   coordinates: :LINE from it, :RECTANGLE with a corner at it, and
   without an anchor :HORIZONTAL, :VERTICAL or :CROSSHAIR lines through
   the pointer; :NONE (the default) draws nothing.  After TIMEOUT
   seconds, or if the window is closed, returns NIL.  Only devices on
   the screen have a cursor, such as :GTK windows.")
  (:method (device &key viewport mode anchor timeout)
    (declare (ignore viewport mode anchor timeout))
    (error "~A has no cursor: only windows (output type :GTK) have one." device)))

(defgeneric close-device (device)
  (:documentation "Finish DEVICE's output and close it.")
  (:method ((device plot-device)) (close-canvas device)))

(defmacro with-device ((var output-type destination &rest keys) &body body)
  "Bind VAR to a device opened with OPEN-DEVICE, closing it afterward."
  `(let ((,var (open-device ,output-type ,destination ,@keys)))
     (unwind-protect (progn ,@body)
       (close-device ,var))))

;;; ============================================================
;;; Coordinate conversion
;;; ============================================================

(declaim (inline device-to-canvas canvas-to-device
                 world-to-device device-to-world))

(defun device-to-canvas (device x y)
  "The page canvas point of the :DEVICE point (X,Y)."
  (values (* x (canvas-width device))
          (* (- 1 y) (canvas-height device))))

(defun canvas-to-device (device cx cy)
  "The :DEVICE point of the page canvas point (CX,CY)."
  (values (/ cx (canvas-width device))
          (- 1 (/ cy (canvas-height device)))))

(defun world-to-device (device x y)
  "The :DEVICE point of the :WORLD point (X,Y)."
  (values x (/ y (device-aspect device))))

(defun device-to-world (device x y)
  "The :WORLD point of the :DEVICE point (X,Y)."
  (values x (* y (device-aspect device))))

(defun current-viewport (device)
  (or (device-viewport device)
      (error "~A has no selected viewport for :PLOT coordinates." device)))

(defun page-to-drawing (device cx cy)
  "A page canvas point in the current drawing space (see header)."
  (let ((vp (device-entered-viewport device)))
    (if vp
        (affine-apply (viewport-frame-inverse vp) cx cy)
        (values cx cy))))

(defun drawing-to-page (device cx cy)
  (let ((vp (device-entered-viewport device)))
    (if vp
        (affine-apply (viewport-frame vp) cx cy)
        (values cx cy))))

(defun plot-transform (device)
  "The transform from :PLOT coordinates of the selected viewport to the
   current drawing space: the viewport's FORWARD when it is entered,
   else through the page into whatever frame is entered."
  (let ((vp (current-viewport device))
        (in (device-entered-viewport device)))
    (if (eq vp in)
        (viewport-forward vp)
        (let ((to-page (affine-compose (viewport-frame vp) (viewport-forward vp))))
          (if in (affine-compose (viewport-frame-inverse in) to-page) to-page)))))

(defun plot-to-drawing (device x y)
  (let ((vp (current-viewport device)))
    (if (eq vp (device-entered-viewport device))
        (plot-to-canvas vp x y)
        (affine-apply (plot-transform device) x y))))

(defun drawing-to-plot (device cx cy)
  (let ((vp (current-viewport device)))
    (if (eq vp (device-entered-viewport device))
        (canvas-to-plot vp cx cy)
        (affine-apply (affine-invert (plot-transform device)) cx cy))))

(defun to-canvas (device coords x y)
  "Convert the point (X,Y) in COORDS (:PLOT, :WORLD, :DEVICE or :CANVAS,
   or :SKY, right ascension and declination in degrees through the
   current viewport's projection, or :POLAR, angle in degrees and
   radius, see polar.lisp) to :CANVAS, the current drawing space."
  (ecase coords
    (:plot (plot-to-drawing device x y))
    (:sky (multiple-value-call #'plot-to-drawing device
            (sky-to-plot (current-viewport device) x y)))
    (:polar (multiple-value-call #'plot-to-drawing device
              (polar-to-plot (current-viewport device) x y)))
    (:world (multiple-value-bind (dx dy) (world-to-device device x y)
              (multiple-value-call #'page-to-drawing device
                (device-to-canvas device dx dy))))
    (:device (multiple-value-call #'page-to-drawing device
               (device-to-canvas device x y)))
    (:canvas (values x y))))

(defun from-canvas (device coords cx cy)
  "Convert the :CANVAS point (CX,CY) to COORDS."
  (ecase coords
    (:plot (drawing-to-plot device cx cy))
    (:world (multiple-value-bind (dx dy)
                (multiple-value-call #'canvas-to-device device
                  (drawing-to-page device cx cy))
              (device-to-world device dx dy)))
    (:device (multiple-value-call #'canvas-to-device device
               (drawing-to-page device cx cy)))
    (:canvas (values cx cy))))

(defun convert-coords (device x y from to)
  "Convert the point (X,Y) from coordinates FROM to coordinates TO."
  (multiple-value-call #'from-canvas device to (to-canvas device from x y)))

(defun coords-transform (device coords)
  "The affine transform (see viewport.lisp) from COORDS to :CANVAS."
  (let ((vp (device-entered-viewport device)))
    (case coords
      (:plot (plot-transform device))
      (:canvas +identity-affine+)
      ;; page-fixed systems: to the page, then into a rotated frame
      (t (if vp
             (affine-compose (viewport-frame-inverse vp)
                             (coords-affine device coords))
             (coords-affine device coords))))))

;;; ============================================================
;;; Paths in any coordinates
;;; ============================================================
;;;
;;; These add to the current path, in canvas coordinates.  Closed shapes
;;; all run the same way round on the canvas (the direction of
;;; increasing angle in ARC), so a union of them fills or clips
;;; correctly with the :WINDING rule.

;;; These run once per data point, so they are written for speed:
;;;  - Each fetches the COORDS -> :CANVAS transform once and maps points
;;;    with the inlined AFFINE-APPLY, rather than calling TO-CANVAS per
;;;    point (which re-dispatches on COORDS and re-finds the viewport:
;;;    ~50 ns vs ~5 ns per point on SBCL).
;;;  - DO-POINTS iterates with the sequence type known.  A plain
;;;    (MAP NIL ...) over sequences of unknown type reads elements
;;;    generically, which for double-float vectors boxes every element
;;;    (a heap allocation per number); that cost more than cairo itself.

(defmacro do-points ((x y xs ys) &body body)
  "Run BODY with X, Y bound to successive elements of sequences XS, YS
   (stopping at the shorter), with fast paths for typical data types."
  (let ((fn (gensym "BODY")) (xv (gensym "XS")) (yv (gensym "YS")))
    (flet ((vector-loop (type)
             `(loop for i of-type fixnum below (min (length ,xv) (length ,yv))
                    do (,fn (aref (the ,type ,xv) i) (aref (the ,type ,yv) i)))))
      `(let ((,xv ,xs) (,yv ,ys))
         (flet ((,fn (,x ,y) ,@body))
           (declare (inline ,fn))
           (macrolet ((both (type) `(and (typep ,',xv ',type) (typep ,',yv ',type))))
             (cond ((both (simple-array double-float (*)))
                    ,(vector-loop '(simple-array double-float (*))))
                   ((both (simple-array single-float (*)))
                    ,(vector-loop '(simple-array single-float (*))))
                   ((both simple-vector)
                    ,(vector-loop 'simple-vector))
                   ((both list)
                    (loop for ,x in ,xv for ,y in ,yv do (,fn ,x ,y)))
                   (t (map nil #',fn ,xv ,yv)))))))))

(defun polyline-path* (device xs ys coords)
  "Add a new subpath through the points XS, YS of COORDS.
   Caller must be inside WITH-BATCHED-GRAPHICS or WITH-CANVAS-LOCK."
  (let ((m (coords-transform device coords))
        (first t)
        (fp (recording-footprint device))
        (px 0d0) (py 0d0))
    (declare (type double-float px py))
    (do-points (x y xs ys)
      (multiple-value-bind (cx cy) (affine-apply m x y)
        (if first
            (progn (move-to/tus device cx cy) (setf first nil))
            (progn (line-to/tus device cx cy)
                   (when fp (note-segment fp px py cx cy))))
        (setf px cx py cy)))))

(defun polygon-path* (device xs ys coords &optional post)
  "Add the closed polygon XS, YS of COORDS as a subpath, its canvas
   points then transformed by the affine POST if given.
   Caller must be inside WITH-BATCHED-GRAPHICS or WITH-CANVAS-LOCK."
  (let* ((m (if post
                (affine-compose post (coords-transform device coords))
                (coords-transform device coords)))
         (n (min (length xs) (length ys)))
         (cxs (make-array n :element-type 'double-float))
         (cys (make-array n :element-type 'double-float))
         (i 0))
    (declare (type fixnum n i))
    (do-points (x y xs ys)
      (multiple-value-bind (cx cy) (affine-apply m x y)
        (setf (aref cxs i) cx (aref cys i) cy))
      (incf i))
    (let ((fp (recording-footprint device)))
      (when fp (note-polygon fp cxs cys)))
    ;; trace the polygon in the canvas direction of increasing angle
    (let* ((area (loop for j below n
                       for k = (if (= j (1- n)) 0 (1+ j))
                       sum (- (* (aref cxs j) (aref cys k))
                              (* (aref cxs k) (aref cys j)))
                         of-type double-float))
           (order (if (minusp area)
                      (loop for j from (1- n) downto 0 collect j)
                      (loop for j below n collect j))))
      (loop for j in order
            for first = t then nil
            do (if first
                   (move-to/tus device (aref cxs j) (aref cys j))
                   (line-to/tus device (aref cxs j) (aref cys j)))))
    (close-path/tus device)))

(defun rectangle-path* (device x1 y1 x2 y2 coords &optional post)
  "Add the rectangle with corners (X1,Y1), (X2,Y2) of COORDS as a subpath,
   transformed by POST as in POLYGON-PATH*."
  (polygon-path* device (list x1 x2 x2 x1) (list y1 y1 y2 y2) coords post))

(defun ellipse-path-canvas (device cx cy ax ay bx by)
  "Add as a subpath the ellipse centered at canvas point (CX,CY) with
   conjugate semi-axis vectors (AX,AY) and (BX,BY) in canvas units: the
   image of the unit circle under (u,v) -> C + u*A + v*B.
   Caller must be inside WITH-BATCHED-GRAPHICS or WITH-CANVAS-LOCK."
  (let ((det (- (* ax by) (* bx ay))))
    (when (zerop det)
      (error "Ellipse semi-axes must be nonzero and not parallel."))
    (let ((fp (recording-footprint device)))
      (when fp
        (let ((xs (make-array 24 :element-type 'double-float))
              (ys (make-array 24 :element-type 'double-float)))
          (dotimes (k 24)
            (let ((u (cos (* k (/ pi 12)))) (v (sin (* k (/ pi 12)))))
              (setf (aref xs k) (float (+ cx (* u ax) (* v bx)) 1d0)
                    (aref ys k) (float (+ cy (* u ay) (* v by)) 1d0))))
          (note-polygon fp xs ys))))
    (new-sub-path/tus device)
    ;; the path keeps canvas coordinates; only the transform is restored
    ;; with-saved-state holds the lock (recursive)
    (with-saved-state (device)
      (translate/tus device cx cy)
      (transform/tus device ax ay bx by 0 0)
      ;; keep the canvas direction the same as other closed shapes
      (if (plusp det)
          (arc/tus device 0 0 1 0 (* 2 pi))
          (arc-negative/tus device 0 0 1 (* 2 pi) 0)))
    (close-path/tus device)))

;;; ============================================================
;;; Rotating shapes, with or without the coordinates' skew
;;; ============================================================
;;;
;;; A shape is rotated on the page so it keeps its shape however unequal
;;; the x and y scales of its coordinates.  If the coordinates are
;;; skewed (a SET-TRANSFORM with shear), FOLLOW-SKEW instead lays the
;;; shape out in the plot's unskewed geometry, rotates it there, and
;;; then skews it like everything else in the plot.
;;;
;;; The skew is found by splitting the linear part of COORDS -> canvas
;;; as M = Q.U (a QR decomposition): Q turns the plot's x axis to its
;;; direction on the page, and U = [SX K; 0 SY] holds the page length SX
;;; of one x unit, the reach SY of one y unit perpendicular to x, and
;;; the shear K.  The plot without its skew is M0 = Q.diag(SX,SY); the
;;; skew alone is KS = M.M0^-1 = I + (K/SY) q1 q2^T, a shear along the
;;; x axis on the page (identity when K = 0).

(defun coords-skew (m)
  "For the transform M (COORDS -> canvas), return values: the skew alone
   as a linear affine (see above), SX and SY."
  (let* ((xx (aref m 0)) (xy (aref m 2))          ; image of the x unit
         (yx (aref m 1)) (yy (aref m 3))          ; image of the y unit
         (sx (sqrt (+ (* xx xx) (* xy xy))))
         (q1x (/ xx sx)) (q1y (/ xy sx))
         (k (+ (* q1x yx) (* q1y yy)))           ; y unit's part along x
         (wx (- yx (* k q1x))) (wy (- yy (* k q1y)))
         (sy (sqrt (+ (* wx wx) (* wy wy))))
         (q2x (/ wx sy)) (q2y (/ wy sy))
         (s (/ k sy)))
    (values (make-affine (+ 1 (* s q1x q2x)) (* s q1x q2y)
                         (* s q1y q2x) (+ 1 (* s q1y q2y)) 0 0)
            sx sy)))

(defun affine-about (m px py)
  "The linear part of M applied about the point (PX,PY) instead of the
   origin."
  (multiple-value-bind (qx qy) (affine-apply m px py)
    (make-affine (aref m 0) (aref m 1) (aref m 2) (aref m 3)
                 (+ (- px qx) (aref m 4)) (+ (- py qy) (aref m 5)))))

(defun shape-rotation (device degrees px py coords follow-skew)
  "The canvas affine rotating DEGREES counterclockwise on the page about
   the point (PX,PY) of COORDS.  With FOLLOW-SKEW the rotation happens
   in the plot's unskewed geometry: KS.R.KS^-1 about the point."
  (multiple-value-bind (cx cy) (to-canvas device coords px py)
    (let ((r (affine-rotation degrees cx cy)))
      (if follow-skew
          (let ((ks (affine-about (coords-skew (coords-transform device coords))
                                  cx cy)))
            (affine-compose ks (affine-compose r (affine-invert ks))))
          r))))

(defun ellipse-vectors (device xc yc a b coords
                        &key (angle 0) (a-scale :x) (b-scale :y) follow-skew)
  "The ellipse of PLOT-ELLIPSE as values CX CY AX AY BX BY for
   ELLIPSE-PATH-CANVAS."
  (let* ((m (coords-transform device coords)))
    (multiple-value-bind (ks sx sy) (coords-skew m)
      (let* ((x-unit sx)
             ;; without FOLLOW-SKEW a y unit is its full length on the page;
             ;; with it, its reach perpendicular to x (the skew adds the rest)
             (y-unit (if follow-skew
                         sy
                         (sqrt (+ (expt (aref m 1) 2) (expt (aref m 3) 2)))))
             (la (* (abs a) (ecase a-scale (:x x-unit) (:y y-unit))))
             (lb (* (abs b) (ecase b-scale (:x x-unit) (:y y-unit))))
             ;; unit vector of COORDS's x axis on the canvas, turned by ANGLE
             ;; (counterclockwise on the page, with canvas y running down)
             (ux (/ (aref m 0) sx)) (uy (/ (aref m 2) sx))
             (theta (* angle (/ pi 180)))
             (rx (+ (* ux (cos theta)) (* uy (sin theta))))
             (ry (- (* uy (cos theta)) (* ux (sin theta))))
             ;; A along the turned axis, B 90 degrees counterclockwise from it
             (ax (* la rx)) (ay (* la ry))
             (bx (* lb ry)) (by (* lb (- rx))))
        (when follow-skew
          (setf (values ax ay) (affine-apply ks ax ay)
                (values bx by) (affine-apply ks bx by)))
        (multiple-value-bind (cx cy) (affine-apply m xc yc)
          (values cx cy ax ay bx by))))))
