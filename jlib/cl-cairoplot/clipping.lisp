;;;; clipping.lisp — clipping regions
;;;;
;;;; A CLIPPING-REGION limits drawing to part of a plot device.  Simple
;;;; regions are shapes given in a coordinate system (:PLOT, :WORLD,
;;;; :DEVICE or :CANVAS; :PLOT means the viewport current when the clip
;;;; is applied):
;;;;
;;;;   RECTANGLE-REGION, POLYGON-REGION, ELLIPSE-REGION, CIRCLE-REGION
;;;;
;;;; and composite regions combine others:
;;;;
;;;;   (REGION-UNION r ...)         inside any of them
;;;;   (REGION-INTERSECTION r ...)  inside all of them
;;;;   (REGION-OUTSIDE r)           outside R
;;;;
;;;; e.g. an annulus is (region-intersection (circle-region 0 0 2)
;;;;                                         (region-outside (circle-region 0 0 1)))
;;;;
;;;; Unions and outsides are each drawn as one path, so they may only
;;;; contain simple regions and unions; an intersection may contain
;;;; anything.  Self-intersecting polygons clip by the :WINDING rule.
;;;; The outside of a union whose parts overlap wrongly includes the
;;;; overlaps (it uses the :EVEN-ODD rule); intersect the outsides of
;;;; the parts instead.

(in-package :cl-cairoplot)

(defclass clipping-region () ()
  (:documentation "Abstract base class of clipping regions."))

(defclass path-region (clipping-region) ()
  (:documentation "A region drawable as a single path (see ADD-REGION-PATH)."))

(defclass shape-region (path-region)
  ((coords :initarg :coords :initform :plot :reader region-coords)))

(defclass rectangle-region (shape-region)
  ((x1 :initarg :x1) (y1 :initarg :y1) (x2 :initarg :x2) (y2 :initarg :y2))
  (:documentation "The inside of a rectangle, as RECTANGLE-REGION makes it."))

(defclass polygon-region (shape-region)
  ((xs :initarg :xs) (ys :initarg :ys))
  (:documentation "The inside of a polygon, as POLYGON-REGION makes it."))

(defclass ellipse-region (shape-region)
  ((xc :initarg :xc) (yc :initarg :yc) (a :initarg :a) (b :initarg :b)
   (angle :initarg :angle) (a-scale :initarg :a-scale) (b-scale :initarg :b-scale)
   (follow-skew :initarg :follow-skew))
  (:documentation "The inside of an ellipse, as ELLIPSE-REGION makes it."))

(defclass union-region (path-region)
  ((regions :initarg :regions :reader region-components)))

(defclass outside-region (clipping-region)
  ((region :initarg :region :reader region-complement)))

(defclass intersection-region (clipping-region)
  ((regions :initarg :regions :reader region-components)))

(defun rectangle-region (x1 y1 x2 y2 &key (coords :plot))
  "The region inside the rectangle with corners (X1,Y1) and (X2,Y2) in
   COORDS."
  (make-instance 'rectangle-region :x1 x1 :y1 y1 :x2 x2 :y2 y2 :coords coords))

(defun polygon-region (xs ys &key (coords :plot))
  "The region inside the polygon with vertices XS, YS in COORDS."
  (make-instance 'polygon-region :xs xs :ys ys :coords coords))

(defun ellipse-region (xc yc a b &key (coords :plot) (angle 0)
                                     (a-scale :x) (b-scale :y) follow-skew)
  "The region inside the ellipse PLOT-ELLIPSE draws with these arguments."
  (make-instance 'ellipse-region :xc xc :yc yc :a a :b b :coords coords
                                 :angle angle :a-scale a-scale :b-scale b-scale
                                 :follow-skew follow-skew))

(defun circle-region (xc yc r &key (coords :plot) round follow-skew)
  "The region inside the circle PLOT-CIRCLE draws with these arguments."
  (ellipse-region xc yc r r :coords coords :a-scale :x :b-scale (if round :x :y)
                            :follow-skew follow-skew))

(defun check-path-regions (regions)
  (dolist (r regions regions)
    (unless (typep r 'path-region)
      (error "~A cannot be part of a union or outside region; ~
              use REGION-INTERSECTION around it instead." r))))

(defun region-union (&rest regions)
  "The region inside any of REGIONS."
  (make-instance 'union-region :regions (check-path-regions regions)))

(defun region-intersection (&rest regions)
  "The region inside all of REGIONS."
  (make-instance 'intersection-region :regions regions))

(defun region-outside (region)
  "The region outside REGION."
  (check-path-regions (list region))
  (make-instance 'outside-region :region region))

;;; ============================================================
;;; Applying regions
;;; ============================================================

(defgeneric add-region-path (region device)
  (:documentation "Add REGION's outline to the current path of DEVICE."))

(defmethod add-region-path ((r rectangle-region) device)
  (with-slots (x1 y1 x2 y2 coords) r
    (rectangle-path* device x1 y1 x2 y2 coords)))

(defmethod add-region-path ((r polygon-region) device)
  (with-slots (xs ys coords) r
    (polygon-path* device xs ys coords)))

(defmethod add-region-path ((r ellipse-region) device)
  (with-slots (xc yc a b coords angle a-scale b-scale follow-skew) r
    (multiple-value-call #'ellipse-path-canvas device
      (ellipse-vectors device xc yc a b coords :angle angle :a-scale a-scale
                       :b-scale b-scale :follow-skew follow-skew))))

(defmethod add-region-path ((r union-region) device)
  (dolist (sub (region-components r))
    (add-region-path sub device)))

(defun clip-current-path (device rule)
  (let ((old (fill-rule device)))
    (set-fill-rule device rule)
    (clip device)
    (set-fill-rule device old)))

(defgeneric apply-clip (device region)
  (:documentation "Intersect DEVICE's clip with REGION (NIL: no change).
   Scope it with WITH-SAVED-STATE, or use WITH-CLIPPING."))

(defmethod apply-clip :around (device region)
  ;; a clip region's outline is not drawing
  (declare (ignore device region))
  (without-footprint (call-next-method)))

(defmethod apply-clip (device (region null))
  (declare (ignore device)))

(defmethod apply-clip (device (region path-region))
  (new-path device)
  (add-region-path region device)
  (clip-current-path device :winding))

(defmethod apply-clip (device (region intersection-region))
  (dolist (sub (region-components region))
    (apply-clip device sub)))

(defmethod apply-clip (device (region outside-region))
  ;; the whole page plus the region: even-odd leaves the region out
  (new-path device)
  (rectangle-path* device 0 0 1 1 :device)
  (add-region-path (region-complement region) device)
  (clip-current-path device :even-odd))

(defmacro with-clipping ((device region) &body body)
  "Run BODY with drawing on DEVICE also clipped to REGION.  DEVICE may be
   a viewport, entered for BODY, whose :PLOT coordinates REGION is in."
  (let ((dev (gensym "DEVICE")) (fn (gensym "BODY")))
    `(let ((,dev ,device))
       (flet ((,fn ()
                (with-saved-state (,dev)
                  (apply-clip (as-canvas ,dev) ,region)
                  ,@body)))
         (if (typep ,dev 'viewport)
             (with-viewport (,dev) (,fn))
             (,fn))))))
