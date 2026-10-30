#|

Fits stamp routine for Cairo, almost identical to the PGPLOT version

|#


(in-package fits-stamp-cairo)



;;;; ============================================================
;;;; Marker Classes (compatible with fits-stamp)
;;;; ============================================================

(defclass fits-stamp-marker ()
  ((color :initform :red :initarg :color :accessor fsm-color)
   (weight :initform 2 :initarg :weight :accessor fsm-weight)
   (fail-silently :initform t :initarg :fail-silently :accessor fail-silently)))

(defclass %xy-mixin ()
  ((x :initform 0 :initarg :x :accessor fsm-x)
   (y :initform 0 :initarg :y :accessor fsm-y)
   (coordinate-units :initform :pixel :initarg :coordinate-units :accessor coordinate-units)))

(defclass crosshairs (fits-stamp-marker %xy-mixin)
  ((r-inner :initform 0.03 :initarg :r-inner :accessor r-inner)
   (r-outer :initform 0.10 :initarg :r-outer :accessor r-outer)))

(defclass circle (fits-stamp-marker %xy-mixin)
  ((r :initform 20 :initarg :r :accessor r)))

(defclass pixel-scale-marker (fits-stamp-marker)
  ((pixel-scale :initform nil :initarg :pixel-scale :accessor pixel-scale)
   (weight :initform 3)
   (bar-length :initform nil :initarg :bar-length :accessor bar-length)))

(defclass text-marker (fits-stamp-marker %xy-mixin)
  ((text :initform "" :initarg :text :accessor text)
   (character-height :initform 0.7 :initarg :character-height :accessor character-height)
   (xoffset :initform 0 :initarg :xoffset :accessor xoffset)
   (yoffset :initform 0 :initarg :yoffset :accessor yoffset)
   (angle :initform 0.0 :initarg :angle :accessor angle)
   (font :initform :default :initarg :font :accessor font)
   (blanked :initform nil :initarg :blanked :accessor blanked)
   (blanking-color :initform nil :initarg :blanking-color :accessor blanking-color)
   (centered :initform t :initarg :centered :accessor centered)))

(defclass compass-rose (fits-stamp-marker)
  ((wcs :initform nil :initarg :wcs :accessor wcs)
   (north-vector :initform nil :initarg :north-vector :accessor north-vector)
   (east-vector :initform nil :initarg :east-vector :accessor east-vector)
   (axis-length :initform 0.15 :initarg :axis-length :accessor axis-length)
   (arrow-head-angle :initform 30 :initarg :arrow-head-angle
		     :accessor arrow-head-angle)
   (arrow-size :initform 1.5 :initarg :arrow-size :accessor arrow-size)
   (weight :initform 5)
   (color :initform :red)))

(defclass orbit-direction-axes (fits-stamp-marker)
  ((wcs :initform nil :initarg :wcs :accessor wcs)
   (pa-solar :initform -90.0 :initarg :pa-solar :accessor pa-solar)
   (pa-orbit :initform +90.0 :initarg :pa-orbit :accessor pa-orbit)
   (pa-plane :initform 0.0 :initarg :pa-plane :accessor pa-plane)
   (dot-solar :initform 1d0 :initarg :dot-solar :accessor dot-solar)
   (dot-orbit :initform 1d0 :initarg :dot-orbit :accessor dot-orbit)
   (dot-plane :initform 1d0 :initarg :dot-plane :accessor dot-plane)
   (axis-length :initform 0.07 :initarg :axis-length :accessor axis-length)
   (xc :initform 0.13 :initarg :xc :accessor xc)
   (yc :initform 0.87 :initarg :yc :accessor yc)
   (arrow-size :initform 1.0 :initarg :arrow-size :accessor arrow-size)
   (arrow-head-angle :initform 20 :initarg :arrow-head-angle
		     :accessor arrow-head-angle)
   (weight :initform 5)
   (color :initform :red)))

(defclass arrow (fits-stamp-marker)
  ((xa :initform 0.0 :initarg :xa :accessor arrow-xa)
   (xb :initform 1.0 :initarg :xb :accessor arrow-xb)
   (ya :initform 0.0 :initarg :ya :accessor arrow-ya)
   (yb :initform 1.0 :initarg :yb :accessor arrow-yb)
   (arrow-size :initform 2.0 :initarg :arrow-size :accessor arrow-size)
   (arrow-head-angle :initform 30 :initarg :arrow-head-angle
		     :accessor arrow-head-angle)
   (weight :initform 5)
   (coordinate-units :initform :pixel :initarg :coordinate-units :accessor coordinate-units)))

(defclass points (fits-stamp-marker)
  ((xvec :initform (make-array 0) :initarg :xvec :accessor xvec)
   (yvec :initform (make-array 0) :initarg :yvec :accessor yvec)
   (ptype :initform :point :initarg :ptype :accessor ptype)
   (point-size :initform 2 :initarg :point-size :accessor point-size)
   (coordinate-units :initform :pixel :initarg :coordinate-units :accessor coordinate-units)))

;;;; ============================================================
;;;; Coordinate Conversion Utilities (for markers)
;;;; ============================================================

(defun convert-pixel-to-unit (x y x0 y0 size)
  (values (/ (+ x (- x0) (/ size 2.0)) size)
          (/ (+ y (- y0) (/ size 2.0)) size)))

(defun convert-unit-to-pixel (xu yu x0 y0 size)
  (values (+ (* xu size) (- x0 (/ size 2.0)))
          (+ (* yu size) (- y0 (/ size 2.0)))))

(defun convert-xy-to-pixels (x y units x0 y0 size wcs)
  (cond ((eq units :pixel)
         (values x y))
        ((eq units :unit)
         (convert-unit-to-pixel x y x0 y0 size))
        ((eq units :equatorial)
         (when (not (wcs:wcs-2d-p wcs))
           (error "UNITS = :equatorial but WCS undefined"))
         (wcs:wcs-convert-ra-dec-to-pix-xy wcs (* 1d0 x) (* 1d0 y)))
        (t (error "Unknown UNITS ~S" units))))

(defun convert-xy-vectors-to-pixels (xvec yvec units x0 y0 size wcs)
  (declare (type vector xvec yvec))
  (if (eq units :pixel)
      (values xvec yvec)
      (let ((xv (make-array (length xvec)))
            (yv (make-array (length yvec))))
        (loop for x across xvec and y across yvec for i from 0 do
          (multiple-value-bind (xx yy) (convert-xy-to-pixels x y units x0 y0 size wcs)
            (setf (aref xv i) xx (aref yv i) yy)))
        (values xv yv))))

;;;; ============================================================
;;;; Marker Rendering Methods
;;;; ============================================================

(defgeneric render-marker (marker ctx &key x0 y0 size wcs))

(defmethod render-marker ((the-crosshairs crosshairs) (ctx stamp-context)
                          &key x0 y0 size wcs)
  (let ((xx (fsm-x the-crosshairs))
        (yy (fsm-y the-crosshairs))
        (color (fsm-color the-crosshairs))
        (weight (fsm-weight the-crosshairs))
        (ri (* size (r-inner the-crosshairs)))
        (ro (* size (r-outer the-crosshairs))))
    (multiple-value-bind (x y)
        (convert-xy-to-pixels xx yy (coordinate-units the-crosshairs) x0 y0 size wcs)
      (loop for (xv yv) in (list (list (vector (+ x ri) (+ x ro)) (vector y y))
                                  (list (vector (- x ri) (- x ro)) (vector y y))
                                  (list (vector x x) (vector (+ y ri) (+ y ro)))
                                  (list (vector x x) (vector (- y ri) (- y ro))))
            do (connect ctx (first (list xv yv)) (second (list xv yv))
                        :color color :line-width weight)))))

(defmethod render-marker ((the-circle circle) (ctx stamp-context)
                          &key x0 y0 size wcs)
  (multiple-value-bind (x y)
      (convert-xy-to-pixels (fsm-x the-circle) (fsm-y the-circle)
                            (coordinate-units the-circle) x0 y0 size wcs)
    (draw-circle ctx x y (r the-circle)
                 :color (fsm-color the-circle)
                 :line-width (fsm-weight the-circle))))

(defmethod render-marker ((the-arrow arrow) (ctx stamp-context)
                          &key x0 y0 size wcs)
  (multiple-value-bind (xa ya)
      (convert-xy-to-pixels (arrow-xa the-arrow) (arrow-ya the-arrow)
                            (coordinate-units the-arrow) x0 y0 size wcs)
    (multiple-value-bind (xb yb)
        (convert-xy-to-pixels (arrow-xb the-arrow) (arrow-yb the-arrow)
                              (coordinate-units the-arrow) x0 y0 size wcs)
      (draw-arrow ctx xa ya xb yb
                  :color (fsm-color the-arrow)
                  :size (arrow-size the-arrow)
		  :head-angle (arrow-head-angle the-arrow)))))

(defmethod render-marker ((the-points points) (ctx stamp-context)
                          &key x0 y0 size wcs)
  (multiple-value-bind (xv yv)
      (convert-xy-vectors-to-pixels (xvec the-points) (yvec the-points)
                                    (coordinate-units the-points) x0 y0 size wcs)
    (draw-points ctx xv yv (ptype the-points)
                 :color (fsm-color the-points)
                 :size (point-size the-points))))

(defmethod render-marker ((the-text-marker text-marker) (ctx stamp-context)
                          &key x0 y0 size wcs)
  (multiple-value-bind (xx yy)
      (convert-xy-to-pixels (fsm-x the-text-marker) (fsm-y the-text-marker)
                            (coordinate-units the-text-marker) x0 y0 size wcs)
    (let ((x (+ xx (* size (xoffset the-text-marker))))
          (y (+ yy (* size (yoffset the-text-marker)))))
      (if (blanked the-text-marker)
          (write-blanked-text ctx (text the-text-marker) x y
                              :center (centered the-text-marker)
                              :character-height (character-height the-text-marker)
                              :color (fsm-color the-text-marker)
                              :angle (angle the-text-marker)
                              :blanking-color (blanking-color the-text-marker))
          (write-text ctx (text the-text-marker) x y
                      :center (centered the-text-marker)
                      :character-height (character-height the-text-marker)
                      :color (fsm-color the-text-marker)
                      :angle (angle the-text-marker))))))

(defmethod render-marker ((the-scale pixel-scale-marker) (ctx stamp-context)
                          &key x0 y0 size wcs)
  (let* ((pix-scale (or (pixel-scale the-scale)
                        (if wcs (wcs:get-pixel-scale-for-wcs wcs))
                        (error "Cannot compute pixel scale")))
         (blen (or (bar-length the-scale)
                   (loop with blen = 0.0001
                         for i below 20
                         for jump = (aref #(2 2.5 2) (mod i 3))
                         for pixlen = (/ blen pix-scale)
                         do (if (> pixlen (* 0.15 size))
                                (return blen)
                                (setf blen (* blen jump))))))
         (plen (/ blen pix-scale))
         (stext (if (< blen 1.0)
                    (format nil "~,2F\"" blen)
                    (format nil "~D\"" (round blen))))
         (xa (+ (* 0.05 size) (- x0 (* size 0.5))))
         (xb (+ xa plen))
         (y (+ (* 0.04 size) (- y0 (* size 0.5)))))
    ;; Main bar
    (connect ctx (vector xa xb) (vector y y)
             :color (fsm-color the-scale) :line-width (fsm-weight the-scale))
    ;; Vertical end bars
    (loop for x in (list xa xb) do
      (connect ctx (vector x x)
               (vector (+ y (* size 0.01)) (- y (* size 0.01))) 
               :color (fsm-color the-scale) :line-width (fsm-weight the-scale)))
    ;; Label
    (write-text-with-outline ctx stext (* 0.5 (+ xa xb)) (+ y (* 0.03 size))
			     :outline-color :very-light-gray
			     :character-height 3.0
			     :center t :color (fsm-color the-scale))))

(defmethod render-marker ((the-cr compass-rose) (ctx stamp-context)
                          &key x0 y0 size wcs)
  (let* ((o-vector (make-array 2 :initial-element 0.0))
	 ;; vectors pointing N,E
         (n-vector (copy-seq (north-vector the-cr)))
         (e-vector (copy-seq (east-vector the-cr)))
	 ;; vectors for positioning the N,E labels
         (nl-vector nil) (el-vector nil)
         (axis-length/pix (* size (axis-length the-cr)))
         (color (fsm-color the-cr))
         (weight (fsm-weight the-cr))
         (wcs (if (not (and n-vector e-vector))
                  (or (wcs the-cr) wcs))))
    (when (not (or (and n-vector e-vector) wcs))
      (error "Cannot render COMPASS-ROSE without N-VECTOR,E-VECTOR or WCS"))
    ;; Compute pixel-space vectors from WCS if needed
    (when (not n-vector)
      (multiple-value-bind (xpix ypix)
	  (wcs:wcs-position-angle-to-pixel-unit-vector wcs 0d0)
	(setf n-vector (vector xpix ypix)))
      (multiple-value-bind (xpix ypix)
	  (wcs:wcs-position-angle-to-pixel-unit-vector wcs 90d0)
	(setf e-vector (vector xpix ypix))))
    ;;
    (setf nl-vector (copy-seq n-vector))
    (setf el-vector (copy-seq e-vector))
    ;; Normalize vectors 
    (flet ((norm-vector (v factor)
             (let ((norm (sqrt (+ (expt (aref v 0) 2) (expt (aref v 1) 2)))))
               (when (zerop norm) (error "Zero norm in compass rose"))
               (setf (aref v 0) (* factor axis-length/pix (/ (aref v 0) norm)))
               (setf (aref v 1) (* factor axis-length/pix (/ (aref v 1) norm))))))
      (norm-vector n-vector 1.0) (norm-vector e-vector 1.0)
      (norm-vector nl-vector 1.20) (norm-vector el-vector 1.20))
    ;; Position in upper right
    (let* ((ximright (+ x0 (* 0.5 size)))
           (yimtop (+ y0 (* 0.5 size)))
           (xright (max 0.0 (aref nl-vector 0) (aref el-vector 0)))
           (ytop (max 0.0 (aref nl-vector 1) (aref el-vector 1)))
           (edge-room (* 0.05 size))
           (dx (- ximright xright edge-room))
           (dy (- yimtop ytop edge-room)))
      (flet ((shift-vec (v) (incf (aref v 0) dx) (incf (aref v 1) dy)))
        (mapc #'shift-vec (list o-vector e-vector el-vector n-vector nl-vector))))
    ;; Draw
    (with-settings (ctx :line-width weight)
      (draw-arrow ctx (aref o-vector 0) (aref o-vector 1)
                  (aref n-vector 0) (aref n-vector 1)
		  :line-width 3
		  :head-angle (arrow-head-angle the-cr) 
                  :color color :size (arrow-size the-cr))
      (draw-arrow ctx (aref o-vector 0) (aref o-vector 1)
                  (aref e-vector 0) (aref e-vector 1)
		  :line-width 3
		  :head-angle (arrow-head-angle the-cr)
                  :color color :size (arrow-size the-cr))
      (write-text-with-outline ctx "N" (aref nl-vector 0) (aref nl-vector 1)
			       :outline-color :very-light-gray
			       :center t :character-height 2.0 :color color)
      (write-text-with-outline ctx "E" (aref el-vector 0) (aref el-vector 1)
		  :outline-color :very-light-gray
                  :center t :character-height 2.0 :color color))))

(defmethod render-marker ((the-daxes orbit-direction-axes) (ctx stamp-context)
                          &key x0 y0 size wcs)
  (declare (ignorable x0 y0 size))
  (let ((pa-solar (pa-solar the-daxes)) (dot-solar (dot-solar the-daxes))
        (pa-orbit (pa-orbit the-daxes)) (dot-orbit (dot-orbit the-daxes))
        (pa-plane (pa-plane the-daxes)) (dot-plane (dot-plane the-daxes))
        (axis-length (axis-length the-daxes))
        (xc (xc the-daxes)) (yc (yc the-daxes))
        (color (fsm-color the-daxes))
        (weight (fsm-weight the-daxes))
        (wcs (or (wcs the-daxes) wcs (error "No WCS for ORBIT-DIRECTION-AXES")))
        solar-vec orbit-vec plane-vec
        solar-len orbit-len plane-len)
    (declare (ignore pa-plane dot-plane plane-vec plane-len color))
    (flet ((make-axis-vec (pa dot)
             (multiple-value-bind (xp yp)
		 (wcs:wcs-position-angle-to-pixel-unit-vector wcs pa)
	       (let* ((vec (vector xp yp))
		      ;; dot is cosine (line of sight); we want sin (on sky)
		      (idot (sqrt (- 1.0001d0 (* dot dot))))
		      ;; the length of vector in image
		      (len (* (max 0.1 idot) ;; impose a minimum length
			      axis-length))) ;; scale factor, including tilt of axis to LOS
		 ;; return the unit vector and the length
		 (values vec len)))))
      
      (multiple-value-setq (solar-vec solar-len)
	(make-axis-vec pa-solar dot-solar))
      (multiple-value-setq (orbit-vec orbit-len)
	(make-axis-vec pa-orbit dot-orbit))
      ;;
      (with-window (ctx 0 1 0 1)
        (with-settings (ctx :line-width weight)
          (flet ((draw-vector (vec len label-char col)
		   ;; the small tick mark on the circle
                   (connect ctx
                            (vector (+ xc (* 0.9 axis-length (aref vec 0)))
                                    (+ xc (* 1.10 axis-length (aref vec 0))))
                            (vector (+ yc (* 0.9 axis-length (aref vec 1)))
                                    (+ yc (* 1.10 axis-length (aref vec 1))))
                            :color :dark-gray :line-width 2)
                   (draw-arrow ctx xc yc
                               (+ xc (* len (aref vec 0)))
                               (+ yc (* len (aref vec 1)))
			       :line-width 2
                               :color col
			       :head-angle (arrow-head-angle the-daxes)
			       :size  (arrow-size the-daxes))
		   ;;
		   (if (eql label-char #\SUN)
			 (draw-sun-symbol ctx
					  (+ xc (* 1.3 axis-length (aref vec 0)))
					  (+ yc (* 1.3 axis-length (aref vec 1)))
					  :blank-backd t
					  :color col :size 1.2)
		       ;;
			 (write-text-with-outline ctx (string label-char)
				     (+ xc (* 1.3 axis-length (aref vec 0)))
				     (+ yc (* 1.3 axis-length (aref vec 1))) 
				     :outline-color :very-light-gray 
				     :outline-width 4.0
				     :color col :center t :character-height 1.5))))
				   

            (draw-circle ctx xc yc axis-length :color :very-light-gray :line-width 6)
            (draw-circle ctx xc yc axis-length :color :dark-gray :line-width 2)
            (draw-vector solar-vec solar-len #\SUN :black)
            (draw-vector orbit-vec orbit-len #\v :red)))))))

(defmethod fill-orbit-direction-axes-using-comet-elem
    ((the-daxes orbit-direction-axes) (comet-elem orbital-elements:comet-elem)
     (mjd real) &key (observatory "geocenter"))
  "Populate ORBIT-DIRECTION-AXES with position angles and dot products"
  (let ((orbit-dirs (slalib-ephem::generate-orbit-dirs-for-comet-elem-for-observatory
                     comet-elem mjd observatory)))
    (setf (pa-solar the-daxes) (slalib-ephem:orbit-dirs-solar-pa orbit-dirs))
    (setf (pa-orbit the-daxes) (slalib-ephem:orbit-dirs-orbit-pa orbit-dirs))
    (setf (pa-plane the-daxes) (slalib-ephem:orbit-dirs-plane-pa orbit-dirs))
    (setf (dot-solar the-daxes) (slalib-ephem:orbit-dirs-solardot orbit-dirs))
    (setf (dot-orbit the-daxes) (slalib-ephem:orbit-dirs-orbitdot orbit-dirs))
    (setf (dot-plane the-daxes) (slalib-ephem:orbit-dirs-planedot orbit-dirs))
    the-daxes))

;;;; ============================================================
;;;; Main Entry Point
;;;; ============================================================

(defun make-stamp (fits-file plot-type x0 y0 size
                   &key
                     output-file
                     extension
                     wcs
                     toplabel
                     (scaling :linear)
                     (sigma-stretch nil)
                     (sigma-offset nil)
                     (num-labels nil)
                     (markers nil)
                     (device-size 800))  ; output size in pixels
  "Extract a stamp from FITS-FILE and render using Cairo.
   PLOT-TYPE is :PDF, :PS, :PNG, :SVG, or :SVGZ.
   X0, Y0, SIZE define the stamp region in pixels.
   Returns T on success."
  (declare (type (member :ps :pdf :png :svg :svgz) plot-type))
  (when (not output-file)
    (error "OUTPUT-FILE is required"))

  ;; Set default scalings
  (unless sigma-stretch
    (setf sigma-stretch (ecase scaling (:linear 5) (:log 3) (:sqrt 3))))
  (unless sigma-offset
    (setf sigma-offset (ecase scaling (:linear -3) (:log -0.5) (:sqrt -1.0))))
  (let* ((nx0 (round x0)) (ny0 (round y0))
         (hs (ash size -1))
         (nxmin (- nx0 hs)) (nxmax (+ nx0 hs))
         (nymin (- ny0 hs)) (nymax (+ ny0 hs))
         (the-wcs (or wcs (ignore-errors (cf:read-wcs fits-file :extension extension))))
         (null-value -1e-30)
         (out-of-bounds-value -2e30)
         img medval sigma)
    ;; Read FITS data
    (cf:with-open-fits-file (fits-file ff)
      (cond
        (extension (cf:move-to-extension ff extension))
        ((not extension)
         (cond
           ((= 2 (cf:fits-file-current-image-ndims ff)) nil)
           ((>= (cf:fits-file-num-hdus ff) 1)
            (cf:move-to-extension ff 2)
            (when (not (= 2 (cf:fits-file-current-image-ndims ff)))
              (error "No image extension found in HDU 1 or 2"))))))
      (setf img (cf:read-fits-image-subsection-into-array
                 ff nxmin nymin nxmax nymax
                 :null-value null-value
                 :out-of-bounds-value out-of-bounds-value)))
    (when img
      (float-utils:with-float-traps-masked (:inexact t :overflow t :underflow t :invalid t)
        ;; Compute median and sigma
        (let ((pixlist nil))
          (loop for k below (array-total-size img)
                for pixval of-type single-float = (row-major-aref img k)
                when (not (or (= pixval null-value)
                              (= pixval out-of-bounds-value)
                              (float-utils:float-nan-p pixval)
                              (float-utils:float-infinity-p pixval)))
                  do (push pixval pixlist))
          (let ((v (make-array (length pixlist)
                               :element-type 'single-float
                               :initial-contents (nreverse pixlist))))
            (setf medval (fastmedian:fast-single-float-1d-array-median v))
            (setf sigma (* 0.5 (- (fastmedian:fast-single-float-1d-array-fraction v 0.84)
                                  (fastmedian:fast-single-float-1d-array-fraction v 0.16))))))
        ;; Apply scaling transformation
        (when (member scaling '(:log :sqrt))
          (loop for k below (array-total-size img)
                for pixval of-type single-float = (row-major-aref img k)
                when (not (or (= pixval null-value)
                              (= pixval out-of-bounds-value)
                              (float-utils:float-nan-p pixval)
                              (float-utils:float-infinity-p pixval)))
                  do (let* ((x (- pixval medval))
                            (sx (if (minusp x) -1 1)))
                       (setf (row-major-aref img k)
                             (float (ecase scaling
                                      (:log (* sx (log (+ 1d0 (abs x)))))
                                      (:sqrt (* sx (sqrt (abs x)))))
                                    1.0))))
          (setf sigma (ecase scaling (:log (log sigma)) (:sqrt (sqrt sigma))))
          (setf medval 0.0))

        (let* ((centval (- medval (* sigma-offset sigma)))
               (val1 (- centval (* sigma-stretch sigma)))
               (val2 (+ centval (* sigma-stretch sigma))))

	  ;; put CHECKERBOARD checkerboard of 3% of WIDTH of image	  
	  (let* ((cbdelta (* 0.05 (* sigma-stretch sigma)))
		 (cb1 (- centval (* 2 cbdelta)))
		 (cb2 (- centval (* 1 cbdelta)))
		 (ncb (ceiling (* 0.03 (array-dimension img 0)))))
	    (loop for ix below (array-dimension img 1)
		  do
		     (loop for iy below (array-dimension img 0)
			   for dark = (zerop (mod (+ (floor ix ncb) (floor iy ncb)) 2))
			   when (= (aref img iy ix) out-of-bounds-value)
			     do (setf (aref img  iy ix)
				      (if dark cb2 cb1)))))

          ;; Replace bad values with val1
          (loop for k below (array-total-size img)
                for pixval of-type single-float = (row-major-aref img k)
                when (or (= pixval null-value)
                         ;;(= pixval out-of-bounds-value)
                         (float-utils:float-nan-p pixval)
                         (float-utils:float-infinity-p pixval))
                  do (setf (row-major-aref img k) val1))

          ;; Render using Cairo
          (with-stamp-context (ctx plot-type :filename output-file
                                       :width device-size :height device-size)
            ;; Set up viewport and window - minimal margins
            ;; Note: vp-y1 controls TOP of plot on screen (due to Y flip)
            ;;       vp-y2 controls BOTTOM of plot on screen
            (let ((vp-x1 (if num-labels 0.05 0.005))   ; left: room for Y labels
                  (vp-x2 0.995)                         ; right: almost none
                  (vp-y1 (if toplabel 0.04 0.01))      ; top: minimal for title
                  (vp-y2 (if num-labels 0.96 0.995)))  ; bottom: room for X labels
              (set-viewport ctx vp-x1 vp-x2 vp-y1 vp-y2))
            (set-window ctx nxmin nxmax nymin nymax)

            ;; Render image
            (render-image ctx img :val1 val1 :val2 val2 :reverse t)

	    
            ;; Draw box
            (draw-box ctx :size 1.2 :x-num-labels num-labels :y-num-labels num-labels)

            ;; Top label - draw in device coordinates above plot
            (when toplabel
              (draw-toplabel ctx toplabel :character-height 1.6))

            ;; Render markers
            (loop for marker in markers
                  when marker do
                    (flet ((do-marker ()
                             (render-marker marker ctx
                                            :x0 x0 :y0 y0 :size size :wcs the-wcs)))
                      (if (fail-silently marker)
                          (ignore-errors (do-marker))
                          (do-marker)))))))))
  t)

(defun make-pdf-stamp (fits-file pdf-file x0 y0 size
                       &key extension wcs toplabel (scaling :linear)
                            sigma-stretch sigma-offset num-labels markers)
  "Convenience wrapper for make-stamp with :PDF output"
  (make-stamp fits-file :pdf x0 y0 size
              :output-file pdf-file
              :extension extension :wcs wcs :toplabel toplabel
              :scaling scaling :sigma-stretch sigma-stretch
              :sigma-offset sigma-offset :num-labels num-labels
              :markers markers))

(defun make-svgz-stamp (fits-file svgz-file x0 y0 size
                       &key extension wcs toplabel (scaling :linear)
                            sigma-stretch sigma-offset num-labels markers)
  "Convenience wrapper for make-stamp with :SVGZ (gzipped SVG) output"
  (make-stamp fits-file :svgz x0 y0 size
              :output-file svgz-file
              :extension extension :wcs wcs :toplabel toplabel
              :scaling scaling :sigma-stretch sigma-stretch
              :sigma-offset sigma-offset :num-labels num-labels
              :markers markers))
