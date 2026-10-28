
(in-package pgplot)

(export
 'make-xform

 )

;; three-d version of pgplot
(defclass pgplot3d (pgplot)
  ((xform :accessor xform :initform nil :initarg :xform)))

;; change the class of a pgplot to be a 3d device
(defmethod make-pgplot-3d ((pgplot pgplot) &rest xform-creation-args)
  (change-class pgplot 'pgplot3d)
  (apply 'set-pgplot3d-xform pgplot xform-creation-args)
;  (setf (xform pgplot) (apply 'create-xform xform-creation-args))
  pgplot)

(defmethod set-pgplot3d-xform ((pgplot3d pgplot3d) &rest xform-creation-args)
  (setf (xform pgplot3d) (apply 'create-xform xform-creation-args))
  (pgplot:set-window pgplot3d
		     (xform-x1 (xform pgplot3d))
		     (xform-x2 (xform pgplot3d))
		     (xform-z1 (xform pgplot3d))
		     (xform-z2 (xform pgplot3d)))
  (xform pgplot3d))
		     

;; break up keyword arguments into 2 sets, one in keep-keys, one not
(defun filter-keyword-ags (karglist keep-keys)
  (loop for thing in karglist
	for should-be-keyword = t then (not should-be-keyword)
	with keepnext = nil
	with keeps = nil
	with rejects = nil
	do (progn (if (and should-be-keyword (not (keywordp thing)))
		      (error (format nil "bad keyword list ~A in filter-keyword-ags"
				     karglist)))
		  (cond ((keywordp thing)
			 (cond ((member thing keep-keys)
				(setf keepnext t)
				(push thing keeps))
			       (t
				(setf keepnext nil)
				(push thing rejects))))
			(t
			 (if keepnext
			     (push thing keeps)
			   (push thing rejects)))))
	finally (return (values (nreverse keeps)  (nreverse rejects)))))
		  

;; analog to open-device, but for a 3d device
(defun open-device-3d (device &rest args)
  (multiple-value-bind (xform-args open-args)
      (filter-keyword-ags args '(:origin :z-angle :y-angle :x-angle
					 :x1 :x2 :z1 :z2))
    (let ((p (apply 'open-device device open-args)))
      (apply 'make-pgplot-3d p  xform-args))))

;; 4x4 matrix transformation
(defstruct xform
  (4matrix (make-array '(4 4) :element-type 'double-float
		       :initial-element 0.0d0)
	   :type (simple-array double-float (4 4)))
  (origin (make-array 3  :element-type 'double-float
		      :initial-element 0.0d0))
  ;; angle of rotation about z axis (first rotation, in xy plane)
  (z-angle 0.0d0 :type double-float)
  ;; angle of rotation about x axis (2nd, tipping xy plane towards you)
  (x-angle 0.0d0 :type double-float)
  ;; angle of rotation about y axis (3rd, rolling the viewport around) 
  (y-angle 0.0d0 :type double-float)
  ;; bounds of viewport in x,z plane
  (x1 0.0d0 :type double-float)
  (x2 1.0d0 :type double-float)
  (z1 0.0d0 :type double-float)
  (z2 1.0d0 :type double-float)
  )

(defun make-z-rotation-matrix (angle)  ;; rotation in xy plane
  (declare (type (double-float -1d10 +1d10) angle))
  (let ((a (make-array '(4 4) :element-type 'double-float
		       :initial-element 0.0d0)))
    (let* ((radians (* #.(/ pi 180.0d0) angle))
	   (c (cos radians))
	   (s (sin radians)))
      (dotimes (i 4) (setf (aref a i i) 1.0d0))
      (setf (aref a 0 0) c)
      (setf (aref a 0 1) (- s))
      (setf (aref a 1 0) (+ s))
      (setf (aref a 1 1) c)
      a)))

(defun make-y-rotation-matrix (angle)  ;; rotation in xz plane
  (declare (type (double-float -1d10 +1d10) angle))
  (let ((a (make-array '(4 4) :element-type 'double-float
		       :initial-element 0.0d0)))
    (let* ((radians (* #.(/ pi 180.0d0) angle))
	   (c (cos radians))
	   (s (sin radians)))
      (dotimes (i 4) (setf (aref a i i) 1.0d0))
      (setf (aref a 0 0) c)
      (setf (aref a 0 2) (- s))
      (setf (aref a 2 0) (+ s))
      (setf (aref a 2 2) c)
      a)))


(defun make-x-rotation-matrix (angle)  ;; rotation in yz plane
  (declare (type (double-float -1d10 +1d10) angle))
  (let ((a (make-array '(4 4) :element-type 'double-float
		       :initial-element 0.0d0)))
    (let* ((radians (* #.(/ pi 180.0d0) angle))
	   (c (cos radians))
	   (s (sin radians)))
      (dotimes (i 4) (setf (aref a i i) 1.0d0))
      (setf (aref a 1 1) c)
      (setf (aref a 1 2) (- s))
      (setf (aref a 2 1) (+ s))
      (setf (aref a 2 2) c)
      a)))

(defun add-rotation-matrix-to-xform (xform a)
  (declare (type xform xform)
	   (type (simple-array double-float (4 4)) a))
  (let ((m (xform-4matrix xform))
	(mm (make-array '(4 4) :element-type 'double-float
				    :initial-element 0.0d0)))
    ;; make copy of m, and clear it
    (dotimes (i 4)
      (dotimes (j 4)
	(setf (aref mm i j) (aref m i j))
	(setf (aref m i j) 0.0d0)))
    ;; and multiply a*mm -> m
    (dotimes (i 4)
      (dotimes (j 4)
	(dotimes (k 4)
	  (incf (aref m i j) (* (aref a i k) (aref mm k j))))))
    xform))
  
;; create a transform with these transformation parameters
(defun create-xform (&key (origin nil) (z-angle nil) (y-angle nil) (x-angle nil)
			  (x1 1.0d0) (x2 1.0d0) (z1 1.0d0) (z2 1.0d0))
  (declare (type (or null (array T (3))) origin)
	   (type (or null real) z-angle y-angle x-angle))
  (let* ((xf (make-xform :x1 (coerce x1 'double-float)
			 :x2 (coerce x2 'double-float)
			 :z1 (coerce z1 'double-float)
			 :z2 (coerce z2 'double-float)))
	 (xo (xform-origin xf))
	 (m (xform-4matrix xf)))
    (dotimes (i 4) (setf (aref m i i) 1.0d0))
    (when origin
      (dotimes (i 3)
	(setf (aref m i 3) (coerce (aref origin i) 'double-float))
	(setf (aref xo i)  (coerce (aref origin i) 'double-float))))
    (when z-angle
      (setf (xform-z-angle xf) (coerce z-angle 'double-float))
      (add-rotation-matrix-to-xform xf
       (make-z-rotation-matrix (coerce z-angle 'double-float))))
    (when y-angle
      (setf (xform-y-angle xf) (coerce y-angle 'double-float))
      (add-rotation-matrix-to-xform xf
       (make-y-rotation-matrix (coerce y-angle 'double-float))))
    (when x-angle
      (setf (xform-x-angle xf) (coerce x-angle 'double-float))
      (add-rotation-matrix-to-xform xf
       (make-x-rotation-matrix (coerce x-angle 'double-float))))
    xf))
	
;; transform v using xform, placing into vout if defined
(defun xform-vector (xform v &optional vout)
  (declare (type xform xform)
	   (type (array * (3)) v)
	   (type (or null (array * (3))) vout))
  (let ((vout (or vout (make-array '3 :element-type 'double-float)))
	(m (xform-4matrix xform)))
    (dotimes (i 3)
      (setf (aref vout i) (aref m i 3)))
    (dotimes (i 3)
      (dotimes (j 3)
	(incf (aref vout i) (* (aref v j) (aref m i j)))))
    vout))

;; same, but with double-float vector only
(defun xform-dvector (xform v &optional vout)
  (declare (type xform xform)
	   (type (simple-array double-float (3)) v)
	   (type (or null (simple-array double-float (3))) vout)
	   (optimize (speed 3) (safety 0)))
  (let ((vout (or vout (make-array '3 :element-type 'double-float)))
	(m (xform-4matrix xform)))
    (dotimes (i 3)
      (declare (type fixnum i))
      (setf (aref vout i) (aref m i 3)))
    (dotimes (i 3)
      (declare (type fixnum i))
      (dotimes (j 3)
	(declare (type fixnum j))
	(incf (aref vout i) (* (aref v j) (aref m i j)))))
    vout))



;; connect points in x,y,z space
(defmethod connect-3d ((p pgplot3d)
		       (xvec vector) (yvec vector) (zvec vector) 
		       &key (line-width nil) (color nil) (line-style nil))
  (if (not (= (length xvec) (length yvec) (length zvec)))
      (error "unequal vectors in connect-3d"))
  (macrolet ((df (x) `(coerce ,x 'double-float)))
    (let* ((n (length xvec))
	   (xx (make-array n :element-type 'double-float))
	   (zz (make-array n :element-type 'double-float))
	   (tmp (make-array 3 :element-type 'double-float))
	   (res (make-array 3 :element-type 'double-float)))
      (loop for x across xvec
	    for y across yvec
	    for z across zvec
	    for i fixnum = 0 then (1+ i)
	    do (progn (setf (aref tmp 0) (df x)
			    (aref tmp 1) (df y)
			    (aref tmp 2) (df z))
		      (xform-vector (xform p) tmp res)
		      ;; save off y and z in res, as projected values
		      (setf (aref xx i) (aref res 0) (aref zz i) (aref res 2))))
      (connect p xx zz
	       :line-width line-width
	       :color color
	       :line-style line-style))))
  

(defmethod draw-3d-axes ((p pgplot3d) &key
			 (xmin 0.0) (xmax 1.0) 
			 (ymin 0.0) (ymax 1.0) 
			 (zmin 0.0) (zmax 1.0)
			 (x0 0.0) (y0 0.0) (z0 0.0) ;; axes pass through #(x0 y0 z0)
			 (line-width nil) (color nil) (line-style nil))
  (macrolet ((df (x) `(coerce ,x 'double-float)))
    (let ((vx (make-array 2 :element-type 'double-float))
	  (vy (make-array 2 :element-type 'double-float))
	  (vz (make-array 2 :element-type 'double-float)))
      (flet ((do-axis (xa ya za xb yb zb)
		      (setf (aref vx 0) (df xa)  (aref vx 1) (df xb)
			    (aref vy 0) (df ya)  (aref vy 1) (df yb)
			    (aref vz 0) (df za)  (aref vz 1) (df zb))
		      (connect-3d p vx vy vz
				  :line-width line-width
				  :color color
				  :line-style line-style)))
	(do-axis xmin y0 z0   xmax y0 z0)
	(do-axis x0 ymin z0   x0 ymax z0)
	(do-axis x0 y0 zmin   x0 y0 zmax)))))
			 
  
(defmethod 3d-axis-labels ((p pgplot3d) &key
			   (character-height nil) (fjust 0.0)
			   (x-label nil) (y-label nil) (z-label nil)
			   ;; nil angle means same angle as axis
			   (x-angle nil) (y-angle nil) (z-angle nil)
			   ;; nudges are vectors in projected x,z plane by
			   ;; which to shift label
			   (x-nudge #(0.0 0.0))
			   (y-nudge #(0.0 0.0))
			   (z-nudge #(0.0 0.0))
			   (x0 0.0) (y0 0.0) (z0 0.0) ;; axes pass through #(x0 y0 z0)
			   (x1 1.0) (y1 1.0) (z1 1.0)) ;; labels put at #(x1 y1 z1)
  (declare (type (array t (2)) x-nudge y-nudge z-nudge)
	   (type (or simple-string null) x-label y-label z-label)
	   (type (or real null) x-angle y-angle z-angle character-height fjust)
	   (type real x0 y0 z0 x1 y1 z1))
  (flet ((do-one-label
	  (label angle nudge x0 y0 z0 x1 y1 z1)
	  (let* ((proj0 (xform-vector (xform p) (vector x0 y0 z0)))
		 (proj1 (xform-vector (xform p) (vector x1 y1 z1)))
		 (dx (- (aref proj1 0) (aref proj0 0)))
		 (dz (- (aref proj1 2) (aref proj0 2)))
		 (angle (or angle (* #.(/ 180.0d0 pi) (atan dz dx)))))
	    ;; if label is upside down, rotate it
	    (if (> (abs angle) 90.0d0)
		(incf angle 180.0d0))
	    (pgplot:write-text p label
			       (+ (aref nudge 0) (aref proj1 0))
			       (+ (aref nudge 1) (aref proj1 2))
			       :angle angle :fjust fjust
			       :character-height character-height))))
    (if x-label	(do-one-label x-label x-angle x-nudge x0 y0 z0  x1 y0 z0))
    (if y-label	(do-one-label y-label y-angle y-nudge x0 y0 z0  x0 y1 z0))
    (if z-label	(do-one-label z-label z-angle z-nudge x0 y0 z0  x0 y0 z1))))
    
    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; routines for polygons
(defstruct ptriangle ;; projected triangle
  (xv (make-array 3 :element-type 'double-float)  ;; vertices x
      :type (simple-array double-float (3)))
  (zv (make-array 3 :element-type 'double-float)  ;; vertices z
      :type (simple-array double-float (3)))
  (ym 0.0d0 :type double-float) ;; mean y value
  (top t :type (member nil t))) ;; are we viewing triangle from top?

;; structure for a line
(defstruct pline
  (x1 :element-type 'double-float)
  (x2 :element-type 'double-float)
  (y1 :element-type 'double-float)
  (y2 :element-type 'double-float)
  (z1 :element-type 'double-float)
  (z2 :element-type 'double-float))

;; convert a quadrilateral to four projected triangles
;; tmp3a tmp3b are temporary 3 vectors to cut consing
(defun quad-to-proj-triangles (xvec yvec zvec xform tmp3a tmp3b)
  (declare (type (simple-array double-float (4)) xvec yvec zvec)
	   (type (simple-array double-float (3)) tmp3a tmp3b)
	   (type xform xform)
	   (optimize (speed 3)))
  (let ((xc 0.0d0)
	(yc 0.0d0)
	(zc 0.0d0))
    (declare (type double-float xc yc zc))
    (loop for i fixnum from 0 to 3
	  do (progn (incf xc (* 0.25d0 (aref xvec i)))
		    (incf yc (* 0.25d0 (aref yvec i)))
		    (incf zc (* 0.25d0 (aref zvec i)))))
    (flet ((build-triangle
	    (x1 y1 z1   x2 y2 z2  x3 y3 z3)
	    (declare (type double-float  x1 y1 z1   x2 y2 z2  x3 y3 x3))
	    (let* ((pt (make-ptriangle))
		   (xv (ptriangle-xv pt))
		   (zv (ptriangle-zv pt)))
	      ;; first vertex
	      (setf (aref tmp3a 0) x1 (aref tmp3a 1) y1 (aref tmp3a 2) z1)
	      (xform-dvector xform tmp3a tmp3b)
	      (incf (ptriangle-ym pt) (aref tmp3b 1))
	      (setf (aref xv 0) (aref tmp3b 0)  (aref zv 0) (aref tmp3b 2))
	      ;; second vertex
	      (setf (aref tmp3a 0) x2 (aref tmp3a 1) y2 (aref tmp3a 2) z2)
	      (xform-dvector xform tmp3a tmp3b)
	      (incf (ptriangle-ym pt) (aref tmp3b 1))
	      (setf (aref xv 1) (aref tmp3b 0)  (aref zv 1) (aref tmp3b 2))
	      ;; third vertex
	      (setf (aref tmp3a 0) x3 (aref tmp3a 1) y3 (aref tmp3a 2) z3)
	      (xform-dvector xform tmp3a tmp3b)
	      (incf (ptriangle-ym pt) (aref tmp3b 1))
	      (setf (aref xv 2) (aref tmp3b 0)  (aref zv 2) (aref tmp3b 2))
	      ;;
	      (setf (ptriangle-ym pt) (/ (ptriangle-ym pt) 3.0d0))
	      ;; use y component of cross product to compute top/bottom
	      (let ((dx1 (- (aref xv 1) (aref xv 0)))
		    (dx2 (- (aref xv 1) (aref xv 2)))
		    (dz1 (- (aref zv 1) (aref zv 0)))
		    (dz2 (- (aref zv 1) (aref zv 2))))
		(declare (type double-float dx1 dx2 dz1 dz2))
		(setf (ptriangle-top pt)
		      (not
		       (minusp (- (* dx1 dz2) (* dx2 dz1))))))
	      pt)))
      ;;
      (list
       (build-triangle (aref xvec 0) (aref yvec 0) (aref zvec 0)
		       (aref xvec 1) (aref yvec 1) (aref zvec 1)
		       xc yc zc)
       (build-triangle (aref xvec 1) (aref yvec 1) (aref zvec 1)
		       (aref xvec 2) (aref yvec 2) (aref zvec 2)
		       xc yc zc)
       (build-triangle (aref xvec 2) (aref yvec 2) (aref zvec 2)
		       (aref xvec 3) (aref yvec 3) (aref zvec 3)
		       xc yc zc)
       (build-triangle (aref xvec 3) (aref yvec 3) (aref zvec 3)
		       (aref xvec 0) (aref yvec 0) (aref zvec 0)
		       xc yc zc)))))
	      
			   
(defun plot-ptriangle (pgplot pt &rest polygon-args)
  (apply #'pgplot:polygon pgplot (ptriangle-xv pt) (ptriangle-zv pt)
	 polygon-args))


(defun break-up-grid-into-triangle-list (a xvec yvec xform)
  (declare (type (simple-array double-float (* *)) a)
	   (type (simple-array double-float (* )) xvec yvec)
	   (type xform xform)
	   (optimize (speed 3) (safety 0)))
  (let* ((dims (array-dimensions a))
	 (nx (first dims))
	 (ny (second dims))
	 (x4 (make-array 4 :element-type 'double-float))
	 (y4 (make-array 4 :element-type 'double-float))
	 (z4 (make-array 4 :element-type 'double-float))
	 (tmp3a (make-array 3 :element-type 'double-float))
	 (tmp3b (make-array 3 :element-type 'double-float))
	 (triangle-list nil))
    (declare (type (integer 0 #.(1- most-positive-fixnum)) nx ny))
    (dotimes (i (1- nx))
      (declare (type (integer 0 #.(1- most-positive-fixnum)) i))
      (dotimes (j (1- ny))
	(declare (type (integer 0 #.(1- most-positive-fixnum)) j))
	(setf (aref x4 0) (aref xvec i)
	      (aref x4 1) (aref xvec i)
	      (aref x4 2) (aref xvec (1+ i))
	      (aref x4 3) (aref xvec (1+ i))
	      ;;
	      (aref y4 0) (aref yvec j)
	      (aref y4 1) (aref yvec (1+ j))
	      (aref y4 2) (aref yvec (1+ j))
	      (aref y4 3) (aref yvec j)
	      ;;
	      (aref z4 0) (aref a i j)
	      (aref z4 1) (aref a i (1+ j))
	      (aref z4 2) (aref a (1+ i) (1+ j))
	      (aref z4 3) (aref a (1+ i) j))
	(setf triangle-list
	      (nconc
	       (quad-to-proj-triangles x4 y4 z4 xform tmp3a tmp3b)
	       triangle-list))))
  ;; return triangle list sorted by ym value
    (sort triangle-list
	  (lambda (t1 t2) (> (ptriangle-ym t1) (ptriangle-ym t2))))))
      
	   
;; plot a surface
(defmethod plot-surface ((pgplot pgplot3d) (a array) &key
			 ;; bounds spanned by array given by xmax..etc, but
			 ;; are over-ridden by explicit xvec,yvec
			 (xmin 0.0) (xmax 1.0)
			 (ymin 0.0) (ymax 1.0)
			 (xvec nil) (yvec nil)
			 ;; keywords for polygon colors
			 (fill t) ;; do we fill at all?
			 (top-color :blue)
			 (bottom-color :cyan)
			 (fill-area-style :solid)
			 ;; keywords for grid (squares)
			 (grid t) ;; draw grid?
			 (grid-top-color :default)
			 (grid-bottom-color :dark-grey)
			 (grid-line-style :solid)
			 (grid-line-width nil)
			 ;; keywords for mesh (triangles, drawn BEFORE grid)
			 (mesh nil) ;; draw mesh?
			 (mesh-top-color :default)
			 (mesh-bottom-color :dark-grey)
			 (mesh-line-style :solid)
			 (mesh-line-width nil)
			 ;; keywords for z-contours
			 
			 )
  ;;
  (if (not (= 2 (length (array-dimensions a))))
      (error "invalid type array in plot-surface -- need 2x2 array"))
  (let* ((dims (array-dimensions a))
	 (nx (first dims))
	 (ny (second dims))
	 ;; coerce a to double-float
	 (a (loop with aa = (make-array (list nx ny) :element-type 'double-float)
		  finally (return aa)
		  for i from 0 to (1- nx)
		  do (loop for j from 0 to (1- ny)
			   do (setf (aref aa i j)
				    (coerce (aref a i j) 'double-float)))))
	 ;; xvec is either xvec mapped to double-float array, or build it
	 (xvec (or (when xvec (map '(simple-array double-float)
				   (lambda (x) (coerce x 'double-float)) xvec))
		   (loop for i from 0 to (1- nx)
			 with v = (make-array nx :element-type 'double-float)
			 finally (return v)
			 with ninv of-type double-float = (/ 1.0d0 (1- nx))
			 do (setf (aref v i)
				  (+ xmin (* (- xmax xmin) i ninv))))))
	 ;; yvec is either xvec mapped to double-float array, or build it
	 (yvec (or (when yvec (map '(simple-array double-float)
				   (lambda (x) (coerce x 'double-float)) xvec))
		   (loop for i from 0 to (1- ny)
			 with v = (make-array ny :element-type 'double-float)
			 with ninv of-type double-float = (/ 1.0d0 (1- ny))
			 finally (return v)
			 do (setf (aref v i)
				  (+ ymin (* (- ymax ymin) i ninv))))))
	 (triangle-list (break-up-grid-into-triangle-list a xvec yvec
							  (xform pgplot))))
    ;;
    (let ((4veca (make-array 4 :element-type 'double-float))
	  (4vecb (make-array 4 :element-type 'double-float))
	  (2veca (make-array 2 :element-type 'double-float))
	  (2vecb (make-array 2 :element-type 'double-float)))
    (dolist (triangle triangle-list)
      ;; plot triangles if filling
      (when fill
	(plot-ptriangle pgplot triangle
			:color (if (ptriangle-top triangle) top-color bottom-color)
			:fill-area-style fill-area-style))
      ;; plot the mesh of triangles
      (when mesh
	(loop for i from 0 to 2
	      with xv =  (ptriangle-xv triangle)
	      with zv =  (ptriangle-zv triangle)
	      finally (setf (aref 4veca 3) (aref xv 0) ;; close triangle
			    (aref 4vecb 3) (aref zv 0))
	      do (setf (aref 4veca i) (aref xv i)
		       (aref 4vecb i) (aref zv i)))
	(connect pgplot 4veca 4vecb
		 :color (if (ptriangle-top triangle) mesh-top-color mesh-bottom-color)
		 :line-style mesh-line-style
		 :line-width mesh-line-width))
      ;; plot the rectangular grid 
      (when grid ;; connect only first line of triangle
	(dotimes (i 2) ;; copy first side of triangle
	  (setf (aref 2veca i) (aref (ptriangle-xv triangle) i)
		(aref 2vecb i) (aref (ptriangle-zv triangle) i)))
	(connect pgplot 2veca 2vecb
		 :color (if (ptriangle-top triangle) grid-top-color grid-bottom-color)
		 :line-style grid-line-style
		 :line-width grid-line-width)))))) 
		 


(defun sfunc (x y)
  (let ((r2  (+ (* x x) (* y y))))
    (+ -0.5 (* (cos r2) (exp (* -0.5 r2))))))

(defun sfunc (x y)
  (let ((r2  (+ (* x x) (* y y))))
    (/ -1.0 (+ r2 0.0001))))

(defparameter *surface*
  (let* ((n 40)
	 (a (make-array (list n n) :element-type 'single-float)))
    (dotimes (i n)
      (dotimes (j n)
	(let ((x (/ (- i (/ n 2)) 5.0))
	      (y (/ (- j (/ n 2)) 5.0)))
	  (setf (aref a i j) (sfunc x y)))))
    a))
	  
	

  





