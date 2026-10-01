#|

routines to fill an image with a value (typically NaN)
for, eg, blocking out bad zones

|#

(in-package imutils)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General pixel region mapping function - created with Claude
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun map-pixels-by-region (setter-func nx ny
                             &key
                               above-y below-y
                               left-of-x right-of-x
                               in-polygon out-of-polygon)
  "Call SETTER-FUNC for each pixel (ix, iy) matching the specified region criteria.
SETTER-FUNC is a function of (ix iy) using 0-based indexing.
NX and NY are the image dimensions.

Region criteria (all use 0-based pixel indices):
  :ABOVE-Y     - pixels with iy > boundary (number or ((x0 y0) (x1 y1) ...) pairs)
  :BELOW-Y     - pixels with iy < boundary
  :LEFT-OF-X   - pixels with ix < boundary
  :RIGHT-OF-X  - pixels with ix > boundary
  :IN-POLYGON  - pixels inside polygon ((x0 y0) (x1 y1) ...)
  :OUT-OF-POLYGON - pixels outside polygon

For piecewise-linear boundaries, the (x y) pairs define vertices sorted by x,
with linear interpolation between them. Values outside the x-range use the
nearest endpoint."
  (declare (type function setter-func)
           (type fixnum nx ny)
           (optimize (speed 3) (safety 1)))

  (labels
      ;; Parse a boundary spec into fast lookup arrays or constant
      ((parse-boundary (spec)
         "Returns (values type data) where type is :constant or :piecewise"
         (cond
           ((null spec) (values nil nil))
           ((numberp spec) (values :constant (coerce spec 'fixnum)))
           ((and (listp spec) (listp (first spec)))
            ;; List of (x y) pairs - convert to parallel fixnum arrays
            (let* ((n (length spec))
                   (sorted (sort (copy-list spec) #'< :key #'first))
                   (xs (make-array n :element-type 'fixnum))
                   (ys (make-array n :element-type 'fixnum)))
              (loop for i from 0 below n
                    for (x y) in sorted
                    do (setf (aref xs i) (truncate x))
                       (setf (aref ys i) (truncate y)))
              (values :piecewise (cons xs ys))))
           (t (error "Invalid boundary spec: ~A" spec))))

       ;; Interpolate Y value at position X using piecewise boundary
       (interpolate-y (x xs ys)
         (declare (type fixnum x)
                  (type (simple-array fixnum (*)) xs ys)
                  (optimize (speed 3) (safety 0)))
         (let ((n (length xs)))
           (declare (type fixnum n))
           (cond
             ((<= x (aref xs 0)) (aref ys 0))
             ((>= x (aref xs (1- n))) (aref ys (1- n)))
             (t (loop for i fixnum from 0 below (1- n)
                      for xi fixnum = (aref xs i)
                      for xi+1 fixnum = (aref xs (1+ i))
                      when (and (>= x xi) (<= x xi+1))
                      do (let ((yi (aref ys i))
                               (yi+1 (aref ys (1+ i))))
                           (declare (type fixnum yi yi+1))
                           (return
                             (if (= xi xi+1) yi
                                 (the fixnum
                                      (+ yi (truncate (* (- yi+1 yi) (- x xi))
                                                      (- xi+1 xi)))))))
                      finally (return (aref ys (1- n))))))))

       ;; Interpolate X value at position Y using piecewise boundary
       (interpolate-x (y xs ys)
         (declare (type fixnum y)
                  (type (simple-array fixnum (*)) xs ys)
                  (optimize (speed 3) (safety 0)))
         ;; For x boundaries, we sort by y and interpolate x
         ;; But the arrays are sorted by x, so we need to handle differently
         ;; Actually for left-of-x/right-of-x with piecewise, treat as x=f(y)
         (let ((n (length ys)))
           (declare (type fixnum n))
           (cond
             ((<= y (aref ys 0)) (aref xs 0))
             ((>= y (aref ys (1- n))) (aref xs (1- n)))
             (t (loop for i fixnum from 0 below (1- n)
                      for yi fixnum = (aref ys i)
                      for yi+1 fixnum = (aref ys (1+ i))
                      when (and (>= y yi) (<= y yi+1))
                      do (let ((xi (aref xs i))
                               (xi+1 (aref xs (1+ i))))
                           (declare (type fixnum xi xi+1))
                           (return
                             (if (= yi yi+1) xi
                                 (the fixnum
                                      (+ xi (truncate (* (- xi+1 xi) (- y yi))
                                                      (- yi+1 yi)))))))
                      finally (return (aref xs (1- n))))))))

       ;; Point-in-polygon test (ray casting algorithm)
       (point-in-polygon-p (px py xs ys)
         (declare (type fixnum px py)
                  (type (simple-array fixnum (*)) xs ys)
                  (optimize (speed 3) (safety 0)))
         (let ((n (length xs))
               (inside nil))
           (declare (type fixnum n))
           (loop with j fixnum = (1- n)
                 for i fixnum from 0 below n
                 for xi fixnum = (aref xs i)
                 for yi fixnum = (aref ys i)
                 for xj fixnum = (aref xs j)
                 for yj fixnum = (aref ys j)
                 do (when (and (or (and (<= yi py) (< py yj))
                                   (and (<= yj py) (< py yi)))
                               (< px (+ xi (truncate (* (- xj xi) (- py yi))
                                                     (- yj yi)))))
                      (setf inside (not inside)))
                    (setf j i))
           inside)))

    ;; Parse all boundary specs
    (multiple-value-bind (above-type above-data) (parse-boundary above-y)
      (multiple-value-bind (below-type below-data) (parse-boundary below-y)
        (multiple-value-bind (left-type left-data) (parse-boundary left-of-x)
          (multiple-value-bind (right-type right-data) (parse-boundary right-of-x)
            ;; Parse polygon specs
            (let ((in-poly-xs nil) (in-poly-ys nil)
                  (out-poly-xs nil) (out-poly-ys nil))
              (when in-polygon
                (let ((n (length in-polygon)))
                  (setf in-poly-xs (make-array n :element-type 'fixnum))
                  (setf in-poly-ys (make-array n :element-type 'fixnum))
                  (loop for i from 0 below n
                        for (x y) in in-polygon
                        do (setf (aref in-poly-xs i) (truncate x))
                           (setf (aref in-poly-ys i) (truncate y)))))
              (when out-of-polygon
                (let ((n (length out-of-polygon)))
                  (setf out-poly-xs (make-array n :element-type 'fixnum))
                  (setf out-poly-ys (make-array n :element-type 'fixnum))
                  (loop for i from 0 below n
                        for (x y) in out-of-polygon
                        do (setf (aref out-poly-xs i) (truncate x))
                           (setf (aref out-poly-ys i) (truncate y)))))

              ;; Main pixel loop
              (loop for iy fixnum from 0 below ny do
                (loop for ix fixnum from 0 below nx do
                  (when (or
                         ;; above-y check
                         (and above-type
                              (> iy (ecase above-type
                                      (:constant above-data)
                                      (:piecewise (interpolate-y ix
                                                    (car above-data)
                                                    (cdr above-data))))))
                         ;; below-y check
                         (and below-type
                              (< iy (ecase below-type
                                      (:constant below-data)
                                      (:piecewise (interpolate-y ix
                                                    (car below-data)
                                                    (cdr below-data))))))
                         ;; left-of-x check
                         (and left-type
                              (< ix (ecase left-type
                                      (:constant left-data)
                                      (:piecewise (interpolate-x iy
                                                    (car left-data)
                                                    (cdr left-data))))))
                         ;; right-of-x check
                         (and right-type
                              (> ix (ecase right-type
                                      (:constant right-data)
                                      (:piecewise (interpolate-x iy
                                                    (car right-data)
                                                    (cdr right-data))))))
                         ;; in-polygon check
                         (and in-poly-xs
                              (point-in-polygon-p ix iy in-poly-xs in-poly-ys))
                         ;; out-of-polygon check
                         (and out-poly-xs
                              (not (point-in-polygon-p ix iy out-poly-xs out-poly-ys))))
                    (funcall setter-func ix iy)))))))))))

;; created with Claude
(defun imfill-pixels-by-region (image value
                                &key
                                  above-y below-y
                                  left-of-x right-of-x
                                  in-polygon out-of-polygon)
  "Fill pixels matching region criteria with VALUE.
IMAGE is a 2D single-float array. VALUE is the fill value (typically NaN).
See MAP-PIXELS-BY-REGION for region criteria documentation."
  (declare (type image image)
           (type single-float value))
  (let ((nx (array-dimension image 1))
        (ny (array-dimension image 0)))
    (map-pixels-by-region
     (lambda (ix iy)
       (declare (type fixnum ix iy)
		(optimize (speed 3) (safety 0)))
       (setf (aref image iy ix) value))
     nx ny
     :above-y above-y
     :below-y below-y
     :left-of-x left-of-x
     :right-of-x right-of-x
     :in-polygon in-polygon
     :out-of-polygon out-of-polygon))
  image)



(defun imfill-corner (image ix iy triangle-loc value)
  "Fill a corner of an image with VALUE, defined by index IY on the side,
by IX on the top/bottom, and TRIANGLE-LOC being one of
 :TOP-LEFT :TOP-RIGHT :BOTTOM-LEFT :BOTTOM-RIGHT"
  (declare (type image image)
	   (type imindex ix iy)
	   (type (member :top-left :top-right :bottom-left :bottom-right)
		 triangle-loc)
	   (type single-float value)
	   (optimize speed))
  (let* ((ix0 0) (ix1 0) (iy0 0) (iy1 0)
	 (nx (1- (array-dimension image 1)))
	 (ny (1- (array-dimension image 0)))
	 (dy 0)
	 (ix (max 0 (min ix nx)))
	 (iy (max 0 (min iy ny))))
    
    (declare (type imindex ix0 ix1 iy0 iy1)
	     (type (integer -1 1) dy))

    (cond ((eq triangle-loc :bottom-left)
	   (setf ix0 0
		 iy0 iy
		 ix1 ix
		 iy1 0
		 dy -1))
	  ((eq triangle-loc :top-left)
	   (setf ix0 0
		 iy0 iy
		 ix1 ix
		 iy1 ny
		 dy +1))
	  ((eq triangle-loc :bottom-right)
	   (setf ix0 ix
		 iy0 0
		 ix1 nx
		 iy1 iy
		 dy -1))
	  ((eq triangle-loc :top-right)
	   (setf ix0 ix
		 iy0 ny
		 ix1 nx
		 iy1 iy
		 dy +1)))

    ;; move from ix0 to ix1, computing y, and filling in
    ;; vertical line in direction dy
    (loop
      ;; line has slope y=ax+b
      with a of-type single-float = (/ (float (- iy1 iy0) 0.0)
				       (- ix1 ix0))
      with b of-type single-float = (- iy0 (* a ix0))
      for jx from ix0 to ix1 
      for y of-type (single-float -1e8 1e8) = (+  (* a jx) b)
      for jy0 = (max 0 (min (round y) ny))
      for jy1 = (if (= dy +1) (1+ ny) -1)
      do
	 (loop for jy of-type (signed-byte 28) = jy0 then (+ jy dy)
	       until (= jy jy1)
	       do
		  (setf (aref image jy jx) value)))))
      
	  

(defun imfill-rectangle (image ix0 iy0 ix1 iy1 value)
    (declare (type image image)
	     (type imindex ix0 iy0 ix1 iy1)
	     (type single-float value)
	     (optimize speed))
  "Fill a rectangle from IX0,IY0 to IX1,IY1 with VALUE"
  ;;
  (when (< ix1 ix0) (rotatef ix1 ix0))
  (when (< iy1 iy0) (rotatef iy1 iy0))
  ;;
  (let* ((nx (1- (array-dimension image 1)))
	 (ny (1- (array-dimension image 0)))
	 (ix0 (max 0 ix0))
	 (ix1 (min nx ix1))
	 (iy0 (max 0 iy0))
	 (iy1 (min ny iy1)))
    (loop for iy from iy0 to iy1
	  do (loop for ix from ix0 to ix1
		   do (setf (aref image iy ix) value)))))

(defun imfill-edge (image width side value)
  "Fill a strip of IMAGE of WIDTH pixels, on SIDE
in :TOP BOTTOM :LEFT :RIGHT, with VALUE."
  (declare (type image image)
	   (type (integer 1 #.(expt 2 28)) width)
	   (type single-float value)
	   (type (member :top :bottom :left :right))
	   (optimize speed))
  (let* ((nx (1- (array-dimension image 1)))
	 (ny (1- (array-dimension image 0)))
	 (w (1- width)) ;; counts are from 0 to width -1
	 (ix0 0) (iy0 0) (ix1 0) (iy1 0))

    (cond ((eq side :top)
	   (setf ix0 0
		 ix1 nx
		 iy0 (- ny w)
		 iy1 ny))
	  ((eq side :bottom)
	   (setf ix0 0
		 ix1 nx
		 iy0 0
		 iy1 w))
	  ((eq side :left)
	   (setf ix0 0
		 ix1 w
		 iy0 0
		 iy1 ny))
	  ((eq side :right)
	   (setf ix0 (- nx w)
		 ix1 nx
		 iy0 0
		 iy1 ny)))

    (imfill-rectangle image ix0 iy0 ix1 iy1 value)))
	  
    
(defun imfill-border (image width value)
  "Fill a border of WIDTH of an IMAGE with VALUE."
  (declare (type image image)
	   (type imindex width)
	   (type single-float value))
  (imfill-edge image width :top value)
  (imfill-edge image width :bottom value)
  (imfill-edge image width :left value)
  (imfill-edge image width :right value))


(defun imfill-above/below-line (image ix0 iy0 ix1 iy1 above/below value &key (extend t))
  "Fill above or below a line from (IX0,IY0) to (IX1,IY1) with VALUE,
where above/below is :ABOVE or :BELOW.   The line is extended in X to edges
of the chip if EXTED is true, by default."
  (declare (type image image)
	   (type imindex ix0 iy0 ix1 iy1)
	   (type (member :above :below) above/below)
	   (type single-float value))
  (let* ((nx (1- (array-dimension image 1)))
	 (ny (1- (array-dimension image 0)))
	 (dy (if (eq above/below :above) +1 -1)))
    ;; move from ix0 to ix1, computing y, and filling in
    ;; vertical line in direction dy
    (loop
      ;; line has slope y=ax+b
      with a of-type single-float = (/ (float (- iy1 iy0) 0.0)
				       (- ix1 ix0))
      with b of-type single-float = (- iy0 (* a ix0))
      for jx from (if extend 0 (max ix0 0)) to (if extend nx (min ix1 nx))
      for y of-type (single-float -1e8 1e8) = (+  (* a jx) b)
      for jy0 = (max 0 (min (round y) ny))
      for jy1 = (if (= dy +1) (1+ ny) -1)
      do
	 (loop for jy of-type (signed-byte 28) = jy0 then (+ jy dy)
	       until (= jy jy1)
	       do
		  (setf (aref image jy jx) value)))))


(defun imfill-polygon (image xvec-poly yvec-poly value)
  "Fill a polygonal region in IMAGE with VALUE, with polygon defined
by vertices in single-float arrays XVEC-POLY, YVEC-POLY."
  (declare (type image image)
	   (type (simple-array single-float (*)) xvec-poly yvec-poly)
	   (type single-float value)
	   (optimize speed))

  (when (not (= (length xvec-poly)
		(length yvec-poly)))
    (error "Polygon vectors not of equal length."))
  
  (let* ((nx (1- (array-dimension image 1)))
	 (ny (1- (array-dimension image 0)))
	 (ix0 #.(ash most-positive-fixnum -2))
	 (iy0 #.(ash most-positive-fixnum -2))
	 (ix1 #.(ash most-negative-fixnum -2))
	 (iy1 #.(ash most-negative-fixnum -2)))

    (declare (type fixnum nx ny ix0 iy0 ix1 iy1))

    ;; only look inside points that COULD be in polygon
    (loop for x of-type (float -1e8 1e8) across xvec-poly
	  for y of-type (float -1e8 1e8) across yvec-poly
	  do
	     (setf ix0 (min (floor x) ix0))
	     (setf ix1 (max (ceiling x) ix1))
	     (setf iy0 (min (floor y) iy0))
	     (setf iy1 (max (ceiling y) iy1)))
    
    ;; make sure limits are in bounds
    (setf ix0 (max ix0 0)
	  iy0 (max iy0 0)
	  ix1 (min ix1 nx)
	  iy1 (min iy1 ny))

    (print (list ix0 iy0 ix1 iy1))
    (flet ((is-in-polygon (x y)
	     (declare (type (single-float -1e8 1e8) x y))
	     (loop with inside = nil
		   with nm1 of-type (unsigned-byte 27) = (1- (length xvec-poly))
		   with j of-type (unsigned-byte 27) = nm1
		   for i of-type (unsigned-byte 27) from 0 to nm1
		   do
		      (when (and (or (and (<= (aref yvec-poly i) y) (< y (aref yvec-poly j)))
				     (and (<= (aref yvec-poly j) y) (< y (aref yvec-poly i))))
				 (< x (+ (aref xvec-poly i)
					 (/ (* (- (aref xvec-poly j) 
						  (aref xvec-poly i)) 
					       (- y (aref yvec-poly i)))
					    (- (aref yvec-poly j) (aref yvec-poly i))))))
			(setf inside (not inside)))
		      (setf j i)
		   finally (return inside))))
      ;;
      (loop for iy from iy0 to iy1
	    do (loop for ix from ix0 to ix1
		     when (is-in-polygon (float ix 1.0) (float iy 1.0))
		       do (setf (aref image iy ix) value))))))
    
    
    
	   
    
		 
	   
    
