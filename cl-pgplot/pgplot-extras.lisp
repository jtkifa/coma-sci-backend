

;; Misc extra routines for pgplot - useful higher level functions

(in-package pgplot)


(defun pgplot-encode-float-sci-notation 
    (x ndigits &key (min-exponent 0) (show-plus-sign nil))
  "Encode a number in scientific notation using pgplot escaping
syntax. MIN-EXPONENT is the smallest absolute value exponent that is
expressed in scientific notation. By default it is zero, so all
numbers are expressed in scientific notaton. If SHOW-PLUS-SIGN is T,
then positive numbers have leading plus sign."
  (let* ((expnt (if (zerop x)
		    0
		    (floor (log (abs x) 10))))
	 (psign (if (and show-plus-sign (not (minusp x))) "+" ""))
	 (mant (/ x (expt 10 expnt))))
    (if (>= (abs expnt) min-exponent)
	(format nil "~A~,vF\\(0727)10\\u~D\\d" psign ndigits mant expnt)
	(format nil "~A~,vF" psign ndigits x))))



(defgeneric draw-wedge (p x0 y0 r1 r2 theta1 theta2
			  &key color line-width line-style fill-area-style
			    draw-polygon draw-boundary)
  (:documentation
   "Draw a wedge, meaning a segment of two nested circles, at
X0,YO. Circles are of radii R1 and R2, and extend from degree angle
THETA1 to THETA2, where THETA=0 is the postive X direction.  The wedge
is drawn as an outline (if DRAW-BOUNDARY is T) and as polygon (if
DRAW-POLYGON is T). Options LINE-WIDTH, LINE-STYLE, COLOR are passed
to both the line drawing and the polygon drawing command, and
FILL-AREA-STYLE is passed to the polygon drawing command as in
PGPLOT:POLYGON."))
   
(defmethodL draw-wedge ((p pgplot)
			x0 y0 r1 r2 theta1 theta2 &key (color :default) 
			(line-width nil)
			(line-style nil) 
			(fill-area-style nil)
			(draw-polygon t)
			(draw-boundary t))

  (let* ((deg (float (- theta2 theta1) 1.0))
	 (ri (min r1 r2))
	 (ro (max r1 r2))
	 (x0 (float x0 1.0))
	 (y0 (float y0 1.0))
	 (zero-cent (zerop ri)) ;; is the inner radius zero?
	 (ncirc (max 2 (round (* deg 3)))) ;; 3 line segments per deg
	 (ntot  
	  (if zero-cent
	      (+ 2 ncirc)
	      (+ 1 (* ncirc 2))))
	 (xvec (make-array ntot :element-type 'single-float :initial-element 0.0))
	 (yvec (make-array ntot :element-type 'single-float :initial-element 0.0)))
    (loop 
     with dtheta = (/ deg (1- ncirc))
     for i below ncirc
     for j = (- ntot i 2) then (1- j)
     for theta from theta1 by dtheta
     for cos = (float (cos (* (/ pi 180) theta)) 1.0)
     for sin = (float (sin (* (/ pi 180) theta)) 1.0)
     do 
     (when (not zero-cent) ;; only do 2nd ring if not zero centered
       (setf (aref xvec i) (+ x0 (* ri cos))
	     (aref yvec i) (+ y0 (* ri sin))))

     (setf
      (aref xvec j) (+ x0 (* ro cos))
      (aref yvec j) (+ y0 (* ro sin))))
    ;; 
    ;; if zero-cent, put in the center point
    (when zero-cent
      (setf (aref xvec (- ntot 1)) x0)
      (setf (aref yvec (- ntot 1)) y0))
    ;;
    ;; tie off the end
    (setf (aref xvec (1- ntot)) (aref xvec 0))
    (setf (aref yvec (1- ntot)) (aref yvec 0))
    (when draw-polygon
      (pgplot:polygon p xvec yvec :color color 
		      :line-width line-width :line-style line-style
		      :fill-area-style fill-area-style))
    (when draw-boundary
      (pgplot:connect p xvec yvec :color color :line-width line-width 
		      :line-style line-style))))





(defun make-2d-transform-matrix (&key (xscale 1.0) (yscale 1.0)
				 (rotation 0.0) 
				 (ixref 0) (iyref 0) 
				 (xref 0.0)  (yref 0.0)
				 x0 y0)
  "Make a transformation matrix (a 2x3 array) as used by IMAGE
and CONTOUR.

For the data array A[I,J], the default coordinate system has
X increasing with J, and Y increasing with I, rescaled to fit
exactly within the user coordinates.   If used, the
transformation matrix (TR) changes this to:
  X = TR[0,0] + TR[0,1]*J + TR[0,2]*I
  Y = TR[1,0] + TR[1,1]*J + TR[1,2]*I

The transformation is construced as

  X      X0                            J 
     =       +  ROTATION x SCALING  x
  Y      Y0                            I

The sense of the rotation is +X rotates into +Y, and flips can
be accomplished by setting XSCALE to a negative value.

The additive X0,Y0 part of the transformation is defined so that array
elements IXREF,IYREF map into XREF,YREF.   Alternatively, if X0 and Y0 are specified,
then IXREF,IYREF,XREF,YREF are ignored.
"
  
  (let ((a (make-array '(2 3) :element-type 'single-float :initial-element 0.0))
	(cosr (cos (* (/ pi 180) rotation)))
	(sinr (sin (* (/ pi 180) rotation))))
    ;;
    (setf (aref a 0 1) (float (* xscale cosr) 1.0))
    (setf (aref a 1 1) (float (* xscale sinr) 1.0))
    (setf (aref a 0 2) (float (* yscale sinr -1) 1.0))
    (setf (aref a 1 2) (float (* yscale cosr) 1.0))
    ;;
    (if (and x0 y0)
	;; if both x0 and are specified use them in transform
	(progn
	  (setf (aref a 0 0) (float x0 1.0))
	  (setf (aref a 1 0) (float y0 1.0)))
	;; otherwise build transform using ixraf, iyref, xref, yref
	(progn
	  (setf (aref a 0 0)
		(float (- xref (+ (* (aref a 0 1) ixref)
				  (* (aref a 0 2) iyref)))))
	  (setf (aref a 1 0)
		(float (- yref (+ (* (aref a 1 1) ixref)
				  (* (aref a 1 2) iyref)))))))
    ;;
    a))
    
(defgeneric draw-plot-legend (p x0 y0 legend-list &key x-separation 
				y-separation character-height
				ymax draw-box)
	    (:documentation "Draw a legend in a plot. 

LEGEND-LIST is a list containing the legend lines
  ((\"text\"  :POINT :CIRCLE .. [keyword arguments for PGPLOT:POINTS])
   (\"text\"  :LINE             [keyword arguments for PGPLOT:CONNECT])
   (\"text\"  :RECTANGLE        [keyword arguments for PGPLOT:RECTANGLE])
   ;; overlapping legend types specified as follows, with lists
   (\"text\"  (:POINT        [keyword arguments for PGPLOT:POINT])
              (:LINE         [keyword arguments for PGPLOT:LINE])))

see PGPLOT::*SAMPLE-PLOT-LEGEND* for an example

All coordinates are such that the viewport is remapped to x=[0,1],y=[0,YMAX]
where YMAX is 1 by default, but can be set to the aspect ratio ysize/xsize
for non-square plots.

X-SEPARATION is the spacing between the token (point, line, rectangle) and
the text. 

Y-SEPARATION is the separation between lines of text

when BLANK-FIRST is set, it first blanks the region with BLANK-COLOR,
and then draws the legend."
))


     


(defmethodL draw-plot-legend 
    ((p pgplot) x0 y0 legend-list &key (x-separation 0.10)
     (y-separation 0.07) (character-height 1.2) (ymax 1.0)
     (blank-first nil) (blank-color :background)
     (draw-box nil))
  
  (with-window (p 0 1 0 ymax) 
    (loop 
     with xmin = x0 and xmax = (+ x0 x-separation)
     with ymin = y0 and ymax = y0
     for legend in legend-list
     for y downfrom y0 by y-separation
     for text = (first legend) 
     ;;for type = (second legend)
     ;;for point-symbol = (if (eq type :point) (third legend))
     ;;for keyword-args = (if (eq type :point) (cdddr legend) (cddr legend))
     do 
     (when (not blank-first)
       (pgplot:write-text  p text (+ x0 x-separation) (- y (* 0.2 y-separation))
			   :character-height character-height))
     
     ;; now note the size of the text
     (when (or draw-box blank-first)
       (multiple-value-bind (xb yb)
	   (pgplot:text-bounding-box   p text 
				       :x0 (+ x0 x-separation) 
				       :y0 (- y (* 0.2 y-separation))
				      :character-height character-height)
	 ;(print (list xb yb))
	 (setf xmax (max xmax (aref xb 2)))
	 (setf ymax (max ymax (aref yb 1)))
	 (setf ymin (min ymin (aref yb 0)))))

     ;;
     (flet ((draw-legend-marker (type point-symbol keyword-args)
	      (cond (blank-first  ;; do nothing - this is just the blanking phase
		     nil)
		    ((eq type :point)
		     (apply 'pgplot:points p (+ x0 (* 0.3 x-separation)) 
			    y point-symbol keyword-args))
		    ((eq type :line)
		     (apply 'pgplot:connect p 
			    (vector x0 (+ x0 (* 0.7 x-separation)))  
			    (vector y y)
			    keyword-args))
		    ((eq type :rectangle)
		     (apply 'pgplot:rectangle p   
			    x0 (+ x0 (* 0.7 x-separation)) 
			    (+ y (* 0.3 y-separation))      (- y (* 0.3 y-separation))
			    keyword-args)))))
       (let* ((marker-stuff (cdr legend))
	      (marker-stuff-list 
	       (if (listp (car marker-stuff)) ;; ie, this is a LIST of markers
		   marker-stuff
		   (list marker-stuff)))) ;; else listify the singular
					  ;; marker for the loop
	 (loop for ms in marker-stuff-list
	       for type = (first ms)
	       for point-symbol = (if (eq type :point) (second ms))
	       for keyword-args = (if (eq type :point) (cddr ms) (cdr ms))
	       do
	       (draw-legend-marker type point-symbol keyword-args))))


     ;;
     finally
     (decf xmin (* 0.02 character-height))   (incf xmax (* 0.02 character-height))
     (decf ymin (* 0.02 character-height))   (incf ymax (* 0.02 character-height))
     (cond (blank-first
	    (pgplot:rectangle  p xmin xmax ymin ymax :fill-area-style :solid
			       :color blank-color)
	    (draw-plot-legend p x0 y0 legend-list 
			      :x-separation x-separation :y-separation y-separation
			      :character-height character-height :blank-first nil
			      :blank-color blank-color :draw-box draw-box))
	   (draw-box
	    (pgplot:rectangle  p xmin xmax ymin ymax 
			       :fill-area-style :outline))))))

;; an example plot legend for draw-plot-legend
(defparameter *sample-plot-legend*
     '(("the point" :point :circle-2 :color :red)
       ("the line" :line :color :green :line-style :dotted)
       ("the point-line"
	(:point :circle-2 :color :red)
	(:line :color :red :line-style :dotted))
       ("the blue rectangle" :rectangle :color :blue :fill-area-style
	:solid)
       ("the red rectangle" :rectangle :color :red :fill-area-style
	:cross-hatched)))





(defgeneric write-blanked-text (p text x y
				  &key fjust angle device-coords
				  color character-height font center
				  buffer blanking-color draw-box
				    box-color)
  (:documentation
   "Like WRITE-TEXT, but draw a blanking rectangle beforehand. BUFFER
is a buffer factor for expanding the rectangle, and BLANKING-COLOR is
the color of the rectangle"))
   

(defmethodL write-blanked-text ((p pgplot) (text string) (x real) (y real) &key
				(fjust 0.0) (angle 0.0) (device-coords nil)
				(color nil) (character-height 1.0) (font nil)
				(line-width nil)
				(center nil) ;; are X,Y the center?
				(buffer 0.1) (blanking-color :background)
				(draw-box nil) (box-color :default))
  (if device-coords (multiple-value-setq (x y) (device-to-world p x y)))
  ;;
  ;; adjust x,y if CENTER is set
  (when center
    (multiple-value-bind (xvec yvec)
	(with-pgplot-settings (p :character-height character-height :font font)
	  (text-bounding-box p text :angle angle :fjust fjust))

      ;; decrement position x,y by mean position of bounding box
      (loop for xc across xvec and yc across yvec
	 do  
	   (decf x (* 0.25 xc))
	   (decf y (* 0.25 yc)))))
  (multiple-value-bind (xb yb)
      (with-pgplot-settings (p :character-height character-height :font font)
	(pgplot:text-bounding-box p text :x0 x :y0 y 
					 :angle angle :fjust fjust
					 :font font))

    ;; expand the rectangle - we have to define vectors along the horiz and vert sides
    ;; of the box - corners are l.left, u.left, u.right l.right
    (let* ((f (* buffer character-height)) ;; expansion factor
	   ;; vert vector
	   (xvv (- (aref xb 1) (aref xb 0))) ;; upper left minus bottom left
	   (yvv (- (aref yb 1) (aref yb 0)))
	   (vvlen (sqrt (+ (expt xvv 2) (expt yvv 2))))
	   (xvh (- (aref xb 3) (aref xb 0))) ;; bottom right minus bottom left
	   (yvh (- (aref yb 3) (aref yb 0)))
	   (vhlen (sqrt (+ (expt xvh 2) (expt yvh 2))))
	   (ff (/ (* f character-height) 40.0))) ;; default char is 1/40 max(height,widhth)
      ;; turn them into unit vectors times f
      (setf xvv (* ff (/ xvv vvlen)))
      (setf yvv (* ff (/ yvv vvlen)))
      (setf xvh (* ff (/ xvh vhlen)))
      (setf yvh (* ff (/ yvh vhlen)))
      ;; and expand the corners by these unit vectors
      (loop 
	 for i below 4
	 for hsign across #(-1 -1 +1 +1)
	 for vsign across #(-1 +1 +1 -1)
	 do
	   ;; push corners outward in width
	   (incf (aref xb i) (* hsign xvh))
	   (incf (aref yb i) (* hsign yvh))
	   ;; push corners outward in height
	   (incf (aref xb i) (* vsign xvv))
	   (incf (aref yb i) (* vsign yvv))))
    (pgplot:polygon p xb yb :fill-area-style :solid :color blanking-color)
    (when draw-box
      (pgplot:polygon p xb yb :fill-area-style nil :line-style :solid 
			      :color box-color))
    ;; write-text does with-pgplot-setting
    (pgplot:write-text p text x y :fjust fjust :angle angle
				  :color color
				  :line-width line-width :device-coords NIL
				  :character-height character-height)

    ))


  


(defun %vecmin (v) 
  (loop with xmin = nil
       for x across v
       when (or (not xmin) (< x xmin))
       do (setf xmin x)
       finally (return xmin)))
;;
(defun %vecmax (v) 
  (loop with xmax = nil
       for x across v
       when (or (not xmax) (> x xmax))
       do (setf xmax x)
       finally (return xmax)))



#+nil
(defun bin-2d-data (xvals yvals nx ny &key xmin xmax ymin ymax)
 (error "Use make-2d-density-image instead~%"))
	
  

(defun compute-aspect-ratio-for-panes (nx ny &key
				       (pane-aspect-ratio 1.0)
				       (x-separation 0.05)
				       (y-separation 0.05)
				       (top-pad 0.02)    ;; extra blank space on top
				       (bottom-pad 0.10) ;; on bottom
				       (right-pad 0.03)  ;; on right
				       (left-pad 0.15))  ;; on left

  "Compute the aspect ratio for a plot necessary to have NXxNY panes
which have their own aspect ratio of PANE-ASPECT-RATIO.  Useful for
designing a plot with (for example) square panes.  Return 
 (VALUES ASPECT-RATIO X-SPAN-OF-ONE-PANE"
  (let* ((s (/ (- 1.0 (+ left-pad right-pad (* (1- nx) x-separation))) nx))
	 (a
	  (/ (* pane-aspect-ratio ny s)
	     (-  1.0 (+ top-pad bottom-pad (* (1- ny) y-separation))))))
    (values a s)))



  


(defun make-2d-density-image (xseq yseq &key (nx 100) (ny 100) 
			      xmin xmax ymin ymax)
  "Given sequences of points XSEQ and YSEQ, bin them into a float
array of size NYxNX, suitable for plotting as an image or a contour
plot.

XMIN XMAX YMIN YMAX, if not given, default to the extrema of
the input arrays.

Return (VALUES ARRAY XMIN XMAX YMIN YMAX N-INSIDE N-OUTSIDE)"
  (declare (type (unsigned-byte 20) nx ny)
	   (type (or null real) xmin xmax ymin ymax)
	   (type (or array list) xseq yseq))
  ;;
  (let* ((xvec (to-dbl-float-vec xseq))
	 (yvec (to-dbl-float-vec yseq))
	 (xmin (or xmin (%vecmin xseq)))
	 (xmax (or xmax (%vecmax xseq)))
	 (ymin (or ymin (%vecmin yseq)))
	 (ymax (or ymax (%vecmax yseq))))
    ;;
    (when (not (= (length xvec) (length yvec)))
      (error "XSEQ and YSEQ have a different number of elements"))
    ;;
    (loop 
       with n-inside = 0 and n-outside = 1
       with nx-1 = (1- nx) and ny-1 = (1- ny)
       with a of-type (simple-array single-float (* *)) 
	 = (make-array (list ny nx) :element-type 'single-float :initial-element 0.0)
       for x of-type double-float across xvec 
       for y of-type double-float across yvec
       for ix = (round (* nx-1 (/ (- x xmin) (- xmax xmin))))
       for iy = (round (* ny-1 (/ (- y ymin) (- ymax ymin))))
       do 
	 (cond ((and (<= 0 ix nx-1) (<= 0 iy ny-1))
		(incf (aref a iy ix) 1.0)
		(incf n-inside 1))
	       (t
		(incf n-outside 1)))
       finally
	 (return (values a xmin xmax ymin ymax n-inside n-outside)))))
	   

  

(defun generate-linear-tick-intervals (xstart xend)
"Create appropriately chosen intervals for small and large ticks between 
 xstart and xend.

 Return (values big-tick-list small-tick-list) where each tick in the
 list is a list (tick-value v-value ndecimals) where v-value is the
 fraction of the line from [xstart,xend], and ndecimals is the number
 of decimals points that the number should be printed"
  (if (= xstart xend)
      ;; return empty lists 
      (values nil nil 0)
      ;; else do the math
      (let* ((swap (> xstart xend))
	     (x1 (if swap xend xstart))
	     (x2 (if swap xstart xend))
	     (dx (* 1d0 (- x2 x1)))
	     (absdx (abs dx))
	     (ceiling-logabsdx (ceiling (log absdx 10)))
	     ;; scale is the nearest power of 10 to dx
	     (scale (expt 10.0 ceiling-logabsdx))
	     ;; how many decimals ticks have to be printed at (if not
	     ;; scientific notation)
	     (ndecimal (cond ((> ceiling-logabsdx 0) 0)
			     (t (+ 1 (- ceiling-logabsdx)))))			 
	     ;; span is the span on a 0-1 range
	     (span (/ dx scale)))
	(multiple-value-bind (big small)
	    (cond 
	      ((<= span 0.3)
	       (incf ndecimal 2) ;; this is ugly
	       (values (* scale 0.05) (* scale 0.025)))
	      ((<= span 0.5) 
	       (incf ndecimal 1) ;; this is ugly too 
	       (values (* scale 0.1) (* scale 0.05)))
	      ((<= span 1.1) (values (* scale 0.2) (* scale 0.1)))
	      (t (error "Span=~A is not in [0,1]. This should not happen" span)))
	  ;; xbot is the smallest multiple of 'big' that fits into x1
	  (let* ((xbot (* big (floor x1 big)))
		 (big-ticks
		  (loop for x = xbot then (+ x big)
			for vv = (/ (- x x1) dx)
			for v = (if swap (- 1d0 vv) vv)
			while (<= x x2)
			when (<= x1 x x2) collect (list x v ndecimal)))
		 ;; small-ticks depends on assumption that small=big/2
		 (small-ticks
		  (loop for x = (+ xbot small) then (+ x big)
			for vv = (/ (- x x1) dx)
			for v = (if swap (- 1d0 vv) vv)
			while (<= x x2)
			when (<= x1 x x2) collect (list x v ndecimal))))
	    (when swap
	      (setf big-ticks (nreverse big-ticks))
	      (setf small-ticks (nreverse small-ticks)))
	    ;;
	    (values big-ticks small-ticks ndecimal))))))
	    

(defun generate-log-tick-intervals (xstart xend)
  "Generate logarithmic tick intervals; see GENERATE-LINEAR-TICK-INTERVALS."
  (if (or (= xstart xend)
	  (not (plusp xstart))
	  (not (plusp xend)))
       ;; return empty lists 
       (values nil nil 0)
      ;; else do the math
      (let* ((swap (> xstart xend))
	     (x1 (if swap xend xstart))
	     (x2 (if swap xstart xend))
	     (startlog (floor (log (* 1d0 x1) 10d0)))
	     (big-ticks nil)
	     (small-ticks nil)
	     (xbot (expt 10d0 startlog)))
	(loop 
	  for xx = xbot then  (* x 10)
	  for x = (if (> xx 1) (round xx) xx) ;; an integer, if possible
	  for ndecimal = 1
	  for v = 1.0
	  until (> xx x2)
	  do (push (list x v ndecimal)  big-ticks)
	     (loop for i from 2 to 9
		   for y = (* i x)
		   for ndecimal = 1
		   for v = 1.0
		   until (> y x2)
		   do (push (list y v ndecimal) small-ticks)))
	;;
	(when (not swap) ;; push already reversed them
	   (setf big-ticks (nreverse big-ticks))
	   (setf small-ticks (nreverse small-ticks)))
	;;
	(values big-ticks small-ticks))))
		   
