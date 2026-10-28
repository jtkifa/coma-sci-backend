;; make some colormaps, including Moreland's 'diverging' colormaps and yellow-blue

(in-package pgplot)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; for 'Diverging' color maps, see
;; "Diverging Color Maps for Scientific Visualization"
;; Kenneth Moreland, Sandia Labs
;; http://www.cs.unm.edu/~kmorel/documents/ColorMaps/ColorMapsExpanded.pdf
;;
;; these are colormaps in CIELAB color space that go from COLOR1 to
;; white to COLOR2, satisfying several useful properties described in
;; the paper

(defpackage pgplot-diverging-colormap 
  (:use :cl)
  (:export #:interpolate-color))

(in-package pgplot-diverging-colormap)

(defparameter *xyzrgb-matrix* ;; take rgb by xyz as defined in Eq2
  (make-array '(3 3) :element-type 'single-float
	      :initial-contents 
	      '((0.4124 0.2126 0.0193)
		(0.3576 0.7152 0.1192)
		(0.1805 0.0722 0.9505))))

(defparameter *rgbxyz-matrix* ;; the inverse of above
    (make-array 
     '(3 3) :element-type 'single-float
     :initial-contents 
     '((3.240625477320054 -0.9689307147293196 0.05571012044551063)
       (-1.537207972210319 1.8757560608852413 -0.2040210505984867)
       (-0.498628598698248 0.041517523842953985 1.0569959422543882))))

(flet ((%3x3-matrix-multiply (mat a b c)
	 (declare (type (simple-array single-float (3 3)) mat)
		  (type single-float a b c))
	 (loop 
	    with x = 0.0 and y = 0.0 and z = 0.0
	    for ix below 3 
	    for w = (cond ((= ix 0) a)  ((= ix 1) b)  ((= ix 2) c) );;YUCK
	    do 
	      ;; note that this is pre-mult according to equation 2
 	      (incf x (* (aref mat ix 0) w))
	      (incf y (* (aref mat ix 1) w))
	      (incf z (* (aref mat ix 2) w))
	    finally (return (values x y z)))))
  ;;
  (defun rgb-to-xyz (r g b) (%3x3-matrix-multiply *xyzrgb-matrix* r g b))
  (defun xyz-to-rgb (x y z) (%3x3-matrix-multiply *rgbxyz-matrix* x y z)))


;; equation 3 of paper       
;; rw,gw,bw are a reference white value
(defun rgb-to-lab (r g b &key (rw 0.8) (gw 0.8) (bw 0.8))
  (multiple-value-bind (xn yn zn)
      (rgb-to-xyz rw gw bw)
    (multiple-value-bind (x y z) (rgb-to-xyz r g b)
      (flet ((f (x)
	       (if (> x 0.008856) 
		   (expt x 0.3333) 
		   (+ (* 7.787 x) 0.13793103))))
	(let* ((fx (f (/ x xn)))
	       (fy (f (/ y yn)))
	       (fz (f (/ z zn)))
	       (L (* 116 (- fy  0.13793103)))
	       (a (* 500 (- fx fy)))
	       (bb (* 200 (- fy fz))))
	  (values l a bb))))))


(defun lab-to-rgb (l a bb &key (rw 0.8) (gw 0.8) (bw 0.8))
  (multiple-value-bind (xn yn zn)
      (rgb-to-xyz rw gw bw)
    (flet ((finv (x) ;; inverse of f above
	     (if (> x 0.20689271) 
		 (expt x 3) 
		 (+ (/ (- x 0.13793103) 7.787)))))
      ;;
      (let* ((fy (/ (+ l 16.0) 116))
	     (fx (+ (/ a 500.0) fy))
	     (fz (- fy (/ bb 200.0)))
	     (x  (* xn (finv fx)))
	     (y  (* yn (finv fy)))
	     (z  (* zn (finv fz))))
	(xyz-to-rgb x y z)))))



;; equation 10 of paper
(defun lab-to-msh (l a b)
  (let* ((m (sqrt (+ (* l l) (* a a) (* b b))))
	 (s (acos (/ l m)))
;	 (h (atan (/ b a))))
	 (h (atan  b a)))
    (values m s h)))
;;
(defun msh-to-lab (m s h)
  (values (* m (cos s))
	  (* m (sin s) (cos h))
	  (* m (sin s) (sin h))))
  
(defun rgb-to-msh (r g b)
  (multiple-value-bind (l a bb)
      (rgb-to-lab r g b)
    (lab-to-msh l a bb)))
;;
(defun msh-to-rgb (m s h)
  (multiple-value-bind (l a bb)
      (msh-to-lab m s h)
    (lab-to-rgb l a bb)))

;; radian difference between two angles
(defun %raddiff (h1 h2)
  (min (abs (- h1 h2))
       (abs (- (+ h1 +6.2831855) h2))
       (abs (- (+ h1 -6.2831855) h2))))

(defun adjust-hue (ms ss hs mu)
  (if (>= ms mu)
      hs
      (progn
	(let ((hspin (/ (* ss (sqrt (- (* mu mu) (* ms ms))))
			(* ms (sin ss)))))
	  (if (> hs (/ 3.141592654 -3))
	      (+ hs hspin)
	      (- hs hspin))))))

;; fig 13 of paper - r,g,b are 0...255
(defun interpolate-color (r1 g1 b1 r2 g2 b2 interp)
  (let (m1 s1 h1 m2 s2 h2 mmid smid hmid)
    (multiple-value-setq (m1 s1 h1) (rgb-to-msh r1 g1 b1))
    (multiple-value-setq (m2 s2 h2) (rgb-to-msh r2 g2 b2))
    ;; if points are saturated and distinct, place white in middle
    (if (and (> s1 0.05) (> s2 0.05) (> (%raddiff h1 h2) (/ pi 3)))
	(progn
	  (setf mmid (max m1 m2 88.0))
	  (if (< interp 0.5)
	      (progn
		(setf m2 mmid)
		(setf s2 0.0)
		(setf h2 0.0)
		(setf interp (* 2 interp)))
	      (progn
		(setf m1 mmid)
		(setf s1 0.0)
		(setf h1 0.0)
		(setf interp (- (* 2 interp) 1.0))))))
	  ;;
	  ;; adjust hue of unsaturated colors
	(if (and (< s1 0.05) (> s2 0.05))
	    (setf h1 (adjust-hue m2 s2 h2 m1))
	    (if (and (< s2 0.05) (> s1 0.05))
		(setf h2 (adjust-hue m1 s1 h1 m2))))
	  ;; linear interpolation on adjusted control points
	(setf mmid     (+ (* (- 1.0 interp) m1) (* interp m2))
	      smid     (+ (* (- 1.0 interp) s1) (* interp s2))
	      hmid     (+ (* (- 1.0 interp) h1) (* interp h2)))
	;;
	;; back to rgb space
	(msh-to-rgb mmid smid hmid)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; end of pgplot-diverging-colormap package
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;		       
(in-package pgplot)


  



(defunL insert-diverging-colormap (p &key 
				   (name :custom)
				   (rgb1 #(0.0 0.2 0.7))
				   (rgb2 #(0.6 0.03 0.01))
				   ;; replace COLOR and HEAT
				   (first-color-is-background nil)
				   (imin 32) (imax 99))
				   "Insert a 'diverging' colormap spanning RGB1 to RGB2 
see  
  'Diverging Color Maps for Scientific Visualization'
    Kenneth Moreland, Sandia Labs
  http://www.cs.unm.edu/~kmorel/documents/ColorMaps/ColorMapsExpanded.pdf

The indices taken are IMIN to IMAX, by default cloobbering the HEAT and COLOR
maps.

If FIRST-COLOR-IS-BACKGROUND is set, then the first color of the
range is set to the background of the plot, for the purpose of creating  
a blanked image with null values."
  (modify-colormap
   p imin imax name
   :first-color-is-background first-color-is-background
   :color-function
   (lambda (x)
     (multiple-value-bind (r g b)
	 (pgplot-diverging-colormap:interpolate-color 
	  (* 255 (aref rgb1 0)) (* 255 (aref rgb1 1)) (* 255 (aref rgb1 2))
	  (* 255 (aref rgb2 0)) (* 255 (aref rgb2 1)) (* 255 (aref rgb2 2))
	  x)
       (values
	(/ (max 0.0 (min r 255.0)) 255.0)
	(/ (max 0.0 (min g 255.0)) 255.0)
	(/ (max 0.0 (min b 255.0)) 255.0))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defunL insert-yellow-blue-colormap (p &key 
				    (name :custom)
				     ;; replace COLOR and HEAT
				     (first-color-is-background nil)
				     (imin 32) (imax 99))
  "Insert a yellow-to-blue colormap.
The indices taken are IMIN to IMAX, by default cloobbering the HEAT
and COLOR maps.

If FIRST-COLOR-IS-BACKGROUND is set, then the first color of the
range is set to the background of the plot, for the purpose of creating  
a blanked image with null values."

  (modify-colormap p imin imax name
		   :first-color-is-background first-color-is-background
		   :color-function
		   (lambda (x)
		     (let* ((f (exp (- (/ x 0.7))))
			    (r f)
			    (g f)
			    (b (- 1 f)))
		       (values r g b)))))




;; 'heat' colomap rising from black to red to yellow to white,
;;  made up of 3 successive ramps of red,green,blue
#+nil 
(defunL insert-heat-colormap-component (cm imin imax name
					&key
					(first-color-is-background nil))
  (flet ((func (x)
	       (declare (type float x))
	       (values
		(min 1.0 (max 0.0 (* 3.0 x)))
		(min 1.0 (max 0.0 (* 3.0 (- x 0.3))))
		(min 1.0 (max 0.0 (* 3.0 (- x 0.6)))))))
    (insert-colormap-component
     cm imin imax name #'func
     :first-color-is-background first-color-is-background)))


(defunL insert-heat-colormap (p
			      &key
			      (first-color-is-background nil)
			      (imin 16) (imax 99))
  (flet ((heat-func (x)
	   (declare (type float x))
	   (values
		(min 1.0 (max 0.0 (* 3.0 x)))
		(min 1.0 (max 0.0 (* 3.0 (- x 0.3))))
		(min 1.0 (max 0.0 (* 3.0 (- x 0.6)))))))
    (modify-colormap
     p imin imax :heat
     :first-color-is-background first-color-is-background
     :color-function #'heat-func)))


(defunl insert-rainbow-colormap (p
				&key
				(first-color-is-background nil)
				(imin 16) (imax 99))
  (labels ((gauss (x x0 sigma)
	     (declare (type single-float x x0 sigma))
	     (exp (- (* 0.5 (expt (/ (- x x0) sigma) 2)))))
	   (rainbow-func (x)
	     (declare (type float x))
	     (values (gauss x 0.0 0.40)
		     (gauss x 0.5 0.33)
		     (gauss x 1.0 0.40))))
    (modify-colormap
     p imin imax :rainbow
     :first-color-is-background first-color-is-background
     :color-function #'rainbow-func)))
				


;; linearly rising grayscale map
#+nil
(defunL insert-gray-colormap-component (cm imin imax name
					&key
					(first-color-is-background nil))
  (flet ((func (x)
	       (declare (type float x))
	       (values x x x)))
    (insert-colormap-component
     cm imin imax name #'func
     :first-color-is-background first-color-is-background)))



(defunL insert-gray-colormap (p
			      &key
			      (first-color-is-background nil)
			      (imin 16) (imax 99))
  "Insert GRAY colormap by default using all non-plotting indices (16
to 99) to allow good grayscale plotting"
  (modify-colormap p imin imax :gray
		   :first-color-is-background first-color-is-background
		   :color-function
		   (lambda (x)
		     (declare (type float x))
		     (values x x x))))


(defunl insert-viridis-family-colormap (p &key (colormap-name :viridis)
					(first-color-is-background nil)
					(imin 32) (imax 99))
  "Insert a viridis type colormap into colormap indices IMIN to IMAX.
   :COLORMAP-NAME can be one of (:VIRIDIS :PLASMA :MAGMA :INFERNO)"
  (when (not (member colormap-name '(:VIRIDIS :PLASMA :MAGMA :INFERNO)))
    (error "COLORMAP-NAME must be one of :VIRIDIS :PLASMA :MAGMA :INFERNO"))
  (let ((cvec (case colormap-name
		(:viridis *viridis-colormap*)
		(:plasma *plasma-colormap*)
		(:magma *magma-colormap*)
		(:inferno *inferno-colormap*))))

    (modify-colormap p imin imax colormap-name
		     :first-color-is-background first-color-is-background
		     :color-function
		     (lambda (x)
		       (interpolate-viridis cvec x)))))
		     
    
  
(defunL insert-colormap (p colormap-name
			 &key 
			 (imin 16) (imax 99)
			 (first-color-is-background nil)
			 (make-default t)
			 ;; for :DIVERGING only
			 (rgb1 #(0.0 0.2 0.7))
			 (rgb2 #(0.6 0.03 0.01)))
  
  (declare (type (member colormap-name :viridis :plasma :magma :inferno
				       :gray :heat :yellow-blue :diverging
				       :rainbow)
		 colormap-name))
  (cond
    ;;
    ((member colormap-name '(:viridis :plasma :magma :inferno))
     (insert-viridis-family-colormap
      p
      :colormap-name colormap-name
      :first-color-is-background first-color-is-background
      :imin imin :imax imax))
    ;;
    ((eq colormap-name :gray)
     (insert-gray-colormap
      p
      :first-color-is-background first-color-is-background
      :imin imin :imax imax))
    ;;
    ((eq colormap-name :heat)
     (insert-heat-colormap
      p
      :first-color-is-background first-color-is-background
      :imin imin :imax imax))
    ;;
    ((eq colormap-name :yellow-blue)
     (insert-yellow-blue-colormap
      p
      :first-color-is-background first-color-is-background
      :imin imin :imax imax))
    ;;
    ((eq colormap-name :diverging)
     (insert-diverging-colormap
      p
      :name :diverging
      :rgb1 rgb1 :rgb2 rgb2
      :first-color-is-background first-color-is-background
      :imin imin :imax imax))
    ;;
    ((eq colormap-name :rainbow)
     (insert-rainbow-colormap
      p
      :first-color-is-background first-color-is-background
      :imin imin :imax imax)))
  
  (when make-default
    (setf (colormap-default-range (pgplot-colormap p)) colormap-name)))
				   
