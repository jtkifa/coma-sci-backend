
;; FIXME - maybe we should move WCS stuff to separate package

#|

Note that FITS has pixels starting at (1,1) not (0,0) so that
all pixel values must have -1 added to them to correspond to
array indices.

This has been verified using xy2sky program, which conforms to
FITS 1-based standard.

The standard reference is Calabretta and Greisen, A&A, 2002, 395 1077

Basic principle of 2d transformations

1) pixels are converted to x,y (in degrees) using CRPIXi, and CDmatrix
2) x,y are converted to Rtheta, phi via trivial polar transformation
3) Rtheta is converted to theta via projection method (eg TAN, SIN)
4) theta,phi are converted to RA,Dec via spherical transformation

Important difference: we redefined C&G's theta as PSI=90-theta so that
small angles work numerically and we can apply Haversine formula.
Otherwise, roundoff errors would be a problem for the small angles
usually of interest.

PROBLEM - search for word MYSTERY - we have to set the local angle PHI to
-PHI for the conversions to work and match conventional libraries.
 
|#





(in-package wcs)



(declaim (#+cmu ext:maybe-inline
	  #+sbcl sb-ext:maybe-inline
	  #-(or cmu sbcl) inline
		wcs-convert-pix-xy-to-world-xy
		wcs-convert-world-xy-to-pix-xy
		wcs-convert-ra-dec-to-pix-xy-tan
		wcs-convert-ra-dec-to-pix-xy-sin
		wcs-convert-pix-xy-to-ra-dec-tan
		wcs-convert-pix-xy-to-ra-dec-tan-tpv
		wcs-convert-pix-xy-to-ra-dec-sin
		wcs-convert-ra-dec-to-pix-xy
		wcs-convert-pix-xy-to-ra-dec 
		wcs-convert-ra-dec-to-world-xy
		wcs-convert-p-to-x
		wcs-convert-x-to-p
		))

(declaim (inline psi-phi--to--ra-dec ra-dec--to--psi-phi
		 xy--to--ra-dec ra-dec--to--xy))
	     


(defun make-tan-proj-wcs-from-field-info
    (ra0 dec0 position-angle crpix1 crpix2 
     arcsec/pix-x arcsec/pix-y equinox)
  "given information about chip scale, position, and orientation,
create a WCS tan projection from it - assume square pixels"
  (let* ((crval1 (float ra0 1d0))
	 (crval2 (float dec0 1d0))
	 (crota1  (float position-angle 1d0))
	 (cdelt1 (/ (float arcsec/pix-x 1d0) 3600d0))
	 (cdelt2 (/ (float arcsec/pix-y 1d0) 3600d0))
	 (cos (cos (* crota1 0.017453292519943295d0)))
	 (sin (sin (* crota1 0.017453292519943295d0)))
	 (cd1_1 (* cos cdelt1))
	 (cd1_2 (* -1d0 sin cdelt2))
	 (cd2_1 (* sin cdelt1))
	 (cd2_2 (* cos cdelt2)))
    (make-wcs-radec-tan
     :crval1 (* 1d0 crval1)
     :crval2 (* 1d0 crval2)
     :crpix1 (* 1d0 crpix1)
     :crpix2 (* 1d0 crpix2)
     :cd1_1  (* 1d0 cd1_1)
     :cd1_2  (* 1d0 cd1_2)
     :cd2_1  (* 1d0 cd2_1)
     :cd2_2  (* 1d0 cd2_2)
     :equinox (* 1d0 equinox))))

	




(defmacro %2x2-lin-solve-macro (a11 a12 a21 a22 b1 b2) ;; macro to solve 2x2 linear system
  `(let ((det (- (* ,a11 ,a22) (* ,a12 ,a21))))
     (values
      (/ (- (* ,a22 ,b1) (* ,a12 ,b2)) det)
      (/ (- (* ,a11 ,b2) (* ,a21 ,b1)) det))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; conversion between RA,DEC and local angular coordinates
;; equation 2 of of C&B 2002 -  phi0=0
;; convert local coords psi, phi to ra,dec given projection center a0,d0
;; all input angles in radians, but output in degrees
(defun psi-phi--to--ra-dec (psi phi a0 d0) ;; return (values psi phi)
  (declare (type double-float psi phi a0 d0))
  (setf phi (- phi)) ;; MYSTERY - see note above
  (let ((sinth (sin psi))
	(costh (cos psi))
	(cosph (cos phi))
	(sinph (sin phi))	  
	(cosd0 (cos (* (/ pi 180) d0)))
	(sind0 (sin (* (/ pi 180) d0))))
	
  (values
   ;;
   ;; ra
   (+ a0
      (* (/ 180 pi)
	 (atan (- (* sinth sinph))
	       (- (* costh cosd0) (* sinth sind0 cosph)))))
   ;; dec
   (* (/ 180 pi)
      (asin (the (double-float -1d0 1d0)
	      (+ (* costh sind0)
		 (* sinth cosd0 cosph))))))))



;; return radian angle between points using haversine rule - expensive, but accurate
;; for small angles
	

;; equation 5 of C&B 2002 - phi0=0
;; inputs are in degrees, outputs are in radians
(defun ra-dec--to--psi-phi (a d a0 d0)
  (declare (type double-float a d a0 d0))
  (let* ((dd (* (/ pi 180) d))
	 (aa (* (/ pi 180) a))
	 (dd0 (* (/ pi 180) d0))
	 (aa0 (* (/ pi 180) a0))
	 (cosd (cos dd))
	 (sind (sin dd))
	 (cosd0 (cos dd0))
	 (sind0 (sin dd0))
	 (da (- aa aa0))
	 (dd (- dd dd0))
	 (cosa* (cos da))
	 (sina* (sin da))
	 ;; for haversine
	 (a (+ (expt (sin (/ dd 2)) 2)
	       (* cosd cosd0 (expt (sin (/ da 2)) 2))))
	 (psi (* 2 (atan (sqrt a) (sqrt (- 1d0 a))))))
    (declare (type double-float dd aa dd0 aa0 cosd sind cosd0 sind0 da dd cosa* sina* psi)
	     (type (double-float 0d0 1d0) a))    ;;
    (values 
     ;; psi - note reversed definition PSI=PI/2-Theta
     psi
     ;; phi
     (- ;; minus is MYSTERY - see note above
      (atan (- (* cosd sina*))
	    (- (* sind cosd0) (* cosd sind0 cosa*)))))))
     ;;


 
;; convert ra,dec into a local x,y
(defun ra-dec--to--xy (a d a0 d0)
  (declare (type double-float a d a0 d0))
  (multiple-value-bind (psi phi)
      (ra-dec--to--psi-phi a d a0 d0)
    (values (* psi (sin phi)) ;; note flipped sin/cos for x/y
	    (* psi (cos phi)))))
  
   



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 #+nil 
(progn
  (defun test-psi-phi-inverse (a d a0 d0)
    (multiple-value-bind (psi phi)
	(ra-dec--to--psi-phi a d a0 d0)
      (format t "Psi=~A  phi=~A~%" psi phi)
      (psi-phi--to--ra-dec psi phi a0 d0)))

  
  (defun test-xy-inverse (a d a0 d0)
    (multiple-value-bind (x y)
	(ra-dec--to--xy a d a0 d0)
      (xy--to--ra-dec x y a0 d0)))
) 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; not needed
;; (declaim (inline %compute-conversion-for-units))
;; (defun %compute-conversion-for-units (units)
;;   (cond ((eq units :natural) #.(/ pi 180))
;; 	((eq units :degrees) 1d0)
;; 	((eq units :arcmin)  60d0)
;; 	((eq units :arcsec)  3600d0)
;; 	(t
;; 	 (error "Units ~A unknown - should be :natural, :degrees, :arcmin, :arcsec" units))))

;; OK
(defun wcs-convert-pix-xy-to-world-xy (wcs px py)
  "Convert pixel px,py into x,y units (degrees).
Takes (wcs-2d-p1 wcs) and (wcs-2d-p2 wcs) using the CD matrix."
  (declare (type wcs-2d wcs) 
	   (type double-float px py)) 
  (let* ((rx (- px (wcs-2d-crpix1 wcs)))
	 (ry (- py (wcs-2d-crpix2 wcs)))
	 (x (+ (* (wcs-2d-cd1_1 wcs) rx)
	       (* (wcs-2d-cd1_2 wcs) ry)))
	 (y (+ (* (wcs-2d-cd2_1 wcs) rx)
	       (* (wcs-2d-cd2_2 wcs) ry))))
    (values x y)))


;; OK
;; given world x,y in wcs, convert it to pixel px,py using inverse linear transform
(defun wcs-convert-world-xy-to-pix-xy  (wcs x y)
  "Convert world xy back to pixel xy"
  (declare (type wcs-2d wcs)
	   (type double-float x y)
	   (optimize speed))
  (let ((px0 (wcs-2d-crpix1 wcs))
	(py0 (wcs-2d-crpix2 wcs))
	(cd1_1 (wcs-2d-cd1_1 wcs))
	(cd1_2 (wcs-2d-cd1_2 wcs))
	(cd2_1 (wcs-2d-cd2_1 wcs))
	(cd2_2 (wcs-2d-cd2_2 wcs)))
    (declare (type double-float px0 py0 cd1_1 cd1_2 cd2_1 cd2_2))
    (multiple-value-bind (xp yp)
	(%2x2-lin-solve-macro cd1_1 cd1_2 cd2_1 cd2_2 x y)
      (values (+ xp px0)
	      (+ yp py0)))))





(defun wcs-convert-ra-dec-to-pix-xy-tan (wcs ra dec)
"Convert ra and dec back to xy sky projection coordinates."
  (declare (type wcs-radec-tan wcs)
	   (type double-float ra dec)
	   (optimize (speed 3) (safety 1)))
  (multiple-value-bind (psi phi) ;; in radians
      (ra-dec--to--psi-phi ra dec (wcs-2d-crval1 wcs) (wcs-2d-crval2 wcs))
    (let* ((rpsi (* (/ 180 pi) (tan psi)))
	   (x (* (sin phi) rpsi)) 
	   (y (* (cos phi) rpsi)))
      (wcs-convert-world-xy-to-pix-xy wcs x y))))
    
(defun wcs-convert-pix-xy-to-ra-dec-tan (wcs xp yp)
  (declare (type wcs-radec-tan wcs)
	   (type double-float xp yp)
	   (optimize (speed 3) (safety 1)))
  (multiple-value-bind (x y)
      (wcs-convert-pix-xy-to-world-xy wcs xp yp)
    (let* ((rpsi (sqrt (+ (* x x) (* y y)))) ;; units are deg
	   (phi (atan x y))
	   (psi (atan (* (/ pi 180) rpsi))))
      (psi-phi--to--ra-dec psi phi (wcs-2d-crval1 wcs) (wcs-2d-crval2 wcs)))))
  


(defun wcs-convert-ra-dec-to-pix-xy-sin (wcs ra dec)
"Convert ra and dec back to xy sky projection coordinates."
  (declare (type wcs-radec-sin wcs)
	   (type double-float ra dec)
	   (optimize (speed 3) (safety 1)))
  (multiple-value-bind (psi phi) ;; in radians
      (ra-dec--to--psi-phi ra dec (wcs-2d-crval1 wcs) (wcs-2d-crval2 wcs))
    (let* ((rpsi (* (/ 180 pi) (sin psi)))
	   (x (* (sin phi) rpsi))
	   (y (* (cos phi) rpsi)))
      (wcs-convert-world-xy-to-pix-xy wcs x y))))
    
(defun wcs-convert-pix-xy-to-ra-dec-sin (wcs xp yp)
  (declare (type wcs-radec-sin wcs)
	   (type double-float xp yp)
	   (optimize (speed 3) (safety 1)))
  (multiple-value-bind (x y)
      (wcs-convert-pix-xy-to-world-xy wcs xp yp)
    (let* ((rpsi (sqrt (+ (* x x) (* y y)))) ;; units are deg
	   (phi (atan x y))
	   (psi (asin (the (double-float -1d0 1d0) (* (/ pi 180) rpsi)))))
      (psi-phi--to--ra-dec psi phi (wcs-2d-crval1 wcs) (wcs-2d-crval2 wcs)))))


;; this is verified to work, giving same answer as wcslib
(defun wcs-convert-pix-xy-to-ra-dec-tan-tpv (wcs xp yp)
  (declare (type wcs-radec-tan wcs)
	   (type double-float xp yp)
	   (optimize (speed 3) (safety 1)))
  (multiple-value-bind (x0 y0)
      (wcs-convert-pix-xy-to-world-xy wcs xp yp)
    (multiple-value-bind (x y)
	(do-pv-style-correction
	  *tpv-correction-coeff* x0 y0
	  (wcs-radec-tan-tpv-pv1vec wcs)
	  (wcs-radec-tan-tpv-pv2vec wcs)
	  :use-r t) ;; this correction has r term
      (declare (type double-float x y))
      (let* ((rpsi (sqrt (the (double-float 0d0)  (+ (* x x) (* y y))))) ;; units are deg
	     (phi (atan x y))
	     (psi (atan (* (/ pi 180) rpsi))))
	(psi-phi--to--ra-dec psi phi (wcs-2d-crval1 wcs) (wcs-2d-crval2 wcs))))))


;; this has been tested for one sensible tan-tpv wcs with a few hundred thousand
;; random points.  It does not seem sensitive to the starting point obtained
;; below, which assumes a guess 
(defun wcs-convert-ra-dec-to-pix-xy-tan-tpv (wcs ra dec)
  (declare (type wcs-radec-tan-tpv wcs)
	   (type double-float ra dec)
	   (optimize (speed 3) (safety 1)))
  ;; starting pixels assume that it's a simple TAN, then we iteratively fix it
  (multiple-value-bind (xp-guess yp-guess)
      (wcs-convert-ra-dec-to-pix-xy-tan wcs ra dec)
    (iteratively-solve-radec-to-pix wcs ra dec xp-guess yp-guess
				    #'wcs-convert-pix-xy-to-ra-dec-tan-tpv)))
    


#+nil
(progn
  ;; tests for inversion
  (defparameter *wcs-tan* (wcs:make-wcs-radec-tan 
			   :crpix1 100d0
			   :crpix2 200d0
			   :crval1 11d0 :crval2 30d0
			   :cd1_1 5d-5       :cd1_1 -5d-5 
			   :cd1_2 1d-6  :cd2_1 1d-6))

  (defun test-tan-proj-inverse (&key (ra 11.1111d0) (dec 30.333d0) (wcs *wcs-tan*))
    (multiple-value-bind (xp yp)
	(wcs-convert-ra-dec-to-pix-xy-tan wcs ra dec)
      (multiple-value-bind (ra1 dec1)
	  (wcs-convert-pix-xy-to-ra-dec-tan wcs xp yp)
	(format t "RA=~A, DEC=~A  --> pix X=~A, Y=~A  --> RA=~A, DEC=~A~%"
		ra dec xp yp ra1 dec1))))

  (defparameter *wcs-sin* (wcs:make-wcs-radec-sin 
			   :crpix1 100d0
			   :crpix2 200d0
			   :crval1 11d0 :crval2 30d0
			   :cd1_1 5d-5       :cd1_1 -5d-5 
			   :cd1_2 1d-6  :cd2_1 1d-6))
  
  (defun test-sin-proj-inverse (&key (ra 11.1111d0) (dec 30.333d0) (wcs *wcs-sin*))
    (multiple-value-bind (xp yp)
	(wcs-convert-ra-dec-to-pix-xy-sin wcs ra dec)
      (multiple-value-bind (ra1 dec1)
	  (wcs-convert-pix-xy-to-ra-dec-sin wcs xp yp)
	(format t "RA=~A, DEC=~A  --> pix X=~A, Y=~A  --> RA=~A, DEC=~A~%"
		ra dec xp yp ra1 dec1))))

)


#+nil
(defun wcs-convert-ra-dec-to-world-xy-s/zpn (wcs)
"convert ra and dec back to xy sky projection coordinates, leaving
the results in (wcs-radec-tan-p1 wcs) and  (wcs-radec-tan-p2 wcs)."
  (declare (type wcs-radec-zpn wcs)
	   (optimize (speed 3) (safety 1))
	   (inline sky-project:sin-project))
  (let ((ra  (wcs-2d-ra wcs))
	(dec  (wcs-2d-dec wcs))
	(alpha0 (wcs-2d-crval1 wcs))
	(delta0 (wcs-2d-crval2 wcs))
	(x0 (wcs-2d-crpix1 wcs))
	(y0 (wcs-2d-crpix2 wcs))
	(cd1_1 (wcs-2d-cd1_1 wcs))
	(cd1_2 (wcs-2d-cd1_2 wcs))
	(cd2_1 (wcs-2d-cd2_1 wcs))
	(cd2_2 (wcs-2d-cd2_2 wcs)) 
	(xt0 0d0) (yt0 0d0)  ;; sinn projected ra,dec
	(xt 0d0) (yt 0d0)  ;; zpn projected ra,dec
	(xp 0d0) (yp 0d0)) ;; pixel values
    (declare (type double-float alpha0 delta0 x0 y0
		   cd1_1 cd1_2 cd2_1 cd2_2 xt0 yt0
		   xt yt xp yp))
    (multiple-value-setq (xt0 yt0)
      (sky-project:sin-project ra dec alpha0 delta0 :units :natural))
    ;; now adjust xt0,yt0 by polynomial
    (loop
       with pvec of-type (simple-array double-float (*))
	 = (wcs-radec-zpn-pvec wcs)
       with r of-type (double-float 0d0 1d0) = 
	 (min 1d0 (sqrt (+ (expt xt0 2) (expt yt0 2))))
       ;; set rr to be r unless r=0, in which case x=y=0
       with rr of-type double-float = (if (zerop r) 1d0 r)
       with psi of-type double-float = (asin r)
       with poly of-type double-float = 0d0
       with nlast of-type (unsigned-byte 20) = (1- (length pvec))
       for i of-type (signed-byte 20) from nlast downto 0
       do (setf poly (+ (aref pvec i) (* psi  poly)))
       finally
	 (setf xt (* (/ xt0 rr) poly)) ;; use rr to avoid 1/0
	 (setf yt (* (/ yt0 rr) poly)))
    ;; convert to degrees
    (setf xt (* xt #.(/ 180 pi)))
    (setf yt (* yt #.(/ 180 pi)))
    ;;
    (multiple-value-setq (xp yp)
      (%2x2-lin-solve-macro cd1_1 cd1_2 cd2_1 cd2_2 xt yt))
    (setf (wcs-2d-p1 wcs) (+ xp x0)) 
    (setf (wcs-2d-p2 wcs) (+ yp y0)) 
    wcs))


 

(defun wcs-convert-ra-dec-to-pix-xy (wcs ra dec)
"Convert ra and dec back to xy pixel coordinates
WARNING - this assumes the normal FITS convention that the first pixel
is at 1,1.  To use 0-based array indices, add -1 to the returned
pixel indices."
  (declare (type wcs-2d wcs)
	   (type real ra dec))
  (let ((ra (float ra 1d0))
	(dec (float dec 1d0)))
    (cond ((wcs-radec-tan-tpv-p wcs)
	   (wcs-convert-ra-dec-to-pix-xy-tan-tpv wcs ra dec))
	  ((wcs-radec-tan-p wcs)  
	   (wcs-convert-ra-dec-to-pix-xy-tan wcs ra dec))
	  ((wcs-radec-sin-p wcs)
	   (wcs-convert-ra-dec-to-pix-xy-sin wcs ra dec))
	  (t
	   (error "Cannot yet handle wcs of type ~A" (type-of wcs))))))
	 


(defun wcs-convert-pix-xy-to-ra-dec (wcs xp yp)
"Convert  xy pixel coordinates to ra,dec.
WARNING - this assumes the normal FITS convention that the first pixel
is at 1,1.  To use 0-based array indices, add +1 to the input pixel
indices."
  (declare (type wcs-2d wcs)
	   (type real xp yp))
  (let ((xp (float xp 1d0))
	(yp (float yp 1d0)))
    (cond ((wcs-radec-tan-tpv-p wcs)
	   (wcs-convert-pix-xy-to-ra-dec-tan-tpv wcs xp yp))
	  ((wcs-radec-tan-p wcs)
	   (wcs-convert-pix-xy-to-ra-dec-tan wcs xp yp))
	  ((wcs-radec-sin-p wcs)
	   (wcs-convert-pix-xy-to-ra-dec-sin wcs xp yp))
	  (t
	   (error "Cannot yet handle wcs of type ~A" (type-of wcs))))))

(defun wcs-convert-ra-dec-to-world-xy (wcs ra dec)
  "convert ra and dec into word xy. Results are in degrees."
  (declare (type wcs-2d wcs)
	   (type real ra dec))
  (multiple-value-bind (px py)
      (wcs-convert-ra-dec-to-pix-xy wcs ra dec)
    ;; a little bit roundabout, going from ra,dec -> pix -> world
    (wcs-convert-pix-xy-to-world-xy wcs px py)))


 
 (defun wcs-convert-p-to-x (wcs p1) 
  "For wcs-linear wcs convert pixel value p1 to x1 valu
WARNING - this assumes that the first index is 1, not 0." 
  (declare (type wcs-linear wcs)
	   (type real p1))
  (+
   (wcs-linear-crval1 wcs)
   (* (wcs-linear-cdelt1 wcs)
      (- p1 (wcs-linear-crpix1 wcs)))))




 (defun wcs-convert-x-to-p (wcs x1)
  "for wcs-linear wcs convert value x1 to pixel value p1
WARNING - this assumes that the first index is 1, not 0." 
  (declare (type wcs-linear wcs)
	   (type real x1))
  (+
   (wcs-linear-crpix1 wcs)
   (* (/ 1d0 (wcs-linear-cdelt1 wcs))
      (- x1 (wcs-linear-crval1 wcs)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun wcs-position-angle-to-pixel-unit-vector (wcs pa-degrees)
    "Convert position angle to unit vector in pixel coordinates.
  PA is measured in degrees from North towards East on the sky.
  PA=0 points North, PA=90 points East.
  World-XY is tangent plane with North=+Y, East=+X.
  Returns (values dx dy) as a unit vector in pixel coordinates."
   (declare (type wcs-2d wcs)
	    (type real pa-degrees))
   (let* ((pa-rad (* pa-degrees (/ pi 180.0d0)))
          ;; World direction: x=East, y=North
          ;; PA from North to East: world-x = sin(PA), world-y = cos(PA)
          (world-dx (/ (sin pa-rad) 3600d0)) ;; 
          (world-dy (/ (cos pa-rad) 3600d0)))
     ;; Convert origin and offset point to pixels, take difference
     (multiple-value-bind (px0 py0)
         (wcs-convert-world-xy-to-pix-xy wcs 0.0d0 0.0d0)
       (multiple-value-bind (px1 py1)
           (wcs-convert-world-xy-to-pix-xy wcs world-dx world-dy)
         (let* ((pix-dx (- px1 px0))
                (pix-dy (- py1 py0))
                (norm (sqrt (+ (* pix-dx pix-dx) (* pix-dy pix-dy)))))
           (if (zerop norm)
               (values 0.0d0 0.0d0)
               (values (/ pix-dx norm) (/ pix-dy norm))))))))
