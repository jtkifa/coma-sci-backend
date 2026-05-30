
(in-package simple-comet-activity-detector)

(defstruct scadresult
  active-p
  x0 y0
  fratio
  fratio-err
  coma-frac     ;; the fraction at r=0, not the total coma flux! (coma flux diverges)
  coma-frac-err
  flux-total ;; within the 
  flux-coma  ;; this is only within the photometric zone
  flux-nucleus
  ;; can't do flux-nucleus because it is infinite for 1/r
  fwhm)  


(defun is-object-active (im xpix ypix
			 &key
			   (r-profile 50) (n-monte-carlo 100)
			   (fit-center t)
			   (fit-center-n 15))
  

 "Result a SCADRESULT object whether an detection has cometry activity
by computing a profile, and comparing the flux within the 2xHWHM to the
flux from 3xHWHM to 4xHWHM.  If FIT-CENTER is T, then the center pixel
is fine-tuned using a quadratic peak.  N-MONTE-CARLO is the number of 
Monte-Carlo iterations to produce the profile error.

Note that the coma fraction is the fraction of the profile height
at r=0, not the fraction of the light.
"

  (declare (type imutils:image im)
	   (type single-float xpix ypix)
	   (type (integer 3 51) fit-center-n)
	   (optimize debug))

  (let ((x0 xpix)
	(y0 ypix))
    ;; fit the center peak if requested
    (when fit-center
      (multiple-value-bind (xq yq signq)
	  (imutils:fit-quadratic im (round x0) (round y0)
				 (if (oddp fit-center-n) fit-center-n (1+ fit-center-n)))
	(when (not (minusp signq))
	  (error "Quadratic peak finding did not find a positive peak (negative definite quadratic form)"))
	(setf x0 xq
	      y0 yq)))
    ;;
    (multiple-value-bind (flux-vec r-vec flux-err-vec)
	(imutils:compute-radial-profile im x0 y0  
					:n-points (round (* 4 r-profile)) ;; divide pixels into 1/4
					:dr 0.5;;
					:interp-method :lanczos2)
      (declare (type (simple-array single-float (*))
		     flux-vec r-vec flux-err-vec))
      ;; this will be a scratch array for monte-carlos
      (let ((fvec (copy-seq flux-vec))
	    (central-flux (aref flux-vec 0)) ;; flux at r=0
	    (fwhm 0.0))
	(declare (type (simple-array single-float (*)) fvec))      
	(flet ((compute-flux-ratio ()
		 ;; subtract backd from fvec
		 (loop with backd of-type single-float
			 = (loop
			     with count = 0
			     with sum = 0.0
			     ;; backd flux is the outer 90% of the profile
			     for i from (- (length fvec) 1) downto (round (* 0.9 (length fvec)))
			     do (incf sum (aref fvec i))
				(incf count 1)
			     finally (return (/ sum count)))
		       for i below (length fvec)
		       do (decf (aref fvec i)  backd))
		 (let* ((hwhm
			  (or (ignore-errors (* 0.5 (get-fwhm-from-fvec-rvec fvec r-vec)))
			      (error "Failed to compute HWHM of profile - probably noisy.")))
			(flux-inside (integrate-fvec-and-rvec fvec r-vec 0.0 (* 2 hwhm)))
			(flux-outside (integrate-fvec-and-rvec fvec r-vec (* 3.0 hwhm) (* 4 hwhm)))
			(flux-total (integrate-fvec-and-rvec fvec r-vec 0.8 (float r-profile 1.0))))
		   (declare (type single-float hwhm flux-inside flux-outside flux-total))
		   (setf fwhm (* 2 hwhm))

		   ;; prevent division by zero - this is oh-so-bogus but the goal is to have
		   ;; a mostly good tool
		   (when (< flux-inside 1e-3)
		     (setf flux-inside (max 1e-5 (* 1e-10 flux-outside))))
		   
		   (values (/ flux-outside flux-inside) flux-inside flux-outside flux-total))

		 )
	       ;;
	       (randomize-fvec ()
		 (loop for i from 0
		       for f across flux-vec
		       for ferr across flux-err-vec
		       do (setf (aref fvec i) (float (+ f (* ferr (random:gaussian))) 1.0)))))
	  ;;
	  (multiple-value-bind (frat0 flux-inside flux-outside flux-total)  ;; the real ratio (not MC'ed)
	      (compute-flux-ratio)
	    (declare (ignorable flux-inside flux-outside))
	    (let* ((frat-mc-list ;; randomized ratios
		     (loop for i below n-monte-carlo
			   collect (progn (randomize-fvec)
					  (compute-flux-ratio))))
		   (frat-mc-dev-list 
		     (mapcar (lambda (frat) (abs (- frat frat0))) frat-mc-list))
		   (med-frat-dev (stats:median-of-elements frat-mc-dev-list))
		   ;; convert median deviation to sigma assuming a Gaussian
		   ;; (ie, integral of Gaussian from -0.67 to +0.67 is 0.5)
		   (sigma-frat (* 0.6745d0 med-frat-dev)))

	      (multiple-value-bind (fcoma fcoma-err)
		  (estimate-coma-fraction-and-err-from-fratio frat0 sigma-frat)
		;;
		(let* ((flux-nucleus (estimate-nucleus-flux fcoma fwhm :central-flux central-flux))
		       (flux-coma (- flux-total flux-nucleus)))

		(make-scadresult
		 :active-p (plusp (- fcoma fcoma-err)) ;; 1 sigma level
		 :x0 x0 :y0 y0
		 :fratio frat0
		 :fratio-err sigma-frat
		 :coma-frac fcoma
		 :coma-frac-err fcoma-err
		 :flux-total flux-total
		 :flux-coma flux-coma
		 :flux-nucleus flux-nucleus
		 :fwhm fwhm))))))))))

		      
			    

		      
		     
		  
	    
      
      
	    
  

  

  
