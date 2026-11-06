
#|

Assume we have a set of points with gaussian errors contaminated with
outliers in some fraction f, and we want to recover the mean value of x

We express the probability distribution as

   P(x,xm,f) = (1-f) delta(x-xm) + f 1/[1.7627472 s(1+((x-xm)/s)^2)^(1/2)]

where s is some width.  Note that this is un-normalized because
the second component has a 1/x tail and the integral 
is s*asinh(x/a)

The term 1.7627472 s (=2*asinh(1)) ensures that the normalization of the contamination
term is 1 between -1s and 1s is 'f'.

For fitting individual points xi with errors dxi, the likelihood is 

L(f,s,xm) = Sum_i  {(1-f) gaussian(xi-xbar,dxi) + 
                        f  1/[1.7627472 s(1+((x-xm)/s)^2)^(1/2)]}


The optimization is solely over xm, not f or s.  

|#

 
(defpackage bayes-outlier
  (:use #:cl)
  (:export 
   #:bayes-outlier-estimate-mean
   #:outlier-result #:outlier-result-p 
   #:outlier-result-xm #:outlier-result-xm-err #:outlier-result-n
   #:outlier-result-likelihood #:outlier-result-f #:outlier-result-s
   #:outlier-result-neval #:outlier-result-delta-chisqr-vec 
   #:outlier-result-delta-x-plus #:outlier-result-delta-x-minus
   #:outlier-result-non-outlier-likelihood  #:outlier-result-outlier-fraction
   #:outlier-result-ok
   ))

(in-package bayes-outlier)

(defun %to-double-vec (seq)
  (cond ((typep seq '(simple-array double-float (*)))
	 seq)
	(t
	 (map '(simple-array double-float (*))
	      (lambda (x) (float x 1d0))
	      seq))))
	      

(defstruct outlier-result 
  (xm                  0d0 :type double-float) ;; final mean
  (xm-err              0d0 :type double-float) ;; err of mean
  (f                   0d0 :type double-float) ;; fraction of contam
  (s                   0d0 :type double-float) ;; tail width
  (likelihood          0d0 :type double-float) 
  (n                   00  :type (unsigned-byte 28)) ;; number of input points
  ;; we explore minimum by delta-chisqr intervals
  (delta-chisqr-vec    (make-array 4 :element-type 'double-float 
				     :initial-contents '(1d0 4d0 9d0 16d0))
   :type (simple-array double-float (*)))
  (neval 0 :type (unsigned-byte 32))
  ;; how far we need to travel out in both directions from xm to get the corresponding 
  ;; delta-chisqr
  ;; 0d0 marks an excursion that could not be found (OK is then NIL)
  (delta-x-plus  (make-array 4 :element-type 'double-float :initial-element 0d0)
   :type (simple-array double-float (*)))
  (delta-x-minus (make-array 4 :element-type 'double-float :initial-element 0d0)
   :type (simple-array double-float (*)))
  ;; the relative likelihood that a given point is in the Gaussian vs
  ;; the outlier distribution
  (non-outlier-likelihood  (make-array 0 :element-type 'double-float )
   :type (simple-array double-float (*)))
  ;; fraction of points for which relative likelihood of being member < 1
  ;; is the peak OK
  (outlier-fraction 0d0 :type double-float)
  (ok nil))
  
  

(defun bayes-outlier-estimate-mean (xseq dxseq 
				    &key
				      (mean-guess nil)
				      (eps 1d-8)
				      (s 1.0) (f 0.1)
				      (max-n-evaluations 10000))
"Given a sequence of data XSEQ and its nominally Gaussian errors DXSEQ,
compute the best estimate of the mean and standard deviation,
returning an OUTLIER-RESULT structure in which

    OUTLIER-RESULT-XM                - the estimate of the mean
    OUTLIER-RESULT-XM-ERR            - the estimate of the mean's error
    OUTLIER-RESULT-DELTA-CHISQR-VEC  - delta-chisqr excursions explored
    OUTLIER-RESULT-DELTA-X-PLUS      - how much xm changed to produce 
    OUTLIER-RESULT-DELTA-X-MINUS           correspnding delta-chisqr 
    OUTLIER-RESULT-OK                - T if the chisqr excursions look 
                                       gaussian (parabolic), a quick test 
                                       if this is a nice minimum.
    NON-OUTLIER-LIKELIHOOD           - ratio of Prob(gauss)/Prog(outlier) 
                                       not using F.
    OUTLIER-FRACTION                 - fraction of points for which 
                                       NON-OUTLIER-LIKELIHOOD < 0.5

    OUTLIER-RESULT-DELTA-CHISQR-VEC  and OUTLIER-RESULT-DELTA-X-xxxx map out
    the minumum in one-sigma intervals (delta(chisqr) = 1,2,4,16).

The model assumed is that (loosely defined) fraction (1-F) of the data are Gaussian,
and fraction F are outliers with a probability distribution 
 1/sqrt(1+[x/s]^2)   - ie, an 'S bulge' with a 1/x Benford tail beyond s.     

The outlier distribution is normalized to F in the range [XM-S,XM+S] - ie, 
F is the fraction of outliers in the 'S bulge'.

S and F are specified, and may be fit if FIT-S and/or FIT-F
keywords are, but it is probably best not to do so."

  (let* ((xvec  (%to-double-vec xseq))
	 (dxvec (%to-double-vec dxseq))
	 (npts (length xvec))
	 ;; relative likelihood of being a good point
	 (rlvec  (make-array npts :element-type 'double-float))
	 (xmin (stats:min-of-elements xvec))
	 (xmax (stats:max-of-elements xvec))
	 (dx-med (stats:median-of-elements dxvec))
	 (s (float s 1d0))
	 (f (float f 1d0))
	 (neval-tot 0)
	 ;; PROB-FUNC's evaluation ceiling; lowered while probing the optional
	 ;; bounds so one evaluation is always left to restore RLVEC
	 (eval-limit max-n-evaluations)
	 (stage nil) ;; what stage of process we're in
	 ;; our initial guess
	 (xm-start  (float (or mean-guess 0d0) 1d0)))
		    
    (declare (type (simple-array double-float (*)) xvec dxvec rlvec)
	     (type double-float xmin xmax dx-med xm-start)
	     (type (unsigned-byte 28) neval-tot max-n-evaluations eval-limit))
    
    (flet ((prob-func (xm)
	     (declare (type double-float xm)
		      (optimize speed))
	     (when (float-utils:double-float-nan-p xm)
	       (error "Prob func got an invalid value"))
	     (incf neval-tot)
	     (when (> neval-tot eval-limit)
	       (error "Too many function evaluations by stage ~A:  ~A>~A"
		      stage neval-tot eval-limit))	     
	     (loop with likelihood of-type double-float = 0d0
		   for i of-type (unsigned-byte 28) from 0
		   for x  of-type double-float across xvec
		   for xxm of-type double-float = (- x xm)
		   for %dx of-type double-float across dxvec 
		   for dx of-type double-float = (max %dx 1d-30) ;; avoid divide by zero
		   ;; compute term in the exponent separately to avoid
		   ;; long computation times for outliers
		   for inexpterm of-type double-float = (* -0.5d0 (expt (/ xxm dx) 2))
		   for expterm of-type double-float 
		     = (if (< inexpterm -500) 
			   0d0 ;; don't bother computing exp of huge negative number
			   (exp inexpterm))
		   for gaussterm of-type double-float
		     = (* 
			(/ 1d0 #.(/ 1 (sqrt (* 2 pi))) dx) ;; 1/[sqrt(2 pi)dx]
			expterm)   ;; exp(-1/2 [(x-xm)/dx]^2)
		   for outlierterm of-type double-float
		     = (/ 1d0 (* #.(* 2 (asinh 1d0)) ;; normalization
				 s
				 (sqrt
				  (+ 1d0 (expt (/ xxm s) 2)))))
		   do (incf likelihood
			    (log 
			     (+ #.(* 64 least-positive-double-float) ;; avoid log(0)
				(the (double-float 0d0)
				     (+ (* (- 1d0 f) gaussterm)
					(* f outlierterm))))))
		      (setf (aref rlvec i) 
			    (/ gaussterm outlierterm))
		   finally (return (- likelihood)))))

      ;; if there is no mean-guess, then get the mean guess by walking across
      ;; xmin to xmax by  and fitting the function in steps
      (when (not mean-guess)
	(setf stage "INITIAL_ESTIMATE_OF_XM")
	(setf xm-start
	      (loop 
		with lik-min = most-positive-double-float 
		with best-mean = xmin
		for xm of-type double-float from xmin to xmax by dx-med
		for y = (funcall #'prob-func xm)
		when (< y lik-min)
		do 
		   (setf best-mean xm)
		   (setf lik-min  y)
		finally
		   (return best-mean))))
      
      ;; now find the best mean
      (setf stage "GOLDEN_SECTION")
      (multiple-value-bind
	    (xm-final neval)
	  (golden-section:findmin #'prob-func 
				  (- xm-start (* 3 dx-med))
				  (+ xm-start (* 3 dx-med))
				  :eps eps :neval-max max-n-evaluations)
	(declare (ignorable neval))
	(let ((likelihood-final (- (funcall #'prob-func xm-final)))
	      (outres (make-outlier-result :f f :s s :n npts))
	      (all-bounds-found t))
	  ;; find the distance we need to go in direction (+/- 1) to
	  ;; produce delta-chisqr
	  ;;
	  ;; The bracket used to be the data spread (XMAX-XMIN), which for a
	  ;; few tightly clustered points is far narrower than the error of the
	  ;; mean, so the root was never bracketed and FINDROOT died with "Too
	  ;; many iterations".  It was also mis-parenthesized as
	  ;; DIRECTION*(XM+SPREAD) rather than XM+DIRECTION*SPREAD.
	  ;;
	  ;; Instead, step outward from XM until the residual changes sign and
	  ;; bisect only that last step, so we get the NEAREST crossing even
	  ;; when the likelihood has secondary peaks (a wide fixed bracket can
	  ;; jump past one).  Steps are half a median error for the first 40,
	  ;; then double, so a far bound is still reached quickly.
	  (flet ((find-chisqr-bound (delta-chisqr direction)
		   (flet ((resid (xm)  ;; recall that deltaLogLik=-0.5 deltaChisqr
			    (declare (type double-float xm))
			    (- (+ (- likelihood-final) (* delta-chisqr 0.5d0))
			       (prob-func xm))))
		     (loop with step of-type double-float = (max (* 0.5d0 dx-med) 1d-12)
			   for nstep of-type fixnum from 1
			   for inner of-type double-float = xm-final then outer
			   for outer of-type double-float = (+ inner (* direction step))
			   when (> nstep 200)
			     do (error "Could not bracket the delta-chisqr=~A bound"
				       delta-chisqr)
			   when (<= (resid outer) 0d0)
			     return (abs (- (bisection-root:findroot #'resid inner outer)
					    xm-final))
			   when (>= nstep 40)
			     do (setf step (* 2d0 step))))))

	    (setf stage "FINDING_CHISQR_BOUNDS")
	    ;; keep one evaluation in hand for the RLVEC fix-up below, so a
	    ;; budget exhausted by the optional bounds can't lose the estimate
	    (setf eval-limit (max 0 (1- max-n-evaluations)))
	    ;; Only the first (delta-chisqr=1) bounds give XM-ERR, so only they
	    ;; may fail fatally.  The wider excursions just feed the OK
	    ;; shape test; with very few points the outlier tail can keep the
	    ;; likelihood from ever rising that far, so a failure there marks
	    ;; the result not-OK instead of losing the whole estimate.
	    (loop for delta-chisqr across (outlier-result-delta-chisqr-vec outres)
		  for i from 0
		  for dx+ = (if (zerop i)
				(find-chisqr-bound delta-chisqr 1)
				(ignore-errors (find-chisqr-bound delta-chisqr 1)))
		  for dx- = (if (zerop i)
				(find-chisqr-bound delta-chisqr -1)
				(ignore-errors (find-chisqr-bound delta-chisqr -1)))
		  ;; a missing side stays 0d0 (see DEFSTRUCT); a found side is kept
		  do (when dx+ (setf (aref (outlier-result-delta-x-plus  outres) i) dx+))
		     (when dx- (setf (aref (outlier-result-delta-x-minus outres) i) dx-))
		     (unless (and dx+ dx-) (setf all-bounds-found nil)))
	    (setf eval-limit max-n-evaluations)

	    ;; one more evaluation to fix RLVEC, which is messed up by 
	    ;; bisection-rootfind
	    (setf (outlier-result-likelihood outres)
		  (- (funcall #'prob-func xm-final)))
	    (setf (outlier-result-xm outres) xm-final)
	    (setf (outlier-result-xm-err outres)
		  (* 0.5d0 (+ (aref (outlier-result-delta-x-plus  outres) 0)
			      (aref (outlier-result-delta-x-minus outres) 0))))
	    (setf (outlier-result-neval outres) neval-tot)
	    (setf (outlier-result-non-outlier-likelihood outres) rlvec)
	    (setf (outlier-result-outlier-fraction outres)
		  (/ (float (count-if (lambda (x) (< x 1d0)) rlvec) 1d0)
		     npts))

	    ;; is minimum OK - do all the chisqr bounds look gaussian?
	    (loop with ok = all-bounds-found
		  for dchisqr across (outlier-result-delta-chisqr-vec outres)
		  for sigma = (sqrt dchisqr)
		  for sigma0 = (outlier-result-xm-err outres)
		  for dx+ across  (outlier-result-delta-x-plus  outres)
		  for dx- across  (outlier-result-delta-x-minus outres)
		  ;; require 10% agreement with expected scaling that 
		  ;; dx increase by equal steps as delta_chisqr=1,4,9,16
		  when (or (> (- 1.0  (/  dx+ sigma sigma0)) 0.1)
			   (> (- 1.0  (/  dx- sigma sigma0)) 0.1))
		    do (setf ok nil)
		  finally (setf (outlier-result-ok outres) ok))
		 

	    outres))))))
       
	    



