
#|


{  
  "TYPE":"REQUEST",
  "COMMAND":"FILTER-IMAGE",
  "ID":"123abc",
  "PARAMETERS": {
                 "FITS-FILE":"/dir/sample.fits",
                 "EXTENSION":1, // if extension is not the default one
                 "OUTPUT-FITS-FILE":"/dir/sample-output.fits",
                 "CLOBBER":false, // by default don't clobber existing output file
                 "FILTER": { 
                              "FILTER-TYPE": "XXXXX"
                              // more parameters
                           }
               }
}

Available FILTER values are:

Do nothing to image:

  {
    "FILTER-TYPE":"IDENTITY"
  }

Apply symmetric Gaussian filter to image (using FFT)

  {
    "FILTER-TYPE":"GAUSSIAN",
    "SIGMA":7.0  // in pixel units
  }

Apply Laplacian filter composed of two Gaussians of opposite sign (using FFT)

  {
    "FILTER-TYPE":"LAPLACIAN",
    "SIGMA1":3.0,  // in pixel units
    "SIGMA2":6.0   // in pixel units
  }


Apply unsharp-mask filter composed of a core Gaussian, plus a Laplacian weighted by
STRENGH (using FFT)
  Kernel = Gaussian(SIGMA0)+STRENGTH*[Gaussian(SIGMA1)-Gaussian(SIGMA2)]
where the part multiplied by STRENGTH is a Laplacian.   Using a tiny
SIGMA0 prevents any blurring of the primary image component, while a larger
SIGMA0 allows more flexibility.

  {
    "FILTER-TYPE":"UNSHARP-MASK",
    "SIGMA0":0.1   // core radius for the image itself (pixel units)
    "SIGMA1":3.0,  // Laplacian sigma1 for unsharp-mask (in pixel units)
    "SIGMA2":6.0   // Laplacian sigma2 for unsharp-mask (in pixel units)
    "STRENGTH":0.5 // strength of unsharp operation, being the weight of the Laplacian
  }

Apply a circular median filter.  If R-INNER is specified and non-zero,
it is an annular filter  useful for attenuating point sources.  This is a
computationally expensive operation.    
The result is OUTPUT=MEDIAN-COEFFICIENT*MEDIAN + IMAGE-COEFFICIENT * ORIGINAL_IMAGE
so that by setting MEDIAN-COEFFICIENT=-1 and IMAGE-COEFFICIENT=0, this
is transformed into a median subtraction, to reveal local detail.

  {
    "FILTER-TYPE":"MEDIAN",
    "R-OUTER":6.0,  // in pixel units
    "R-INNER":3.0    // in pixel units
    "MEDIAN-COEFFICIENT":1.0,
    "IMAGE-COEFFICIENT" :0.0
  }

Apply a Larson-Sekanina filter such that
 F'(r,a ; dr,da) = 2 F(r,a) - F(r-dr,a+da) - F(r-dr, a-da)
where 'dr' is a radial offset in pixels, and 'a' 
is an angular offset in degrees.

  {
    "FILTER-TYPE":"LARSON-SEKANINA",
    "X0":100.0,  // rotation center X, in pixel units
    "Y0":200.0,  // rotation center Y, in pixel units
    "DR":5.0,    // radial shift, in pixel units
    "DA":5.0,    // angular shift, in degrees
  }


Subtract a powerlaw coma  F(R)=BACKGROUND+NORM*SQRT(R^2+R_CORE^2)^EXPONENT

  {
    "FILTER-TYPE":"COMA-SUBTRACT",
    "X0":100.0,  // coma-center X, pixel units
    "Y0":200.0,  // coma-center Y, pixel units
    "R-FIT":150, // radius to which to perform fit 
    "BACKGROUND":999.0, // OPTIONAL background flux in ADU; fit if not given
    "R-CORE":5.0,       // OPTIONAL core radius of coma in pixels; fit if not given
    "EXPONENT",-1.0     // OPTIONAL exponent; fit if not given
  }

 


|#

(In-package coma-json-server)


(def-json-command filter-image (json-req)
  (with-json-command-setup (json-req)
    (let* ((fits-file (get-param "FITS-FILE" :required t))
	   (fits-exists
	     (or (probe-file fits-file)
		 (return-with-error "FITS-FILE-NOT-FOUND"
				    (format nil "Fits file ~A not found" fits-file))))
	   (inst (or (ignore-errors
		      (if fits-exists (instrument-id:identify-instrument fits-file)))
		     (return-with-error
		      "COULD-NOT-IDENTIFY-FITS-FILE"
		      "Could not identify type of fits file.")))
	   ;; if extension=NIL then use the default single-image
	   ;; extension, or the first finite image extension		      
	   (extension  (get-param "EXTENSION"))
	   (output-fits (get-param "OUTPUT-FITS-FILE" :required t))
	   (clobber (get-param "CLOBBER"))
	   (filter-obj  (get-param "FILTER" :required t))
	   (filter-type nil))

      (labels ((run-filter (filter-function filter-obj fits)
		 ;; assume fits is already extracted and can be modified
		 (multiple-value-bind (result err)
		     (ignore-errors
		      (funcall filter-function filter-obj fits json-resp))
		   (when (not result)
		     (return-with-error
		      "IMAGE-FILTER-FAILED"
		      (format nil "Error in calling function ~A: <~A>"
			      filter-function err))))))
						 
		 (jcom-test-expr (not (probe-file fits-file))
				 "FITS-FILE-NOT-FOUND"
				 (format nil "Fits file ~A not found" fits-file))

		 (jcom-test-expr (or (not filter-obj)
				     (not (hash-table-p filter-obj))
				     (not (setf filter-type (gethash "FILTER-TYPE" filter-obj))))
				 "FILTER-NOT-VALID"
				 "FILTER not given, or it is not a JSON object with a FILTER-TYPE field.")
	 

		 (when (and (not clobber)
			    (probe-file output-fits))
		   (return-with-error
		    "OUTPUT-FITS-ALREADY-EXISTS"
		    (format nil "OUTPUT-FITS ~A already exists and will not overwrite unless CLOBBER is true"
			    output-fits)))

		 ;; create a clone of input into output-fits
		 (cond ((typep inst 'instrument-id:multichip)
			(when (not extension)
			  (return-with-error
			   "EXTENSION-NOT-GIVEN"
			   "No extension given for a multichip Fits file"))
			(instrument-id:extract-one-image-from-mosaic-fits
			 fits-file extension output-fits :decompress t))
		       (t ;; one chip
			(cl-fad:copy-file fits-file output-fits :overwrite t)))
			 
		 
		 (cond ((equalp "IDENTITY" filter-type)
			(run-filter 'filter-image/identity filter-obj output-fits))
		       ((equalp "GAUSSIAN" filter-type)
			(run-filter 'filter-image/gaussian filter-obj output-fits))
		       ((equalp "LAPLACIAN" filter-type)
			(run-filter 'filter-image/laplacian filter-obj output-fits))
		       ((equalp "UNSHARP-MASK" filter-type)
			(run-filter 'filter-image/unsharp-mask filter-obj output-fits))
		       ((equalp "MEDIAN" filter-type)
			(run-filter 'filter-image/median filter-obj output-fits))
		       ((equalp "LARSON-SEKANINA" filter-type)
			(run-filter 'filter-image/larson-sekanina filter-obj output-fits))
		       ((equalp "COMA-SUBTRACT" filter-type)
			(run-filter 'filter-image/coma-subtract filter-obj output-fits))
		       ;;
		       (t
			(return-with-error
			 "UNKNOWN-FILTER-TYPE"
			 (format nil "Filter type ~A is unknown"  filter-type))))

		 (setf (gethash "OUTPUT-FITS-FILE" parameters-out) output-fits)
		 ;; copy input params to output
		 (loop for key being the hash-key of parameters
		       for val being the hash-value of parameters
		       when (not (equalp key "TYPE"))
		       do (set-param key val))))))


;; the filter functions should return some true value

;; a dummy function that performs no filtration
(defun filter-image/identity (filter-obj output-fits json-resp)
  (declare (ignorable filter-obj output-fits json-resp))
  json-resp) ;; no error possible

;; a dummy function that performs no filtration
(defun filter-image/gaussian (filter-obj output-fits json-resp)
  (block retblock
    (flet ((return-with-error (err-name err-desc)
	     (setf (json-object-error json-resp)
		   (make-error-object
		    :error err-name
		    :desc err-desc))
	     (return-from retblock json-resp)))
      (cf:with-open-fits-file (output-fits ff :mode :io :throw-error t)
	(cf:move-to-extension ff (instrument-id:get-image-extension-for-onechip-fits
				  output-fits))
	(let* ((imsec (cf:read-image-section ff))
	       (im    (cf:image-section-data imsec))
	       (sigma (gethash "SIGMA" filter-obj))
	       (imk   (imutils:make-same-size-image im)) ;; kernel image
	       (xmax  (float  (array-dimension im 1) 1.0))
	       (ymax  (float  (array-dimension im 0) 1.0)))
	  (when (not (and (realp sigma)
			  (plusp sigma)))
	    (return-with-error "SIGMA-NOT-GIVEN" "No or invalid Gaussian sigma in filter"))
	  (setf sigma (float sigma 1.0))
	  (ignore-errors (imutils:imclean-avg im :max-iter 200))
	  ;; we have to add gaussians in each corner because of way FFTs work
	  (imutils:add-gaussian-to-image imk 0.0  0.0  sigma sigma  0.0 1.0)
	  (imutils:add-gaussian-to-image imk xmax 0.0  sigma sigma  0.0 1.0)
	  (imutils:add-gaussian-to-image imk 0.0  ymax sigma sigma  0.0 1.0)
	  (imutils:add-gaussian-to-image imk xmax ymax sigma sigma  0.0 1.0)
	  (imutils:fft-convolve-images im imk :im-out im)
	  (cf:write-back-image-section imsec))))
    json-resp))


(defun filter-image/laplacian (filter-obj output-fits json-resp)
  (block retblock
    (flet ((return-with-error (err-name err-desc)
	     (setf (json-object-error json-resp)
		   (make-error-object
		    :error err-name
		    :desc err-desc))
	     (return-from retblock json-resp)))
      (cf:with-open-fits-file (output-fits ff :mode :io :throw-error t)
	(cf:move-to-extension ff (instrument-id:get-image-extension-for-onechip-fits
				  output-fits))
	(let* ((imsec (cf:read-image-section ff))
	       (im    (cf:image-section-data imsec))
	       (sigma1 (gethash "SIGMA1" filter-obj))
	       (sigma2 (gethash "SIGMA2" filter-obj))
	       (imk   (imutils:make-same-size-image im)) ;; kernel image
	       (xmax  (float  (array-dimension im 1) 1.0))
	       (ymax  (float  (array-dimension im 0) 1.0)))
	  (when (not (and (realp sigma1)
			  (plusp sigma1)))
	    (return-with-error "SIGMA1-NOT-GIVEN" "No or invalid Gaussian SIGMA1 in filter"))
	  (when (not (and (realp sigma2)
			  (plusp sigma2)))
	    (return-with-error "SIGMA2-NOT-GIVEN" "No or invalid Gaussian SIGMA2 in filter"))
	  (setf sigma1 (float sigma1 1.0))
	  (setf sigma2 (float sigma2 1.0))
	  (ignore-errors (imutils:imclean-avg im :max-iter 200))
	  ;; we have to add gaussians in each corner because of way FFTs work
	  (loop for sigma in (list sigma1 sigma2)
		for norm in (list +1.0 -1.0)
		do
		   (imutils:add-gaussian-to-image imk 0.0  0.0  sigma sigma  0.0 norm)
		   (imutils:add-gaussian-to-image imk xmax 0.0  sigma sigma  0.0 norm)
		   (imutils:add-gaussian-to-image imk 0.0  ymax sigma sigma  0.0 norm)
		   (imutils:add-gaussian-to-image imk xmax ymax sigma sigma  0.0 norm))
	  (imutils:fft-convolve-images im imk :im-out im)
	  (cf:write-back-image-section imsec))))
    json-resp))


;; an unsharp-mask with sigma0 being the scale of the identity-function
;; filtration (image itself); and  sigma1 and sigma2 being the added
;; Gaussian laplacian, and strength beign the weight of the Laplacian.
;;
;; Kernel = Gaussian(SIGMA0)+STRENGTH*[Gaussian(SIGMA1)-Gaussian(SIGMA2)]
(defun filter-image/unsharp-mask (filter-obj output-fits json-resp)
  (block retblock
    (flet ((return-with-error (err-name err-desc)
	     (setf (json-object-error json-resp)
		   (make-error-object
		    :error err-name
		    :desc err-desc))
	     (return-from retblock json-resp)))
      (cf:with-open-fits-file (output-fits ff :mode :io :throw-error t)
	(cf:move-to-extension ff (instrument-id:get-image-extension-for-onechip-fits
				  output-fits))
	(let* ((imsec (cf:read-image-section ff))
	       (im    (cf:image-section-data imsec))
	       (sigma0   (gethash "SIGMA0" filter-obj))
	       (sigma1   (gethash "SIGMA1" filter-obj))
	       (sigma2   (gethash "SIGMA2" filter-obj))
	       (strength (gethash "STRENGTH" filter-obj))
	       (imk   (imutils:make-same-size-image im)) ;; kernel image
	       (xmax  (float  (array-dimension im 1) 1.0))
	       (ymax  (float  (array-dimension im 0) 1.0)))
	  (when (not (and (realp sigma0)
			  (plusp sigma0)))
	    (return-with-error "SIGMA0-NOT-GIVEN" "No or invalid Gaussian SIGMA0 in filter"))
	  (when (not (and (realp sigma1)
			  (plusp sigma1)))
	    (return-with-error "SIGMA1-NOT-GIVEN" "No or invalid Gaussian SIGMA1 in filter"))
	  (when (not (and (realp sigma2)
			  (plusp sigma2)))
	    (return-with-error "SIGMA2-NOT-GIVEN" "No or invalid Gaussian SIGMA2 in filter"))
	  (when (not (and (realp strength)
			  (plusp strength)))
	    (return-with-error "STRENGTH-NOT-GIVEN" "No or invalid Gaussian STRENGTH in filter"))
	  (setf sigma0 (float sigma0 1.0))
	  (setf sigma1 (float sigma1 1.0))
	  (setf sigma2 (float sigma2 1.0))
	  (setf strength (float strength 1.0))
	  (ignore-errors (imutils:imclean-avg im :max-iter 200))
	  ;; we have to add gaussians in each corner because of way FFTs work
	  (loop for sigma in (list sigma0 sigma1 sigma2)
		for norm in (list 1.0 (+ strength) (- strength))
		do
		   (imutils:add-gaussian-to-image imk 0.0  0.0  sigma sigma  0.0 norm)
		   (imutils:add-gaussian-to-image imk xmax 0.0  sigma sigma  0.0 norm)
		   (imutils:add-gaussian-to-image imk 0.0  ymax sigma sigma  0.0 norm)
		   (imutils:add-gaussian-to-image imk xmax ymax sigma sigma  0.0 norm))
	  (imutils:fft-convolve-images im imk :im-out im)
	  (cf:write-back-image-section imsec))))
    json-resp))


(defun filter-image/median (filter-obj output-fits json-resp)
  (block retblock
    (flet ((return-with-error (err-name err-desc)
	     (setf (json-object-error json-resp)
		   (make-error-object
		    :error err-name
		    :desc err-desc))
	     (return-from retblock json-resp)))
      (cf:with-open-fits-file (output-fits ff :mode :io :throw-error t)
	(cf:move-to-extension ff (instrument-id:get-image-extension-for-onechip-fits
				  output-fits))
	(let* ((params-out (json-object-parameters json-resp))
	       (imsec (cf:read-image-section ff))
	       (im    (cf:image-section-data imsec))
	       (r-inner (gethash "R-INNER" filter-obj))
	       (r-outer (gethash "R-OUTER" filter-obj))
	       (median-coefficient (or (gethash "MEDIAN-COEFFICIENT" filter-obj) 1.0))
	       (image-coefficient (or (gethash "IMAGE-COEFFICIENT" filter-obj) 1.0)))
							   
	       

	  (when (not (and (realp r-outer)
			  (plusp r-outer)))
	    (return-with-error "R-OUTER-INVALID" "No or invalid R-OUTER in filter"))
	  (when (and r-inner
		     (not (and (realp r-inner)
			       (plusp r-inner))))
	    (return-with-error "R-INNER-INVALID" "Invalid R-INNER in filter"))
	  (when (not (realp median-coefficient))
	    (return-with-error "BAD-MEDIAN-COEFFICIENT" "Invalid MEDIAN-COEFFICIENT in filter."))
	  (when (not (realp image-coefficient))
	    (return-with-error "BAD-IMAGE-COEFFICIENT" "Invalid IMAGE-COEFFICIENT in filter."))
		     
  	  (setf r-outer (float r-outer 1.0))
	  (when r-inner
	    (setf r-inner (float r-inner 1.0)))
	  (setf median-coefficient (float median-coefficient 1.0))
	  (setf image-coefficient (float image-coefficient 1.0))

	  ;; these two may not have been provided do we add then to output
	  (setf (gethash "MEDIAN-COEFFICIENT" params-out) median-coefficient)
	  (setf (gethash "IMAGE-COEFFICIENT" params-out) image-coefficient)

	  ;; median-ring-filter-image ignores NaN and InF
	  (let ((im-original (imutils:copy-image im)))
	    (imutils:median-ring-filter-image 
	     im
	     :image-out im
	     :r1 (or r-inner 0.0)
	     :r2 r-outer)
	    (when (not (and (= image-coefficient 0.0)
			    (= median-coefficient 1.0)))
	      (loop for i below (array-total-size im)
		    for medval = (row-major-aref im i)
		    for orig-val = (row-major-aref im-original i)
		    when (not (float-utils:single-float-nan-or-infinity-p orig-val))
		    do 	(setf (row-major-aref im i)
			      (+ (* median-coefficient medval)
				 (* image-coefficient orig-val))))))
	  
	  (cf:write-back-image-section imsec))))
    json-resp))


(defun filter-image/larson-sekanina (filter-obj output-fits json-resp)
  (block retblock
    (flet ((return-with-error (err-name err-desc)
	     (setf (json-object-error json-resp)
		   (make-error-object
		    :error err-name
		    :desc err-desc))
	     (return-from retblock json-resp)))
      (cf:with-open-fits-file (output-fits ff :mode :io :throw-error t)
	(cf:move-to-extension ff (instrument-id:get-image-extension-for-onechip-fits
				  output-fits))
	(let* ((imsec (cf:read-image-section ff))
	       (im    (cf:image-section-data imsec))
	       (x0 (gethash "X0" filter-obj))
	       (y0 (gethash "Y0" filter-obj))
	       (dr (gethash "DR" filter-obj))
	       (da (gethash "DA" filter-obj)))
	       

	  (when (not (and (realp x0)
			  (realp y0)
			  (realp dr)
			  (realp da)))
	    (return-with-error "INVALID-PARAMETERS"
			       "X0,Y0,DR,DA must all be real numbers."))

	  (setf x0 (+ -1 (float x0 1.0))) ;; 1-index to 0-index
	  (setf y0 (+ -1 (float y0 1.0)))
	  (setf dr (float dr 1.0))
	  (setf da (float da 1.0))

	  (imutils:larson-sekanina-filter im x0 y0 dr da
					  :im-out im
					  :interpolation :linear)
	  
	  (cf:write-back-image-section imsec))))
    json-resp))


(defun filter-image/coma-subtract (filter-obj output-fits json-resp)
  (block retblock
    (flet ((return-with-error (err-name err-desc)
	     (setf (json-object-error json-resp)
		   (make-error-object
		    :error err-name
		    :desc err-desc))
	     (return-from retblock json-resp)))
      (cf:with-open-fits-file (output-fits ff :mode :io :throw-error t)
	(cf:move-to-extension ff (instrument-id:get-image-extension-for-onechip-fits
				  output-fits))
	(let* ((imsec (cf:read-image-section ff))
	       (im    (cf:image-section-data imsec))
	       (nx (array-dimension im 1))
	       (ny (array-dimension im 0))
	       (im-median (imutils:compute-sampled-image-median-and-sigma
			   im 50000
			   :ix0 (round (* 0.2 nx)) :ix1 (round (* 0.8 nx))
			   :iy0 (round (* 0.2 ny)) :iy1 (round (* 0.8 ny))))
	       (x0 (gethash "X0" filter-obj))
	       (y0 (gethash "Y0" filter-obj))
	       (r-fit (gethash "R-FIT" filter-obj))
	       (r-core (gethash "R-CORE" filter-obj))
	       (exponent (gethash "EXPONENT" filter-obj))
	       (background-val (gethash "BACKGROUND" filter-obj))
	       (background (cond ((not background-val) nil)
				 ((realp background-val) background-val)
				 ((equalp background-val "MEDIAN")
				  im-median)
				 (t
				  (return-with-error "INVALID-BACKGROUND"
						     "BACKGROUND must be a real, MEDIAN, or not given")))))



	  (when (not (and (realp x0)
			  (realp y0)
			  (realp r-fit)))
	    (return-with-error "INVALID-PARAMETERS"
			       "X0,Y0,R-FIT must all be real numbers."))
	  (when (and exponent
		     (not (realp exponent)))
	    (return-with-error "INVALID-EXPONENT"
			       "EXPONENT should either not be given, or should be real."))

	  (setf x0 (+ -1 (float x0 1.0))) ;; 1-index to 0-index
	  (setf y0 (+ -1 (float y0 1.0)))
	  (setf r-fit (float r-fit 1.0))
	  (setf r-core (if r-core (float r-core 1.0)))
	  (setf exponent (if exponent (float exponent 1.0)))
      
	  (let* ((fr ;; radial profile at 1 pix
		   (imutils:compute-radial-profile
		    im x0 y0 
		    :n-points (ceiling r-fit)
		    :edge-value most-negative-single-float
		    :compute-error nil)) ;; error on profile is expensive
		 (ps (powell:build-powell-struct
		      4
		      :optim-flags  (vector (if (not background) t nil)
					    t ;; norm is always free
					    (if (not exponent) t nil)
					    (if (not r-core) t nil))))
		 (x-vec (make-array 4 :element-type 'double-float
				      :initial-contents
				      (list (float im-median 1d0) ;; backd
					    1d0 ;; norm guess
					    (if exponent (* 1d0 exponent) -1d0) ;; exponent
					    (if r-core (* 1d0 r-core) 1d0)))))
					    
					    
					       
					    
	    (labels ((fit-function (background norm exponent r-core)
		       (declare (type double-float norm r-core background exponent))
		       (loop with sum of-type double-float = 0d0
			     for i below (length fr)
			     for rprof of-type double-float = (* 1d0 i)
			     for rprof/core of-type (double-float 0d0) = (sqrt (+ (* r-core r-core)
										  (* rprof rprof))) 
			     for flux of-type double-float = (float (aref fr i) 1d0)
			     for flux-pred of-type double-float 
			       = (+ background
				    (* norm (expt rprof/core exponent)))
			     ;; when we're in a valid part of the curve
			     when  (not (= flux most-negative-single-float))
			     ;; error weight error term by r (area weighting)
			     do (incf sum (* rprof (abs (- flux flux-pred))))
			     finally
				(return (+ sum))))
		     (powell-fit-function (ps)
		       (declare (type powell:powell-struct ps))
		       (let ((xvec (powell:powell-struct-x-vec ps)))
			 (setf (powell:powell-struct-y ps)
			       (fit-function (aref xvec 0) (aref xvec 1) (aref xvec 2) (aref xvec 3))))))

	      (powell:init-powell-struct ps #'powell-fit-function  x-vec)     

	      (multiple-value-bind (result err)
		  (ignore-errors
		   (powell:run-powell-on-ps #'powell-fit-function ps 1d-6))
		(when (not result)
		  (return-with-error "ERROR-IN-OPTIMIZER"
				     (format nil "Error in Powell optimizer to subtract power law: ~A" err))))

	      ;; report back fit parameters, and subtract fit from image
	      (let* ((xvec-out (powell:powell-struct-x-vec ps))
		     (parameters-out (json-object-parameters json-resp))
		     (backd-out (float (aref xvec-out 0) 1.0))
		     (norm-out  (float (aref xvec-out 1) 1.0))
		     (exponent-out  (float (aref xvec-out 2) 1.0))
		     (r-core-out  (float (aref xvec-out 3) 1.0)))

		(setf (gethash "BACKGROUND" parameters-out) backd-out)
		(setf (gethash "NORM" parameters-out) norm-out)
		(setf (gethash "EXPONENT" parameters-out) exponent-out)
		(setf (gethash "R-CORE" parameters-out) r-core-out)

		(loop
		  for ix below nx
		  for x of-type single-float = (- ix x0)
		  do
		     (loop
		       for iy below ny
		       for fimage = (aref im iy ix)
		       for y of-type single-float = (- iy y0)
		       for r2 of-type (single-float 0.0) =  (+ (* x x) (* y y))
		       for r/core of-type (single-float 0.0)
			 = (sqrt (the (single-float 0.0) (+ r2 (* r-core-out r-core-out))))
		       for model-flux ;; don't subtract backd
			 = (* norm-out (expt r/core exponent-out))
		       do (when (not (float-utils:single-float-nan-or-infinity-p fimage))
			    (setf (aref im iy ix) (- fimage model-flux)))))))
			   
	    (cf:write-back-image-section imsec))))
    json-resp)))
