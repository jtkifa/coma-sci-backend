

(in-package imred)


;; mark pixels that are saturated or have an otherwise invalid value
;; with +invalid-pixel-value+ for processing
(defun mark-bad-pixels (array did-imred saturation-val
			invalid-func output-null-val)
  (let ((saturation-val (float saturation-val 1.0))
	(output-null-val (float output-null-val 1.0)))
    (declare (type (simple-array single-float (* *)) array)
	     (type single-float saturation-val output-null-val)
	     (type (function (single-float) t) invalid-func)
	     (optimize speed))
    (if did-imred
	(loop 
	  ;; values outside most-pos-allowed and most-neg-allowed are at risk
	  ;; of floating point overflow/underflow
	  with most-neg-allowed = (* 0.25 most-negative-single-float)
	  with most-pos-allowed = (* 0.25 most-positive-single-float)
	  for i below (array-total-size array)
	  for x = (row-major-aref array i)
	  when  (or (float-nan-p x)
		    (and (not (float-nan-p output-null-val))
			 (= x output-null-val))
		    (< x most-neg-allowed)
		    (> x most-pos-allowed))
	    do  (setf (row-major-aref array i) +invalid-pixel-value+))
	;; else we didn't do imred yet, so we check for initial invalid values
	;; and saturation
	(loop for i below (array-total-size array)
	      for x = (row-major-aref array i)
	      when (or 
		    (float-nan-p x) ;; always invalid
		    (= x saturation-val)
		    (funcall invalid-func x))
		do (setf (row-major-aref array i) +invalid-pixel-value+)))))


(defun ccdproc-one-extension (ff-out ff-bias ff-flat
			      extdesc 
			      saturation-val invalid-func output-null-val
			      normalize-gain)
  (declare (type (function (single-float) t) invalid-func))
  (let* ((read-type :float)
	 (saturation-val (float saturation-val 1.0))
	 (output-null-val (float output-null-val 1.0))
	 (extnum (cf:fits-file-current-hdu-num ff-out))
	 (gain (if normalize-gain
		   (float (or
			   (instrument-id:get-gain-for-fits
			    ff-out
			    :extension extnum)
			   (error "Cannot get gain for ~A[~A]"
				  (cf:fits-file-filename ff-out) extnum))
			  1.0)
		   1.0))
	 (do-bias (and ff-bias
		       (not (cf:read-fits-header ff-out "IMRED.DEBIAS"))))
	 (do-flat (and ff-flat
		       (not (cf:read-fits-header ff-out "IMRED.FLATTEN"))))
	 (imsec
	  (progn
	    (cf:move-to-extension ff-out (extdesc-n-ext extdesc))
	    (cf:read-image-section  ff-out  :type read-type)))
	 (imsec-bias
	  (when do-bias
	    (cf:move-to-extension ff-bias (extdesc-n-ext extdesc))
	    (cf:read-image-section  ff-bias  :type read-type)))
	 (imsec-flat
	  (when do-flat
	    (cf:move-to-extension ff-flat (extdesc-n-ext extdesc))
	    (cf:read-image-section  ff-flat :type read-type)))
	 (data (cf:image-section-data imsec))
	 (npix (array-total-size data)))

    (declare (type imutils:image data)
 	     (type fixnum npix))
    
    (mark-bad-pixels (cf:image-section-data imsec) 
		     (cf:read-fits-header ff-out "IMSEC.NULLVAL")
		     saturation-val invalid-func output-null-val)
    ;; note that bias and flat have already been processed, so the only possible
    ;; bad pixels are OUTPUT-NULL-VAL (assuming consistency of processing)
    (when imsec-bias  
      (mark-bad-pixels (cf:image-section-data imsec-bias) 
		       (cf:read-fits-header ff-bias "IMSEC.NULLVAL")
		       saturation-val invalid-func output-null-val))
    (when imsec-flat  
      (mark-bad-pixels (cf:image-section-data imsec-flat)
		       (cf:read-fits-header ff-flat "IMSEC.NULLVAL")
		       saturation-val invalid-func output-null-val))

    (locally 
	(declare (optimize speed))
      (when do-bias
	(loop 
	   with bdata of-type imutils:image = (cf:image-section-data imsec-bias)
	   initially
	     (when (not (equalp (array-dimensions data)
				(array-dimensions bdata)))
	       (error "Data has dim=~A but bias has ~A" 
		       (array-dimensions data)
		       (array-dimensions bdata)))
	   for i of-type fixnum below npix
	   for bval of-type single-float = (row-major-aref bdata i)
	   for ival of-type single-float = (row-major-aref data i)
	   for outval of-type single-float
	     = (if (or (= bval +invalid-pixel-value+)
		       (= ival +invalid-pixel-value+))
		   +invalid-pixel-value+
		   (- ival bval))
	   do (setf (row-major-aref data i) outval)))

      (when do-flat
	(loop 
	   with fdata of-type imutils:image = (cf:image-section-data imsec-flat)
	   for i of-type fixnum below npix
	   for fval of-type single-float = (row-major-aref fdata i)
	   for ival of-type single-float = (row-major-aref data i)
	   for outval of-type single-float
	     = (if (or (= fval +invalid-pixel-value+)
		       (= ival +invalid-pixel-value+)
		       (zerop fval)
		       (minusp fval))
		   +invalid-pixel-value+
		   (/ ival fval))
	   do (setf (row-major-aref data i) outval)))

      (when (and normalize-gain (not (= gain 1)))
	(loop 
	   for i of-type fixnum below npix
	   when (not (= (row-major-aref data i) +invalid-pixel-value+))
	   do (setf (row-major-aref data i)
		    (* (row-major-aref data i) gain)))))

    ;; now mark bad output values, setting +invalid-pixel-value+
    ;; pixels to output-null-val
    (loop for i of-type fixnum below npix
	 when (= (row-major-aref data i) +invalid-pixel-value+)
	 do (setf (row-major-aref data i) output-null-val))

    (cf:write-back-image-section imsec)
    
    ;; note that BLANK header is invalid for floating point images
    (cf:delete-fits-header ff-out "BLANK") 

    (cf:write-fits-header ff-out "IMRED.NULLVAL" 
			  (if (float-nan-p output-null-val) "NaN" output-null-val)
			  :comment "Value of NULL pixels")

    (when do-bias
      (cf:write-fits-header ff-out "IMRED.DEBIAS" 
			    (astro-time:ut-to-date-string (get-universal-time))
			    :comment "Bias subtracted by imred")
      (instrument-id:set-standard-header ff-out :debiased "YES"
						:extension (cf:fits-file-current-hdu-num ff-out)))
    (when do-flat 
      (cf:write-fits-header ff-out "IMRED.FLATTEN" 
			    (astro-time:ut-to-date-string (get-universal-time))
			    :comment "Flattened by imred")
      (instrument-id:set-standard-header ff-out :flattened "YES"
					 :extension (cf:fits-file-current-hdu-num ff-out)))

    ;; write gain as IMRED.GAIN (even though we modify original below)
    (cf:write-fits-header ff-out "IMRED.GAIN"
			  (if normalize-gain 1.0 gain)
			  :extension nil  
			  :comment "Final gain of image after imred.")
     
    (when (and normalize-gain (not (= gain 1)))
      (cf:write-fits-header ff-out "IMRED.OLDGAIN" gain :comment "Old gain value")
      (instrument-id:write-gain-for-fits ff-out 1.0 :extension nil) ;; current ext
      (cf:write-fits-header ff-out "IMRED.NORMGAIN" 
			    (astro-time:ut-to-date-string (get-universal-time))
			    :comment "Gain normalized by imred"))
    ))



(defun ccdproc (fits fits-out
		&key 
		  (bias nil)
		  (flat nil)
		  (reduction-plan *default-reduction-plan*)
		  ;; the following forces gain to not be normalized
		  ;; no matter what REDUCTION-PLAN says.  It is useful
		  ;; for making flats, so that we remain in ADUs, and the
		  ;; flux bounds on flats still make sense.
		  (never-normalize-gain nil)		
		  (if-exists :error))

  "Trim, debias, and flatten an image.

FLAT and BIAS specify fits files."

  (declare (type (member :error :supersede) if-exists))

  (when (probe-file fits-out)
    (if (eq if-exists :error)
	(error "Output file ~A exists" fits-out)
	(delete-file fits-out)))

  
  (when (not (probe-file fits)) (error "input file ~A does not exist" fits))
  (when (and bias (not (probe-file bias)))
    (error "Bias file ~A does not exist" bias))
  (when (and flat (not (probe-file flat))) 
    (error "Flat file ~A does not exist" flat))

  (let ((fits (fullfile fits))
	(fits-out (fullfile fits-out :create t :delete t)) ;; see fullfile def in utils.lisp
	(bias (if bias (fullfile bias)))
	(flat (if flat (fullfile flat)))
	;;
	(trim (reduction-plan-trim reduction-plan))
	(overscan-subtract (reduction-plan-overscan-subtract reduction-plan))
	(normalize-gain
	  (if never-normalize-gain ;; we're doing a flat, probably, and
	      nil                  ;; we want the bounds to be in ADU
	      (reduction-plan-gain-normalize reduction-plan))))
	  
	  

    (when (equal fits-out bias) 
      (error "output ~A would overwrite bias ~A" fits-out bias))
    (when (equal fits-out flat) 
      (error "output ~A would overwrite flat ~A" fits-out flat))
    (when (equal fits fits-out)
      (error "output file ~A would overwrite input file ~A" fits-out fits))

    

    (with-temporary-output-file (fits-out fits-out-tmp :extra-suffix "_TMP")
      ;; maybe trim, otherwise copy fits to fits-out-tmp - all work will be
      ;; with fits-out-tmp
      (cond ((or trim overscan-subtract)
	     ;; do them together to avoid another intermediate file
	     (trim/os-image fits fits-out-tmp reduction-plan
			    :write-type :float :if-exists :supersede))
	    (t
	     (copy-image fits fits-out-tmp :type :float :if-exists :supersede)))
  
      ;; now fits-out-tmp (not fits) must be compatible with bias and flat
      (when (not (all-images-compatible-p
		  (remove nil (list fits-out-tmp bias flat))
		  :reduction-plan reduction-plan))
	(error "fits image, bias, and flat are not compatible"))

      (when (or bias flat)
	(let (ff-flat ff-bias)
	  (unwind-protect
	       (progn
		 (when bias (setf ff-bias (cf:open-fits-file bias)))
		 (when flat (setf ff-flat (cf:open-fits-file flat)))

		 (on-error-delete-file ;; ensure that a junk fits-out-tmp is not
		     fits-out-tmp      ;; left around on an error
		
		   (cf:with-open-fits-file (fits-out-tmp  ff-out :mode :io) 
		     (loop 
		       with extdesc-list = (build-extdesc-list-for-fits 
					    fits :reduction-plan reduction-plan)
		       ;;for ihdu from 1 to (cf:fits-file-num-hdus ff-out)
		       for extdesc in extdesc-list
		       for ihdu = (extdesc-n-ext extdesc)
		       do 
			  (cf:move-to-extension ff-out ihdu)
			  (cond
			    ;; do nothing to a  non-image extension
			    ((not (extdesc-reduce-p extdesc))
			     NIL)
			    (t
			     (ccdproc-one-extension
			      ff-out ff-bias ff-flat
			      extdesc 
			      (reduction-plan-saturation-value reduction-plan) 
			      (reduction-plan-invalid-pixel-function reduction-plan)
			      (reduction-plan-output-null-pixel-value reduction-plan) 
			      normalize-gain))))
		     ;; IMRED.CCDPROC is flag that this output fits was processed by this package 
		     (cf:write-fits-header fits-out-tmp "IMRED.CCDPROC" t :comment "CCDPROC'ed by IMRED Lisp pkg")
		     ))))

	  (progn
	    (when ff-flat (cf:close-fits-file ff-flat))
	    (when ff-bias (cf:close-fits-file ff-bias))))))))
 
	   
