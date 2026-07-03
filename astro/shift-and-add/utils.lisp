
(in-package shift-and-add)



(defmacro with-error-catching (location-string saaplan &body body)
  `(multiple-value-bind (value error)
       (ignore-errors ,@body)
     (if (not value)
	 (progn (saaplan-log-format
		 ,saaplan
		 "SHIFT-AND-ADD - at location ~A caught error ~A" ,location-string
		 error)
		nil)
	 value)))




;; circularly mask out (set mask-value, typically zero)
(defun mask-circle-in-int16-image  (ix iy im r mask-value)
  (declare (type (signed-byte 28) ix iy)
	   (type (signed-byte 16) mask-value)
	   (type (simple-array (unsigned-byte 16) (* *)) im)
	   (type single-float r))
  (let* ((nx (array-dimension im 1))
	 (ny (array-dimension im 0))
	 (r2 (expt r 2))
	 (jx0 (max (round (- ix r)) 0))
	 (jx1 (min (round (+ ix r)) (1- nx)))
	 (jy0 (max (round (- iy r)) 0))
	 (jy1 (min (round (+ iy r)) (1- ny))))

    (loop for jx from jx0 to jx1
	  do (loop for jy from jy0 to jy1
		   for rr2 = (+ (expt (float (- ix jx)) 2)
				(expt (float (- iy jy)) 2))
		   when (<= rr2 r2)
		     do (setf (aref im jy jx) mask-value)))))
    

;; get the extension that the image is in - if an identifiable
;; instrument, use that, otherwise assume first extension
(defun get-image-extension (fits)
  (let ((inst (instrument-id:identify-instrument fits)))
    (if inst
	(instrument-id:get-image-extension-for-onechip-fits fits)
	1)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; image shifting routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; create a version of fits-file shifted by  dra,ddec in arcsec
(defun shift-fits-by-dra-ddec (fits fits-out-base dra ddec saaplan
			       &key
				 (header-suffix "saahead")
				 (verbose t)
				 (where nil) ;; where, for logging
				 (weight-type "NONE") ;; for terapix
				 (weight-suffix "weight.fits")
				 (combine-type nil) ;; override saaplan-swarp-combine-type
				 (delete-headerfile nil))
  (create-shifted-head-file fits dra ddec saaplan
			    :header-suffix header-suffix
			    :verbose verbose
			    :where where)
   (terapix:run-swarp  
	    (list fits)
	    fits-out-base    
	    :verbose-type (saaplan-verbose-type saaplan)
	    :output nil
	    :weight-type weight-type
	    :weight-suffix (concatenate 'string "."
					(string-right-trim "." weight-suffix))
	    :combine-type (or combine-type
			      (saaplan-swarp-combine-type saaplan))
	    :resampling-type (saaplan-swarp-resampling-type saaplan)
	    :copy-keywords (append (if (saaplan-mjd-keyword saaplan)
				       (list (saaplan-mjd-keyword saaplan)))
				   (instrument-id:get-critical-headers-for-fits
				    fits))
	    :header-suffix header-suffix)
  
  (when delete-headerfile
    (delete-file (format nil "~A.~A" fits-out-base header-suffix))))
			    

;; shift an image fits to be on top of ref-fits 
(defun shift-fits-relative-to-fits (ref-fits fits fits-out-base saaplan
				    &key
				      (sign +1) ;; +1 to shift from fits to ref-fits
				      (header-suffix "saahead")
				      (verbose t)
				      (where nil) ;; where, for logging
				      (weight-type "NONE") ;; for terapix
				      (weight-suffix "weight.fits")
				      (combine-type nil) ;; override saaplan-swarp-combine-type
				      (delete-headerfiles nil)
				      (delete-weight+dir  nil))

  (let* ((headerfile
	   (create-shifted-head-file-relative-fits   ref-fits fits saaplan
						     :header-suffix header-suffix
						     :sign sign
						     :verbose verbose
						     :where where))
	 (n-ext (get-image-extension ref-fits))
	 (wcs (cf:read-wcs ref-fits :extension n-ext))
	 ;; create a headerfile-out that matches the WCS of fits-ref
	 (headerfile-out (format nil "~A.~A" fits-out-base header-suffix))
	 (naxis1 (cf:read-fits-header ref-fits "NAXIS1" :extension n-ext))
	 (naxis2 (cf:read-fits-header ref-fits "NAXIS2" :extension n-ext)))

    (make-head-file-from-wcs headerfile-out wcs)
  ;;
    (terapix:run-swarp  
     (list fits)
     fits-out-base
     :image-size (list naxis1 naxis2)
     :verbose-type (saaplan-verbose-type saaplan)
     :output nil
     :weight-type weight-type
     :weight-suffix (concatenate 'string "."
				 (string-right-trim "." weight-suffix))
     :combine-type (or combine-type
		       (saaplan-swarp-combine-type saaplan))
     :resampling-type (saaplan-swarp-resampling-type saaplan)
     :copy-keywords (append (if (saaplan-mjd-keyword saaplan)
				(list (saaplan-mjd-keyword saaplan)))
			    (instrument-id:get-critical-headers-for-fits
			     fits))
     :header-suffix header-suffix)
    ;;
    (when delete-headerfiles
      (delete-file headerfile)
      (delete-file headerfile-out))
    
    (when delete-weight+dir
      (ignore-errors
       (delete-file (format nil "~A.~A" fits-out-base weight-suffix)))
      (uiop:delete-directory-tree
       (pathname (format nil "~A_DIR/" fits-out-base))
       :validate t
       :if-does-not-exist :ignore))

    
    (values (format nil "~A.fits" fits-out-base)
	    headerfile )))



;; make a terapix swarphead file with the given suffix, to shift fits back
;; onto ref-fits - returns shifted header file name
(defun create-shifted-head-file-relative-fits
    (ref-fits fits saaplan
     &key
       (sign +1) ;; sign of shift (+ is fits onto ref-fits)
       (header-suffix "saahead")
       (verbose t)
       (where nil)) ;; where, for logging
  (multiple-value-bind (dra ddec)
      (get-radec-shift-for-fits-and-fits-ref saaplan fits ref-fits)
    ;; 
    (setf dra  (* dra sign)
	  ddec (* ddec sign))
    (cond ((not dra)
	   (saaplan-log-format 
	    saaplan 
	    "SHIFT-AND-ADD: shifter failed on~%   ~A, ~A with error ~A" 
	    fits ref-fits ddec )
	   nil)
	  (t 
	   (create-shifted-head-file ;; returns header file
	    fits dra ddec
	    saaplan
	    :header-suffix header-suffix
	    :verbose verbose
	    ;; a bit of a kludge - expand the 'where' string to describe which
	    ;; files are being shifted
	    :where (when (and where verbose)
		     (format nil "~A (between ~A and ~A) "
			     where
			      (file-io:file-minus-dir ref-fits) 
			      (file-io:file-minus-dir fits)
			      )))))))



  

(defun create-shifted-head-file (fits dra ddec saaplan
				 &key
				   (header-suffix "saahead")
				   (verbose t)
				   (where nil)) ;; where, for logging
  (let* ((basefile (file-io:file-basename fits))
	 (outheaderfile (concatenate 'string basefile "." header-suffix))
	 (n-ext (get-image-extension fits))
	 (wcs (cf:read-wcs fits :extension n-ext))
	 (ra   (wcs:wcs-radec-tan-crval1 wcs))
	 (dec  (wcs:wcs-radec-tan-crval2 wcs))
	 (ra-new (+ ra (/ dra (cos (* (/ pi 180) dec)))))
	 (dec-new (+ dec ddec)))

    (when verbose
      ;;
      (saaplan-log-format 
       saaplan
       "SHIFT-AND-ADD~A: shifting ~,5F, ~,5F deg to make ~A"
       (if (not where) "" (format nil " @~A " where))
       dra ddec
      
       (file-io:file-minus-dir outheaderfile)))
    
    ;; fix the wcs
    (setf  (wcs:wcs-radec-tan-crval1 wcs) ra-new)
    (setf  (wcs:wcs-radec-tan-crval2 wcs) dec-new)
    (make-head-file-from-wcs outheaderfile wcs)
    outheaderfile))


;; given a WCS, make a Terapix-style head file.
;; WARNING - VERY SENSITIVE TO THE FORMAT, like single vs double quotes
(defun make-head-file-from-wcs (head-file wcs)
  (with-open-file (sout head-file :direction :output :if-exists :supersede
			   :if-does-not-exist :create)
    (when (not (typep wcs 'wcs:wcs-radec-tan))
      (error "WCS ~A is not WCS:WCS-RADEC-TAN" wcs))
    (format sout "CTYPE1  = 'RA---TAN'~%") 
    (format sout "CTYPE2  = 'DEC--TAN'~%") 
    (format sout "RADESYS = 'ICRS'~%")
    (format sout "EQUINOX = ~,8F~%" (wcs:wcs-radec-tan-equinox wcs))
    (format sout "CRVAL1  = ~,8F~%" (wcs:wcs-radec-tan-crval1 wcs))
    (format sout "CRVAL2  = ~,8F~%" (wcs:wcs-radec-tan-crval2 wcs))
    (format sout "CRPIX1  = ~,5F~%" (wcs:wcs-radec-tan-crpix1 wcs))
    (format sout "CRPIX2  = ~,5F~%" (wcs:wcs-radec-tan-crpix2 wcs))
    (format sout "CD1_1   = ~,8,,,,,'eE~%" (wcs:wcs-radec-tan-cd1_1 wcs))
    (format sout "CD1_2   = ~,8,,,,,'eE~%" (wcs:wcs-radec-tan-cd1_2 wcs))
    (format sout "CD2_1   = ~,8,,,,,'eE~%" (wcs:wcs-radec-tan-cd2_1 wcs))
    (format sout "CD2_2   = ~,8,,,,,'eE~%" (wcs:wcs-radec-tan-cd2_2 wcs))
    (format sout "CUNIT1  = ~A~%" "'deg'")
    (format sout "CUNIT2  = ~A~%" "'deg'")
    (format sout "END      ~%")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; a helper function to build a stationary stack
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun make-stationary-stack-name (saaplan
				   &key
				     (name "backdsky")
				     (append-suffix nil) ;; add .fits
				     (stack-dir nil))    ;; add _DIR
  (format nil "~A/~A_~A~A"
	  (string-right-trim "/" (saaplan-output-directory saaplan))
	  (saaplan-imageout-base saaplan)
	  name
	  (cond (append-suffix ".fits")
		(stack-dir "_DIR")
		(t ""))))


;; this has to to build an extension by extension duplicate (not respecting actual data)
;; but with the weight in the right extension
(defun build-stationary-stack (saaplan fits-list &key (force-rebuild nil))
  (let ((stack-name (make-stationary-stack-name saaplan
						:append-suffix t))
	(keywords-to-copy
	  (cons (saaplan-mjd-keyword saaplan)
			    (instrument-id:get-critical-headers-for-fits
			     (first fits-list))))
	(outfits
	  (make-stationary-stack-name saaplan  :append-suffix nil)))

    ;;(format t "Making ~A from ~A~%" outfits fits-list)
    ;;(format t "Keywords: ~A~%" keywords-to-copy)
    
    (when (or force-rebuild
	      (not (probe-file stack-name)))
      (terapix:run-swarp  
       fits-list
       outfits
       :verbose-type (saaplan-verbose-type saaplan)
       :output nil 
       :weight-type "NONE"
       :weight-suffix ".weight.fits"
       :combine-type "median"
       :resampling-type (saaplan-swarp-resampling-type saaplan)
       :copy-keywords keywords-to-copy
       :header-suffix "headXXXX")) ;; don't use a header for stationary stack
    ;;
    stack-name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
