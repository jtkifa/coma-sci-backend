

#|

Compute a zeropoint from legacy IRAF coeff file as described in
the wiki at page openproject/projects/coma/wiki/compute-legacy-coeff-zeropoint

The coeff data is text of the form



V                    ;; filter
 0.08   0.0019       ;; airmass term and err
 0.85   0.0018       ;; (zp-24) and error of zp
-0.098 0.033         ;; color coeff, V-R
R
 0.06   0.0017
 0.94   0.0016
-0.042  0.0029       ;; color coeff, R-I
I 
 0.05   0.0029   
 0.34   0.0031
 0.05   0.00595      ;; color coeff, usually V-R but could be R-I



Request: 


{
    "TYPE":"REQUEST",
    "COMMAND":"COMPUTE-LEGACY-COEFF-ZEROPOINT",
    "ID":"123abc",
    "PARAMETERS":
          {
            "TYPE":"PARAMETERS", // OPTIONAL annotation
            "COEFF-TEXT": "......", // full context of coeff file
            "COLOR": 0.0, // OPTIONAL: the color value to assume for epsilon, defaults to 0
            "FITS-FILE":"/path/to/image.fits"
           }
}


Response:

{
    "TYPE":"RESPONSE",
    "COMMAND":"COMPUTE-LEGACY-COEFF-ZEROPOINT",
    "ID":"123abc",
    "PARAMETERS":
          {
            "TYPE":"PARAMETERS", // optional
            "ZPMAG"=25.55,       // zeropoint from coeffs, exptime, and airmass
            "ZPMAGERR"=0.0123,"  // uncertainty on zeropoint
           }
}

|#

(in-package coma-json-server)

(defparameter *sample-legacy-coeff-data*
  "V
 0.08   0.0019
 0.85   0.0018
-0.098 0.033
R
 0.06   0.0017
 0.94   0.0016
-0.042  0.0029
I 
 0.05   0.0029
 0.34   0.0031
 0.05   0.00595
")

(defstruct legacy-coeff
  filter
  k k-err
  zp24 zp24-err
  cterm cterm-err)

(defun parse-legacy-coeff-data (text)
  (declare (type string text))
  (flet ((read-2flt-line (s)
	   (let* ((line (read-line s nil nil))
		  (sline (when line
			   (string-utils:split-string line '(#\space #\tab))))
		  (val1 (when sline
			  (ignore-errors (jk-parse-float:parse-float (first sline)))))
		  (val2 (when sline
			  (ignore-errors (jk-parse-float:parse-float (second sline))))))
	     #+nil
	     (format t "LINE=~A SLINE=~A VAL1=~A VAL2=~A~%"
		     line sline val1 val2)
	     (list val1 val2))))
  (with-input-from-string (s text)
    (loop for fline = (read-line s nil nil)
	  for filt   = (if fline (string-trim '(#\space #\tab) fline))
	  for filtsym = (cond ((equal filt "V") :vj)
			      ((equal filt "B") :bj)
			      ((equal filt "R") :rc)
			      ((equal filt "I") :ic)
			      (t nil))
	  for (k kerr) = (read-2flt-line s)
	  for (zp zperr) = (read-2flt-line s)
	  for (c cerr) = (read-2flt-line s)
	  when (and fline (not (and k kerr zp zperr c cerr)))
	    do (error "Missing fields for filter ~A" filt)
	  when fline
	    collect
	    (make-legacy-coeff
	     :filter filtsym
	     :k k :k-err kerr
	     :zp24 zp :zp24-err zperr
	     :cterm c :cterm-err cerr)
	  until (not fline)))))
	       


(def-json-command compute-legacy-zeropoint (json-req)
  (with-json-command-setup (json-req)
    (let* ((coeff-text (get-param "COEFF-TEXT" :required t))
	   (fits-file  (get-param "FITS-FILE" :required t))
	   (color      (get-param "COLOR" :default 0.0)))
      (multiple-value-bind (lcoeffs lcoeff-err)
	  (ignore-errors (parse-legacy-coeff-data coeff-text))
	(jcom-test-expr (not lcoeffs)
			"ERROR-PARSING-LEGACY-COEFFICIENTS"
			(format nil "ERROR ~A parsing COEFF-TEXT" lcoeff-err))
	(when (not (probe-file fits-file))
	  (return-with-error "FITS-FILE-NOT-FOUND"
			     (format nil "Cannot find FITS-FILE ~A" fits-file)))
	(let* ((inst (or (ignore-errors (instrument-id:identify-instrument fits-file))
			 (return-with-error
			  "CANNOT-IDENTIFY-FITS-FILE"
			  (format nil "INSTRUMENT-ID cannot identify FITS-FILE ~A~%"
				  fits-file))))
	       (next (or (ignore-errors (instrument-id:get-image-extension-for-onechip-fits
					 fits-file))
			 (return-with-error
			  "CANNOT-FIND-IMAGE-EXTENSION"
			  (format nil "Cannot get onechip image ext for FITS-FILE ~A"
				  fits-file))))
	       (wcs (or (ignore-errors (cf:read-wcs fits-file :extension next))
			(return-with-error
			 "CANNOT-READ-WCS"
			 (format nil "Cannot read WCS for FITS-FILE ~A"
				 fits-file))))
	       (naxis1 (or (ignore-errors (cf:read-fits-header fits-file
							       "NAXIS1" :extension next))
			   (return-with-error
			    "CANNOT-READ-NAXIS1"
			    (format nil "Cannot read NAXIS1 for FITS-FILE ~A"
				    fits-file))))
	       (naxis2 (or (ignore-errors (cf:read-fits-header fits-file
							       "NAXIS2" :extension next))
			   (return-with-error
			    "CANNOT-READ-NAXIS2"
			    (format nil "Cannot read NAXIS2 for FITS-FILE ~A"
				    fits-file))))
	       (mjd (or (ignore-errors (instrument-id:get-mjd-mid-for-fits
					fits-file :instrument inst))
			(return-with-error
			 "CANNOT-GET-MJD-MID"
			 (format nil "Cannot get MJD-MID for for FITS-FILE ~A" fits-file))))
	       (observatory (or (ignore-errors (instrument-id:get-observatory-for-fits
						fits-file :instrument inst))
				(return-with-error
				 "CANNOT-GET-OBSERVATORY"
				 (format nil "Cannot get observatory for FITS-FILE ~a"
					 fits-file))))
	       (filter (or (ignore-errors (instrument-id:get-standard-filter-for-fits
					   fits-file :instrument inst))
			   (return-with-error
			    "CANNOT-GET-FILTER"
			    (format nil "Cannot get filter for FITS-FILE ~A~%" fits-file))))
	       (exptime
		 (or (ignore-errors (instrument-id:get-exptime-for-fits fits-file
									:instrument inst))
		     (return-with-error
		      "CANNOT-GET-EXPTIME"
		      (format nil "Cannot get EXPTIME for FITS-FILE ~a"
					 fits-file))))
	       ;; find the correct lcalib
	       (lcalib
		 (or (find filter lcoeffs :key 'legacy-coeff-filter)
		     (return-with-error
		      "CANNOT-FIND-SUITABLE-LEGACY-CALIB"
		      (format nil "Cannot get legacy calib for filter ~A" filter)))))
	  
	  (when (not (plusp exptime))
	    (return-with-error "EXPTIME-INVALID"
			       (format nil "Invalid EXPTIME ~A" exptime)))

	  (multiple-value-bind (ra dec) ;; ra,dec of image center
	      (wcs:wcs-convert-pix-xy-to-ra-dec wcs (* 0.5d0 naxis1) (* 0.5d0 naxis2))
	    (let* ((airmass (slalib-ephem:compute-airmass-for-ra-dec-for-observatory
			     ra dec mjd observatory))
		   ;; ZPMAG = 24 + (Z-24) - k.Airmass + colorTerm.Color + 2.5 log10(Exptime)
		   (zeropoint
		     (+ 24.0
			(legacy-coeff-zp24 lcalib)
			(* -1 airmass (legacy-coeff-k lcalib))
			(* color (legacy-coeff-cterm lcalib))
			(* 2.5 (log exptime 10d0))))
		   (zeropoint-err
		     (sqrt
			    (+
			     (expt (legacy-coeff-zp24-err lcalib) 2)
			     (expt (* airmass (legacy-coeff-k-err lcalib)) 2)
			     (expt (* color (legacy-coeff-cterm-err lcalib)) 2)))))

	      (set-param "ZPMAG" zeropoint)
	      (set-param "ZPMAGERR" zeropoint-err)
	      (set-param "AIRMASS" airmass)
	      (set-param "K" (legacy-coeff-k lcalib))
	      (set-param "ZP24" (legacy-coeff-zp24 lcalib))
	      (set-param "EPSILON" (legacy-coeff-cterm lcalib))
	      )))))))
	       
	
	 
			 
      
     
