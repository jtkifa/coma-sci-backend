(in-package coma-json-server)

#|

  "parameters": {"FITS-FILE":"/dir/sample.fits",
                 // default image extension, or first image one, or 1-indexed
                 // or named extension 
                 "EXTENSION":null, 
                 
                 "WCS-FIT":true,
                 "REDO-WCS-FIT":false,    // force redo if done
                 "WCS-CATALOG":"refcat",
                 "WCS-NSTARS-MIN": 8,
                 "WCS-RMS-MAX": 0.5,  // arcsec
                 "WCS-FIT-NORDER": 1,  // order of fit (1=linear)

                 "PHOT-CALIB":true,
                 "PHOT-CATALOG":"refcat", // "refcat" "ps1" or "sdss" - refcat is LOCAL
                 "REDO-PHOT-CALIB":false, // force redo if done
                 "PHOT-CALIB-NSTARS-MIN": 8,

                 "COMPUTE-IMAGE-QUALITIES":true,
                 "REDO-QUALITIES":false,

                 "CLEANUP": true // delete sample_DIR fits file directory after
                 }
|#


;; returns ONE-BASED fits extension
(defun %figure-out-fits-extension-for-calib (fits-file)
  (let ((inst (instrument-id:identify-instrument fits-file)))
    (or (if (typep fits-file 'instrument-id:onechip)
	    (instrument-id:get-image-extension-for-onechip-fits fits-file :onechip inst))
       (cf:with-open-fits-file (fits-file ff)
	 (loop for iext from 1 to  (cf:fits-file-num-hdus ff)
	       do
		  (cf:move-to-extension ff iext)
	       when
	       (let ((size (cf:fits-file-current-image-size ff))
		     (ndims (cf:fits-file-current-image-ndims ff)))
		 (and (eql ndims 2)
		      (plusp (aref size 0))
		      (plusp (aref size 1))))
	       do (return iext))))))


(defun %calib-cleanup (fits-file)
  (delete-dir-for-fits-file fits-file)) ;; in utils.lisp

(def-json-command calibrate-image (json-req)
  (with-json-command-setup (json-req)
    (let ((fits-file (get-param "FITS-FILE" :required t :satisfies 'stringp))
	  ;; if extension=NIL then use the default single-image
	  ;; extension, or the first finite image extension
	  (extension   (get-param "EXTENSION"))
	  ;;
	  (wcs-fit-p (get-param "WCS-FIT" :default t))
	  (wcs-catalog (get-param "WCS-CATALOG" :default "refcat"))
	  (redo-wcs-fit-p (get-param "REDO-WCS-FIT" :default nil))
	  (wcs-nstars-min (get-param "WCS-NSTARS-MIN" :default 8))
	  (wcs-rms-max (get-param "WCS-RMS-MAX" :default 0.5))
	  (wcs-fit-norder (get-param "WCS-FIT-NORDER" :default 1))
	  ;;
	  (phot-calib-p (get-param  "PHOT-CALIB" :default t))
	  (phot-calib-catalog (get-param "PHOT-CATALOG" :default "refcat"))
	  (redo-phot-calib-p (get-param "REDO-PHOT-CALIB" :default nil))
	  (phot-calib-nstars-min (get-param  "PHOT-CALIB-NSTARS-MIN" :default 8))
	  ;;
	  (redo-qualities-p (get-param "REDO-QUALITIES" :default nil))
	  ;;
	  ;; compute other image qualities
	  (compute-image-qualities (get-param "COMPUTE-IMAGE-QUALITIES" :default t))
	  ;;
	  (cleanup-p (get-param "CLEANUP" :default nil)))

       
      (jcom-test-expr (not (probe-file fits-file))
		      "CANNOT-LOCATE-FITS-FILE"
		      (format nil "Cannot locate fits file ~A" fits-file))
      (jcom-test-expr (not (ignore-errors (instrument-id:identify-instrument fits-file)))
		      "CANNOT-IDENTIFY-FITS-FILE"
		      (format nil
			      "Cannot identify instrument for fits file ~A"
			      fits-file))
      (jcom-test-expr (not (is-integer-in wcs-nstars-min 5 10000000))
		      "INVALID-WCS-NSTARS-MIN"
		      (format nil "Invalid WCS-NSTARS-MIN (>=5) ~A" wcs-nstars-min))
      (jcom-test-expr (not (is-real-in wcs-rms-max 0d0 10d0))
		      "INVALID-WCS-RMS-MAX"
		      (format nil "Invalid WCS-RMS-MAX (0-10 arcsec) ~A" wcs-rms-max))
      (jcom-test-expr (not (is-integer-in wcs-fit-norder 1 10))
		      "INVALID-WCS-FIT-NORDER"
		      (format nil "Invalid WCS-FIT-NORDER (1-10)) ~A" wcs-fit-norder))
      (jcom-test-expr (not (is-integer-in phot-calib-nstars-min 3 10000000))
		      "INVALID-PHOT-CALIB-NSTARS-MIN"
		      (format nil "Invalid PHOT-CALIB-NSTARS-MIN (>=3) ~A"
			      phot-calib-nstars-min))
      (jcom-test-expr (not
		       (or extension
			   (setf extension
				 (ignore-errors (%figure-out-fits-extension-for-calib fits-file)))))
		      "CANNOT-FIGURE-OUT-EXTENSION"
		      (format nil
			      "Cannot figure out which extension to use for ~A"
			      fits-file))
		       
       
       
       
   

      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; invocation is OK, so do the calibration
      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      (unwind-protect ;; protect cleanup
	   (let* ((instrument
		    (or (ignore-errors (instrument-id:identify-instrument fits-file))
			(return-with-error
			 "CANNOT-IDENTIFY-FITS-FILE"
			 (format nil
				 "INSTRUMENT-ID:IDENTIFY-INSTRUMENT failed for ~A"
				 fits-file))))
		  (wcs-precalibration
		    (instrument-id:preproc-wcs-origin instrument))
		  (phot-precalibration
		    (instrument-id:preproc-phot-calib-origin instrument)))
	     
	     
	     ;; WCS, if precalibrated
	     (when (and wcs-fit-p wcs-precalibration)
	       (multiple-value-bind (jresp err)
		   (ignore-errors
		    (%wcs-ingest-precalibration json-resp fits-file
						wcs-precalibration
						extension))
		 (when (not jresp)
		   (return-with-error
		    "ERROR-IN-WCS-INGEST-PRECALIBRATION"
		    (format nil "ERROR <~A> in %WCS-INGEST-PRECALIBRATION"
			    err)))))

    
	     ;; WCS, if no errors present
	     (when (and wcs-fit-p
			(not wcs-precalibration)
			(not (json-object-error json-resp)))
	       (multiple-value-bind (jresp err)
		   (ignore-errors
		    (%calib-fit-wcs
		     json-resp fits-file
		     :extension extension
		     :wcs-catalog wcs-catalog
		     :redo-wcs-fit-p redo-wcs-fit-p
		     :wcs-rms-max wcs-rms-max
		     :wcs-nstars-min wcs-nstars-min
		     :wcs-fit-norder wcs-fit-norder))
		 (when (not jresp)
		   (return-with-error
		    "ERROR-IN-WCS"
		    (format nil "ERROR <~A> in %CALIB-FIT-WCS" err)))))

	     #+nil
	     (format t "WCS succeeded with WCS-INFO=~A~%"
		     (alexandria:hash-table-alist
		      (gethash "WCS-CALIB-INFO"
			       (json-object-parameters json-resp))))
	  
	     ;; phot-calib, if precalibrated
	     (when (and phot-calib-p phot-precalibration)
	       (multiple-value-bind (jresp err)
		   (ignore-errors
		    (%phot-ingest-precalibration json-resp fits-file
						 phot-precalibration
						 extension))
		 (when (not jresp)
		   (return-with-error
		    "ERROR-IN-PHOT-INGEST-PRECALIBRATION"
		    (format nil "ERROR <~A> in %PHOT-INGEST-PRECALIBRATION"
			    err)))))
	      
	  
	     ;; PHOT-CALIB, if no errors present
	     (when (and phot-calib-p
			(not phot-precalibration)
			(not (json-object-error json-resp)))
	       (multiple-value-bind (jresp err)
		   (ignore-errors
		    (%calib-phot-calib
		     json-resp fits-file
		     :extension extension
		     :phot-calib-catalog phot-calib-catalog
		     :redo-phot-calib-p redo-phot-calib-p
		     :phot-calib-nstars-min phot-calib-nstars-min))
		 (when (not jresp)
		   (return-with-error
		    "ERROR-IN-PHOT-CALIB"
		    (format nil "ERROR <~A> in %CALIB-PHOT-CALIB" err)))))
	  
	     ;; QUALITIES, if no errors present
	     (when (and compute-image-qualities
			(not (json-object-error json-resp)))
	       (multiple-value-bind (jresp err)
		   (ignore-errors
		    (%calib-compute-image-qualities
		     json-resp fits-file
		     :extension extension
		     :redo-qualities-p redo-qualities-p))
		 (when (not jresp)
		   (return-with-error
		    "ERROR-IN-COMPUTE-IMAGE-QUALITIES"
		    (format nil
			    "ERROR <~A> in %CALIB-COMPUTE-IMAGE-QUALITIES" err)))))) 
	;; unwind-protect'ed cleanup
	(when cleanup-p
	  (%calib-cleanup fits-file)))))) ;; top macro returns JSON-RESP
      
				  
		  
		
	      
