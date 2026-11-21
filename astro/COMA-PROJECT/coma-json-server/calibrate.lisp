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
                 "WCS-ALLOW-SATURATED": true,
                 "WCS-POSITION-MAXERR": 2.0, // SCAMP POSITION_MAXERR, arcmin
                 // WCS-KITCHEN-SINK tries the (catalog, position-maxerr)
                 // sequence in *WCS-KITCHEN-SINK-SEQUENCE* (refcat 0.2',1',3',
                 // then ps1-stack 1',3') until a fit meets WCS-NSTARS-MIN and
                 // WCS-RMS-MAX. ps1-stack is skipped south of dec
                 // *PS1-DEC-MIN* (-30). WCS-CATALOG and WCS-POSITION-MAXERR
                 // are ignored when it is on. Each try is reported in
                 // WCS-ATTEMPTS (a single entry when it is off).
                 "WCS-KITCHEN-SINK": false,


                 "PHOT-CALIB":true,
                 "PHOT-CATALOG":"refcat", // "refcat" "ps1" "ps1-stack" "sdss" - refcat is LOCAL
                 "REDO-PHOT-CALIB":false, // force redo if done
                 "PHOT-CALIB-NSTARS-MIN": 8,
                 // PHOT-KITCHEN-SINK tries *PHOT-KITCHEN-SINK-SEQUENCE*
                 // (refcat, ps1, ps1-stack stars only, ps1-stack allowing
                 // galaxies as standards) until a calibration is OK with
                 // PHOT-CALIB-NSTARS-MIN stars. PS1 catalogs are skipped south
                 // of dec *PS1-DEC-MIN* (-30). PHOT-CATALOG is ignored when
                 // it is on. Each try is reported in PHOT-ATTEMPTS (a
                 // single entry when it is off).
                 "PHOT-KITCHEN-SINK": false,

                 "COMPUTE-IMAGE-QUALITIES":true,
                 "REDO-QUALITIES":false,

                 "CLEANUP": true // delete sample_DIR fits file directory after
                 }
|#


;; returns ONE-BASED fits extension
(defun %figure-out-fits-extension-for-calib (fits-file)
  (let ((inst (instrument-id:identify-instrument fits-file)))
    (or (if (typep inst 'instrument-id:onechip)
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


;; return the 1-based HDU number if EXTENSION (a name or a 1-based index)
;; refers to a nonempty 2D image in FITS-FILE, else NIL
(defun %validate-calib-extension (fits-file extension)
  (when (or (stringp extension)
	    (and (integerp extension) (<= 1 extension 1023)))
    (ignore-errors
     (cf:with-open-fits-file (fits-file ff)
       (cf:move-to-extension ff extension :throw-error t)
       (let ((size  (cf:fits-file-current-image-size ff))
	     (ndims (cf:fits-file-current-image-ndims ff)))
	 (when (and (eql ndims 2)
		    (plusp (aref size 0))
		    (plusp (aref size 1)))
	   (cf:fits-file-current-hdu-num ff)))))))

(defun %calib-cleanup (fits-file)
  (delete-dir-for-fits-file fits-file)) ;; in utils.lisp

(def-json-command calibrate-image (json-req)
  (with-json-command-setup (json-req)
    (let ((fits-file (get-param "FITS-FILE" :required t :satisfies 'stringp))
	  ;; if extension=NIL then use the default single-image
	  ;; extension, or the first finite image extension
	  (extension   (get-param "EXTENSION"))
	  ;;
	  (wcs-fit-p (get-param "WCS-FIT" :default t
				:satisfies 'is-json-boolean
				:satisfies-desc "must be true or false"))
	  (wcs-catalog (get-param "WCS-CATALOG" :default "refcat"))
	  (redo-wcs-fit-p (get-param "REDO-WCS-FIT" :default nil
				     :satisfies 'is-json-boolean
				     :satisfies-desc "must be true or false"))
	  (wcs-nstars-min (get-param "WCS-NSTARS-MIN" :default 8))
	  (wcs-rms-max (get-param "WCS-RMS-MAX" :default 0.5))
	  (wcs-fit-norder (get-param "WCS-FIT-NORDER" :default 1))
	  (wcs-allow-saturated (get-param "WCS-ALLOW-SATURATED" :default T
					  :satisfies 'is-json-boolean
					  :satisfies-desc "must be true or false"))
	  (wcs-position-maxerr (get-param "WCS-POSITION-MAXERR"
					  :default *default-wcs-position-maxerr*))
	  (wcs-kitchen-sink (get-param "WCS-KITCHEN-SINK" :default nil
				       :satisfies 'is-json-boolean
				       :satisfies-desc "must be true or false"))
	  ;;
	  (phot-calib-p (get-param "PHOT-CALIB" :default t
				   :satisfies 'is-json-boolean
				   :satisfies-desc "must be true or false"))
	  (phot-calib-catalog (get-param "PHOT-CATALOG" :default "refcat"))
	  (redo-phot-calib-p (get-param "REDO-PHOT-CALIB" :default nil
					:satisfies 'is-json-boolean
					:satisfies-desc "must be true or false"))
	  (phot-calib-nstars-min (get-param  "PHOT-CALIB-NSTARS-MIN" :default 8))
	  (phot-kitchen-sink (get-param "PHOT-KITCHEN-SINK" :default nil
					:satisfies 'is-json-boolean
					:satisfies-desc "must be true or false"))
	  ;;
	  (redo-qualities-p (get-param "REDO-QUALITIES" :default nil
				       :satisfies 'is-json-boolean
				       :satisfies-desc "must be true or false"))
	  ;;
	  ;; compute other image qualities
	  (compute-image-qualities (get-param "COMPUTE-IMAGE-QUALITIES" :default t
					      :satisfies 'is-json-boolean
					      :satisfies-desc "must be true or false"))
	  ;;
	  (cleanup-p (get-param "CLEANUP" :default t
				:satisfies 'is-json-boolean
				:satisfies-desc "must be true or false")))

      (jcom-test-expr (not (probe-file fits-file))
		      "CANNOT-LOCATE-FITS-FILE"
		      (format nil "Cannot locate fits file ~A" fits-file))
      (jcom-test-expr (not (bt-ignore-errors (instrument-id:identify-instrument fits-file)))
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
      ;; the kitchen sinks choose their own catalogs and position error, so
      ;; WCS-CATALOG, WCS-POSITION-MAXERR and PHOT-CATALOG are simply ignored
      ;; (not checked) when they are on - lenient with the caller
      (jcom-test-expr (and (not wcs-kitchen-sink)
			   (not (is-real-in wcs-position-maxerr 0.01d0 60d0)))
		      "INVALID-WCS-POSITION-MAXERR"
		      (format nil "Invalid WCS-POSITION-MAXERR (0.01-60 arcmin) ~A"
			      wcs-position-maxerr))
      (jcom-test-expr (not (is-integer-in phot-calib-nstars-min 3 10000000))
		      "INVALID-PHOT-CALIB-NSTARS-MIN"
		      (format nil "Invalid PHOT-CALIB-NSTARS-MIN (>=3) ~A"
			      phot-calib-nstars-min))
      (jcom-test-expr (not
		       (or extension
			   (setf extension
				 (bt-ignore-errors (%figure-out-fits-extension-for-calib fits-file)))))
		      "CANNOT-FIGURE-OUT-EXTENSION"
		      (format nil
			      "Cannot figure out which extension to use for ~A"
			      fits-file))

      ;; reject an extension that isn't a nonempty 2D image, but keep
      ;; EXTENSION as given (name or number) rather than normalizing it
      (jcom-test-expr (not (%validate-calib-extension fits-file extension))
		      "INVALID-EXTENSION"
		      (format nil
			      "EXTENSION ~S is not a valid 2D image extension in ~A"
			      extension fits-file))

      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; invocation is OK, so do the calibration
      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      (unwind-protect ;; protect cleanup
	   (let* ((instrument
		    (or (bt-ignore-errors (instrument-id:identify-instrument fits-file))
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
		   (bt-ignore-errors
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
		   (bt-ignore-errors
		    (%calib-fit-wcs
		     json-resp fits-file
		     :extension extension
		     :wcs-catalog wcs-catalog 
		     :redo-wcs-fit-p redo-wcs-fit-p
		     :wcs-rms-max wcs-rms-max
		     :wcs-nstars-min wcs-nstars-min
		     :wcs-fit-norder wcs-fit-norder
		     :wcs-allow-saturated wcs-allow-saturated
		     :wcs-position-maxerr wcs-position-maxerr
		     :wcs-kitchen-sink wcs-kitchen-sink))
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
		   (bt-ignore-errors
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
		   (bt-ignore-errors
		    (%calib-phot-calib
		     json-resp fits-file
		     :extension extension
		     :phot-calib-catalog phot-calib-catalog
		     :redo-phot-calib-p redo-phot-calib-p
		     :phot-calib-nstars-min phot-calib-nstars-min
		     :phot-kitchen-sink phot-kitchen-sink))
		 (when (not jresp)
		   (return-with-error
		    "ERROR-IN-PHOT-CALIB"
		    (format nil "ERROR <~A> in %CALIB-PHOT-CALIB" err)))))
	  
	     ;; QUALITIES, if no errors present
	     (when (and compute-image-qualities
			(not (json-object-error json-resp)))
	       (multiple-value-bind (jresp err)
		   (bt-ignore-errors
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
      
				  
		  
		
	      
