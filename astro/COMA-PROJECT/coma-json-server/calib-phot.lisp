(in-package coma-json-server)


;; CATALOG=NIL means an external precalibration (NSTARS not known).
;; ANY-CATALOG means accept a previous COMA calibration made with any
;; catalog (used by the photometric kitchen sink).
(defun %read-coma-phot-calib-info (fits-file extension catalog &key any-catalog)
  (cf:maybe-with-open-fits-file (fits-file ff)
    (cf:move-to-extension ff extension)
    (when (or
	   ;; ignore catalog for pre-calib
	   (not catalog)
	   any-catalog
	   ;; require catalog to be same as current catalog
	   (equalp (cf:read-fits-header ff "COMA.PHOT-CALIB.CATALOG") catalog))
      (let ((h (make-hash-table :test 'equalp)))
	;; STARS-ONLY is NIL when galaxies were also used as standards
	(multiple-value-bind (stars-only comment key)
	    (cf:read-fits-header ff "COMA.PHOT-CALIB.STARS-ONLY")
	  (declare (ignore comment))
	  (when key (setf (gethash "PHOT-STARS-ONLY" h) (json-bool stars-only))))
	(when (and (setf (gethash "PHOT-CATALOG" h)
		 	 (cf:read-fits-header ff "COMA.PHOT-CALIB.CATALOG"))
		   (setf (gethash "FILTER" h)
			 (cf:read-fits-header ff "COMA.PHOT-CALIB.FILTER"))
		   (if (and (not catalog) (not any-catalog))
		       t ;; if external calib, NSTARS not known
		       (setf (gethash "PHOT-NSTARS" h)
			     (cf:read-fits-header ff "COMA.PHOT-CALIB.NSTARS")))
		   ;;
		   ;; OK is special, because the value can be NIL
		   (multiple-value-bind (okval comment key)
		       (cf:read-fits-header ff "COMA.PHOT-CALIB.OK")
		     (declare (ignore comment))
		     (progn
		       (when key (setf (gethash "OK" h) (or okval 'yason:false))) ;; record the value, T or NIL
		       key)) ;; KEY was present
		   ;;
		   (setf (gethash "ZPMAG" h)
			 (cf:read-fits-header ff "COMA.PHOT-CALIB.ZPMAG"))
		   (setf (gethash "ZPMAGERR" h)
			 (cf:read-fits-header ff "COMA.PHOT-CALIB.ZPMAGERR"))
		   ;; in theory, we can fail to get an instrumental mag if we don't
		   ;; know the GAIN, so treat ZPINSTMAG specially
		   (multiple-value-bind (zpinstmag comment key)
		       (cf:read-fits-header ff "COMA.PHOT-CALIB.ZPINSTMAG")
		     (declare (ignore comment))
		     (and
		      (setf (gethash "ZPINSTMAG" h) zpinstmag)
		      key))
		   ;;
		   ;; the zp inst mag error is there, even if zpinstmag isn't
		   (setf (gethash "ZPINSTMAGERR" h)
			 (cf:read-fits-header ff "COMA.PHOT-CALIB.ZPINSTMAGERR")))
	  h)))))


(defun %write-coma-phot-calib-info (fits-file
				   extension
				   &key
				     ok-p
				     catalog filter nstars
				     zp-mag zp-mag-err
				     zp-inst-mag zp-inst-mag-err
				     (stars-only t))
  ;;
  (cf:maybe-with-open-fits-file (fits-file ff :mode :io)
    (cf:move-to-extension ff extension)
    (cf:write-fits-header ff "COMA.PHOT-CALIB.CATALOG" catalog)
    (cf:write-fits-header ff "COMA.PHOT-CALIB.STARS-ONLY" stars-only
			  :comment "F if galaxies were also used as standards")
    (cf:write-fits-header ff "COMA.PHOT-CALIB.FILTER" filter)
    (cf:write-fits-header ff "COMA.PHOT-CALIB.NSTARS" nstars)
    (cf:write-fits-header ff "COMA.PHOT-CALIB.OK" ok-p)
    (cf:write-fits-header ff "COMA.PHOT-CALIB.ZPMAG" zp-mag
			  :comment "add to -2.5 log10(ADU)")
    (cf:write-fits-header ff "COMA.PHOT-CALIB.ZPMAGERR" zp-mag-err)
    (cf:write-fits-header ff "COMA.PHOT-CALIB.ZPINSTMAG" zp-inst-mag :comment "mag giving 1e/s")
    (cf:write-fits-header ff "COMA.PHOT-CALIB.ZPINSTMAGERR" zp-inst-mag-err))
  (%read-coma-phot-calib-info fits-file extension catalog))



(defun %phot-ingest-precalibration (json-resp fits-file 
				    phot-precalibration-source
				    extension)
  (cf:with-open-fits-file (fits-file ff :mode :io)
    (cf:move-to-extension ff extension)
    (multiple-value-bind (zp-mag zp-mag-err)
	(instrument-id:get-calibrated-zeropoint-for-fits
	 fits-file
	 :extension extension
	 :flux-units :adu
	 :exposure :exptime)
      (when (not (and zp-mag zp-mag-err))
	(error "Pre-calibrated zeropoint not found in ~A" fits-file))
      ;;
      (let ((filter (instrument-id:get-standard-filter-for-fits fits-file))
	    ;; for converting to instrumental zp
	    (exptime (instrument-id:get-exptime-for-fits fits-file)))
	(when (not filter)
	  (error "Filter not found in ~A" fits-file))
	(when (not exptime)
	  (error "Exposure time not found in ~A" fits-file))
	;;
	(cf:write-fits-header
	 ff "COMA.PHOT-CALIB.CATALOG" phot-precalibration-source
	 :comment "Src of ext calib")
	(cf:write-fits-header ff "COMA.PHOT-CALIB.FILTER"
			      filter)
	(cf:write-fits-header
	 ff "COMA.PHOT-CALIB.NSTARS" NIL ;; no nstars header
	 :comment "Unknown # of stars for ext calib.")
	(cf:write-fits-header
	 ff "COMA.PHOT-CALIB.OK" t
	 :comment "Ext phot calib assumed OK.")
	(cf:write-fits-header
	 ff "COMA.PHOT-CALIB.ZPMAG" zp-mag
	 :comment "Ext. calib: add to -2.5 log10(ADU)")
	(cf:write-fits-header
	 ff "COMA.PHOT-CALIB.ZPMAGERR" zp-mag-err
	 :comment "Derived from ext calib")

	;; now 1e-/mag instrumental zp
	(multiple-value-bind (zp-inst-mag zp-inst-mag-err)
	    (instrument-id:get-calibrated-zeropoint-for-fits
	     ff
	     :extension extension
	     :flux-units :electrons
	     :exposure :one-second)
	  
	  (cf:write-fits-header
	   ff "COMA.PHOT-CALIB.ZPINSTMAG" zp-inst-mag :comment "mag giving 1e/s")
	  (cf:write-fits-header ff "COMA.PHOT-CALIB.ZPINSTMAGERR"
				zp-inst-mag-err))))

    (let* ((parameters (json-object-parameters json-resp)))
      (setf (gethash "PHOT-CALIB-INFO" parameters)
	    (%read-coma-phot-calib-info ff extension NIL)))
    json-resp))
	
      
	
      

;; photometric kitchen sink: (CATALOG STARS-ONLY) pairs tried in order until
;; a calibration is OK with at least PHOT-CALIB-NSTARS-MIN stars. STARS-ONLY=NIL
;; lets galaxies serve as brightness standards, which faint PS1-stack fields
;; often need. SDSS is left out because of its limited sky coverage, and PS1
;; catalogs are skipped south of *PS1-DEC-MIN*.
(defparameter *phot-kitchen-sink-sequence*
  '(("refcat"    t)
    ("ps1"       t)
    ("ps1-stack" t)
    ("ps1-stack" nil)))

(defun %astro-catalog-type-for-phot-catalog (phot-calib-catalog)
  (cond ((equalp phot-calib-catalog "ps1")
	 'astro-catalog:psps-3pi-mean-psf-mag-catalog)
	((equalp phot-calib-catalog "ps1-stack")
	 'astro-catalog:psps-3pi-stack-psf-mag-catalog)
	((equalp phot-calib-catalog "sdss")
	 'astro-catalog:sdss8-catalog)
	((equalp phot-calib-catalog "refcat")
	 'astro-catalog:refcat-catalog)))

(defun %calib-phot-calib (json-resp fits-file
			  &key extension phot-calib-catalog
			    redo-phot-calib-p 
			    phot-calib-nstars-min
			    phot-kitchen-sink
			    ;; the following parameters are currently fixed
			    ;; but we might want to make them flexible
			    (min-calib-flux 3000) 
			    (stars-only t)
			    (max-catalog-mag 23)
			    (min-catalog-mag 0)
			    (max-catalog-radius 60) ;; arcmin
			    (max-pos-error 1.0))
  (block retblock
    (flet ((return-with-error (err-name err-desc)
	     (setf (json-object-error json-resp)
		   (make-error-object
		    :error err-name
		    :desc err-desc))
	     (return-from retblock json-resp)))

      (when (and (not phot-kitchen-sink)
		 (not (%astro-catalog-type-for-phot-catalog phot-calib-catalog)))
	(return-with-error
	 "UNKNOWN-PHOTOMETRIC-CATALOG"
	 (format nil "Photometric catalog ~S unknown" phot-calib-catalog)))
      
      (let* ((parameters (json-object-parameters json-resp))
	     (old-phot-calib-info
	       (and (not redo-phot-calib-p)
		    (%read-coma-phot-calib-info fits-file extension phot-calib-catalog
						:any-catalog phot-kitchen-sink))))
	;; read old phot-calib, and not forcing redo-phot-calib, so return
	;; the old info - but the kitchen sink only reuses an OK calibration
	(when (and old-phot-calib-info
		   (or (not phot-kitchen-sink)
		       (eq (gethash "OK" old-phot-calib-info) t)))
	  (setf (gethash "PHOT-CALIB-INFO" parameters) old-phot-calib-info)
	  (return-from retblock json-resp))

	;; try each (CATALOG STARS-ONLY) in turn - just one unless kitchen sink
	(loop with sequence = (if phot-kitchen-sink
				  *phot-kitchen-sink-sequence*
				  (list (list phot-calib-catalog stars-only)))
	      with dec = (and phot-kitchen-sink (%approx-dec-of-fits fits-file extension))
	      with attempts = nil and last-err = nil
	      for (catalog %stars-only) in sequence
	      for t0 = (get-internal-real-time)
	      for skip = (and phot-kitchen-sink (%kitchen-sink-skip-reason catalog dec))
	      for h = (make-hash-table :test 'equalp)
	      do (setf (gethash "CATALOG" h) catalog
		       (gethash "STARS-ONLY" h) (json-bool %stars-only))
		 (push h attempts)
	      when skip
		do (setf (gethash "OK" h) 'yason:false
			 (gethash "SKIPPED" h) skip)
	      else
		do (multiple-value-bind (result err)
		       (ignore-errors
			(phot-calib:calibrate-image-with-catalog-type/mag-auto
			 (%astro-catalog-type-for-phot-catalog catalog) fits-file
			 :stars-only %stars-only
			 :min-catalog-mag min-catalog-mag
			 :max-catalog-mag max-catalog-mag
			 :max-catalog-radius (/ max-catalog-radius 60.0) ;;deg
			 :tol/arcsec max-pos-error 
			 :min-calib-stars phot-calib-nstars-min
			 :min-obj-flux min-calib-flux
			 :write-headers t)) ;; write our standard headers too
		     (setf (gethash "SECONDS" h) (/ (- (get-internal-real-time) t0)
						    (* 1d0 internal-time-units-per-second)))
		     (cond ((not result)
			    (setf (gethash "OK" h) 'yason:false
				  (gethash "ERROR" h) (format nil "~A" err)
				  last-err err))
			   (t
			    (setf (gethash "OK" h) (json-bool (phot-calib:phot-calib-result-ok result))
				  (gethash "NSTARS" h) (phot-calib:phot-calib-result-nstars result)
				  (gethash "ZPMAG" h) (phot-calib:phot-calib-result-zpmag result))
			    (when (not (phot-calib:phot-calib-result-ok result))
			      (setf (gethash "ERROR" h) "calibration not OK (likelihood peak not clean)"
				    last-err "calibration not OK (likelihood peak not clean)"))))
		     ;; the kitchen sink needs an OK result; a single attempt
		     ;; returns whatever it got, as before
		     (when (and result
				(or (not phot-kitchen-sink)
				    (phot-calib:phot-calib-result-ok result)))
		       (when phot-kitchen-sink
			 (setf (gethash "PHOT-ATTEMPTS" parameters)
			       (coerce (reverse attempts) 'vector)))
		       (let ((phot-calib-info
			       (%write-coma-phot-calib-info
				fits-file extension
				:catalog catalog
				:stars-only %stars-only
				:ok-p (phot-calib:phot-calib-result-ok result)
				:filter (phot-calib:phot-calib-result-std-filter result)
				:nstars (phot-calib:phot-calib-result-nstars result)
				:zp-mag (phot-calib:phot-calib-result-zpmag result)
				:zp-mag-err (phot-calib:phot-calib-result-zpmag-err result)
				:zp-inst-mag (phot-calib:phot-calib-result-zpmag-inst result)
				:zp-inst-mag-err (phot-calib:phot-calib-result-zpmag-inst-err result))))
			 (when (not phot-calib-info)
			   (return-with-error
			    "FAILED-TO-WRITE-PHOT-CALIB-INFO"
			    "Failed to write PHOT-CALIB-INFO into headers after photometric calibration."))
			 (setf (gethash "PHOT-CALIB-INFO" parameters) phot-calib-info)
			 (return-from retblock json-resp))))
	      finally
		 (when phot-kitchen-sink
		   (setf (gethash "PHOT-ATTEMPTS" parameters)
			 (coerce (reverse attempts) 'vector)))
		 (return-with-error
		  "PHOT-CALIB-FAILURE"
		  (if phot-kitchen-sink
		      (format nil "All ~D photometric kitchen-sink attempts failed (see PHOT-ATTEMPTS); last error <~A>"
			      (length sequence) (or last-err "none - all skipped"))
		      (format nil "Error <~A> inside PHOT-CALIB:CALIBRATE-IMAGE-WITH-CATALOG-TYPE/MAG-AUTO"
			      last-err))))))))
