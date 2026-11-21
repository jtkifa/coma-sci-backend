#|

Helper functions to calibrate wcs

WCS header dictionary

   COMA.WCS.NSTARS
   COMA.WCS.RMSFIT
   COMA.WCS.NORDER
   COMA.WCS.CATALOG
   COMA.WCS.MAXERR    - SCAMP POSITION_MAXERR (arcmin) of the successful fit

|#
 



(in-package coma-json-server)


;; CATALOG=NIL means an external precalibration (NSTARS not known).
;; ANY-CATALOG means accept a previous COMA fit made with any catalog
;; (used by the WCS kitchen sink, where the winning catalog varies).
(defun %read-coma-wcs-info (fits-file extension catalog &key any-catalog)
  (cf:maybe-with-open-fits-file (fits-file ff)
    (cf:move-to-extension ff extension)
    (when (or
	   ;; no catalog, if using precalib
	   (not catalog)
	   any-catalog
	   ;; not changing catalogs
	   (equalp (cf:read-fits-header ff "COMA.WCS.CATALOG") catalog))
      ;;
      (let ((h (make-hash-table :test 'equalp))
	    (maxerr (cf:read-fits-header ff "COMA.WCS.MAXERR")))
	(when maxerr
	  (setf (gethash "WCS-POSITION-MAXERR" h) maxerr))
	(when (and (if (and (not catalog) (not any-catalog))
		       t  ;; if external calib, NSTARS not known
		       (setf (gethash "WCS-NSTARS" h)
			     (cf:read-fits-header ff "COMA.WCS.NSTARS")))
		   (setf (gethash "WCS-RMSFIT" h)
			 (cf:read-fits-header ff "COMA.WCS.RMSFIT"))
		    (setf (gethash "WCS-NORDER" h)
			 (cf:read-fits-header ff "COMA.WCS.NORDER"))
		   (setf (gethash "WCS-CATALOG" h)
			 (cf:read-fits-header ff "COMA.WCS.CATALOG"))
		   (let ((wcs (cf:read-wcs ff)))
		     (setf (gethash "WCS-PIXEL-SCALE" h)
			   (bt-ignore-errors (wcs:get-pixel-scale-for-wcs wcs)))
		     (setf (gethash "WCS" h)
			   (hashify-wcs wcs))))
	  h)))))

;; write the WCS info and return the hash table
(defun %write-coma-wcs-info (fits-file extension wcs &key catalog nstars rmsfit norder
							maxerr)
  (cf:with-open-fits-file (fits-file ff :mode :io)
    (cf:move-to-extension ff extension)
    (cf:write-wcs wcs ff)
    (cf:write-fits-header ff "COMA.WCS.CATALOG" catalog)
    (cf:write-fits-header ff "COMA.WCS.NSTARS" nstars)
    (cf:write-fits-header ff "COMA.WCS.RMSFIT" rmsfit)
    (cf:write-fits-header ff "COMA.WCS.NORDER" norder)
    (when maxerr
      (cf:write-fits-header ff "COMA.WCS.MAXERR" maxerr
			    :comment "SCAMP POSITION_MAXERR (arcmin)")))
  (%read-coma-wcs-info fits-file extension catalog))


;; this is not well validated
(defun %compute-wcs-order (wcs)
  (cond ((typep wcs 'wcs:wcs-radec-tan-tpv)
	 (+  (count-if-not 'zerop (wcs:wcs-radec-tan-tpv-pv1vec wcs))
	     (count-if-not 'zerop (wcs:wcs-radec-tan-tpv-pv2vec wcs))))
	((typep wcs 'wcs:wcs-radec-tan-sip)
	 (flet ((count-sip-terms (sip)
		  (loop with matrix = (wcs:sip-cor-matrix sip)
			for i below (array-total-size matrix)
			when (not (zerop (row-major-aref matrix i)))
			  sum 1)))
	 (+ (count-sip-terms (wcs:wcs-radec-tan-sip-a wcs))
	    (count-sip-terms (wcs:wcs-radec-tan-sip-b wcs)))))
	((typep wcs 'wcs:wcs-radec-zpn)
	 (loop for p across (wcs:wcs-radec-zpn-pvec wcs)
	       for i from 0
	       when (and (plusp i) ;; ignore 0th term, which is 1.0
			 (not (zerop p)))
		 count 1))
	(t ;; all others
	 1)))
	       
	
	
	
	    
;; get the precalibration (eg, survey like PS1 or ATLAS)
;; from a fits file, and turn it into COMA headers 
(defun %wcs-ingest-precalibration (json-resp fits-file
				   wcs-precalibration-source
				   extension)
  (cf:maybe-with-open-fits-file (fits-file ff :mode :io)
    (cf:move-to-extension ff extension)
    (let* ((wcs (cf:read-wcs ff))
	   (pixel-scale (wcs:get-pixel-scale-for-wcs wcs)))
      (when (not wcs)
	(error "Precalibrated WCS not found."))
      (cf:write-fits-header
       ff "COMA.WCS.CATALOG" wcs-precalibration-source
       :comment "Source of external calibration.")
      (cf:write-fits-header
       ff "COMA.WCS.NSTARS" NIL
       :comment "Unknown no. stars for ext calib")
      (cf:write-fits-header
       ff "COMA.WCS.RMSFIT" 0.05
       :comment "Dummy RMS fit for ext calib.")
      (cf:write-fits-header
       ff "WCS-PIXEL-SCALE" pixel-scale
       :comment "Derived from ext calib")
      
      (cf:write-fits-header
       ff "COMA.WCS.NORDER"
       (or (bt-ignore-errors (%compute-wcs-order wcs))
	   1)
       :comment "WCS order for ext calib"))
    ;;
    (let* ((parameters (json-object-parameters json-resp)))
      (setf (gethash "WCS-CALIB-INFO" parameters)
	    (%read-coma-wcs-info ff extension NIL)))
    ;;
    json-resp))
   
	
  
 
;; WCS kitchen sink: (CATALOG POSITION-MAXERR/ARCMIN) pairs tried in order
;; until one fit meets WCS-NSTARS-MIN and WCS-RMS-MAX (which are not relaxed).
;; Small POSITION_MAXERR first, because a tight search is less prone to
;; false matches.
(defparameter *wcs-kitchen-sink-sequence*
  '(("refcat"    0.2)
    ("refcat"    1.0)
    ("refcat"    3.0)
    ("ps1-stack" 1.0)
    ("ps1-stack" 3.0)))

;; default SCAMP POSITION_MAXERR (arcmin), as in TERAPIX:DO-NONLINEAR-ASTROMETRY
(defparameter *default-wcs-position-maxerr* 2.0)

;; Pan-STARRS 3pi southern limit (deg). Kitchen-sink sequences skip PS1
;; catalogs south of this; a degree or so of slop doesn't matter.
(defparameter *ps1-dec-min* -30d0)

(defun %ps1-catalog-p (catalog)
  (member catalog '("ps1" "ps1-stack") :test 'equalp))

;; rough image dec: the WCS reference point, which is near the image
;; center - good enough for coarse survey bounds. NIL if no WCS.
(defun %approx-dec-of-fits (fits-file extension)
  (let ((wcs (bt-ignore-errors (cf:read-wcs fits-file :extension extension))))
    (and wcs (bt-ignore-errors (wcs:wcs-2d-crval2 wcs)))))

;; reason string if a kitchen-sink sequence should skip CATALOG at DEC, else NIL.
;; An unknown DEC never skips.
(defun %kitchen-sink-skip-reason (catalog dec)
  (when (and dec (%ps1-catalog-p catalog) (< dec *ps1-dec-min*))
    (format nil "skipped: image dec ~,1F is south of the PS1 limit ~,1F"
	    dec *ps1-dec-min*)))

;; one SCAMP fit attempt; does not touch JSON-RESP.
;; Returns (VALUES WCS NSTARS RMSFIT ERROR); WCS is NIL on failure.
(defun %calib-wcs-attempt (fits-file &key extension catalog position-maxerr
				       nstars-min rms-max norder allow-saturated)
  (multiple-value-bind (wcs nstars/err rmsfit)
      (bt-ignore-errors
       (terapix:do-nonlinear-astrometry fits-file
	 :extension extension
	 :nstars-min nstars-min
	 :astref-catalog catalog
	 :position-maxerr position-maxerr
	 :rms-max rms-max
	 :distort-degrees norder
	 :allow-saturated-stars allow-saturated))
    (if wcs
	(values wcs nstars/err rmsfit nil)
	(values nil nil nil (or nstars/err "Unknown error running terapix scamp")))))

(defun %calib-fit-wcs (json-resp fits-file &key extension wcs-catalog
					     wcs-fit-norder
					     redo-wcs-fit-p wcs-rms-max
					     wcs-nstars-min
					     wcs-allow-saturated
					     (wcs-position-maxerr
					      *default-wcs-position-maxerr*)
					     wcs-kitchen-sink)

  (block retblock
    (flet ((return-with-error (err-name err-desc)
	     (setf (json-object-error json-resp)
		   (make-error-object
		    :error err-name
		    :desc err-desc))
	     (return-from retblock json-resp)))
      ;;
      (let* ((parameters (json-object-parameters json-resp))
	     (old-wcs-info (and (not redo-wcs-fit-p)
				;; kitchen sink accepts a previous fit from any catalog
				(%read-coma-wcs-info fits-file extension wcs-catalog
						     :any-catalog wcs-kitchen-sink))))
	;; read old wcs, and not forcing redo-wcs, so return
	;; the old wcs-info
	(when old-wcs-info
	  (setf (gethash "WCS-INFO" parameters) old-wcs-info)
	  (return-from retblock json-resp))

	;; if necessary, create a WCS to act as starting point
	(let* ((wcs-orig (cf:read-wcs fits-file :extension extension))
	       (wcs-guess (when (not wcs-orig)
			    (instrument-id:get-initial-wcs-for-fits
			     fits-file :extension extension))))
	  (when (not (or wcs-orig wcs-guess))
	    (return-with-error
	     "FAILED-TO-GET-INITIAL-WCS-GUESS"
	     "WCS-FIT: Cannot generate an initial WCS guess."))
	  (when (not wcs-orig)
	    (cf:write-wcs wcs-guess fits-file :extension extension)))
	

	;; try each (CATALOG MAXERR) in turn - just one unless kitchen sink.
	;; A failed attempt leaves the fits headers untouched, so each
	;; attempt starts from the same initial WCS.
	(loop with sequence = (if wcs-kitchen-sink
				  *wcs-kitchen-sink-sequence*
				  (list (list wcs-catalog wcs-position-maxerr)))
	      with dec = (and wcs-kitchen-sink (%approx-dec-of-fits fits-file extension))
	      with attempts = nil and last-err = nil
	      for (catalog maxerr) in sequence
	      for t0 = (get-internal-real-time)
	      for skip = (and wcs-kitchen-sink (%kitchen-sink-skip-reason catalog dec))
	      when skip
		do (let ((h (make-hash-table :test 'equalp)))
		     (setf (gethash "CATALOG" h) catalog
			   (gethash "POSITION-MAXERR" h) maxerr
			   (gethash "OK" h) 'yason:false
			   (gethash "SKIPPED" h) skip)
		     (push h attempts))
	      else
	      do (multiple-value-bind (wcs nstars rmsfit err)
		     (%calib-wcs-attempt fits-file
					 :extension extension
					 :catalog catalog
					 :position-maxerr maxerr
					 :nstars-min wcs-nstars-min
					 :rms-max wcs-rms-max
					 :norder wcs-fit-norder
					 :allow-saturated wcs-allow-saturated)
		   (let ((h (make-hash-table :test 'equalp)))
		     (setf (gethash "CATALOG" h) catalog
			   (gethash "POSITION-MAXERR" h) maxerr
			   (gethash "OK" h) (json-bool wcs)
			   (gethash "SECONDS" h) (/ (- (get-internal-real-time) t0)
						    (* 1d0 internal-time-units-per-second)))
		     (if wcs
			 (setf (gethash "NSTARS" h) nstars
			       (gethash "RMSFIT" h) rmsfit)
			 (setf (gethash "ERROR" h) (format nil "~A" err)
			       last-err err))
		     (push h attempts))
		   ;;
		   (when wcs
		     ;; reported in both modes - a single entry if no kitchen sink
		     (setf (gethash "WCS-ATTEMPTS" parameters)
			   (coerce (reverse attempts) 'vector))
		     (let ((wcs-info (%write-coma-wcs-info
				      fits-file extension wcs
				      :catalog catalog
				      :nstars nstars
				      :rmsfit rmsfit
				      :norder wcs-fit-norder
				      :maxerr maxerr)))
		       (when (not wcs-info)
			 (return-with-error "FAILED-TO-WRITE-WCS-INFO"
					    "Failed to write WCS-INFO into headers after WCS fit."))
		       (setf (gethash "WCS-INFO" parameters) wcs-info)
		       (return-from retblock json-resp))))
	      finally
		 ;; every attempt failed
		 (setf (gethash "WCS-ATTEMPTS" parameters)
		       (coerce (reverse attempts) 'vector))
		 (return-with-error
		  "FAILED-TO-RUN-WCS-FIT"
		  (if wcs-kitchen-sink
		      (format nil "All ~D WCS kitchen-sink attempts failed (see WCS-ATTEMPTS); last error <~A>"
			      (length sequence) (or last-err "none - all skipped"))
		      (format nil "Error <~A> when running TERAPIX:DO-NONLINEAR-ASTROMETRY (terapix SCAMP)"
			      last-err))))))))
	    
						
	  
