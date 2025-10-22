#|

Helper functions to calibrate wcs

WCS header dictionary

   COMA.WCS.NSTARS
   COMA.WCS.RMSFIT
   COMA.WCS.NORDER
   COMA.WCS.CATALOG

|#
 



(in-package coma-json-server)


(defun %read-coma-wcs-info (fits-file extension catalog)
  (cf:maybe-with-open-fits-file (fits-file ff)
    (cf:move-to-extension ff extension)
    (when (or
	   ;; no catalog, if using precalib
	   (not catalog)
	   ;; not changing catalogs
	   (equalp (cf:read-fits-header ff "COMA.WCS.CATALOG") catalog))
      ;;
      (let ((h (make-hash-table :test 'equalp)))
	(when (and (if (not catalog)
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
			   (ignore-errors (wcs:get-pixel-scale-for-wcs wcs)))
		     (setf (gethash "WCS" h)
			   (hashify-wcs wcs))))
	  h)))))

;; write the WCS info and return the hash table
(defun %write-coma-wcs-info (fits-file extension wcs &key catalog nstars rmsfit norder)
  (cf:with-open-fits-file (fits-file ff :mode :io)
    (cf:move-to-extension ff extension)
    (cf:write-wcs wcs ff)
    (cf:write-fits-header ff "COMA.WCS.CATALOG" catalog)
    (cf:write-fits-header ff "COMA.WCS.NSTARS" nstars)
    (cf:write-fits-header ff "COMA.WCS.RMSFIT" rmsfit)
    (cf:write-fits-header ff "COMA.WCS.NORDER" norder))
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
       (or (ignore-errors (%compute-wcs-order wcs))
	   1)
       :comment "WCS order for ext calib"))
    ;;
    (let* ((parameters (json-object-parameters json-resp)))
      (setf (gethash "WCS-CALIB-INFO" parameters)
	    (%read-coma-wcs-info ff extension NIL)))
    ;;
    json-resp))
   
	
  

(defun %calib-fit-wcs (json-resp fits-file &key extension wcs-catalog
					     wcs-fit-norder
					     redo-wcs-fit-p wcs-rms-max
					     wcs-nstars-min)

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
				(%read-coma-wcs-info fits-file extension wcs-catalog))))
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
	

	(multiple-value-bind (wcs nstars/err rmsfit)
	    (ignore-errors
	     (terapix:do-nonlinear-astrometry fits-file
	       :extension (- extension 1) ;; zero based for terapix
	       :nstars-min wcs-nstars-min
	       :astref-catalog wcs-catalog
	       :rms-max wcs-rms-max
	       :distort-degrees wcs-fit-norder))
	  ;;
	  (when (not wcs)
	    (return-with-error
	     "FAILED-TO-RUN-WCS-FIT"
	     (if (typep nstars/err 'error)
		 (format
		  nil
		  "Error <~A> when running TERAPIX:DO-NONLINEAR-ASTROMETRY (terapix SCAMP)"
		  nstars/err)
		 "Unknown running terapix scamp")))
	  ;;
	  (let ((wcs-info (%write-coma-wcs-info
			   fits-file extension wcs
			   :catalog wcs-catalog
			   :nstars nstars/err
			   :rmsfit rmsfit
			   :norder wcs-fit-norder)))
	    (when (not wcs-info)
	      (return-with-error "FAILED-TO-WRITE-WCS-INFO"
				 "Failed to write WCS-INFO into headers after WCS fit."))
	    (setf (gethash "WCS-INFO" parameters) wcs-info)
	    json-resp))))))
	    
						
	  
