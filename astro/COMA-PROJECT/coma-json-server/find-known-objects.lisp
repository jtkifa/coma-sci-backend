(in-package coma-json-server)

#|

{
  "TYPE":"REQUEST",
  "COMMAND":"FIND-KNOWN-OBJECTS",
  "ID":"123abc",
  "PARAMETERS": {"FITS-FILE":"/dir/sample.fits",
                 "EXTENSION":1, // if extension is not the default one
                 //
                 // or give space-time coordinates if not giving a fits file
                 "RA":1.23,
                 "DEC":2.34,
                 "MJD":55555.0,
                 "RADIUS":100, 
                 "OBSERVATORY":"568"
                }
}


|#



(defvar *sbid-mem* (small-body-identify:make-sbid-mem))

(def-json-command find-known-objects (json-req)
  (with-json-command-setup (json-req)
  
  (let* ((fits-file (get-param "FITS-FILE"))
	 (extension (get-param "EXTENSION"))
	 (instrument (and fits-file
			  (ignore-errors
			   (and (probe-file fits-file)
				(instrument-id:identify-instrument fits-file)))))
	 (ra  (get-param "RA"))
	 (dec (get-param "DEC"))
	 (radius (get-param "RADIUS")) ;; arcsec
	 (mjd (get-param "MJD"))
	 (observatory (get-param "OBSERVATORY"))
	 (onechip-ext (when (typep instrument 'instrument-id:onechip)
			(instrument-id:get-image-extension-for-onechip-fits  fits-file)))
	 (ext-to-use (or extension onechip-ext))
	 (wcs (ignore-errors (cf:read-wcs fits-file :extension ext-to-use)))
	 (naxis1 (if instrument
		     (cf:read-fits-header fits-file "NAXIS1" :extension ext-to-use)))
	 (naxis2 (if instrument
		     (cf:read-fits-header fits-file "NAXIS2" :extension ext-to-use))))

    
    (when (and (or ra dec mjd radius observatory)
	       (not (and ra dec mjd radius observatory)))
      (return-with-error "RA-DEC-MJD-RADIUS-NOT-ALL-SPECIFIED"
			 "All of {RA,DEC,MJD,RADIUS,OBSERVATORY} must be given, or none"))
    (when (and ra fits-file)
      (return-with-error "BOTH-POSITION-AND-FITS-FILE-GIVEN"
			 "Either {RA,DEC,MJD,RADIUS,OBSERVATORY} or FITS-FILE must be given, not both."))
    (when (and fits-file (not instrument))
      (return-with-error "COULD-NOT-IDENTIFY-FITS-FILE"
			 "Could not identify type of fits file, or fits file does not exist."))
    (when (not (or ra fits-file))
      (return-with-error "LOCATION-NOT-GIVEN"
			 "One of FITS-FILE or set {RA,DEC,MJD,RADIUS} must be given"))
    (when (and fits-file (not wcs))
      (return-with-error "NO-WCS-FOUND-IN-FITS"
			 "No WCS headers found in fits."))
    (when (not ra) ;; we are using fits-file
      (multiple-value-setq (ra dec)
	(wcs:wcs-convert-pix-xy-to-ra-dec wcs (* naxis1 0.5d0) (* naxis2 0.5d0)))
      (setf mjd (or (instrument-id:get-mjd-mid-for-fits fits-file)
		    (return-with-error "COULD-NOT-GET-MJD-FROM-FITS"
				       "Could not get MJD from fits file.")))
      (setf observatory (instrument-id:get-observatory-for-instrument instrument fits-file))
      (setf radius (* 0.5d0 ;; distance from center to corner
		      (wcs:get-pixel-scale-for-wcs wcs)
		      (sqrt (+ (expt naxis1 2) (expt naxis2 2))))))
    (set-param "RA" ra)
    (set-param "DEC" dec)
    (set-param "MJD" mjd)
    (set-param "OBSERVATORY"  observatory)
    (let ((candidates (sbid:generate-candidates-with-caching 
		       ra dec mjd *sbid-mem*
		       :max-distance radius
		       :observatory observatory)))
      (loop with xpix = 0d0 and ypix = 0d0
	    for candidate in candidates
	    for is-in-frame  ;; if this is a fits file, require candidate to be in frame
	      = (or (not fits-file)
		    (progn
		      (multiple-value-setq (xpix ypix)
			(wcs:wcs-convert-ra-dec-to-pix-xy
			 wcs (sbid:candidate-ra candidate) (sbid:candidate-dec candidate)))
		      (and (<= 1 xpix naxis1)
			   (<= 1 ypix naxis2))))
	    when is-in-frame
	      collect
	      (let ((cand-hash (make-hash-table :test 'equalp)))
		(setf (gethash "OBJECT" cand-hash) (sbid:candidate-id candidate))
		(let ((parsed-object
			(ignore-errors (small-body-name:parse-small-body-name (sbid:candidate-id candidate)))))
		  (when parsed-object
		    (setf (gethash "OBJECT-PARSED-ID" cand-hash)
			  (first parsed-object))))
		(setf (gethash "DISTANCE" cand-hash) (sbid:candidate-dist candidate))
		(setf (gethash "RA" cand-hash) (sbid:candidate-ra candidate))
		(setf (gethash "DEC" cand-hash) (sbid:candidate-dec candidate))
		(setf (gethash "DELTA" cand-hash) (sbid:candidate-delta candidate))
		(setf (gethash "DRA/DT" cand-hash) (sbid:candidate-dra/dt candidate))
		(setf (gethash "DDEC/DT" cand-hash) (sbid:candidate-ddec/dt candidate))
		(setf (gethash "ORBIT" cand-hash)
		      (comet-elem-to-json (sbid:candidate-elem candidate)))
		(when fits-file
		  (setf (gethash "XPIX" cand-hash) xpix)
		  (setf (gethash "YPIX" cand-hash) ypix))
		cand-hash)
	      into candidate-hash-list
	    finally
	       (set-param "OBJECT"
			  (coerce candidate-hash-list 'vector)))))))		


		
	  
	
	
					    
	
	

    

    
    
    

	 
