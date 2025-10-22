

(in-package coma-json-server)

;; all of these have a COMA.QUALITY.XX associated
(defparameter *coma-quality-list*
  '("COMPUTED" ;; t/nil
    "PIXEL-SCALE" ;; arcsec/pix
    "PSF-NOBJ" ;; number of objects used to compute median p
    ;; 
    "PSF-FWHM-PIX" 
    "PSF-MAJOR-AXIS-PIX" 
    "PSF-MINOR-AXIS-PIX" 
    "PSF-FWHM-ARCSEC" 
    "PSF-MAJOR-AXIS-ARCSEC" 
    "PSF-MINOR-AXIS-ARCSEC" 
    "PSF-PA-PIX"  
    "PSF-PA-WORLD"
    ;;
    "MAG-5-SIGMA"      ;; mag at which error is 0.2 mag
    "MAG-10-SIGMA"     ;; mag at which error is 0.1 mag
    "NDENSITY-MAG-20"   ;; object density (per arcmin^2) brighter than 20
    "NDENSITY-5-SIGMA"  ;; object density (per arcmin^2) with 5 sigma detections
    ;;
    "GAIN"
    "INSTRUMENT"
    "SKY-BACKD-ADU-PIX"
    "SKY-BACKD-PHOTONS-PIX"
    "SKY-BACKD-ADU-ARCSEC2"
    "SKY-BACKD-PHOTONS-ARCSEC2"
    "SKY-BACKD-MAG-ARCSEC2"
    ))

(defun %read-coma-qualities-info (fits-file extension &key (fail-on-missing nil))
  (cf:with-open-fits-file (fits-file ff)
    (cf:move-to-extension ff extension)
    (when (cf:read-fits-header ff "COMA.QUALITY.COMPUTED") ;; qualities are computed
      (let ((h (make-hash-table :test 'equalp)))
	(loop for quality in *coma-quality-list*
	      for fits-keyword = (format nil "COMA.QUALITY.~A" quality)
	      do (multiple-value-bind (value dummy present?)
		     (cf:read-fits-header ff fits-keyword)
		   (declare (ignore dummy))
		   (when (and fail-on-missing (not present?))
		     (format t "%READ-COMA-QUALITY-INFO: quality <~A> missing in fits" quality))
		   (setf (gethash quality h) value)))
	h))))

;; this one is different because we give it a hash table of qualities because
;; there might be a large set
(defun %write-coma-qualities-info (fits-file qhash extension)
  (cf:with-open-fits-file (fits-file ff :mode :io)
    (cf:move-to-extension ff extension)
    (loop for quality being the hash-key of qhash
	  for value   being the hash-value of qhash
	  for fits-keyword = (format nil "COMA.QUALITY.~A" quality)
	  do
	     (when (not (find quality *coma-quality-list* :test 'equalp))
	       (format t "%WRITE-COMA-QUALITY-INFO: Quality ~A is not in *COMA-QUALITY-LIST*"
		      quality))
	     (cf:write-fits-header ff fits-keyword value))
    (cf:write-fits-header ff "COMA.QUALITY.COMPUTED" t :comment "Qualities were computed"))
  (%read-coma-qualities-info fits-file extension))
	     
    
;; IDEA - make the terapix directory be somefile_DIR/extension-NN/
;;        can do this ONLY for multi-extension fits, so we can defer doing it
;;        and not break other code
;;
;;        TRY-WCS-CAT means try using the WCS catalog if it is there, but adjust
;;        the magnitude by COMA.PHOT-CALIB.ZPMAG

(defun %get-catalog-for-computing-qualities (fits-file extension &key (try-wcs-cat t))
  (let* ((fits-dir (terapix:get-fits-directory fits-file :if-does-not-exist t))
	 (zp (or
	       (cf:read-fits-header fits-file "COMA.PHOT-CALIB.ZPMAG"
				    :extension extension)
	       (error "Could not read zeropoint header COMA.PHOT-CALIB.ZPMAG  in ~A extension ~A"
		      fits-file extension)))
	 (wcs-fit-cat-name ;; kludgy - the way terapix astrometry does it
	   (when (and fits-dir try-wcs-cat)
	     (format nil "~A/sex_WCSFIT~A.cat"
		     fits-dir
		     (if (not extension) ;; the extension tag denotes the extension
			 ""
			 (format nil "_ext-~A" extension)))))
	 (wcs-cat
	  (when (and wcs-fit-cat-name
		     (probe-file wcs-fit-cat-name))
	    (terapix:read-sextractor-catalog wcs-fit-cat-name)))
	 (new-cat
	   (when (not wcs-cat)
	     (let* ((output-cat-name (format nil "coma_qual~A.cat"
					    (if extension
						(format nil "_ext-~A" extension)
						""))))
	       (terapix:run-sextractor fits-file
				       :mag-zeropoint zp
				       :extension (1- extension)
				       :md5-avoid-rerun t
				       :output-catalog output-cat-name)
	       (terapix:read-sextractor-catalog
		(format nil "~A/~A" fits-dir output-cat-name))))))
    ;;
    ;; if we have wcs-cat, adjust the magnitude in every slot that is "MAG" but not "ERR"
    (when wcs-cat
      (loop for key being the hash-key of wcs-cat
	    for vec being the hash-value of wcs-cat
	    when (and (search "MAG" key :test 'equalp)
		      (not (search "ERR" key :test 'equalp)))
	      do
		 (loop 
		   for i below (array-total-size vec)
		   do (incf (row-major-aref vec i) zp))))
    
    (or wcs-cat new-cat)))


;; FIXME -some images have a BLANK field for blank pixels, so our
;;  computed area is wrong, which messes up the number density.
;;  For example, some PS1 images are half blank.

(defun %compute-imsec-area-minus-nans (imsec)
  (let* ((wcs (cf:image-section-wcs imsec))
	 (pixel-scale (if wcs (wcs:get-pixel-scale-for-wcs wcs)
			  (error "No pixel scale from wcs.")))
	 (pixel-area/arcmin2 (/  (expt pixel-scale 2) 3600))
	 (im (cf:image-section-data imsec)))
    (loop with nvalid = 0
	  for i below (array-total-size im)
	  for x = (row-major-aref im i)
	  when (not (float-utils:single-float-nan-or-infinity-p x))
	    do (incf nvalid)
	  finally
	     (return (* pixel-area/arcmin2 nvalid)))))
    
	 
	      

;; catalog is assumed to have corrected mags
(defun %calib-qualities-compute-qualities (qual-info catalog imsec zp instrument gain)
  (let* ((wcs (cf:image-section-wcs imsec))
	 (pixel-scale (if wcs (wcs:get-pixel-scale-for-wcs wcs))) ;; should always be true
	 (naxis1 (aref (cf:image-section-size imsec) 0))
	 (naxis2 (aref (cf:image-section-size imsec) 1))
	 (image-area/arcmin2
	   (%compute-imsec-area-minus-nans imsec)
	   #+nil
	   (* naxis1 naxis2 pixel-scale pixel-scale (/ 1 60.0 60.0)))
	 (mag-vec (gethash "MAG_BEST" catalog))
	 (mag-err-vec (gethash "MAGERR_BEST" catalog))
	 (fwhm-vec    (gethash "FWHM_IMAGE" catalog))
	 (a-vec    (gethash "A_IMAGE" catalog))
	 (b-vec    (gethash "B_IMAGE" catalog))
	 (theta-vec    (gethash "THETA_IMAGE" catalog))
	 (flag-vec (gethash "FLAGS" catalog))
	 ;; high signal to noise versions
	 a-vec/hisn  b-vec/hisn theta-vec/hisn 
	 flag-vec/hisn  fwhm-vec/hisn  mag-vec/hisn mag-err-vec/hisn)

    
    (when pixel-scale
      (setf (gethash "PIXEL-SCALE" qual-info) pixel-scale))

    ;; segregate the high-SN detections, but do this trying
    ;; to use really good detections first, then meciocre,
    ;; then poor
    (flet ((set-hisn-vecs (sn-threshold)
	     (loop for a across a-vec
		   for b across b-vec
		   for theta across theta-vec
		   for flag across flag-vec
		   for mag across mag-vec
		   for mag-err across mag-err-vec
		   for fwhm across fwhm-vec
		   when (< 0.0003 mag-err sn-threshold)
		     collect a into a%
		     and collect b into b%
		     and collect fwhm into fwhm%
		     and collect theta into theta%
		     and collect mag into mag%
		     and collect mag-err into mag-err%
		     and collect flag into flag%
		   finally
		      (setf a-vec/hisn (coerce a% 'vector))
		      (setf b-vec/hisn (coerce b% 'vector))
		      (setf fwhm-vec/hisn (coerce fwhm% 'vector))
		      (setf mag-vec/hisn (coerce mag% 'vector))
		      (setf mag-err-vec/hisn (coerce mag-err% 'vector))
		      (setf theta-vec/hisn (coerce theta% 'vector))
		      (setf flag-vec/hisn (coerce flag% 'vector)))))
      (set-hisn-vecs 0.02) ;; try using only prime objects
      (when (< (length mag-vec/hisn) 10)
	(set-hisn-vecs 0.05))
      (when (< (length mag-vec/hisn) 10)
	(set-hisn-vecs 0.10))
      (when (< (length mag-vec/hisn) 10)
	(set-hisn-vecs 0.15)))
    
	     
	     
    ;; for our estimates, use mode, but fall back on median
    (flet ((best-estimator (vec)
	     (or (and (> (length vec) 100) ;; don't compute mode of short vector
		      (ignore-errors (stats:mode-of-elements vec)))
		 (stats:median-of-elements vec))))
      
      (when (plusp (length a-vec/hisn))
	(setf (gethash "PSF-NOBJ" qual-info) (length a-vec/hisn))
	(let ((fwhm (best-estimator fwhm-vec/hisn))
	      (a (best-estimator a-vec/hisn))
	      (b (best-estimator b-vec/hisn)))
	  (setf (gethash "PSF-FWHM-PIX" qual-info) fwhm)
	  (setf (gethash "PSF-MAJOR-AXIS-PIX" qual-info) a)
	  (setf (gethash "PSF-MINOR-AXIS-PIX" qual-info) b)
	  (when pixel-scale
	    (setf (gethash "PSF-FWHM-ARCSEC" qual-info) (* pixel-scale fwhm))
	    (setf (gethash "PSF-MAJOR-AXIS-ARCSEC" qual-info) (* pixel-scale a))
	    (setf (gethash "PSF-MINOR-AXIS-ARCSEC" qual-info) (* pixel-scale b))))

	  ;; compute position angle
	  (let* ((xvec (map 'vector
			    (lambda (th) (cos (* (/ pi 180) th)))
			    theta-vec/hisn))
		 (yvec (map 'vector
			    (lambda (th) (sin (* (/ pi 180) th)))
			    theta-vec/hisn))
		 (xmed (stats:median-of-elements xvec))
		 (ymed (stats:median-of-elements yvec))
		 (pa-med (position-angle-from-xy xmed ymed)))
	    ;;
	    (setf (gethash "PSF-PA-PIX" qual-info) pa-med)
	    ;;
	    ;; if wcs is defined correctly compute PA in world coordinates
	    (when (wcs:wcs-2d-p wcs)
	      ;; convert to local equatorial coordinates, first creating
	      ;; a wcs-clone centered on 0,0
	      (let ((wcs0 (copy-structure wcs)))
		(setf (wcs:wcs-2d-crpix1 wcs0) 0d0)
		(setf (wcs:wcs-2d-crpix2 wcs0) 0d0)
		(multiple-value-bind (xwcs ywcs)
		    (wcs:wcs-convert-pix-xy-to-world-xy wcs0 xmed ymed)
		  (setf (gethash "PSF-PA-WORLD" qual-info)
			(position-angle-from-xy xwcs ywcs)))))
	    
	    ;;
	    ;; compute  MAG-5-SIGMA and MAG-10-SIGMA
	    (loop for mag across mag-vec
		  for mag-err across mag-err-vec
		  when (<= 0.18 mag-err 0.22)
		    collect mag into mag-05-list
		  when (<= 0.09 mag-err 0.11)
		    collect mag into mag-10-list
		  finally
		     (when (plusp (length mag-05-list))
		       (setf (gethash "MAG-5-SIGMA" qual-info)
			     (stats:median-of-elements mag-05-list))
		     (when (plusp (length mag-05-list))
		       (setf (gethash "MAG-10-SIGMA" qual-info)
			     (stats:median-of-elements mag-10-list)))))
	    ;;
	    ;; compute object densities
	    (loop with n-mag20 = 0
		  with n-5sigma = 0
		  for mag across mag-vec
		  for mag-err across mag-err-vec
		  when (< mag 20)
		    do (incf n-mag20)
		  when (< mag-err 0.05)
		    do (incf n-5sigma)
		  finally
		     (setf (gethash "NDENSITY-MAG-20" qual-info)
			   (/ n-mag20 image-area/arcmin2))
		     (setf (gethash "NDENSITY-5-SIGMA" qual-info)
			   (/ n-5sigma image-area/arcmin2)))

	    ;; compute sky brightness
	    (let* ((im (cf:image-section-data imsec))
		   (adu/pix (imutils:image-median 
			     im
			     :ix0 (round (* 0.25 naxis1))
			     :iy0 (round (* 0.25 naxis2))
			     :ix1 (round (* 0.75 naxis1))
			     :iy1 (round (* 0.75 naxis2))))
		   (adu/arcsec
		     (* adu/pix (/ 1d0 (expt pixel-scale 2))))
		   (mag/arcsec
		     (when (plusp adu/arcsec)
		       (+ zp (* -2.5 (log adu/arcsec 10))))))
		   ;;
	      (setf (gethash  "GAIN" qual-info) gain)
	      (setf (gethash  "INSTRUMENT" qual-info) (format nil "~A" (type-of instrument)))
	      (setf (gethash  "SKY-BACKD-ADU-PIX" qual-info)
		    adu/pix)
	       (setf (gethash  "SKY-BACKD-PHOTONS-PIX" qual-info)
		     (* gain adu/pix))
	      (setf (gethash  "SKY-BACKD-ADU-ARCSEC2" qual-info)
		    adu/arcsec)
	      (setf (gethash  "SKY-BACKD-PHOTONS-ARCSEC2" qual-info)
		    (* gain adu/arcsec))
	      (when mag/arcsec
		(setf (gethash  "SKY-BACKD-MAG-ARCSEC2" qual-info)
		      mag/arcsec)))

	    ;;
	    qual-info
	    )))))
	    
      
      
	

    
	     
	  
	  
	  
	 
	 
    

  

(defun %calib-compute-image-qualities (json-resp fits-file
				       &key extension redo-qualities-p)

    (block retblock
      (flet ((return-with-error (err-name err-desc)
	       (setf (json-object-error json-resp)
		     (make-error-object
		      :error err-name
		      :desc err-desc))
	       (return-from retblock json-resp)))
	
	(let* ((parameters (json-object-parameters json-resp))
	       (old-qualities-info (and (not redo-qualities-p)
					(%read-coma-qualities-info fits-file extension))))

	  ;; read old qualities if not redoing
	  (when old-qualities-info
	    (setf (gethash "QUALITIES-INFO" parameters) old-qualities-info)
	    (return-from retblock json-resp))

	  ;; otherwise start processing
	  (let* ((qual-info (make-hash-table :test 'equalp))
		 (inst (instrument-id:identify-instrument fits-file))
		 (gain (instrument-id:get-gain-for-instrument
			inst fits-file
			:extension extension))
		 (next (or extension
			   (when (typep inst 'instrument-id:onechip)
			     (instrument-id:get-image-extension-for-onechip-instrument
			      inst fits-file))))
		 (zp (or
		      (cf:read-fits-header fits-file "COMA.PHOT-CALIB.ZPMAG"
					   :extension extension)
		      (error
		       "Could not read zeropoint header COMA.PHOT-CALIB.ZPMAG  in ~A extension ~A"
		       fits-file extension)))
		 (imsec (multiple-value-bind (%imsec err)
			    (cf:read-image-section fits-file
						   :extension next
						   :null-value  float-utils:*single-float-nan*)
			  (when (not %imsec)
			    (return-with-error
			     "FAILED-TO-READ-IMAGE-SECTION"
			     (format nil "Failed to read image section with error <~A>" err)))
			  %imsec))
		 (catalog
		   (multiple-value-bind (cat err)
		       (%get-catalog-for-computing-qualities fits-file extension
							     :try-wcs-cat t)
		     (when (not cat)
		       (return-with-error
			"FAILED-TO-GET-CATALOG-FOR-QUALITIES"
			(format nil "Failed to get sextractor catalog with error <~A>" err)))
		     cat)))
	    ;;
	    (%calib-qualities-compute-qualities qual-info catalog imsec zp inst gain)
	    (%write-coma-qualities-info fits-file qual-info extension)
	    (setf (gethash "QUALITIES-INFO" parameters) qual-info)
	    ;;
	    json-resp)))))
	    
					
