
#|

{
    "TYPE":"REQUEST",
    "COMMAND":"DESCRIBE-FITS",  // or "DESCRIBE-FITS-LONG"
    "ID":"123abc",
    "PARAMETERS": {
        "TYPE":"PARAMETERS",
        "FITS-FILE":"./test.fits",
        "FIX-OBJECT-NAME":true 
    }
}



|#




(in-package coma-json-server)

;; get the RA,DEC for a fits file, using WCS in the imaging extension, or in the first
;; usable extension if multiple
#+ignore ;; this is the old version that uses NAXIS1,2 (invalid for fzip) and doesn't look
         ;; at all extensions
(defun %get-ra-dec-for-fits (ff instrument)
  (multiple-value-bind (wcs next)
      ;; try to find one wcs, preferring cf:read-wcs but also trying initial
      (ignore-errors
       (cond ((typep instrument 'instrument-id:onechip)
	      (ignore-errors
	       (values
		(or (cf:read-wcs ff :extension
				 (instrument-id:get-image-extension-for-onechip-fits ff))
		    (instrument-id:get-initial-wcs-for-fits ff))
		(instrument-id:get-image-extension-for-onechip-fits ff))))
	     ;;
	     (t ;; loop over all extensions
	      (loop for iext from 1 to (cf:fits-file-num-hdus ff)
		    for wcs = (ignore-errors
			       (or
				(cf:read-wcs ff :extension iext)
				(instrument-id:get-initial-wcs-for-fits ff
									:extension iext)))
		    when wcs do (return (values wcs iext))))))
    (if (typep wcs 'wcs:wcs-2d)
	(let ((xcent (ignore-errors
		      (* 0.5d0 (cf:read-fits-header ff "NAXIS1" :extension next))))
	      (ycent (ignore-errors
		      (* 0.5d0 (cf:read-fits-header ff "NAXIS2" :extension next)))))
	  ;; try to use center pixels
	  (if (and xcent ycent)
	      (wcs:wcs-convert-pix-xy-to-ra-dec wcs xcent ycent)
	      ;; else fall back on crval1, crval2
	      (values (wcs:wcs-2d-crval1 wcs)
		      (wcs:wcs-2d-crval2 wcs)))))))

(defun %get-ra-dec-radius-for-fits-extension (ff instrument iext)
  (cf:with-fits-extension (ff iext)
    (when (and (eq (cf:fits-file-current-hdu-type ff) :image)
	       (eql (length (cf:fits-file-current-image-size ff)) 2)) ;; 2d image
      (let* ((inst (or instrument (instrument-id:identify-instrument ff))) ;; for easier debugging 
	     (wcs (ignore-errors
		   (or
		    (cf:read-wcs ff :extension iext)
		    (instrument-id:get-initial-wcs-for-fits
		     ff :extension iext :instrument inst)))))
	(when (typep wcs 'wcs:wcs-2d)
	  ;; don't use naxis1,naxis2 because image can be compressed
	  (let* ((naxis1 (aref (cf:fits-file-current-image-size ff) 0))
		 (naxis2 (aref (cf:fits-file-current-image-size ff) 1))
		 (pix-scale (wcs:get-pixel-scale-for-wcs wcs))
		 (xcent (* 0.5d0 naxis1))
		 (ycent (* 0.5d0 naxis2))
		 (image-radius/arcmin ;; arcsec distance from center to corner
		   (* (/ 1d0 60) pix-scale (sqrt (+ (expt xcent 2) (expt ycent 2))))))
	    ;; try to use center pixels
	    (multiple-value-bind (ra0 dec0)
		(wcs:wcs-convert-pix-xy-to-ra-dec wcs xcent ycent)
	      (values ra0 dec0 image-radius/arcmin))))))))


(defun %get-ra-dec-radius-for-fits (ff instrument)
  (declare (optimize debug))
  (let* ((inst (or instrument (instrument-id:identify-instrument ff)))) ;; for easier debugging
    (cond
      ;; for one-chip instrument, get the single extension
      ((typep inst 'instrument-id:onechip)
	   (%get-ra-dec-radius-for-fits-extension 
	    ff inst
	    (instrument-id:get-image-extension-for-onechip-fits ff :onechip inst)))
      ;; for multipchip, acculumate all the centers and radii, and pick center of those
      ((typep inst 'instrument-id:multichip)
       (let* ((ra-dec-r-list
		(loop with outlist = nil
		      for iext from 1 to (cf:fits-file-num-hdus ff)
		      do (multiple-value-bind (ra dec r/arcmin)
			     (%get-ra-dec-radius-for-fits-extension ff inst iext)
			   (when ra
			     (push (list ra dec r/arcmin) outlist)))
		      finally (return outlist)))
	      
	     ;; now generate a bigger list of (RA DEC) taking several points in each circle to define the boundary
	     (ra-dec-list-expanded
	       (loop with outlist = nil
		     for (ra dec r) in ra-dec-r-list
		     do
			(loop for theta from 0.0 to 359.0 by 30.0 ;; angles around each chip's circle
			      for theta/rad = (* theta (/ pi 180))
			      for dx = (* r (cos theta))
			      for dy = (* r (sin theta))
			      do
				 (multiple-value-bind (ra/theta dec/theta)
				     (astro-coords:sky-angles-slew ra dec dx dy :units :arcmin)
				   (push (list ra/theta dec/theta) outlist)))
		     finally
			(return outlist))))
	 
	 (when ra-dec-list-expanded ;; will be NULL if no image extensions
	   (multiple-value-bind (ra-center dec-center radius/deg)
	       (astro-obj:find-bounding-radec-circle-for-ra-dec-vecs
		(map 'vector 'first ra-dec-list-expanded)
		(map 'vector 'second ra-dec-list-expanded))
	     (values
	      ra-center
	      dec-center
	      (* 60 radius/deg)))))))))
	    
		     
	 
			      
	  
    
    


;; return a hash table describing an extension
(defun %describe-extension-for-fits (ff iext)
  (let ((h (make-hash-table :test 'equal)))
    ;; there is no speed advatage to moving to relative extension
    (cf:move-to-extension ff iext)
    (setf (gethash "TYPE" h) "IMAGE-EXTENSION-DECRIPTION")
    (setf (gethash "HDU-NUM" h) iext)
    (setf (gethash "HDU-TYPE" h) (json-ify (cf:fits-file-current-hdu-type ff)))
    (when (cf:fits-file-current-image-size ff)
      (setf (gethash "IMAGE-SIZE" h) (json-ify (cf:fits-file-current-image-size ff))))
    (when (cf:fits-file-current-image-type ff)
      (setf (gethash "IMAGE-TYPE" h) (json-ify (cf:fits-file-current-image-type ff))))
    (setf (gethash "EXT-NAME" h) (json-ify (cf:fits-file-current-hdu-name ff)))
    (when (cf:read-fits-header ff "WCSFITOK")
      (setf (gethash "WCS-IS-FIT" h) t))
    (when (cf:read-fits-header ff "PCZPMAG")
      (setf (gethash "PHOTCALIB" h) t))


    (when (equalp (gethash "HDU-TYPE" h) "IMAGE")
      (let ((primary-science-image
	      (ignore-errors (instrument-id:get-image-extension-for-onechip-fits ff))))
	;;
	(when primary-science-image
	  (setf (gethash "PRIMARY-SCIENCE-IMAGE" h) 
		(json-bool (equalp primary-science-image (gethash "HDU-NUM" h)))))
	(setf (gethash "COMPRESSED" h)
	      (json-bool (cf:compressed-image-p ff)))
	(let ((est-gain (ignore-errors (instrument-id:get-gain-for-fits ff :extension iext))))
	  (setf (gethash "GAIN-ESTIMATE" h) est-gain))))
    ;;
    (bt-ignore-errors
      (let ((wcs (cf:read-wcs ff :extension iext)))
	(when wcs
	  (setf (gethash "WCS" h) (hashify-wcs wcs)))))
    ;;
    h))
    
     

(defun %do-describe-fits (json-req &key (long-form nil))
  (with-json-command-setup (json-req)
    (let ((fits-file (get-param "FITS-FILE" :required t)))

      (jcom-test-expr (not (probe-file fits-file))
		  "FITS-NOT-FOUND"
		  (format nil "Fits file ~A not found" fits-file))


      (cf:with-open-fits-file (fits-file ff)
	(let* ((instrument (or (instrument-id:identify-instrument fits-file)
			       (return-with-error
				"FILE-NOT-IDENTIFIABLE"
				(format nil "Fits file ~A not identifiable" fits-file))))
	       (mjd-start (bt-ignore-errors (instrument-id:get-mjd-start-for-fits  ff)))
	       (mjd-mid   (bt-ignore-errors (instrument-id:get-mjd-mid-for-fits  ff)))
	       (mjd mjd-mid)
	       (filter    (bt-ignore-errors
			    (string (or (instrument-id:get-standard-filter-for-fits ff)
					:unknown))))
	       (exptime (bt-ignore-errors (instrument-id:get-exptime-for-fits ff)))
	       (reduction-keyword
		 (instrument-id:is-reduced-for-fits ff))
	       (is-reduced (not (not reduction-keyword))) ;; turn into bool
	       (fixname-p (get-param "FIX-OBJECT-NAME")))
	       	  

	  
	  
	  (set-param "FITS-FILE" fits-file)
	  (set-param "INSTRUMENT" (string (type-of instrument)))
	  (set-param "MJD-START" mjd-start)
	  (set-param "MJD-MID" mjd-mid)
	  (set-param "FILTER" filter)
	  (set-param "EXPTIME" exptime)
	  (set-param "IS-REDUCED" is-reduced)
	  (set-param "REDUCTION-SOFTWARE" (string reduction-keyword))


	  ;; object name section
	  (let* ((original-object-name
		   (bt-ignore-errors (instrument-id:get-object-for-fits ff)))
		 (coma-object-name ;; possible override
		   (bt-ignore-errors
		     (cf:read-fits-header ff "COMA.OBJECT" :extension 1)))
		 ;;
		 ;; if we are fixing, call namefix-object-name
		 (fixed-name-list
		   (when fixname-p
		     (multiple-value-list
		      (namefix-object-name original-object-name)))) 
		 (fixed-id (when fixname-p (first fixed-name-list))) ;; eg  "9P"
		 ;;
		 (fixed-parsed-list  ;; eg '("9P" "Tempel" :comet)
		   (when fixname-p (fourth fixed-name-list))) 
		 ;;
		 (original-parsed-list
		   (ignore-errors
		    (small-body-name:parse-small-body-name
		     original-object-name)))
		    ;;
		    (coma-object-parsed-list
		      (when coma-object-name  ;; this is to extract the type
			(ignore-errors
			 (small-body-name:parse-small-body-name
			  coma-object-name))))
		    ;;
		    (parsed-list
		      (or coma-object-parsed-list
			  fixed-parsed-list
			  original-parsed-list))
		    ;;
		    ;; highest precedence is COMA.OBJECT, then fixed name,
		    ;; the our parsed name
		    (final-parsed-name
		      (first parsed-list))) 
	       ;;
	       ;; OBJECT is 1) COMA.OBJECT; 2) fixed object; 3) full raw original object
	    (set-param "OBJECT" 
		       (or coma-object-name
			   fixed-id
			   original-object-name))
	       ;;
	       (when original-object-name
		 (set-param "ORIGINAL-OBJECT-RAW" original-object-name))
	       (set-param "OBJECT-PARSED-ID"  final-parsed-name)
	       (when parsed-list
		 (set-param "OBJECT-PARSED-COMMON-NAME"
			    (second parsed-list))
		   (set-param "OBJECT-PARSED-INFERRED-TYPE"
			      (string-upcase (format nil "~A" (third parsed-list)))))
	    (set-param "OBJECT-SOURCE" 
		       (cond (coma-object-name "COMA")
			     (fixed-id "NAMEFIX")
			     (t "HEADER"))))
	  ;; end of object name section
	       
	       

	  ;; object type section
	  (let* ((original-obs-type ;; really this is a symbol
		   (bt-ignore-errors (instrument-id:get-object-type-for-fits ff)))
		 (original-obs-type-string ;; convert to a string
		   (if original-obs-type (string-upcase (string original-obs-type))))
		 (coma-obs-type-string ;; this is always a string
		   (bt-ignore-errors
		     (cf:read-fits-header ff "COMA.OBSTYPE" :extension 1))))
	    (set-param "OBSTYPE"
		       (or coma-obs-type-string  original-obs-type-string))
	    (when coma-obs-type-string ;; set the original, if there is a secondary
	      (set-param "ORIGINAL-OBSTYPE" 
			 original-obs-type-string)))	     


	  (multiple-value-bind (ra dec radius/arcmin)
	      (bt-ignore-errors (%get-ra-dec-radius-for-fits ff instrument))
	     ;;
	     (bt-ignore-errors
	       (let* ((observatory-name (instrument-id:get-observatory-for-fits ff))
		      (observatory (observatories:get-observatory observatory-name)))
		 (when observatory-name
		   (set-param  "OBSERVATORY"  observatory-name))
		 (when observatory
		   (bt-ignore-errors
		     (set-param "OBSCODE"
				(observatories:observatory-obscode observatory)))
		   (bt-ignore-errors
		     (set-param "AIRMASS"
				(slalib-ephem:compute-airmass-for-ra-dec-for-observatory
				 ra dec mjd observatory))))
	    
		 (when (and ra dec)
		   (set-param "RA-J2000-APPROX" ra)
		   (set-param "DEC-J2000-APPROX" dec)
		   (set-param "FIELD-RADIUS-ARCMIN-APPROX" radius/arcmin)
		   (multiple-value-bind (l-ii b-ii)
		       (astro-coords:j2000->galactic-ii ra dec)
		     (when (and observatory mjd)
		       (let* ((hour-angle (slalib-ephem:compute-hour-angle-for-ra-for-observatory
					   ra mjd observatory )))
			 (multiple-value-bind (alt az)
			     (astro-coords:convert-declination-ha-to-alt-az
			      dec 
			      (* hour-angle 15) ;; to degrees
			      :latitude (observatories:observatory-latitude observatory))
			   (set-param "HOUR-ANGLE-APPROX"  hour-angle)
			   (set-param "GALACTIC-LII-APPROX"  l-ii)
			   (set-param "GALACTIC-BII-APPROX"  b-ii)
			   (set-param "TELESCOPE-ALT-APPROX" alt)
			   (set-param "TELESCOPE-AZ-APPROX"  az)
			   ))))))))
	  ;; end of (multiple-value-bind (ra dec) ...)
	  
	  (bt-ignore-errors
	    (set-param "NUM-EXTENSIONS"  (cf:fits-file-num-hdus ff)))
	  
	  (when long-form
	    (bt-ignore-errors
	      (set-param "EXTENSIONS" 
			 (coerce
			  (loop for iext from 1 to (cf:fits-file-num-hdus ff)
				collect (%describe-extension-for-fits ff iext))
			  'vector))))

	  ;; (print (alexandria:hash-table-alist parameters-out))
	  )))))

	     
	     
	     
	   
		 
(def-json-command describe-fits (json-req)
  (%do-describe-fits json-req :long-form nil))


(def-json-command describe-fits-long (json-req)
  (%do-describe-fits json-req :long-form t))
    
    
   
