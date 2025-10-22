
#|

Determine which extension an obect is in, and return the extension
number, the pixel location within it, whether the WCS has been fit,
and whether it is in fact a multi-extension fits file.

{
  "TYPE":"REQUEST",
  "COMMAND":"WHICH-EXTENSION",
  "ID":"123abc",
  "PARAMETERS": {
                 "TYPE":"PARAMETERS", // optional
                 "FITS-FILE":"/dir/sample.fits",

                 // POSITION is either a "OBJECT" to use the orit, or
                 //   a J2000 vector [RA,DEC]
                 "POSITION": "OBJECT",

                 // the usual ORBIT object, or strings describing where to obtain the orbit
                 "ORBIT": {ID: .. EPOCH-MJD: .. ...}, // or "JPL-ORBIT" or "MPC-ORBIT"
 
                 "MJD":58849.0, // MJD of midpoint of exposure; get from image if not given

                 // if the stamp center is determined using MPC-ORBIT, 
                 //  JPL-ORBIT, or JPL-EPHEM
                 "OBJECT-NAME": "2P", // will try to get from fits file if not given
               }
              
}

A  response will look like:

{
    "TYPE":"RESPONSE”,
    "COMMAND": "WHICH-EXTENSION",
    "ID": "123abc",
    "PARAMETERS": {
         "TYPE": "PARAMETERS",
         "IS-MULTI-EXTENSION": true, // it is in fact a multi-ext (not single image) fits
         "EXTENSION": 10, // extension number, starting at 1 - NULL if none
         "EXTNAME": "chip11", // or NULL
         "OBJECT-NAME":"2P",
         "WCS-IS-FIT": true, // is the WCS fit?  if not, PERHAPS don't trust the answer
                             // but it should generally be OK if WCS initial guess is good

          // the used object RA,DEC  (J2000)
         "RA-OBJECT":74.78420406894371,
         "DEC-OBJECT":18.87491825408268,
         "XPIX": 99.0,       // pixel location in EXTENSION
         "YPIX": 101.0
    }

}


|#

(in-package coma-json-server)


(def-json-command which-extension (json-req)
  (with-json-command-setup (json-req)
    (let* ((fits-file (get-param "FITS-FILE" :required t))
	   ;; if extension=NIL then use the default single-image
	   ;; extension, or the first finite image extension
	   (object-name-given (get-param "OBJECT-NAME"))
	   (mjd-given (let ((mjd (get-param "MJD")))
			(when (and mjd (not (realp mjd)))
			  (return-with-error "INVALID-MJD" "MJD is invalid - not a real number."))
			mjd))
	   (json-orbit (get-param "ORBIT")) ;; this can be an orbit object, or string MPC-ORBIT, JPL-ORBIT
	   (position (get-param "POSITION"))
	   
	   ;; for inst, throw an error if FITS-FILE not found, then another one
	   ;; if fits file can't be ID'd
	   (inst  (progn
		     (when (not (ignore-errors (probe-file fits-file)))
		       (return-with-error
			"FITS-FILE-NOT-FOUND"
			(format nil "Fits file ~A not found" fits-file)))
		     ;;
		    (or (ignore-errors
			 (instrument-id:identify-instrument fits-file))
		      (return-with-error  "COULD-NOT-IDENTIFY-FITS-FILE"
					  "Could not identify type of fits file."))))
	   ;;
	   (object-name
	     (or object-name-given
		 (ignore-errors
		  (first
		   (small-body-name:parse-small-body-name 
		    (instrument-id:get-object-for-fits fits-file))))))
	   ;;
	   (mjd (or (and mjd-given (* 1d0 mjd-given))
		    (instrument-id:get-mjd-mid-for-fits fits-file)))
	  
	   (observatory
	     (ignore-errors
	      (instrument-id:get-observatory-for-fits fits-file)))
	   ;;
	   (require-orbit ;; do we need an orbit?
	     (equalp position "OBJECT"))
	   ;;
	   orbit orbit-error ra dec)

      

      ;; preserve the source of the orbit error
      (when json-orbit
	(multiple-value-setq (orbit orbit-error)
	  (get-orbit-using-method
	   json-orbit object-name :mjd mjd)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (when (not json-orbit)
	(when (or (not position)
		  (not (vectorp position))
		  (not (every 'realp position)))
	  (return-with-error "INVALID-POSITION-VECTOR"
			     "POSITION was not 'ORBIT', so must be a vector of decimal [RA,DEC] in J2000")))

      ;;
      ;;
      (when (and json-orbit (not orbit))
	(return-with-error
	 "FAILED-TO-OBTAIN-ORBIT"
	 (format
	  nil
	  "Failed to parse or obtain provided ORBIT=\"~A\" - error is '~A'"
	  json-orbit orbit-error)))


      (when (and require-orbit
		 (not (slalib-ephem:comet-elem-p orbit)))
	(return-with-error
	 "NO-ORBIT-OBTAINED"
	 "An orbit is required for object position - valid ORBIT not specified?"))

      (when (and require-orbit (not mjd))
	(return-with-error
	 "NO-MJD-FOUND"
	 "An MJD is required along with orbit for positioning, crosshairs, or orbit direction marker, but none was given or found in headers."))

      (when (and require-orbit (not observatory))
	(return-with-error
	 "COULD-NOT-GET-OBSERVATORY"
	 "Could not determine observatory to compute object location using orbit."))

      (if (not require-orbit)
	  (progn
	    (when (setf ra  (* 1d0 (aref position 0))))
	    (when (setf dec (* 1d0 (aref position 1)))))
	  (progn
	    (multiple-value-setq (ra dec)
	      (ignore-errors
	       (slalib-ephem:compute-radecr-from-comet-elem-for-observatory
		orbit mjd observatory)))
	    (when (not ra)
	      (return-with-error
	       "ORBIT-COMPUTATION-FAILED" 
	       (format nil "Failed to compute error - error <~A>" dec))))) ;; dec has error object


      ;; open fits file and march through extensions
      (let (ext-of-obj  ;; final extension
	    xpix ypix   ;; pixel location
	    multichip-p
	    extname
	    wcs-is-fit) 
	(cf:with-open-fits-file (fits-file ff)
	  (flet ((is-in-extension (iext) ;; move to iext, and return T and set ext-of-obj, xpix,ypix
		   (cf:move-to-extension ff iext)
		   (let* ((naxis  (cf:fits-file-current-image-ndims ff))
			  (is-image (and (eq (cf:fits-file-current-hdu-type ff) :image)
					 (eql naxis 2)))
			  ;; NAXIS1,2 headers weird in case of compressed so use FF object
			  (naxis1 (if is-image (aref (cf:fits-file-current-image-size ff) 0)))
			  (naxis2 (if is-image (aref (cf:fits-file-current-image-size ff) 1)))
			  (wcs    (if is-image
				      (ignore-errors ;; cfht can have bogus WCS with no CDi_j in extension 1
				       (cf:read-wcs ff)))))
		     #+nil
		     (format t "IEXT=~A  WCS=~A NAXIS1=~A NAXIS2=~A~%"
			     iext (type-of wcs) naxis1 naxis2)
		     (when (and naxis1 naxis2 wcs)
		       (multiple-value-bind (xp yp)
			   (ignore-errors ;; could be a non-linear WCS that crashes away from region of validity
			    (wcs:wcs-convert-ra-dec-to-pix-xy wcs ra dec))
			 (when (and xp yp
				    (<= 1 xp naxis1)
				    (<= 1 yp naxis2))
			   (setf ext-of-obj iext
				 xpix xp
				 ypix yp)
			   (setf extname (cf:read-fits-header ff "EXTNAME"))
			   t)))))
		 (is-wcs-fit? () ;; in current extension
		   (or (instrument-id:preproc-wcs-origin-for-fits ff) ;; preproc WCS
		       (cf:read-fits-header ff "WCSFITOK"))))         ;; our own WCS
		 
	    (cond
	      ;; multipchip: search all image extensions
	      ((typep inst 'instrument-id:multichip)
	       (setf multichip-p t)
	       (setf wcs-is-fit (is-wcs-fit?))
	       (loop for iext from 1 to (cf:fits-file-num-hdus ff)
		     until (is-in-extension iext)))
	      ;; onechip: search just the actual image extension
	      ((typep inst 'instrument-id:onechip)
	       (setf multichip-p nil)
	       (setf wcs-is-fit (is-wcs-fit?))
	       (is-in-extension (instrument-id:get-image-extension-for-onechip-instrument inst fits-file))))))

	;; at this point, ext-of-obj, xpix, ypix, and multipchip-p,wcs-is-calibrated  are set

	(set-param "IS-MULTI-EXTENSION" (json-bool multichip-p))
	(set-param "EXTENSION" ext-of-obj)
	(set-param "EXTNAME" extname)
	(set-param "OBJECT-NAME" object-name)
	(set-param "WCS-IS-FIT" (json-bool wcs-is-fit))
	(set-param "RA-OBJECT" ra)		
	(set-param "DEC-OBJECT" dec)
	(set-param "XPIX" xpix)
	(set-param "YPIX" ypix)))))
	
	  
	     
							      
					      
		     


      
