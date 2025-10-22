
#|

{
  "TYPE":"REQUEST",
  "COMMAND":"OBJECT-LOCATION-IN-IMAGE",
  "ID":"123abc",
  "PARAMETERS": {"FITS-FILE":"/dir/sample.fits",
                 "EXTENSION":1, // if extension is not the default one

                 // the usual ORBIT object, or strings describing where to obtain the orbit
                 "ORBIT": {ID: .. EPOCH-MJD: .. ...}, // or "JPL-ORBIT" or "MPC-ORBIT" or "JPL-EPHEM"
              
                 // the MJD midpoint of this image, used with the orbit to compute
                 // the position - if not given, will try to get it from the image
                 // it from the  fits image
                 "MJD":58849.0,
 
                 // if the stamp center is determined using MPC-ORBIT, 
                 //  JPL-ORBIT, or JPL-EPHEM
                 "OBJECT-NAME": "2P", // will try to get from fits file if not given
 
               }
}

|#

(in-package coma-json-server)


(def-json-command object-location-in-image (json-req)
  (with-json-command-setup (json-req)
    (let* ((fits-file (get-param "FITS-FILE" :required t))
	   (inst nil)
	   ;; if extension=NIL then use the default single-image
	   ;; extension, or the first finite image extension
	   (extension  (get-param "EXTENSION"))
	   (object-name
	     (or (get-param "OBJECT-NAME")
		 (ignore-errors
		  (first
		   (small-body-name:parse-small-body-name 
		    (instrument-id:get-object-for-fits fits-file))))))
	   (mjd (or (ignore-errors (* 1d0 (get-param "MJD")))
		    (instrument-id:get-mjd-mid-for-fits fits-file)))
	   (json-orbit (get-param "ORBIT"))
	   orbit orbit-error
	   (require-orbit (not (equalp json-orbit "JPL-EPHEM")))
	   (observatory
	     (ignore-errors
	      (instrument-id:get-observatory-for-fits fits-file)))
	   ;;
	   ;;
	   (wcs (ignore-errors
		 (cf:read-wcs fits-file :extension extension)))
	   (naxis1 (cf:read-fits-header fits-file "NAXIS1"
					:extension extension))
	   (naxis2 (cf:read-fits-header fits-file "NAXIS2"
					:extension extension))
	   
	   ;;
	   x0/pix y0/pix ra0 dec0
	   out-of-bounds
	   err3sigma ra-err3sigma dec-err3sigma
	   %ignore)

      
      
      (when (not json-orbit) ;; this can be a METHOD, not just JSON orbit struct
	(return-with-error
	 "NO-ORBIT-GIVEN"
	 "ORBIT was not given; needs to be a JSON orbit object or one of JPL-ORBIT, MPC-ORBIT, JPL-EPHEM"))


      ;; preserve the source of the orbit error
      (multiple-value-setq (orbit orbit-error)
	(get-orbit-using-method
	 json-orbit object-name :mjd mjd))

      
      (jcom-test-expr (not (probe-file fits-file))
		      "FITS-FILE-NOT-FOUND"
		      (format nil "Fits file ~A not found" fits-file))
      (setf inst (ignore-errors
		  (instrument-id:identify-instrument fits-file)))
      
      (jcom-test-expr (not inst)
		      "COULD-NOT-IDENTIFY-FITS-FILE"
		      "Could not identify type of fits file.")
      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      (when (not orbit) 
	(return-with-error
	 "FAILED-TO-OBTAIN-ORBIT"
	 (format
	  nil
	  "Failed to parse or obtain provided ORBIT=\"~A\" - error is '~A'"
	  json-orbit orbit-error)))

      (jcom-test-expr (and require-orbit
			   (not (slalib-ephem:comet-elem-p orbit)))
		      "NO-ORBIT-OBTAINED"
		      "An orbit is required for object position.")

      (jcom-test-expr (and require-orbit (not mjd))
		      "NO-MJD-FOUND"
		      "An MJD is required along with orbit for positioning, crosshairs, or orbit direction marker, but none was given or found in headers.")

      (jcom-test-expr (and require-orbit (not observatory))
		      "COULD-NOT-GET-OBSERVATORY"
		      "Could not determine observatory to compute object location using orbit.")
		 

      (when orbit
	(multiple-value-setq  (x0/pix y0/pix ra0 dec0)
	  (ignore-errors
	   (compute-xypix-using-orbit-and-wcs orbit mjd wcs
					      :observatory observatory)))
	;;
	(when (not x0/pix)
	  (return-with-error
	   "COULD-NOT-COMPUTE-CENTER-USING-ORBIT"
	   (format nil "Could not compute stamp center X,Y using orbit ~A"
		   orbit)))
	;;
	;; get orbital errors from our external source
	(multiple-value-setq (err3sigma ra-err3sigma dec-err3sigma)
	  (ignore-errors (get-orbital-precision-for-object object-name mjd))))


      (when (equalp json-orbit "JPL-EPHEM")
	(multiple-value-setq (ra0 dec0 %ignore %ignore %ignore  ra-err3sigma dec-err3sigma)
	  (jpl-horizons:get-jpl-radecr-and-rates-for-observatory
	   object-name mjd observatory :ntries 3))
	(setf err3sigma (sqrt (+ (expt ra-err3sigma 2) (expt dec-err3sigma 2)))))


      (when (or (<  x0/pix 0)
		(<  y0/pix 0)
		(>  x0/pix naxis1)
		(>  y0/pix naxis2))
	(setf out-of-bounds t))

		 
		 
      (set-param "ORBIT" json-orbit)
      (set-param "OBJECT" object-name)
      (set-param "XPIX" x0/pix)
      (set-param "YPIX" y0/pix)
      (set-param "RA" ra0)
      (set-param "DEC" dec0)
      (set-param "OUT-OF-BOUNDS" (if out-of-bounds
				     'yason:true
				     'yason:false))

      (when err3sigma
	(set-param "ERR3SIGMA" err3sigma)
        (set-param "RA-ERR3SIGMA" ra-err3sigma)
        (set-param "DEC-ERR3SIGMA" dec-err3sigma)))))

