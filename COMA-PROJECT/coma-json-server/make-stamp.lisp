
#|

{
  "TYPE":"request",
  "COMMAND":"MAKE-STAMP",
  "ID":"123abc",
  "PARAMETERS": {"FITS-FILE":"/dir/sample.fits",
                 "EXTENSION":1, // if extension is not the default one

                 "STAMP-FILE":"/stamp_dir/sample_stamp.pdf",
                 "STAMP-TYPE":"PDF", //  PDF, PS, or GIF 
                
                 // the method used to determine the stamp center
                 "STAMP-CENTER"=[1000,1100], // [xpix,ypix] or [ra,dec] or "OBJECT"
                 "STAMP-CENTER-UNIT"="PIXEL", // "PIXEL" (in orig. image) or "EQUATORIAL"


                 // the usual ORBIT object, or strings describing where to obtain the orbit
                 "ORBIT": {ID: .. EPOCH-MJD: .. ...}, // or "JPL-ORBIT" or "MPC-ORBIT"
              
                 // the MJD midpoint of this image, used with the orbit to compute
                 //  the position - if not given, will try to get it from the image
                 it from the // fits image
                 "MJD":58849.0,
 
                 // if the stamp center is determined using MPC-ORBIT, 
                 //  JPL-ORBIT, or JPL-EPHEM
                 "OBJECT-NAME": "2P", // will try to get from fits file if not given
 
                 "STAMP-SIZE": 500.0,  //  size (side length) 
                 "STAMP-SIZE-UNIT": "PIXEL", // "PIXEL" (default) or "ARCSEC"

                 // optional crosshair location - crosshairs are specfied as "NONE"
                 //    or a [x,y] or [ra,dec] vector, or as "CENTER" (of stamp) or "OBJECT"
                 "CROSSHAIR-LOCATION":[100,200], // or "CENTER" or "OBJECT" or "NONE" (default)
                 // unit is "PIXEL", "UNIT" (0..1) or "EQUATORIAL", 
                 "CROSSHAIR-LOCATION-UNIT": "PIXEL", 

                 // optional toplabel, as PGPLOT formatted string
                 "TOPLABEL": "This is the TOPLABEL"

                 // optional scale bar, giving pixel scale
                 "SCALE-BAR": true,    // true by default, so false disables

                 // optional compass (N,E axes) showing directions
                 "COMPASS": true,      // true by default, so false disables

                 // optional orbit direction axes, showing orbit and solar directions
                 //   this requires an ORBIT field to be present
                 "ORBIT-DIRECTIONS": false // false by default, so true enables
                 }
}

|#

(in-package coma-json-server)

;; lock on stamp making - don't want to open more than maximum
;; number of pgplot devices. Pgplot is not the best  package for this.
(defparameter *make-stamp-lock* (bt:make-lock "make-stamp-lock"))

(def-json-command make-stamp (json-req)
  (with-json-command-setup (json-req)
    (let* ((fits-file (get-param "FITS-FILE" :required t))
	   ;; if extension=NIL then use the default single-image
	   ;; extension, or the first finite image extension
	   (extension  (get-param "EXTENSION" :default nil))
	   (stamp-file  (get-param "STAMP-FILE" :required t))
	   (stamp-type  (get-param "STAMP-TYPE" :default "pdf"))
	   (stamp-center (get-param "STAMP-CENTER"))
	   (stamp-center-unit
	     (get-param "STAMP-CENTER-UNIT" :default "PIXEL"))
	   (stamp-size (get-param "STAMP-SIZE" :default 512))
	   (stamp-size-unit
	     (get-param "STAMP-SIZE-UNIT" :default "PIXEL"))
	   (crosshair-location
	     (get-param "CROSSHAIR-LOCATION" :default "NONE"))
	   (crosshair-location-method
	     (get-param "CROSSHAIR-LOCATION-METHOD" :default "VECTOR"))
	   (crosshair-location-unit
	     (get-param "CROSSHAIR-LOCATION-UNIT" :default "PIXEL"))
	   (scale-bar (get-param "SCALE-BAR" :default t))
	   (compass-rose (get-param "COMPASS" :default t))
	   (orbit-directions ;; NO by default
	     (get-param "ORBIT-DIRECTIONS" :default nil)) 
	   (toplabel (get-param "TOPLABEL"))
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
	   (observatory
	     (ignore-errors
	      (instrument-id:get-observatory-for-fits fits-file)))
		  
	   ;;
	   (wcs (ignore-errors
		 (cf:read-wcs fits-file :extension extension)))
	   ;; in case image is compressed, don't use literal NAXIS1,2
	   (naxes (get-fits-naxes fits-file :extension extension))
	   (naxis1 (aref naxes 0))
	   (naxis2 (aref naxes 1))
	   ;;
	   (pixel-scale (if wcs (wcs:get-pixel-scale-for-wcs wcs)))
	   (require-orbit ;; do we need an orbit?
	     (or (equalp stamp-center "OBJECT")
		 (equalp crosshair-location-method "OBJECT")
		 orbit-directions))
	   x0/pix y0/pix size/pix
	   ;; the crosshairs object for later retrieval of ra,dec
	   ;; for reporting purposes
	   crosshairs 
	   marker-list)

      ;; preserve the source of the orbit error
      (when json-orbit
	(multiple-value-setq (orbit orbit-error)
	  (get-orbit-using-method
	   json-orbit object-name :mjd mjd)))
	     
      ;;
      (when (not (probe-file fits-file))
	(return-with-error
	 "FITS-FILE-NOT-FOUND"
	 (format nil "Fits file ~A not found" fits-file)))
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
	 "An orbit is required for stamp-centering, crosshairs, or orbit direction marker."))
      
      (when (and require-orbit (not mjd))
	(return-with-error
	 "NO-MJD-FOUND"
	 "An MJD is required along with orbit for stamp-centering, crosshairs, or orbit direction marker, but none was given or found in headers."))
      
      (when (and require-orbit (not observatory))
	(return-with-error
	 "COULD-NOT-GET-OBSERVATORY"
	 "Could not determine observatory to compute object location using orbit."))
      
      ;;
      ;; change strings to symbols
      (setf stamp-type
	    (cond ((equalp stamp-type "PDF") :pdf)
		  ((equalp stamp-type "PS") :ps)
		  ((equalp stamp-type "GIF") :gif)
		  (t (return-with-error
		      "INVALID-STAMP-TYPE"
		      (format nil "Unknown stamp type ~A"
			      stamp-type)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (cond
	((not (and (realp stamp-size) (plusp stamp-size)))
	 (return-with-error
	  "INVALID-STAMP-SIZE" "Stamp size is not a positive number"))
	((equalp stamp-size-unit "pixel")
	 (setf size/pix stamp-size))
	;;		   
	((equalp stamp-size-unit "arcsec")
	 (when (not pixel-scale)
	   (return-with-error
	    "NO-PIXEL-SCALE"
	    "STAMP-SIZE-UNIT is ARCSEC but no WCS for pixel scale."))
	 (setf size/pix (/ stamp-size pixel-scale)))
	;;
	(t
	 (return-with-error
	  "INVALID-STAMP-SIZE-UNIT" "STAMP-SIZE-UNIT is not PIXEL or ARCSEC")))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;
      (when (> size/pix 10000)
	(return-with-error
	 "STAMP-TOO-LARGE"
	 (format nil "STAMP has size=~A - this is larger than limit of 10000 pixels"
		 size/pix)))
      
      (when (and toplabel (not (stringp toplabel)))
	(return-with-error
	 "INVALID-TOPLABEL"
	 "TOPLABEL is not a string"))
      
      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; put the correct stamp center into x0/pix, y0/pix
      (cond ((and (vectorp stamp-center)
		  (every 'realp stamp-center)
		  (equalp stamp-center-unit "PIXEL"))
	     (when (not (and (vectorp stamp-center)
			     (= (length stamp-center) 2)))
	       (return-with-error
		"STAMP-CENTER not found"
		"STAMP-CENTER parameter not given, for STAMP-CENTER-METHOD=PIXEL"))
	     (setf x0/pix (aref stamp-center 0))
	     (setf y0/pix (aref stamp-center 1)))
	    ;;
	    ((and  (vectorp stamp-center)
		   (every 'realp stamp-center)
		   (equalp stamp-center-unit "EQUATORIAL"))
	     (when (not (and (vectorp stamp-center)
			     (= (length stamp-center) 2)))
	       (return-with-error
		"STAMP-CENTER-NOT-FOUND"
		"STAMP-CENTER parameter not given, for STAMP-CENTER-METHOD=EQUATORIAL"))
	     (when (not (wcs:wcs-2d-p wcs))
	       (return-with-error
		"NO-WCS-FOUND"
		"No valid 2D WCS to convert STAMP-CENTER-METHOD=EQUATORIAL to pixels."))
	     (multiple-value-setq (x0/pix y0/pix)
	       (wcs:wcs-convert-ra-dec-to-pix-xy
		wcs  (aref stamp-center 0)  (aref stamp-center 1))))
	    ;; if we're using an orbital or ephem approach to centering,
	    ;; check inputs, and call the conversion function in utils
	    ;;
	    ((equalp stamp-center "OBJECT")
	     ;; we've already tested if MJD and ORBT and observatory are present
	     (multiple-value-setq  (x0/pix y0/pix)
	       (ignore-errors
		(compute-xypix-using-orbit-and-wcs orbit mjd wcs
						   :observatory observatory)))
	     ;;
	     (when (not x0/pix)
	       (return-with-error
		"COULD-NOT-COMPUTE-CENTER-USING-ORBIT"
		(format nil "Could not compute stamp center X,Y using orbit ~A"
			orbit))))
	    ;;
	    (t
	     (return-with-error
	      "INVALID-STAMP-CENTER"
	      (format nil "STAMP-CENTER=~A not a 2d vector or string 'OBJECT'"
		      stamp-center))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (let ((s/2 (* size/pix 0.5)))
	(when (or (<  x0/pix (- s/2))
		  (<  y0/pix (- s/2))
		  (>  x0/pix (+ naxis1  s/2))
		  (>  y0/pix (+ naxis2  s/2)))
	  (return-with-error
	   "STAMP-OUT-OF-BOUNDS"
	   "Requested stamp would be entirely off the fits image.")))
      ;;
      ;; at this point, the correct center x0/pix y0/pix size/pix should be set
      ;;
      (when compass-rose
	(push (make-instance 'fits-stamp:compass-rose) marker-list))
      ;;
      (when scale-bar
	(push (make-instance 'fits-stamp:pixel-scale-marker) marker-list))
      ;;
      (when orbit-directions
	(push (fits-stamp:fill-orbit-direction-axes-using-comet-elem
	       (make-instance 'fits-stamp:orbit-direction-axes)
	       orbit mjd :observatory observatory)
	      marker-list))
      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; crosshair
      (when (not (equalp crosshair-location "NONE"))
	(cond
	  ((equalp crosshair-location "CENTER")
	   (setf crosshair-location (vector 0.5 0.5))
	   (setf crosshair-location-unit "unit"))
	  ;;
	  ((equalp crosshair-location "OBJECT")
	   (multiple-value-bind  (xcross ycross)
	       (compute-xypix-using-orbit-and-wcs ;; NIL on error
		orbit mjd wcs
		:observatory observatory)
	     (when (not xcross)
	       (return-with-error
		"COULD-NOT-COMPUTE-CROSSHAIR-USING-ORBIT"
		(format nil "Could not compute crosshair RA,DEC using orbit ~A and WCS"
			orbit)))
	     (setf crosshair-location (vector xcross ycross))
	     (setf crosshair-location-unit "pixel"))))
	
	(when (not (and (vectorp crosshair-location)
			(= (length crosshair-location) 2)))
	  (return-with-error "INVALID-CROSSHAIR-LOCATION"
			     "CROSSHAIR-LOCATION is not valid, either CENTER or a 2-vector."))
	(push
	 (setf crosshairs
	       (make-instance
		'fits-stamp:crosshairs
		:coordinate-units
		(cond ((equalp crosshair-location-unit "arcsec") :arcsec)
		      ((equalp crosshair-location-unit "pixel") :pixel)
		      ((equalp crosshair-location-unit "unit") :unit)
		      (t (return-with-error
			  "INVALID-CROSSHAIR-LOCATION-UNIT"
			  (format
			   nil
			   "CROSSHAIR-LOCATION-UNIT is ~A; not one of ARCSEC,UNIT,PIXEL"
			   crosshair-location-unit))))
		:x (aref crosshair-location 0)
		:y (aref crosshair-location 1)))
	 marker-list))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      
      (multiple-value-bind (success err)
	  (ignore-errors
	   (bt:with-lock-held (*make-stamp-lock*) ;; only making one stamp at once
	     (fits-stamp:make-stamp fits-file stamp-type
				    (round x0/pix) (round y0/pix) (round  size/pix)
				    :extension extension
				    :output-file stamp-file
				    :toplabel toplabel
				    :markers marker-list)))
	(when (not success)
	  (return-with-error "FAILED-IN-FITS-STAMP-MAKE-STAMP"
			     (format nil "INTERNAL ERROR in stamp generation: ~A" err))))
      
      (when (not (probe-file stamp-file))
	(return-with-error "STAMP-FILE-NOT-CREATED"
			   "Stamp file creation failed for unknown reason."))
      
      (set-param "SUCCESS" t)
      (set-param "FITS-FILE" fits-file)
      (when extension
	(set-param "EXTENSION" extension))
      (set-param "STAMP-FILE" stamp-file)
      (set-param "OBJECT-PARSED-ID" object-name)
      
      (set-param "XPIX-CENTER" x0/pix)
      (set-param "YPIX-CENTER" y0/pix)
      (set-param "SIZE-PIX" size/pix)
      (when pixel-scale
	(set-param "SIZE-ARCSEC" (* pixel-scale size/pix)))
		 
      (when wcs
	(multiple-value-bind (ra dec)
	    (wcs:wcs-convert-pix-xy-to-ra-dec wcs (* 1d0 x0/pix) (* 1d0 y0/pix))
	  (set-param "RA-CENTER" ra)
	  (set-param "DEC-CENTER" dec)))
      
      
      
      
      ;; return the final XPIX,YPIX RA,DEC of the crosshairs
      (bt-ignore-errors ;; shouldn't fail
       (when (and crosshairs wcs)
	 (multiple-value-bind (cxpix cypix)
	     (fits-stamp::convert-xy-to-pixels
	      (fits-stamp::fsm-x crosshairs)
	      (fits-stamp::fsm-y crosshairs)
	      (fits-stamp::coordinate-units crosshairs)
	      x0/pix y0/pix size/pix wcs)
	   (multiple-value-bind (ra dec)
	       (wcs:wcs-convert-pix-xy-to-ra-dec wcs (* 1d0 cxpix) (* 1d0 cypix))
	     (set-param "XPIX-CROSSHAIR" cxpix)
	     (set-param "YPIX-CROSSHAIR" cypix)
	     (set-param "RA-CROSSHAIRA"   ra)
	     (set-param "DEC-CROSSHAIRC"  dec))))))))

		 
		 
			 
			  
			  

			      
			     
			     
	     
		 
		 
