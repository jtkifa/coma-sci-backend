
#|

{
  "TYPE":"REQUEST",
  "COMMAND":"DO-PHOTOMETRY",
  "ID":"123abc",
  "PARAMETERS": {"FITS-FILE":"/dir/sample.fits",
                 "EXTENSION":1, // if extension is not the default one

                 "POSITION": [1000,1100], // [xpix,ypix] or [ra,dec] or "OBJECT"
                 "POSITION-UNIT": "PIXEL", // or "EQUATORIAL"

                 "FIND-PEAK": true, // employ peak finding algorithm around initial object x,y
                 "FIND-PEAK-DIST": 3.0, // search to this distance in arcsec
                 "FIND-PEAK-FAIL": true, // if FIND-PEAK is true and peak finding fails, fail photometry
         
                 "MAX-ASTROMETRIC-3SIGMA-ERROR": 2.0, // arcsec - if not given, criterion is not applied

                 // the usual ORBIT object, or strings describing where to obtain the orbit
                 "ORBIT": {ID: .. EPOCH-MJD: .. ...}, // or "JPL-ORBIT" or "MPC-ORBIT"
              
                 // the MJD midpoint of this image, used with the orbit to compute
                 // the position - if not given, will try to get it from the image
                 // it from the  fits image
                 "MJD":58849.0, 
                 // if the stamp center is determined using MPC-ORBIT, 
                 //  JPL-ORBIT, or JPL-EPHEM
                 "OBJECT-NAME": "2P", // will try to get from fits file if not given
 
                 // Zero point if not in image (add to -2.5 log10(flux/ADU)
                 "ZPMAG"=27.0,
                 "ZPMAGERR"=0.01,

                 // vector of photometry requests, of different types
        	"PHOTOMETRY-REQUESTS": 
                 [ {
                    "ID":"TheAperturePhotometry", 
                    "PHOTOMETRY-TYPE":"APERTURE",
                    "APERTURES":[5.0,10.0,15.0], // DIAMETERS in arcsec
                    "TUNE-POSITION":true, 
                    "BACKGROUND-RADIUS":"AUTO",
                   },
	           {
                    "ID":"TheSextractorPhotometry", 
                    "PHOTOMETRY-TYPE":"SEXTRACTOR",
                    "APERTURES":[5.0,10.0,15.0],
                    "TUNE-POSITION":true
                   },
	           {
                     "ID":"TheRoundGaussianPhotometry", 
                     "PHOTOMETRY-TYPE":"GAUSSIAN-FIT",
	             "FIT-METRIC":"SQUARE", // or "ABS" for absolute deviation
                     "FITTING-REGION-RADIUS": 10.0, // in arcsec
                     "ROUND":true    // Gaussian is round (symmetric)
                   },
	           {
                     "ID":"TheEllipticalGaussianPhotometry", 
                     "PHOTOMETRY-TYPE":"GAUSSIAN-FIT",
	             "FIT-METRIC":"SQUARE",
                     "FITTING-REGION-RADIUS": 10.0, // arcsec
                     "ROUND":false // Gaussian is elliptical
                   },
                   {
                    "ID":"TheActivityDetectorPhotometry",
                    "PHOTOMETRY-TYPE":"ACTIVITY-DETECTOR",
                    "TUNE-POSITION": true,  // adjust center with quadratic fit
                    "PROFILE-RADIUS": 20,    // radius to which to compute profile, arcsec
                   },

                 ]
               }
}

A full response might look like:

{
  "COMMAND": "do-photometry",
  "ID": "123abc",
  "PARAMETERS": {
    "TYPE": "PARAMETERS",
    
    "XPIX-OBJECT-INPUT": 1158.6247064718652,  // the initial guess of the object position, either given or from orbit, pixels
    "YPIX-OBJECT-INPUT": 2413.0623421970113,  //   this guess may be adjusted below by centroiding
    "RA-OBJECT-INPUT": 112.45524684883168,    // degrees
    "DEC-OBJECT-INPUT": 28.05531750989625,    // degrees
    
    "ZPMAG": 30.8420206601869,   // see above input parameters (magnitudes)
    "ZPMAGERR": 0.00434297738747702,  // magnitudes
    "FILTER": "RSDSS",
    
    // only if MAX-ASTROMETRIC-3SIGMA-ERROR was specified in input parameters
    //   same outputs as a call to ORBITAL-PRECISION
    ORBITAL-PRECISION:  {
                         "TYPE": "ORBITAL-PRECISION",
                         "ERR3SIGMA": 0.10617090774723976,  // the RA and DEC errors in quadrature
                         "RA-ERR3SIGMA":  0.07647306299603171,  // arcsec
                         "DEC-ERR3SIGMA": 0.07364870866402115   // arcsec
                         "MJD-GAP": 7.0,    // the average gap in days between given MJD and the 2 closest ephemeris points 
                         "OK": true         // OK unless MJD-GAP indicates missing JPL data    
                        }
    
    // the vector of photometry results, one for each request
    "PHOTOMETRY-RESULTS": [
      {
        "ID": "TheAperturePhotometry",
        "TYPE": "APERTURE-PHOTOMETRY-RESULT",
        "APERTURES": [  // arcsec
          5,
          10,
          15
        ], 
        "APERTURE-MAGS": [         // magnitudes
          19.142799787611192,  
          18.81181185731334,
          18.680092268079942
        ],
        "APERTURE-MAG-ERRORS": [   // magnitudes
          0.00950686726719141,
          0.013349651359021664,
          0.01750161126255989
        ],
        "BACKGROUND-RADII": [      // arcsec
          23.760002403259353,
          23.760002403259353,
          23.760002403259353
        ],
        
        "XPIX-FINAL": 1158.9627685546875,  // pixel location of final centroid
        "YPIX-FINAL": 2414.349609375,      // J2000 RA,DEC of final centroid, degrees
        "RA-FINAL": 112.45522598974624,     
        "DEC-FINAL": 28.055384146724045,
        "POSITION-WAS-TUNED": true         // this position was fine-tuned from what was given
      },
      {
        "ID": "TheSextractorPhotometry",
        "TYPE": "SEXTRACTOR-PHOTOMETRY-RESULT",
        "APERTURES": [  // arcsec
          5,
          10,
          15
        ],
        "MAG-AUTO": 18.58391633584608,   // sextractor 'MAG_AUTO', magnitudes
        "MAG-AUTO-ERR": 0.013927701860666275,   // magnitudes
        "FWHM": 3.681863307952881,  // arcsec - this is the sextractor FWHM
        "SEXTRACTOR-FLAG": 0,            // zero flag means nothing weird about this detection - see sextractor manual
        "NEIGHBOR-DIST": 6.887943744659424,  // distance to nearest neighbor, in arcsec
        "NEIGHBOR-MAG": 21.955094009185924,  // magnitude of nearest neighbor - there may be a WORSE neighbor further away, so this isn't very smart
        "APERTURE-MAGS": [       // magnitudes
          19.06987538888563,
          18.686643272186412,
          18.499359756256236
        ],
        "APERTURE-MAG-ERRORS": [  // magnitudes
          0.009333782829344273,
          0.012417870573699474,
          0.015436778776347637
        ],
        "BACKGROUND-LEVEL-ADU/PIXEL":[ // computed using VARIANCE of background aparture, not flux - see NOTE below
          61.48406415574386,
          61.48406415574386,
          61.48406415574386
        ],
        "BACKGROUND-LEVEL-ADU/ARCSEC2":[
          1759.943265268219,
          1759.943265268219,
          1759.943265268219
        ],
        "BACKGROUND-LEVEL-PHOTONS/PIXEL":[
          95.66305541992188,
          95.66305541992188,
          95.66305541992188
        ],
        "BACKGROUND-LEVEL-PHOTONS/ARCSEC2":[
          2738.295726430822,
          2738.295726430822,
          2738.295726430822
        ],

        "XPIX-FINAL": 1158.9627685546875, // final pixel location as given by sextractor
        "YPIX-FINAL": 2414.349609375,     
        "RA-FINAL": 112.45522598974624,   // final sextractor RA,DEC degrees
        "DEC-FINAL": 28.055384146724045,  
      },
      {
        "ID": "TheRoundGaussianPhotometry",
        "TYPE": "GAUSSIAN-PHOTOMETRY-RESULT",
        "XPIX-FIT": 1157.7763671875,  // pixel location of best fit center
        "YPIX-FIT": 2413.427978515625,
        "RA-FIT": 112.45529654448814,  // RA,DEC location of best fit center
        "DEC-FIT": 28.055337037037095, 
        "A": 0.534274115884319,       // radius (sigma) of Gaussian, arcsec
        "A-FWHM": 1.2581193805369935, // Gaussian sigma converted to FWHM, arcsec (sigma*2.355)
        "FLUX/ADU": 29544.876953125,  // Flux in ADU (normalization of Gaussian)
        "FLUX/PHOTONS": 48503.82449394531,  // Flux in photons, using gain
        "MAG": 19.6658150251161,            // magnitude computed from flux
        "MAG-PSEUDO-ERROR": 0.023,          // Approx estimate of mag error
        "BACKGROUND-LEVEL-ADU/PIXEL":122.7082290649414,
        "BACKGROUND-LEVEL-ADU/ARCSEC2":3512.4470755347515,       // units explained by variable name  
        "BACKGROUND-LEVEL-PHOTONS/PIXEL":190.92173360214235,
        "BACKGROUND-LEVEL-PHOTONS/ARCSEC2":5465.016404824521
        "BACKGROUND-LEVEL-PER-PIXEL/ADU": 434.5755920410156,    
        "BACKGROUND-LEVEL-PER-PIXEL/PHOTONS": 713.4427494537354 
      },
      {
        "ID": "TheEllipticalGaussianPhotometry",
        "TYPE": "GAUSSIAN-PHOTOMETRY-RESULT",
        "XPIX-FIT": 1157.763427734375,    // pixel location of best fit center
        "YPIX-FIT": 2413.436767578125,
        "RA-FIT": 112.4552973001148,      // RA,DEC location of best fit center
        "DEC-FIT": 28.055337501693224, 
        "A": 0.5588235939566888,          // semi-major axis of Gaussian, arcsec
        "A-FWHM": 1.3159289828116505,     // FWHM of semi-major axis (A*2.355)
        "B": 0.5106277614139756,          // semi-minor axis of Gaussian, arcsec
        "B-FWHM": 1.202436471794643,      // FWHM of semi-minor axis (B*2.355)
        "THETA": -77.15401458740234,      // position angle of Gaussian in degrees in pixel xy coords. 
                                          // THETA=0 is UP (+y), THETA=90 is RIGHT (+x)
        "PA": 76.46857896378175,          // position angle of Gaussian in RA,DEC (North up, East positive)
        "FLUX/ADU": 29562.359375,         // Flux in ADU (normalization of Gaussian)
        "FLUX/PHOTONS": 48532.5253859375, // Flux in photons, usign gain
        "MAG": 19.66517320230116,         // Magnitude
        "MAG-PSEUDO-ERROR": 0.023,        // Approx estimate of mag error
        "BACKGROUND-LEVEL-ADU/PIXEL":122.63739013671875,
        "BACKGROUND-LEVEL-ADU/ARCSEC2":3510.419355078139,
        "BACKGROUND-LEVEL-PHOTONS/PIXEL":190.81151531372072,
        "BACKGROUND-LEVEL-PHOTONS/ARCSEC2":5461.861474566076
     },
     {
        "ID":"TheActivityDetectorPhotometry",
        "TYPE":"ACTIVITY-DETECTOR-RESULT",
        "IS-ACTIVE": true,             // at least 1 sigma active
        "COMA-FRACTION": 0.1,          // coma fraction at r=0 (not of total flux) 
        "COMA-FRACTION-ERROR: 0.05     // 1 sigma erro ron above
        "SIGNIFICANCE-OF-ACTIVITY": 2.0     // significance of activity, equal to
                                            //     COMA-FRACTION / COMA-FRACTION-ERROR
        "NUCLEUS-RADIUS": 1.11,        // radius of nucleus, if an orbit was present
        "TOTAL-MAG":      22.31,       // magnitude estimate from this method
        "NUCLEUS-MAG":    22.40        // magnitude of nuclus from this method
     },
     
     // an example of an error caught in one type of photometry 
     {
        "ID": "Some-Photometry-That-Had-an-Error",
        "TYPE": "<some>-PHOTOMETRY-RESULT",
        "ERROR:" {
                   "TYPE": "ERROR", 
                   "ERROR": "PHOTOMETRY-ERROR",
                   "DESCRIPTION": "Error during PHOTOMETRY-TYPE XXXX: Failed to combobulate the gyrotron of the yoyonator."
                 }
     }
     
    ]  // end of vector of returned photometry objects
  }
}



|#

(in-package coma-json-server)


(def-json-command do-photometry (json-req)
  (with-json-command-setup (json-req)
    (let* ((fits-file (get-param "FITS-FILE" :required t))
	   ;; if extension=NIL then use the default single-image
	   ;; extension, or the first finite image extension
	   (extension  (or (get-param "EXTENSION")
			   (instrument-id:get-image-extension-for-onechip-fits fits-file)
			   (return-with-error
			    "COULD-NOT-GET-IMAGE-EXTENSION"
			    "Could not get valid image extension for fits file, either supplied, or onechip default.")))
	   (object-name-given (get-param "OBJECT-NAME"))
	   (mjd-given (let ((mjd (get-param "MJD")))
			(when (and mjd (not (realp mjd)))
			  (return-with-error "INVALID-MJD" "MJD is invalid - not a real number."))
			mjd))
	   (json-orbit (get-param "ORBIT")) ;; this can be an orbit object, or string MPC-ORBIT, JPL-ORBIT
	   (position (get-param "POSITION"))
	   (find-peak (get-param "FIND-PEAK" :default t))
	   (find-peak-dist (get-param "FIND-PEAK-DIST"
				      :default 3.0
				      :satisfies 'realp
				      :satisfies-desc "must be real number"));; arcsec
	   (find-peak-fail (get-param "FIND-PEAK-FAIL" :default nil))
	   (max-astrometric-3sigma-error-given (get-param "MAX-ASTROMETRIC-3SIGMA-ERROR"))
	   (position-unit (get-param  "POSITION-UNIT" :default "PIXEL"))
	   (photometry-request-vector (get-param "PHOTOMETRY-REQUESTS"))
	   (zpmag-given (get-param "ZPMAG" :satisfies 'realp 
					   :satisfies-desc "must be a real number"))
	   (zpmagerr-given (get-param "ZPMAGERR" :satisfies 'realp
					   :satisfies-desc "must be a real number"))
	   
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
	   (mjd (or (and mjd-given (* 1d0 mjd-given))
		    (instrument-id:get-mjd-mid-for-fits fits-file)))
	  
	   (max-astrometric-3sigma-error
	     (when max-astrometric-3sigma-error-given
	       (or
		(and (realp max-astrometric-3sigma-error-given)
		      (* 1d0 max-astrometric-3sigma-error-given))
		(return-with-error "INPUT-FAILURE"  "Failed to parse MAX-ASTROMETRIC-3SIGMA-ERROR"))))
	   (astrometric-errors-list
	     (if max-astrometric-3sigma-error
		 (ignore-errors (get-orbital-precision-for-object object-name mjd))))
	   (astrometric-3sigma-error (if astrometric-errors-list (first astrometric-errors-list)))
	   ;; same nomenclature as orbital-precision
	   (err3sigma astrometric-3sigma-error) ;; a synonym
	   (ra3sigma (if astrometric-errors-list  (second astrometric-errors-list)))
	   (dec3sigma (if astrometric-errors-list  (third  astrometric-errors-list)))
	   (mjd-gap  (if astrometric-errors-list  (fourth astrometric-errors-list)))
	   ;;				
	  
	   (observatory
	     (ignore-errors
	      (instrument-id:get-observatory-for-fits fits-file)))
	   ;;
	   (wcs (ignore-errors
		 (cf:read-wcs fits-file :extension extension)))
	   (naxis1 (cf:read-fits-header fits-file "NAXIS1"
					:extension extension))
	   (naxis2 (cf:read-fits-header fits-file "NAXIS2"
					:extension extension))
		  
	   (zpmag nil)
	   (zpmagerr nil)
	   (filter nil)
	   orbit orbit-error
		  
	   (require-orbit ;; do we need an orbit?
	     (equalp position "OBJECT"))
	   ;;
	   x0/pix y0/pix ra0 dec0)

      ;; preserve the source of the orbit error
      (when json-orbit
	(multiple-value-setq (orbit orbit-error)
	  (get-orbit-using-method
	   json-orbit object-name :mjd mjd)))

      (when (not json-orbit)
	(when (or (not position)
		  (not (vectorp position))
		  (not (every 'realp position)))
	  (return-with-error "INVALID-POSITION-VECTOR"
			     "POSITION was not 'ORBIT', so must be a vector of decimal [RA,DEC] in J2000")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


      (when max-astrometric-3sigma-error
	(when (not astrometric-3sigma-error)
	  (return-with-error
	   "COULD-NOT-COMPUTE-MAX-ASTROMETRIC-3SIGMA-ERROR"
	   "MAX-ASTROMETRIC-3SIGMA-ERROR was specified but could not compute it for this object and MJD"))
	(when (> astrometric-3sigma-error max-astrometric-3sigma-error)
	  (return-with-error
	   "ASTROMETRIC-ERROR-TOO-BIG"
	   (format nil "ASTROMETRIC-3SIGMA-ERROR for object ~A at MJD ~A is ~A arcsec but MAX-ASTROMETRIC-3SIGMA-ERROR is ~A"
		   object-name mjd astrometric-3sigma-error max-astrometric-3sigma-error))))
		      


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (setf filter
	    (or (cf:read-fits-header fits-file "COMA.PHOT-CALIB.FILTER")
		(instrument-id:get-standard-filter-for-instrument
		 inst fits-file)
		(return-with-error
		 "COULD-NOT-DETERMINE-FILTER"
		 "Could not determine filter using COMA.PHOT-CALIB.FILTER header or INSTRUMENT-ID FILTER METHOD.")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (setf zpmag
	    (ignore-errors
	     (or zpmag-given
		 (cf:read-fits-header fits-file "COMA.PHOT-CALIB.ZPMAG"
				      :extension extension))))
      (setf zpmagerr
	    (ignore-errors
	     (or
	      (ignore-errors
	       (or zpmagerr-given
		   (cf:read-fits-header fits-file "COMA.PHOT-CALIB.ZPMAGERR"
					:extension extension))))))

      ;; if we don't have ZPMAG,ZPMAGERR then try using conventional instrument-id
      (when (not zpmag)
	(multiple-value-setq (zpmag zpmagerr)
	  (ignore-errors
	   (instrument-id:get-calibrated-zeropoint-for-instrument 
	    inst fits-file :extension extension
	    :flux-units :adu :exposure :exptime))))
		 
			     
      (when (not (and zpmag zpmagerr))
	(return-with-error
	 "ZEROPOINT-NOT-FOUND"
	 "ZPMAG, ZPMAGERR not given, and COMA.PHOT-CALIB.ZPMAG and COMA.PHOT-CALIB.ZPMAGERR or other known zeropoint calibrations not in headers."))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
		 

      (when (not (and (vectorp photometry-request-vector)
		      (plusp (length photometry-request-vector))))
	(return-with-error
	 "INVALID-PHOTOMETRY-REQUESTS"
	 "PHOTOMETRY-REQUESTS is not a vector of non-zero length."))
		 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; put the correct stamp center into x0/pix, y0/pix
      (cond
	;; if we're using an orbital or ephem approach to centering,
	;; check inputs, and call the conversion function in utils
	((equalp position "OBJECT")
	 ;; we've already tested if MJD and ORBIT and observatory are present
	 ;; if out of bounds, thrown a clear error now to avoid confusion
	 (let ((out-of-bounds-dist/deg nil)
	       (err nil)
	       (ra-ig nil)
	       (dec-ig nil))

	   (bt-ignore-errors
	     (multiple-value-setq  (x0/pix y0/pix ra-ig dec-ig
					   out-of-bounds-dist/deg
					   err)
	       (compute-xypix-using-orbit-and-wcs orbit mjd wcs
						  :naxis1 naxis1
						  :naxis2 naxis2 
						  :observatory observatory)))
	   #+nil
	   (format t "x0/pix: ~A  y0/pix: ~A  ra-ig: ~A  out-of-bounds-dist/deg: ~A err: ~A~%"
		   x0/pix y0/pix ra-ig out-of-bounds-dist/deg err)
	   ;; throw an error if out of bounds
	   (when (and out-of-bounds-dist/deg ;; we have a value
		      (not (minusp out-of-bounds-dist/deg))) ;; out of bounds
	     (return-with-error
	      "OBJECT-OUT-OF-BOUNDS" 

	      (format nil "Object is out of image bounds by ~A degrees (pixel location x=~A,y=~A in ~Ax~A image); this suggests bad orbit or mis-identified object.~A"
		      (if (floatp out-of-bounds-dist/deg)
			  (format nil "~,3F" out-of-bounds-dist/deg)
			  "<UNDEFINED>")
		      (if x0/pix (round x0/pix) "<UNDEFINED>")
		      (if y0/pix (round y0/pix) "<UNDEFINED>")
		      naxis1 naxis2		      
		      (if (not err)
			  ""
			  (format nil " Additional error during computation: ~A" err)))))
	   ;; throw an error if in-bounds but too close to boundary
	   (when (and out-of-bounds-dist/deg ;; we have a value
		      ;; minus means it's been successfully computed, and is in image
		      (minusp out-of-bounds-dist/deg))
	     (let ((safety-dist/arcsec 4)) ;; is it too close to edge?
	       (when (< (* 3600 (- out-of-bounds-dist/deg))
			safety-dist/arcsec)
		 (return-with-error
		  "OBJECT-TOO-CLOSE-TO-BOUNDARY"
		  (format nil "Object is within ~A arcsec of image edge (pixel location x=~D,y=~D in ~Dx~D image); this suggests poor telescope pointing or inconsistent orbit data."
			  safety-dist/arcsec
			  (round x0/pix) (round y0/pix)
			  naxis1 naxis2)))))   )
			  
	   
	 ;;
	 (when (not x0/pix)
	   (return-with-error
	    "COULD-NOT-COMPUTE-CENTER-USING-ORBIT"
	    (format nil "Could not compute stamp center X,Y using orbit ~A - is the object so far off-field that non-linear WCS fails?"
		    orbit))))
	;;
	((and (vectorp position)
	      (every 'realp position)
	      (equalp position-unit "PIXEL"))
	 (when (not (and (vectorp position)
			 (= (length position) 2)))
	   (return-with-error
	    "POSITION not found"
	    "POSITION parameter not given, for POSITION-METHOD=PIXEL"))
	 (setf x0/pix (aref position 0))
	 (setf y0/pix (aref position 1)))
	;;
	((and  (vectorp position)
	       (every 'realp position)
	       (equalp position-unit "EQUATORIAL"))
	 (when (not (and (vectorp position)
			 (= (length position) 2)))
	   (return-with-error
	    "POSITION-NOT-FOUND"
	    "POSITION parameter not given, for POSITION-METHOD=EQUATORIAL"))
	 (when (not (wcs:wcs-2d-p wcs))
	   (return-with-error
	    "NO-WCS-FOUND"
	    "No valid 2D WCS to convert POSITION-METHOD=EQUATORIAL to pixels."))
	 (setf ra0 (aref position 0))
	 (setf dec0 (aref position 0))
	 (multiple-value-setq (x0/pix y0/pix)
	   (wcs:wcs-convert-ra-dec-to-pix-xy
	    wcs ra0 dec0)))
	;;
	(t
	 (return-with-error
	  "INVALID-POSITION"
	  (format nil "POSITION=~A not a 2d vector or string 'OBJECT'"
		  position))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; this test is already handled above, but maybe not for cases when ORBIT not given, so keep it
      (when (or (<  x0/pix 0)
		(<  y0/pix 0)
		(>  x0/pix naxis1)
		(>  y0/pix naxis2))
	(return-with-error
	 "OBJECT-OUT-OF-BOUNDS"
	 (format
	  nil
	  "Requested object position xpix=~A ypix=~A would be entirely off the fits image." x0/pix y0/pix)))

      ;;
      ;; at this point, the correct center x0/pix y0/pix size/pix should be set
      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		 
		 
      (set-param "XPIX-OBJECT-INPUT"  x0/pix)
      (set-param "YPIX-OBJECT-INPUT"  y0/pix)

      (when (and wcs (not ra0))
	(multiple-value-setq (ra0 dec0)
	  (wcs:wcs-convert-pix-xy-to-ra-dec wcs x0/pix y0/pix)))
      (when ra0
	(set-param "RA-OBJECT-INPUT"  ra0)
	(set-param "DEC-OBJECT-INPUT"  dec0))

      ;; if peak finding enabled, run it, and either return with error or
      ;; reset x0/pix y0/pix to the new peak
      (when find-peak ;; we're tweaking the center
	(when (not (<= 0.1 find-peak-dist 20.0))
	  (return-with-error "FIND-PEAK-DIST-INVALID"
			     (format nil "FIND-PEAK-DIST=~A should be in [0.1,20]" find-peak-dist)))
	(multiple-value-bind (x0-peak y0-peak)
	    (ignore-errors (%phot-peakfind-source-in-fits fits-file extension x0/pix y0/pix find-peak-dist))
	  (when (and (not x0-peak) ;; an error
		     find-peak-fail) ;; required peak finding to succeed
	    (return-with-error "PEAK-FINDING-FAILED"
			       "Peak finding failed but parameter FIND-PEAK-FAIL is true."))
	  (when x0-peak ;; leave x0/pix,y0-pix unchanged if peak finding failed and FIND-PEAK-FAIL=NIL
	    (set-param "XPIX-OBJECT-PEAKFIND"  x0-peak)
	    (set-param "YPIX-OBJECT-PEAKFIND"  y0-peak)
	    (setf x0/pix x0-peak
		  y0/pix y0-peak)
	    ;;
	    (when (and wcs (not ra0))
	      (multiple-value-setq (ra0 dec0)
		(wcs:wcs-convert-pix-xy-to-ra-dec wcs x0/pix y0/pix)))
	    (when ra0
	      (set-param "RA-OBJECT-PEAKFIND"   ra0)
	      (set-param "DEC-OBJECT-PEAKFIND"  dec0)))))


      

      (set-param "ZPMAG"  zpmag)
      (set-param "ZPMAGERR"  zpmagerr)
      (set-param "FILTER"  (string-upcase filter))

      (when astrometric-3sigma-error
	(let ((prec-params (make-hash-table :test 'equalp)))
	  (setf (gethash "TYPE" prec-params) "ORBIT-PRECISION")
	  (setf (gethash "ERR3SIGMA" prec-params) err3sigma)
	  (setf (gethash "RA-ERR3SIGMA" prec-params) ra3sigma)
	  (setf (gethash "DEC-ERR3SIGMA" prec-params) dec3sigma)
	  (setf (gethash "MJD-GAP" prec-params) mjd-gap)
	  (setf (gethash  "OK" prec-params)
		(< mjd-gap (* 0.5 (+ 1.0 *orbital-precision-server-day-interval*))))
	  ;;
	  (set-param "ORBIT-PRECISION"  prec-params)))
		    
		 
      (loop
	with phot-results-list = nil
	for phot-request across photometry-request-vector
	when (not (hash-table-p phot-request))
	  do (return-with-error
	      "INVALID-PHOTOMETRY-REQUESTS"
	      "Element of PHOTOMETRY-REQUESTS is not a JSON object")
	do (let ((phot-type (gethash "PHOTOMETRY-TYPE" phot-request)))
	     ;; macro to call a photometry function but catch errors
	     (macrolet ((safely-do-phot (&rest args)
			  `(multiple-value-bind (%results %err)
			       (ignore-errors (funcall ,@args))
			     (if (not %results)
				 ;; if an error, create a faux phot-results carrying nothing but an error object
				 (let ((faux-phot-result (make-hash-table :test 'equal)) ;; dummy object to carry error
				       (error-object  (make-hash-table :test 'equalp)))
				   (setf (gethash "ID" faux-phot-result)
					 (or (gethash "ID" phot-request) "NONE"))
				   (setf (gethash "ERROR" faux-phot-result) error-object)
				   (setf (gethash "TYPE" error-object) "ERROR")
				   (setf (gethash "DESCRIPTION" error-object)
					 (format nil "ERROR during PHOTOMETRY-TYPE ~A: ~A"
						 phot-type %err))
				   (push faux-phot-result phot-results-list))
				 ;; otherwise save the phot-results onto the result list
				 (push %results phot-results-list)))))
	       ;;
	       (cond ((equalp phot-type "APERTURE")
		      (safely-do-phot
		       '%do-aperture-photometry
		       phot-request fits-file extension inst
		       x0/pix y0/pix zpmag zpmagerr wcs))
		     ;;
		     ((equalp phot-type "SEXTRACTOR")
		      (safely-do-phot
		       '%do-sextractor-photometry
		       phot-request fits-file extension inst
		       x0/pix y0/pix zpmag zpmagerr wcs))
		     ;;
		     ((equalp phot-type "GAUSSIAN-FIT")
		      (safely-do-phot
		       '%do-gaussian-photometry
		       phot-request fits-file extension inst
		       x0/pix y0/pix zpmag zpmagerr wcs))
		     ;;
		     ((equalp phot-type "ACTIVITY-DETECTOR")
		      (safely-do-phot
		       '%do-activity-detector-photometry
		       phot-request fits-file extension inst
		       x0/pix y0/pix zpmag zpmagerr orbit mjd filter))
		     ;; other photometry types go here
		     ;;
		     (t
		      (return-with-error
		       "INVALID-PHOTOMETRY-TYPE"
		       (format nil "PHOTOMETRY-TYPE = ~S is not known." phot-type))))))
	finally
	   (set-param "PHOTOMETRY-RESULTS" 
		      (nreverse (coerce phot-results-list 'vector)))))))


;; try to peak-find the source, in increments out to search-dist/arcsec, demanding that the peak
;; be negative definite but not at edge of search region

(defun %phot-peakfind-source-in-fits (fits-file extension x0/pix y0/pix find-peak-dist)
  (let* ((imsec (ignore-errors (cf:read-image-section fits-file :extension extension)))
	 (wcs (or (cf:image-section-wcs imsec) ;; get it from wcs to be safe
		   (error "Could not find WCS in image section.")))
	 (pixel-scale (wcs:get-pixel-scale-for-wcs wcs))
	 (im (cf:image-section-data imsec)))
    (%phot-peakfind-source im x0/pix y0/pix find-peak-dist pixel-scale)))
    
    

	
(defun %phot-peakfind-source (im x0 y0 search-dist/arcsec pix-scale &key (verbose nil))
  (block retblock
    (flet ((find-source-at-frac (f) ;; find source at frac of search distance, and return (values x y) if OK
	     (let ((search-dist/pix  (* f search-dist/arcsec (/ pix-scale))))
	       (multiple-value-bind (x y errorcode)
		   (ignore-errors
		    (imutils:find-peak im x0 y0 :search-distance search-dist/pix))
		 (when verbose ;; debugging code
		   (format verbose "%phot-peakfind-source: f=~A dist=~A x=~A y=~A ec=~A~%"
			   f search-dist/pix x y errorcode))
		 (when (and x
			    (or (not errorcode)
				;; allow only divergence of quad peak from initial peak
				(= errorcode 4)))
		   (return-from retblock (values x y)))))))
      (loop for f = 0.25 then (* f 1.10)
	    for i from 0
	    until (or (> f 1.0)
		      (> i 100)) ;; be safe
	    do
	       (find-source-at-frac f))
      nil)))
		 
(defparameter *default-apertures-for-photometry*
  #(2.0 3.0 4.0 5.0 6.0 7.0 8.0 9.0 10.0 12.0 15.0))

(defun %do-aperture-photometry (phot-request fits-file extension inst
				x0/pix y0/pix zpmag zpmagerr wcs)
  (let ((apertures (or (gethash "APERTURES" phot-request)
		       *default-apertures-for-photometry*))
	(id (or (gethash "ID" phot-request) "NONE"))
	(gain (instrument-id:get-gain-for-fits fits-file))
	(tune-position (gethash "TUNE-POSITION" phot-request))
	(pixel-scale (if wcs (wcs:get-pixel-scale-for-wcs wcs)))
	(background-radius (gethash "BACKGROUND-RADIUS" phot-request))
	(phot-result (make-hash-table :test 'equalp)))
    (setf (gethash "TYPE" phot-result) "APERTURE-PHOTOMETRY-RESULT")
    (block retblock
      (flet ((return-with-error (error-desc)
	       (setf (gethash "ERROR" phot-result)
		     (build-error-hash-json
		      :name "PHOTOMETRY-ERROR"
		      :desc error-desc))
	       (return-from retblock phot-result)))
	
	;;
	(setf (gethash "ID" phot-result) id)
	;;
	(when (not
	       (and (vectorp apertures)
		    (every 'realp apertures)
		    (every 'plusp apertures)))
	  (return-with-error "Apertures are not a vector of positive real numbers."))
	(setf (gethash "APERTURES" phot-result)
	      apertures)
	(when (not inst) (return-with-error "Could not identify instrument type in fits-file"))
	;;
	(let ((orbphot-result
		(orbphot:do-auto-phot fits-file  
		  :extension extension
		  :photometry-type :aperture
		  :phot-apertures/arcsec (coerce apertures 'list)
		  :saturation-level (instrument-id:saturation-level inst)
		  :xpos x0/pix :ypos y0/pix
		  :tune-peak-location tune-position
		  :use-photcalib nil ;; use the zeropoint from above
		  :zeropoint zpmag
		  :zeropoint zpmagerr 
		  :background-aperture-radius
		  (cond
		    ((equalp background-radius "AUTO") :auto)
		    ((and (realp background-radius)
			  (plusp background-radius))
		     background-radius)
		    (t (return-with-error
			(format nil "Invalid BACKGROUND-RADIUS ~S"
				background-radius)))))))
	  
					    
	  (setf (gethash "APERTURE-MAGS" phot-result)
		(coerce
		 (nsubstitute 99.0 nil (orbphot:phot-result-mag-ap orbphot-result))
		 'vector))
	  (setf (gethash "APERTURE-MAG-ERRORS" phot-result)
		(coerce
		 (nsubstitute 99.0 nil  (orbphot:phot-result-mag-ap-err orbphot-result))
		 'vector))
	  (setf (gethash "BACKGROUND-RADII" phot-result)
		(coerce
		 (orbphot:phot-result-background-radii-arcsec orbphot-result)
		 'vector))

	  (when pixel-scale
	    (setf (gethash "BACKGROUND-LEVEL-ADU/PIXEL" phot-result)
		  (map 'vector
		       (lambda (flux) (* (/ flux gain) (expt pixel-scale 2)))
		       (orbphot:phot-result-sky-flux-photons/arcsec2 orbphot-result))))
	  (setf (gethash "BACKGROUND-LEVEL-ADU/ARCSEC2" phot-result)
		(map 'vector
		     (lambda (flux) (/ flux gain))
		     (orbphot:phot-result-sky-flux-photons/arcsec2 orbphot-result)))
	  (when pixel-scale
	    (setf (gethash "BACKGROUND-LEVEL-PHOTONS/PIXEL" phot-result)
		  (map 'vector
		       (lambda (flux) (* flux (expt pixel-scale 2)))
		       (orbphot:phot-result-sky-flux-photons/arcsec2 orbphot-result))))
	  (setf (gethash "BACKGROUND-LEVEL-PHOTONS/ARCSEC2" phot-result)
		(map 'vector
		     'identity
		     (orbphot:phot-result-sky-flux-photons/arcsec2 orbphot-result)))
	  
	  
	  (setf (gethash "XPIX-FINAL" phot-result)
		(orbphot:phot-result-x-tuned orbphot-result))
	  (setf (gethash "YPIX-FINAL" phot-result)
		(orbphot:phot-result-y-tuned orbphot-result))
	  (setf (gethash "RA-FINAL" phot-result)
		(orbphot:phot-result-ra-tuned orbphot-result))
	  (setf (gethash "DEC-FINAL" phot-result)
		(orbphot:phot-result-dec-tuned orbphot-result))
	  (setf (gethash "POSITION-WAS-TUNED" phot-result)
		(if (orbphot:phot-result-tuned-peak orbphot-result)
		    'yason:true
		    'yason:false))

	  phot-result)))))
	  


    

	       
	     


(defun %do-sextractor-photometry (phot-request fits-file extension inst 
				  x0/pix y0/pix zpmag zpmagerr wcs)
  (declare (ignore wcs))
  (let ((apertures (or (gethash "APERTURES" phot-request)
		       *default-apertures-for-photometry*))
	(id (or (gethash "ID" phot-request) "NONE"))
	(tune-position (gethash "TUNE-POSITION" phot-request))
	(phot-result (make-hash-table :test 'equalp)))
    (setf (gethash "TYPE" phot-result) "SEXTRACTOR-PHOTOMETRY-RESULT")
    (block retblock
      (flet ((return-with-error (error-desc)
	       (setf (gethash "ERROR" phot-result)
		     (build-error-hash-json
		      :name "PHOTOMETRY-ERROR"
		      :desc error-desc))
	       (return-from retblock phot-result)))
	
	;;
	(setf (gethash "ID" phot-result) id)
	;;
	(when (not
	       (and (vectorp apertures)
		    (every 'realp apertures)
		    (every 'plusp apertures)))
	  (return-with-error "Apertures are not a vector of positive real numbers."))
	(setf (gethash "APERTURES" phot-result)
	      apertures)
	(when (not inst) (return-with-error "Could not identify instrument type in fits-file"))
	;;
	(let ((orbphot-result
		(orbphot:do-auto-phot fits-file  
		  :extension extension
		  :photometry-type :sextractor
		  :phot-apertures/arcsec (coerce apertures 'list)
		  :saturation-level (instrument-id:saturation-level inst)
		  :xpos x0/pix :ypos y0/pix
		  :tune-peak-location tune-position
		  :use-photcalib nil ;; use the zeropoint from above
		  :zeropoint zpmag
		  :zeropoint zpmagerr )))

	  (setf (gethash "MAG-AUTO" phot-result)
		(orbphot:phot-result-mag-auto orbphot-result))
	  (setf (gethash "MAG-AUTO-ERR" phot-result)
		(orbphot:phot-result-mag-auto-err orbphot-result))
	  (setf (gethash "FWHM" phot-result)  ;; arcsec
		(orbphot:phot-result-fwhm orbphot-result))
	  (setf (gethash "SEXTRACTOR-FLAG" phot-result)
		(orbphot:phot-result-sextractor-flag orbphot-result))
	  (setf (gethash "NEIGHBOR-DIST" phot-result)
		(orbphot:phot-result-neighbor-dist orbphot-result))
	  (setf (gethash "NEIGHBOR-MAG" phot-result)
		(orbphot:phot-result-neighbor-mag orbphot-result))
	  
	  
	  (setf (gethash "APERTURE-MAGS" phot-result)
		(coerce
		 (nsubstitute 99.0 nil (orbphot:phot-result-mag-ap orbphot-result))
		 'vector))
	  (setf (gethash "APERTURE-MAG-ERRORS" phot-result)
		(coerce
		 (nsubstitute 99.0 nil  (orbphot:phot-result-mag-ap-err orbphot-result))
		 'vector))
	  (setf (gethash "XPIX-FINAL" phot-result)
		(orbphot:phot-result-x-tuned orbphot-result))
	  (setf (gethash "YPIX-FINAL" phot-result)
		(orbphot:phot-result-y-tuned orbphot-result))
	  (setf (gethash "RA-FINAL" phot-result)
		(orbphot:phot-result-ra-tuned orbphot-result))
	  (setf (gethash "DEC-FINAL" phot-result)
		(orbphot:phot-result-dec-tuned orbphot-result))
	   (setf (gethash "POSITION-WAS-TUNED" phot-result)
		(if (orbphot:phot-result-tuned-peak orbphot-result)
		    'yason:true
		    'yason:false))

	  phot-result)))))
	  
	  
(defun %do-gaussian-photometry (phot-request fits-file extension inst
				x0/pix y0/pix zpmag zpmagerr wcs-ignore)
  (declare (ignorable zpmagerr wcs-ignore))
  (let ((id (or (gethash "ID" phot-request) "NONE"))
	(round (gethash "ROUND" phot-request))
	(fit-metric (or (gethash "FIT-METRIC" phot-request) "SQUARE"))
	(fitting-region/arcsec (or
				(gethash "FITTING-REGION-RADIUS" phot-request)
				10.0))
	(phot-result (make-hash-table :test 'equalp)))
    (setf (gethash "TYPE" phot-result) "GAUSSIAN-PHOTOMETRY-RESULT")
    (block retblock
      (flet ((return-with-error (error-desc)
	       (setf (gethash "ERROR" phot-result)
		     (build-error-hash-json
		      :name "PHOTOMETRY-ERROR"
		      :desc error-desc))
	       (return-from retblock phot-result)))
	;;
	(setf (gethash "ID" phot-result) id)
	;;
	(when (or (not (numberp fitting-region/arcsec))
		  (not (< 1.0 fitting-region/arcsec)))
	  (return-with-error
	   (format nil "FITTING-REGION-RADIUS=~A (arcsec) is not a valid number in [1,100]"
		   fitting-region/arcsec)))
			
	(let* ((imsec (or
		       (ignore-errors (cf:read-image-section fits-file :extension extension))
		       (return-with-error "Could not read image section in fits file.")))
	       (wcs (or (cf:image-section-wcs imsec) ;; get it from wcs to be safe
			(return-with-error "Could not find WCS in image section.")))
	       (pixel-scale (instrument-id:get-pixel-scale-for-instrument inst fits-file))
	       (fitting-region/pix
		 (/ fitting-region/arcsec pixel-scale))
	       (nfit (ceiling fitting-region/pix))
	       (gain (instrument-id:get-gain-for-instrument inst fits-file))
	       (im (cf:image-section-data imsec)))
	  ;;
	  (when (evenp nfit) (incf nfit)) ;; fitting region is odd size
	  ;;
	  (setf fit-metric
		(cond ((equalp fit-metric "SQUARE") :square)
		      ((equalp fit-metric "ABS") :abs)
		      (t (return-with-error "FIT-METRIC must be ROUND or SQUARE"))))
	  ;;
	  (when (not (and (< 0 x0/pix (1- (array-dimension im 1)))
			  (< 0 y0/pix (1- (array-dimension im 0)))))
	    (return-with-error
	     (format nil "Starting position x0=~A y0=~A is out of bounds for ~Ax~A image"
		     x0/pix y0/pix (array-dimension im 0) (array-dimension im 1))))
	  ;;
	  (let (v x0 y0 ra0 dec0 a b theta backd norm err)
	    (cond (round
		   (multiple-value-setq (v err)
		     (imutils:fit-round-gaussian im (round x0/pix) (round y0/pix)
						 :sigma0 6.0 :size nfit
						 :metric fit-metric)))
		  (t 
		   (multiple-value-setq (v err)
		     (imutils:fit-2axis-gaussian im (round x0/pix) (round y0/pix)
						 :a0 6.0 :b0 6.0 :size nfit
						 :metric fit-metric))))
	    (when (not v)
	      (return-with-error
	       (format nil "FITTING ERROR ~A" err)))
	    ;; 1d Gaussian: #(X0 Y0 SIGMA NORM BACKD)
	    ;; 2d Gaussian: #(X0 Y0 THETA A B NORM BACKD)
	    (let ((k -1))
	      (setf x0 (aref v (incf k)))
	      (setf y0 (aref v (incf k)))
	      (setf theta (if (not round) (aref v (incf k))))
	      (setf a  (aref v (incf k)))
	      (setf b  (if (not round) (aref v (incf k)) nil))
	      (setf norm (aref v (incf k)))
	      (setf backd (aref v (incf k)))
	      )

	    (let (flux/photons
		  backd-photons/pixel
		  mag-error)

	      (when gain
		(setf flux/photons (* norm gain))
		(setf backd-photons/pixel  (* backd gain)))
	      
	      (when (and gain (plusp norm))
		(let* ((bb (if round a b))
		       (backd-sigma/photons
			 ;; compute backd sigma within an annuls
			 (* gain
			   (imutils:image-median-and-sigma-in-annulus
			     im x0 y0
			     (float (* 2d0 (min 20d0 (* (max a bb)))) 1.0)
			     (float (* 2d0 (* 1.2 (min 20d0 (max a bb)))) 1.0)
			     :no-median t :ignore-nan t)))
		       (area-of-detection  (* 4d0 ;; go out to 2 sigma
					      (* a bb pi)))
		       (flux/photons (* norm gain))
		       (total-photon-error (sqrt (+ flux/photons
						    (* area-of-detection
						       (expt backd-sigma/photons 2))))))
		;; mag-error defined only if we know gain, and if norm>0
		(setf mag-error
		      (nth-value 1 (imutils:compute-mag+dmag-from-flux+dflux
				    flux/photons
				    total-photon-error)))))
	      
	      ;;
	      (multiple-value-setq (ra0 dec0)
		(wcs:wcs-convert-pix-xy-to-ra-dec wcs (* 1d0 x0) (* 1d0  y0)))

	      (setf (gethash "XPIX-FIT" phot-result) x0)
	      (setf (gethash "YPIX-FIT" phot-result) y0)
	      (setf (gethash "RA-FIT" phot-result) ra0)
	      (setf (gethash "DEC-FIT" phot-result) dec0)
	      (setf (gethash "A" phot-result) (* a pixel-scale))
	      (setf (gethash "A-FWHM" phot-result)
		    (imutils:gaussian-fwhm (* a pixel-scale)))
	      (when (not round)
		(setf (gethash "B" phot-result) (* b pixel-scale))
		(setf (gethash "B-FWHM" phot-result)
		      (imutils:gaussian-fwhm (* b pixel-scale)))
		(setf (gethash "THETA" phot-result) theta)
		(setf (gethash "PA" phot-result)
		      (position-angle-pixels-to-sky theta wcs)))
	      (setf (gethash "FLUX/ADU" phot-result) norm)
	      (setf (gethash "FLUX/PHOTONS" phot-result) flux/photons)
	      (setf (gethash "MAG" phot-result)
		    (when (plusp norm)
		      (+ (* -2.5 (log norm 10)) zpmag)))
	      (setf (gethash "MAG-PSEUDO-ERROR" phot-result) mag-error) ;; can be NULL
	      (setf (gethash "BACKGROUND-LEVEL-ADU/PIXEL" phot-result) backd)
	      (setf (gethash "BACKGROUND-LEVEL-ADU/ARCSEC2" phot-result)
		    (/ backd (expt pixel-scale 2)))
	      (setf (gethash "BACKGROUND-LEVEL-PHOTONS/PIXEL" phot-result)
		    backd-photons/pixel)
	      (setf (gethash "BACKGROUND-LEVEL-PHOTONS/ARCSEC2" phot-result)
		    (when gain (/ (* gain backd) (expt pixel-scale 2)))))

	  phot-result))))))
	    

(defun %compute-rhelio-delta-phase-angle (comet-elem mjd)
  (let ((pv (slalib-ephem:compute-pv-from-comet-elem comet-elem mjd
						      :units :au     
						      :correct-mjd-to-tt t)))
      (multiple-value-bind (phase-angle pv-earth)
	  (slalib-ephem:compute-phase-angle-for-pv pv mjd :epoch 2000d0)
	
	(let ((delta
		(sqrt (+ (expt (- (aref pv-earth 0) (aref pv 0)) 2)
			 (expt (- (aref pv-earth 1) (aref pv 1)) 2)
			 (expt (- (aref pv-earth 2) (aref pv 2)) 2))))
	      (rhelio
		(sqrt (+ (expt (aref pv 0) 2)
			 (expt (aref pv 1) 2)
			 (expt (aref pv 2) 2)))))
	  (values rhelio delta phase-angle)))))

;; compute cometary nucleus in KM using formula provided by Meech and
;; standard albedo.   The phase function is  -beta*PhaseAngle assuming
;; that PhaseAngle is never >40 or so.  
(defun %compute-nucleus-radius (nucleus-mag mjd orbit filter
				&key (albedo 0.04)
				;; phasing factor
				  (beta 0.04))
  (multiple-value-bind (rhelio delta phase-angle)
      (%compute-rhelio-delta-phase-angle orbit mjd)
    (let* ((phase-angle (min 80d0 phase-angle)) ;; keep it sensible
	   (mag-sun
	     (magnitude-of-sun:magnitude-of-sun filter))
	   ;; this is the simple cometary form Phi(PhaseAngle)=beta*PhaseAngle
	   (phi-phase (* beta phase-angle))
	   (rnucleus
	     (* 0.001 ;; convert m to km
		(sqrt
		 (*
		  (/ 1d0 albedo)
		  2.235d22 rhelio rhelio delta delta
		  (expt 10 (* 0.4 (- mag-sun nucleus-mag)))
		(expt 10 (* 0.4 phi-phase)))))))
      rnucleus)))
	    
    
	  
(defun %do-activity-detector-photometry (phot-request fits-file extension inst
					 x0/pix y0/pix
					 zpmag      ;; used for nucleus size
					 zpmagerr   ;; could be used for nucleus size err but ignored
					 orbit
					 mjd
					 filter) ;; needed for solar magnitude
  (declare (ignorable zpmagerr))
  (let ((id (or (gethash "ID" phot-request) "NONE"))
	(tune-position (gethash "TUNE-POSITION" phot-request t)) ;; Tune by default
	(r-profile/arcsec (or (gethash "PROFILE-RADIUS" phot-request) 20))
	(phot-result (make-hash-table :test 'equalp)))

    (setf (gethash "TYPE" phot-result) "ACTIVITY-DETECTOR-RESULT")
       (block retblock
      (flet ((return-with-error (error-desc)
	       (setf (gethash "ERROR" phot-result)
		     (build-error-hash-json
		      :name "PHOTOMETRY-ERROR"
		      :desc error-desc))
	       (return-from retblock phot-result)))
	;;
	(setf (gethash "ID" phot-result) id)
	;;
	(let* ((imsec (or
		       (ignore-errors (cf:read-image-section fits-file :extension extension))
		       (return-with-error "Could not read image section in fits file.")))
	       (pixel-scale (instrument-id:get-pixel-scale-for-instrument inst fits-file))
	       (nr-profile/pix (ceiling (/ r-profile/arcsec pixel-scale)))
	       (im (cf:image-section-data imsec)))
	  ;;
	  (multiple-value-bind (scadresult err-scad)
	      (scad:is-object-active
	       im (float x0/pix 1.0) (float y0/pix 1.0)
	       :r-profile nr-profile/pix
	       :n-monte-carlo 100
	       :fit-center tune-position
	       :fit-center-n 15)  ;; size of quadratic region for tuning

	    (when (not scadresult)
	      (return-with-error (format nil "ERROR in SIMPLE-COMET-ACTIVITY-DETECTOR: ~A" err-scad)))
	    
	    (setf (gethash "IS-ACTIVE" phot-result)
		  (json-bool 
		   (scad:scadresult-active-p scadresult)))
	    (setf (gethash "COMA-FRACTION" phot-result)
		  (scad:scadresult-coma-frac scadresult))
	    (setf (gethash "COMA-FRACTION-ERROR" phot-result)
		  (scad:scadresult-coma-frac-err scadresult))
	    (setf (gethash "FWHM" phot-result)
		  (scad:scadresult-fwhm scadresult))
	    (setf (gethash "SIGNIFICANCE-OF-ACTIVITY" phot-result)
		  (ignore-errors ;; just in case weird numerics occur
		   (/
		    (scad:scadresult-coma-frac scadresult)
		    (min 1e-4 ;; avoid divide by zero
			 (scad:scadresult-coma-frac-err scadresult))))) 

	    ;; now compute nucleus size producing
	    ;;   "NUCLEUS-RADIUS": 1.11,        // radius of nucleus, if an orbit was present
            ;;   "TOTAL-MAG":      22.31,       // magnitude estimate from this method
            ;;   "NUCLEUS-MAG":    22.40        // magnitude of nuclus from this method
	    (let ((total-mag (if (plusp (scad:scadresult-flux-total scadresult))
				 (+ zpmag (* -2.5 (log (scad:scadresult-flux-total scadresult) 10)))
				 99.0))
		  (nucleus-mag (if (plusp (scad:scadresult-flux-nucleus scadresult))
				 (+ zpmag (* -2.5 (log (scad:scadresult-flux-nucleus scadresult) 10)))
				 99.0)))

	      (setf (gethash "TOTAL-MAG" phot-result) total-mag)
	      (setf (gethash "NUCLEUS-MAG" phot-result) nucleus-mag)
	      (setf (gethash "NUCLEUS-RADIUS" phot-result) 
		    (when (and orbit ;; NULL if no orbit or invalid nucleus mag
			       (< nucleus-mag 99.0))
		      (%compute-nucleus-radius nucleus-mag mjd orbit  filter))))
	    phot-result))))))
		  
			  
			      
			     
			     
	     
		 
		 
