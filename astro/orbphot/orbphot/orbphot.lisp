
(defpackage orbphot
  (:use #:cl)
  (:export
   ;; the returned data structure
   #:phot-result #:phot-result-p #:phot-result-object #:phot-result-fits
   #:phot-result-ephem #:phot-result-ra-ephem #:phot-result-dec-ephem
   #:phot-result-instrument #:phot-result-photometry-type
   #:phot-result-mjd
   #:phot-result-ra-cat #:phot-result-dec-cat 
   #:phot-result-ra-peak-cat #:phot-result-dec-peak-cat
   #:phot-result-ra-tuned #:phot-result-dec-tuned
   #:phot-result-x #:phot-result-y
   #:phot-result-tuned-peak
   #:phot-result-xpeak #:phot-result-ypeak
   #:phot-result-x-tuned #:phot-result-y-tuned
   #:phot-result-match-arcsec 
   #:phot-result-apertures-pix #:phot-result-apertures-arcsec
   #:phot-result-sky-flux-photons/arcsec2
   #:phot-result-background-radii-arcsec
   #:phot-result-mag-ap #:phot-result-mag-ap-err
   #:phot-result-mag-auto #:phot-result-mag-auto-err
   #:phot-result-sextractor-flag
   #:phot-result-zpmag #:phot-result-zpmag-err
   #:phot-result-zpmag-source
   #:phot-result-filter
   #:phot-result-exptime
   #:phot-result-fwhm #:phot-result-neighbor-dist
   #:phot-result-neighbor-mag
   ;;
   #:do-auto-phot
   #:output-phot-result))


(in-package orbphot)

;; cache orbits
(defvar *orbit-hash-lock*
  (bordeaux-threads:make-lock "orbit-hash-lock"))
(defvar *orbit-hash* (make-hash-table :test 'equalp))
(defun %get-cached-orbit (targ)
  (bordeaux-threads:with-lock-held (*orbit-hash-lock*)
    (gethash targ *orbit-hash*)))
(defun %cache-orbit (targ orbit)
  (bordeaux-threads:with-lock-held (*orbit-hash-lock*)
    (setf (gethash targ *orbit-hash*) orbit)))

;; get an mpc orbit, trying variations until MPC gives
;; an answer
(defun %get-mpc-orbit-for-target-name (target-name)
  (when (plusp (length target-name))
    (let ((targ (substitute #\space #\_ target-name)))
      (or 
       ;; try the pure name
       (mpc:get-mpc-elements target-name)
       ;; try C2013 A1 -> C/2013 A1
       (when (char-equal (aref targ 0) #\C)
	 (mpc:get-mpc-elements 
	  (format nil "C/~A" (subseq targ 1))))
       ;; try P2013 A1 -> P/2013 A1
       (when (char-equal (aref targ 0) #\P)
	 (mpc:get-mpc-elements 
	  (format nil "P/~A" (subseq targ 1))))
       ;; a name like 22P Kopff turns to 22P
       (multiple-value-bind (n k)
	   (ignore-errors (parse-integer targ :junk-allowed t))
	 (when (and n 
		    (> (length target-name) k)
		    (char-equal (aref targ k) #\P))
	   (mpc:get-mpc-elements (subseq targ 0 (1+ k)))))))))
		  
	 
	      
       
  
  
;; get the ephemeris for target in the OBJECT field of
(defun get-targ-ephem (fits &key (comet-elem nil))
  (let* ((instrument (instrument-id:identify-instrument fits))
	 (targ (instrument-id:get-object-for-fits
		fits :instrument instrument))
	 (mjd (instrument-id:get-mjd-mid-for-fits
	       fits :instrument instrument))
	 (observatory
	   (instrument-id::instrument-observatory instrument))
	 (ephem nil))
    (when (and targ mjd)
      (let ((orbit (or  comet-elem
			(%get-cached-orbit targ)
			(%get-mpc-orbit-for-target-name targ))))
	(when orbit
	  ;; don't cache if given
	  (when  (not comet-elem) (%cache-orbit targ orbit))
	  (setf ephem (slalib-ephem:compute-ephem-for-observatory 
		       orbit mjd observatory)))

	(let ((wcs (cf:read-wcs
		    fits
		    :extension
		    (instrument-id:get-image-extension-for-onechip-fits fits))))
	  ;;
	  (multiple-value-bind (xpix ypix)
	      (wcs:wcs-convert-ra-dec-to-pix-xy
	       wcs
	       (slalib-ephem:ephem-ra ephem)
	       (slalib-ephem:ephem-dec ephem))
	    ;;
	    (values targ ephem xpix ypix)))))))

;; make a fake EPHEM structure using either headers or explicit values
(defun get-fake-targ-ephem (fits xpos-header ypos-header xpos ypos &key (object-name nil))
  (let* ((instrument (instrument-id:identify-instrument fits))
	 (targ (or
		object-name
		(ignore-errors ;; there might not be an object
		 (instrument-id:get-object-for-fits
		  fits :instrument instrument))
		"UNKNOWN"))
	 (mjd (instrument-id:get-mjd-mid-for-fits
	       fits :instrument instrument))
	 (observatory
	   (instrument-id::instrument-observatory instrument))
	 (wcs (cf:read-wcs
	       fits
	        :extension
		(instrument-id:get-image-extension-for-onechip-fits fits)))
	 (xpix (or xpos (cf:read-fits-header fits xpos-header)))
	 (ypix (or ypos (cf:read-fits-header fits ypos-header))))
    (when  (not (and wcs xpos ypos))
      (error "WCS, ~A, ~A not defined in ~A" xpos-header ypos-header fits))
    (multiple-value-bind (ra dec)
	(wcs:wcs-convert-pix-xy-to-ra-dec wcs (float xpix 1d0) (float ypix 1d0))
      (values
       targ
       (slalib-ephem::make-ephem :observatory observatory
				 :mjd mjd :ra ra :dec dec)
       xpix ypix))))

;; all x,y coordinates are in 1-based indexing, not Lisp 0-based
(defstruct phot-result
  fits
  photometry-type ;; :sextractor or :aperture
  instrument
  ephem
  object
  mjd
  ra-ephem dec-ephem 
  ra-cat dec-cat
  ra-peak-cat dec-peak-cat
  ra-tuned dec-tuned  ;; optional quadratically tuned pos.
  x y xpeak ypeak
  tuned-peak  ;; did we succeed in tuning the peak?
  x-tuned y-tuned ;; optional quadratically corrected position
  match-arcsec
  apertures-pix ;; vector of apertures (DIAMETERS)
  apertures-arcsec ;; vector of apertures in arcsec
  background-radii-arcsec  ;; inner background annulus radius, for :APERTURE phot only
  sky-flux-photons/arcsec2  ;; :APERTURE only
  mag-ap ;; vector of aperture mags
  mag-ap-err ;; vector of aperture mag errors
  mag-auto
  mag-auto-err
  sextractor-flag
  zpmag zpmag-err zpmag-source filter exptime
  fwhm
  neighbor-dist ;; arcsec
  neighbor-mag)  

;; read in image section around object and fine-tune it with a
;; quadratic fit - at least one of fits and imsec must be gien
;; the input coordinates are 1-indexed
(defun tune-peak-xy (x y &key (search-distance 10.0) (n-hot-block 5) (n-quad-fit 5)  fits imsec)
  (when (not (or fits imsec)) (error "One of FITS or IMSEC must be given in QUADRATIC-TUNE-XY"))
  ;;
  (let* ((imsec (or imsec
		    (cf:with-open-fits-file (fits ff)
		      (cf:move-to-extension ff (instrument-id:get-image-extension-for-onechip-fits fits))
		      (cf:read-image-section ff))))
	 (wcs (or (cf:image-section-wcs imsec)
		  (error  "WCS not found in image section."))))
    
    (multiple-value-bind (x-fit y-fit errorcode)
	(imutils:find-peak 
	 (cf:image-section-data imsec)
	 (- x 1.0) ;; convert 1-based to 0-based indices
	 (- y 1.0) 
	 :search-distance search-distance
	 :nb n-hot-block
	 :nq n-quad-fit)

      (incf x-fit 1.0) (incf y-fit 1.0) ;; convert fit indices back to 1-based

      (when errorcode ;; fit was bad
	(setf x-fit x
	      y-fit y))
      (multiple-value-bind (ra-fit dec-fit)
	  (wcs:wcs-convert-pix-xy-to-ra-dec wcs x-fit y-fit)
	(values x-fit y-fit ra-fit dec-fit
		errorcode))))) ;; sign should be -1 for a valid fit
     
	  
;; all x,y coordinates are in 1-based indexing, not Lisp 0-based
(defun do-auto-phot
    (fits &key
	    extension
	    (photometry-type :sextractor) ;; :sextractor or :aperture
	    ;; supply our own orbit, not derived from headers
	    (comet-elem nil)
	    (phot-apertures/arcsec ;; DIAMETERS
	     '(2.0 3.0 4.0 5.0 6.0 7.0 8.0 9.0 10.0 12.0 16.0)) 
	    (phot-autoparams '(2.5 3.5))
	    (sextractor-catalog "sex_auto_phot.cat")
	    saturation-level
	    (deblend-mincont 1.0)
	    (max-match-dist 2.0) ;; arcsec
	    ;; override ephem with x,y headers
	    (xpos-header nil)
	    (ypos-header nil)
	    ;; override ephem with explicit x,y 
	    xpos ypos
	    ;; an overriding object name, taking precedence over fits header
	    object-name
	    ;; quadratically tune the peak and fill in the 'quad' fields?
	    (tune-peak-location t)
	    (use-photcalib t) ;; use PHOTCALIB.* headers
	    (zeropoint nil) (zeropoint-err nil)
	    (tune-peak-location-npix-fit 7) ;; 7x7 region
	    ;; only for aperture type phot 
	    (background-aperture-radius :auto)) 
  
  (declare (optimize debug)
	   (type (member :sextractor :aperture) photometry-type))

  (when (and (or xpos ypos xpos-header ypos-header) ;; not using OBJECT
	     (not
	      (or (and (and xpos ypos) (not (or xpos-header ypos-header)))
		  (and (not (or xpos ypos)) (and xpos-header ypos-header)))))
    (error "ERROR - the pair XPOS,YOS  or the pair XPOS-HEADER,YPOS-HEADER may be supplied, but not both.  Now XPOS=~A YPOS=~A  XPOS-HEADER=~A YPOS-HEADER=~A"
	   xpos ypos xpos-header ypos-header))

  (when (and use-photcalib zeropoint)
    (error "Cannot specify both ZEROPOINT and USE-PHOTCALIB."))
  
  (let* (targ ephem gain
	      (inst (instrument-id:identify-instrument fits))
	      (extension (or extension (instrument-id:get-image-extension-for-onechip-instrument inst fits)))
	      xpix-pred ypix-pred ;; predicted pixel xy
	     (zpmag 0.0) (zpmag-err 0.0) (zpmag-source "NONE")
	     (filter (or (instrument-id:get-standard-filter-for-fits fits)
			 :unknown))
	     (pixel-scale (or (instrument-id:get-pixel-scale-for-instrument inst fits :extension extension)
			      (error "Cannot get pixel scale for ~A" fits)))
	     (saturation-level (or saturation-level
				   (instrument-id:saturation-level inst)))
	     (phot-apertures/pix (mapcar (lambda (ap) (/ ap pixel-scale))
					 phot-apertures/arcsec))
	(extra-sextractor-params '("XPEAK_IMAGE" "YPEAK_IMAGE" 
				   "ALPHAPEAK_J2000" "DELTAPEAK_J2000")))
				 

    (when use-photcalib
      (multiple-value-setq (zpmag zpmag-err)
	(instrument-id:get-calibrated-zeropoint-for-fits
	 fits :flux-units :adu :exposure :exptime))
      
      (when (not (and zpmag zpmag-err))
	(error "Could not get zeropoint mag and error in ~A - missing PHOTCALIB.ZPMAG and ZPMAGERR?" fits))

      (setf zpmag-source
	    (or (cf:read-fits-header fits "PHOTCALIB.CATALOGTYPE" :extension extension)
		(type-of (instrument-id:identify-instrument fits))
		"Unknown"))
      (setf filter
	    (or (instrument-id:get-standard-filter-for-fits fits)
		(cf:read-fits-header fits "PHOTCALIB.FILTER" :extension extension)
		;; we need a filter for photometric transformation, so it is an error not to have one
		(error "Could not read INSTRUMENT-ID standard filter, or header PHOTCALIB.FILTER in ~A" fits)))

      (setf filter (intern (string-upcase filter) :keyword)))

    (when zeropoint
      (setf zpmag-err (or zeropoint-err 0.0))
      (setf zpmag zeropoint)
      (setf filter (instrument-id:get-standard-filter-for-instrument inst fits)))
      
    
    ;; construct a genuine EPHEM, or fake one using XPOS,YPOS
    (if (not (or (and xpos-header ypos-header)
		 (and xpos ypos)))
      (multiple-value-setq (targ ephem xpix-pred ypix-pred)
	(get-targ-ephem fits :comet-elem comet-elem))
      (multiple-value-setq (targ ephem xpix-pred ypix-pred)
	(get-fake-targ-ephem fits xpos-header ypos-header xpos ypos
			     :object-name object-name)))
      
    (when (not ephem) (error "Could not compute ephemeris for target ~A in ~A"
			     targ fits))
    ;;
    (setf gain (or (instrument-id:get-gain-for-fits fits)
		   (error "Could not get gain for ~A" fits)))
    (when (not (plusp gain))
      (error "Bogus GAIN = ~A - gain must be positive number" gain))

    ;;
    (cond ((eq photometry-type :sextractor)
	   (%finish-auto-phot/sextractor
	    fits extension targ ephem filter pixel-scale max-match-dist
	    tune-peak-location tune-peak-location-npix-fit
	    sextractor-catalog gain deblend-mincont
	    phot-apertures/pix phot-autoparams
	    saturation-level extra-sextractor-params
	    xpix-pred ypix-pred zpmag zpmag-err zpmag-source))
	  ((eq photometry-type :aperture)
	   (%finish-auto-phot/aperture
	    fits extension targ ephem filter pixel-scale max-match-dist
	    tune-peak-location tune-peak-location-npix-fit
	    sextractor-catalog gain deblend-mincont
	    phot-apertures/pix phot-autoparams
	    saturation-level extra-sextractor-params
	    xpix-pred ypix-pred zpmag zpmag-err zpmag-source
	    ;;
	    :background-aperture-radius background-aperture-radius)))))
    

(defparameter *orbphot-back-size* 512) ;; really big for comets
 

;; complete auto-phot, if we're using sextractor
(defun %finish-auto-phot/sextractor
    (fits extension targ ephem filter pixel-scale max-match-dist
     tune-peak-location tune-peak-location-npix-fit
     sextractor-catalog gain deblend-mincont
     phot-apertures/pix phot-autoparams
     saturation-level extra-sextractor-params
     xpix-pred ypix-pred zpmag zpmag-err zpmag-source)

  
  (terapix:run-sextractor  
   fits
   :extension extension ;; sextractor package now uses 1-based HDU numbering
   :output nil :display-errors nil  
   :output-catalog sextractor-catalog 
   :gain gain  :deblend-mincont deblend-mincont
   :md5-avoid-rerun t
   :back-size *orbphot-back-size*
   :phot-apertures phot-apertures/pix ;; diameters
   :phot-autoparams phot-autoparams
   :satur-level saturation-level 
   :pixel-scale :from-wcs 
   :extra-output-parameters extra-sextractor-params
   :checkimage-types-and-names 
   '(("OBJECTS" "sextractor_OBJECTS.fits")))
  
  
  (let ((shash (terapix:read-sextractor-catalog  
		(format nil "~A/~A" (terapix:get-fits-directory fits :extension extension)
			sextractor-catalog)
		:extra-columns extra-sextractor-params))
	match-dist neighbor-mag n-cat neighbor-dist neighbor-index tuned-peak)

    ;; Compute neighbor distances - can be NIL if no neighbors (1 object)
    (terapix:add-neighbor-dist-to-sextractor-catalog shash) 

    ;; use the PEAK pixel so that comet positions are nucleus,
    ;; not the center of mass
    (loop with ra-vec = (gethash "ALPHAPEAK_J2000" shash)
	  with dec-vec = (gethash "DELTAPEAK_J2000" shash)
	  with ra-pred = (slalib-ephem:ephem-ra ephem)
	  with dec-pred = (slalib-ephem:ephem-dec ephem)
	  with dist-best = 1d100
	  with n-best = nil
	  for i from 0
	  for ra across ra-vec and dec across dec-vec 
	  for dist = (astro-coords:sky-angle ra-pred dec-pred  ra dec 
					     :units :arcsec)
	  when (and (< dist dist-best)
		    (< dist max-match-dist))
	    do (setf n-best i)
	       (setf dist-best dist)
	  finally
	     (setf n-cat n-best)
	     (setf match-dist dist-best)
	     (when (not n-cat)
	       (error "Could not find a matching sextractor detection within 
~A arcsec of ra=~,4F dec=~,4F (x=~D y=~D) in ~A"
		      max-match-dist
		      ra-pred dec-pred
		      (round xpix-pred) (round ypix-pred)
		      fits)))

    (setf neighbor-index (aref (gethash "%NEIGHBOR-INDEX" shash) n-cat))
    (when neighbor-index
      (setf neighbor-mag 
	    (aref (gethash "MAG_AUTO" shash) neighbor-index))
      (setf neighbor-dist
	    (aref  (gethash "%NEIGHBOR-DIST" shash) n-cat)))
  
  
    (flet ((add-zp-err (magerr)
	     (sqrt (+ (expt zpmag-err 2) (expt magerr 2))))
	   (add-zpmag (mag)
	     (+ zpmag mag))
	   (extract-row (array irow)
	     (if (= (array-rank array) 1)
		 (list (aref array irow)) ;; it's just a vector
		 (loop for i below (array-dimension array 1)
		       collect (aref array irow i)))))
    
      ;; quadratic peak tuning
      (let (x-tuned y-tuned ra-tuned dec-tuned errorcode
	       (ra-orig (aref (gethash "ALPHAWIN_J2000"  shash) n-cat))
	       (dec-orig (aref (gethash "DELTAWIN_J2000"  shash) n-cat))
	       (x-orig (aref (gethash "XPEAK_IMAGE" shash) n-cat))
	       (y-orig (aref (gethash "YPEAK_IMAGE" shash) n-cat)))      
	(when tune-peak-location
	  (setf tuned-peak t)
	  (multiple-value-setq (x-tuned y-tuned ra-tuned dec-tuned errorcode)
	    (ignore-errors
	     (tune-peak-xy
	      x-orig
	      y-orig
	      :search-distance 5.0 ;; make it samll for existing catalog obj.
	      :n-quad-fit tune-peak-location-npix-fit
	      :fits fits)))
	  ;;
	  ;; if peak is not negative definite then answer is invalid
	  (when (or (not x-tuned) ;; ERROR in IGNORE-ERRORS
		    errorcode)
	    (setf tuned-peak nil)
	    (setf x-tuned x-orig
		  y-tuned y-orig
		  ra-tuned ra-orig
		  dec-tuned dec-orig)))
	;;
	(make-phot-result
	 :fits fits
	 :photometry-type :sextractor
	 :instrument (type-of (instrument-id:identify-instrument fits))
	 :ephem ephem
	 :object targ
	 :mjd (slalib-ephem:ephem-mjd ephem)
	 :ra-ephem (slalib-ephem:ephem-ra ephem)
	 :dec-ephem (slalib-ephem:ephem-dec ephem)
	 ;; Use windowed coords
	 :ra-cat       (aref (gethash "ALPHAWIN_J2000"  shash) n-cat)
	 :dec-cat      (aref (gethash "DELTAWIN_J2000"  shash) n-cat)
	 :ra-peak-cat  (aref (gethash "ALPHAPEAK_J2000" shash) n-cat)
	 :dec-peak-cat (aref (gethash "DELTAPEAK_J2000" shash) n-cat)
	 ;; Use windowed coords
	 :x (aref (gethash "XWIN_IMAGE" shash) n-cat)
	 :y (aref (gethash "YWIN_IMAGE" shash) n-cat)
	 :tuned-peak tuned-peak
	 :xpeak (aref (gethash "XPEAK_IMAGE" shash) n-cat)
	 :ypeak (aref (gethash "YPEAK_IMAGE" shash) n-cat)
	 :x-tuned x-tuned :y-tuned y-tuned :ra-tuned ra-tuned :dec-tuned dec-tuned
	 :match-arcsec match-dist
	 :zpmag zpmag :zpmag-err zpmag-err :zpmag-source zpmag-source
	 :filter filter
	 :exptime (instrument-id:get-exptime-for-fits fits)
	 :apertures-pix (gethash "%APERTURES" shash)
	 :apertures-arcsec (mapcar (lambda (ap) (* pixel-scale ap))
				   (gethash "%APERTURES" shash))
	 :mag-ap  (mapcar #'add-zpmag 
			  (extract-row (gethash "MAG_APER" shash) n-cat))
	 :mag-ap-err (mapcar #'add-zp-err 
			     (extract-row (gethash "MAGERR_APER" shash) n-cat))
	 :mag-auto  (add-zpmag (aref (gethash "MAG_AUTO" shash) n-cat))
	 :mag-auto-err  (add-zp-err (aref (gethash "MAGERR_AUTO" shash) n-cat))
	 :fwhm  (* 3600 (aref (gethash "FWHM_WORLD" shash) n-cat)) ;; deg to arcsec
	 :neighbor-dist neighbor-dist
	 :neighbor-mag (if neighbor-mag (add-zpmag neighbor-mag))
	 :sextractor-flag (aref (gethash "FLAGS" shash) n-cat)
	 )))))
    

(defun %finish-auto-phot/aperture
    (fits extension targ ephem filter pixel-scale max-match-dist
     tune-peak-location tune-peak-location-npix-fit
     sextractor-catalog gain deblend-mincont
     phot-apertures/pix ;; diameters
     phot-autoparams
     saturation-level extra-sextractor-params
     xpix-pred ypix-pred zpmag zpmag-err zpmag-source
     &key
       ;; this can be :AUTO to automatically find a small radius,
       ;; or a number (arcsec), or :SCALE to use our scaling formula
       (background-aperture-radius :auto))

  ;; declare all vars pertaining to sextractor as ignored
  (declare (ignore sextractor-catalog deblend-mincont max-match-dist
		   phot-autoparams extra-sextractor-params saturation-level)
	   (type (or real (member :auto :scale)) background-aperture-radius))

  ;; our IMRED routines use zero-indexed pixels, so we turn 1 to 0
  (decf xpix-pred 1.0)
  (decf ypix-pred 1.0)
  
  
  (let* (mag-ap-list
	 mag-ap-err-list
	 (ra (slalib-ephem:ephem-ra ephem))
	 (dec (slalib-ephem:ephem-dec ephem))
	 (imsec (cf:read-image-section fits :extension extension))
	 ;; note that zpmag is for ADU units, but we're computing
	 ;; flux and error in PHOTONS by supplying gain to ap-phot,
	 ;; so we need zpmag in photon units 
	 (zpmag-phot (+ zpmag (* 2.5 (log gain 10))))
	 ;; if  background-aperture-radius=:SCALE then we use these rules for the backd ap
	 (backd-radius-min (* pixel-scale (* 1.1 (stats:max-of-elements phot-apertures/pix))))
	 (backd-radius-scale 2.0) ;; rap1 of backd is this times ap, at least
	 (backd-width 5.0)   ;; rap2 = rap1 + backd-width
	 ;;
	 (max-allowed-backd-radius 100.0) ;; for :AUTO background
	 (background-radii-arcsec nil) ;; collect background radii
	 (sky-flux-photons/arcsec2 nil) ;; collect backgrounds
	 tuned-peak
	 xpix-tuned ypix-tuned ra-tuned dec-tuned flag-tuned
	 (r-clean ;; distance to clean the image
	   (* pixel-scale ;; back to pixel units
	   (cond ((eq background-aperture-radius :auto)
		  max-allowed-backd-radius)
		 ((realp background-aperture-radius)
		  (float background-aperture-radius 1.1))
		 (t ;; else it is :SCALE
		  (+ backd-width
		     (max backd-radius-min ;; at least this many arcsec
			  (*
			   ;; at least a multiple of aperture
			   backd-radius-scale
			   ;; pixels * (arcsec/pix) = arcsec
			   (* (stats:max-of-elements phot-apertures/pix)
			      pixel-scale)))))))))
    (when (or (< xpix-pred 0)
	      (< ypix-pred 0)
	      (>= xpix-pred (aref (cf:image-section-size imsec) 0))
	      (>= ypix-pred (aref (cf:image-section-size imsec) 1)))
      (error "Object position is off the image in aperture photometry: x=~,1F y=~,1F and image is ~Ax~A"
	     xpix-pred ypix-pred
	     (aref (cf:image-section-size imsec) 0)
	     (aref (cf:image-section-size imsec) 1)))
    ;; when tuning peak
    (when tune-peak-location
      (setf tuned-peak t)
      (multiple-value-setq (xpix-tuned ypix-tuned ra-tuned dec-tuned flag-tuned)
	(ignore-errors
	 (tune-peak-xy
	  xpix-pred
	  ypix-pred
	  :search-distance 10.0 ;; larger, for blinding ap fitting
	  :n-hot-block 5
	  :n-quad-fit tune-peak-location-npix-fit
	  :imsec imsec))))
    ;;
    ;; if no tuning done, or tuning peak is not negative definite then
    ;; set the -tuned coords to be the input coords
    (when  (or  (not tune-peak-location)
		(not xpix-tuned) ;; ERROR in IGNORE-ERRORS
		flag-tuned) ;; error signaled by tuning
      ;; revert tuned values 
      (setf xpix-tuned xpix-pred
	    ypix-tuned ypix-pred
	    ra-tuned   ra
	    dec-tuned  dec
	    tuned-peak nil))
    ;; clean the image a bit beyond largest aperture - this
    ;; substitutes surrounding average for NaN and Inf
    (imutils:imclean-avg
     (cf:image-section-data imsec)
     ;; turn 1-based indices to 0-based for imutils
     :ix0 (1- (round (- xpix-tuned r-clean 4)))
     :ix1 (1- (round (+ xpix-tuned r-clean 4)))
     :iy0 (1- (round (- ypix-tuned r-clean 4)))
     :iy1 (1- (round (+ ypix-tuned r-clean 4))))
    ;; automatic background estimation by computing central 5" flux
    ;; with increasing background ap radii, until we get a steady
    ;; answer
    (when (eq background-aperture-radius :auto)
      (loop
	with max-allowed-backd-radius = 100.0
	with done = nil
	with aper/pix = (/ 5.0 pixel-scale) ;; 5" aperture is benchmark
	with last-flux = nil
	for rbg/arcsec = backd-radius-min then (* rbg/arcsec 1.2)
	for rbg/pix = (/ rbg/arcsec pixel-scale)
	until (or done (> rbg/arcsec max-allowed-backd-radius)) 
	do (multiple-value-bind (flux flux-err backd-flux/pix)
	       (imutils:ap-phot  
		(cf:image-section-data imsec)
		;; imutils takes 0-based indices
		(1- (float xpix-tuned 1.0)) (1- (float ypix-tuned 1.0))
		:r-phot (float aper/pix 1.0)
		:r-background-1 (float rbg/pix 1.0)
		:r-background-2 (float (+ rbg/pix  (/ backd-width pixel-scale)) 1.0)
		:background-noise-method :sigma
		:gain (float gain 1.0))
	     (declare (ignorable backd-flux/pix))
	     (when last-flux
	       ;; terminate when the change is 20% of the flux error
	       (when (< (abs (/ (- flux last-flux)
				(* 0.2 (max 1.0 flux-err))) ))
		 (setf done t)))
	     ;;(format t "rbg/pix=~A FLUX=~A FLUXERR=~A BACKD-FLUX/PIX=~A~%" rbg/pix flux flux-err BACKD-FLUX/PIX)
	     (setf last-flux flux))
	   ;;
	finally
	   ;;(format t "Backgound radius: ~A~%" rbg/arcsec)
	   (setf background-aperture-radius rbg/arcsec)))
		     
    
    (loop for ap in (reverse phot-apertures/pix)
	  ;; background annulus is max(BackdApMin, BackdApScale*ap)
	  for rb1/arcsec = (or
			    (if (realp background-aperture-radius) background-aperture-radius)
			    (max backd-radius-min (* backd-radius-scale ap)))
	  for rb1/pix = (/ rb1/arcsec pixel-scale)
	  for rb2/pix = (+ rb1/pix (/ backd-width pixel-scale))
	  when t ;; (< ap rb1/pix) ;; no sense going beyond backd ap
	    do (multiple-value-bind (flux flux-error ;; in electrons, not ADU
				     backd-flux/pix backd-flux-error/pix
				     npix-center npix-background
				     ;; this is the sigma in the backd annulus which
				     ;; is useful to compute the real background, taking
				     ;; into account backd subtraction
				     backd-flux-sigma/pix
				     )
		   (ignore-errors ;; might have backd failures for large apertures
		    (imutils:ap-phot 
		     (cf:image-section-data imsec)
		     ;; imutils takes 0-based indices
		     (1- (float xpix-tuned 1.0))
		     (1- (float ypix-tuned 1.0))
		     :r-phot (float (* 0.5 (float ap 1.0)) 1.0) ;; ap is DIAMETER
		     :r-background-1 (float rb1/pix 1.0)
		     :r-background-2 (float rb2/pix 1.0)
		     ;; we don't know image is backd subtracted or not,
		     ;; so we use its deviation to compute backd sigma to
		     ;; compute errors
		     :background-noise-method :sigma
		     :gain  (float gain 1.0)))
		 (declare (ignorable backd-flux/pix
				     backd-flux-error/pix
				     npix-center npix-background)) ;; we will use the sigma

		 (flet ((valid-number-p (x &optional require-positive)
			  (and x
			       (not (float-utils:single-float-nan-or-infinity-p x))
			       (or (not require-positive)
				   (plusp x)))))
		   
		   
		   (let* ((mag (if (valid-number-p flux t)
				       (+ zpmag-phot (* -2.5 (log flux 10)))
				       99.0))
			  (mag-err (if (and
					(valid-number-p flux t)
					(valid-number-p flux-error t))
				       (sqrt (+ (expt (/ flux-error flux) 2)
						(expt zpmag-err 2)))
				       99.0))
			  ;; the background flux inferred from the sigma, more trustworthy
			  ;; than raw counts if a background was subtracted
			  (backd-flux/pix-inferred
			    (if (valid-number-p backd-flux-sigma/pix t)
				(expt backd-flux-sigma/pix 2)
				0.0))
			  (backd-flux/arcsec2-inferred
			    (if (valid-number-p backd-flux/pix-inferred)
				(/ backd-flux/pix-inferred
				   (expt pixel-scale 2))
				0.0)))
		     (push rb1/arcsec background-radii-arcsec)
		     (push mag mag-ap-list)
		     (push mag-err mag-ap-err-list)
		     (push backd-flux/arcsec2-inferred  sky-flux-photons/arcsec2))))
	  finally
	     (setf background-radii-arcsec (nreverse background-radii-arcsec)))


    (make-phot-result
     :fits fits
     :photometry-type :aperture
     :instrument (type-of (instrument-id:identify-instrument fits))
     :ephem ephem
     :object targ
     :mjd (slalib-ephem:ephem-mjd ephem)
     :ra-ephem ra
     :dec-ephem dec
     ;; no catalog coords
     :ra-cat         nil
     :dec-cat        nil
     :ra-peak-cat    nil
     :dec-peak-cat   nil
     ;; Use ephem xpix-pred, ypix-pred everywhere
     :x xpix-pred
     :y ypix-pred
     :xpeak xpix-pred
     :ypeak ypix-pred
     :tuned-peak tuned-peak
     :x-tuned xpix-tuned :y-tuned ypix-tuned :ra-tuned ra-tuned :dec-tuned dec-tuned
     :match-arcsec 0.0
     :zpmag zpmag :zpmag-err zpmag-err :zpmag-source zpmag-source
     :filter filter
     :exptime (instrument-id:get-exptime-for-fits fits)
     :apertures-pix phot-apertures/pix
     :apertures-arcsec (mapcar (lambda (ap) (* pixel-scale ap)) phot-apertures/pix)
     :background-radii-arcsec  background-radii-arcsec
     :sky-flux-photons/arcsec2 sky-flux-photons/arcsec2
     :mag-ap  mag-ap-list
     :mag-ap-err mag-ap-err-list
     :mag-auto  nil
     :mag-auto-err  nil
     :fwhm  nil
     :neighbor-dist nil
     :neighbor-mag nil
     :sextractor-flag nil
     )))
  

  

(defun output-phot-result (phot-result stream)
  (declare (type phot-result phot-result))

  (let ((pr phot-result))
    (format stream "~A (UT=~A)~%" (file-io:file-minus-dir (phot-result-fits pr))
	    (astro-time:mjd-to-decimal-date-string
	     (phot-result-mjd pr)))
    (format stream "   OBJECT:  ~A~%" (phot-result-object pr))
    (format stream "   FILTER:  ~A~%" (phot-result-filter pr))
    (format stream "   INSTRUMENT:  ~A~%" (phot-result-instrument pr))
    (format stream "   MJD:  ~,5F~%" (phot-result-mjd pr))
    (format stream "   DECIMAL_DATE: ~A~%"
	    (astro-time:mjd-to-decimal-date-string
	     (phot-result-mjd pr) :ndecimal 6
				  :separator " "))
    (format stream "   UTDATE: ~A~%" (astro-time:mjd-to-ut-string
				      (phot-result-mjd pr) :frac-seconds t))
    (when (eq (phot-result-photometry-type phot-result) :sextractor)
      (format stream "   SEXTRACTOR_FLAG: ~A~%" (phot-result-sextractor-flag pr)))
    (format stream "   EXPTIME: ~,2F~%" (phot-result-exptime pr))
    (format stream "   X,Y: ~,2F, ~,2F~%" (phot-result-xpeak pr) (phot-result-ypeak pr))
    (format stream "   RA,DEC: ~A, ~A~%"
	    (ra-dec:deg->hms-string (or (phot-result-ra-peak-cat pr)
					(phot-result-ra-ephem pr)))
	    (ra-dec:deg->dms-string (or (phot-result-dec-peak-cat pr)
					(phot-result-dec-ephem pr))))
    (format stream "   POSITION_ERROR: ~,3f arcsec  (from expected)~%"
	    (phot-result-match-arcsec pr))
    
    (when (eq (phot-result-photometry-type phot-result) :sextractor)
      (format stream "   NEIGHBOR_DIST,MAG: ~,1F\", ~,2F~%"
	      (phot-result-neighbor-dist pr) (phot-result-neighbor-mag pr)))
    (when (eq (phot-result-photometry-type phot-result) :sextractor)
      (format stream "   FWHM_OBJ: ~,3F arcsec~%" (phot-result-fwhm pr)))
    (format stream "   IMG_ZEROPOINT: ~,3F +/- ~,3F (~A)~%"
	    (phot-result-zpmag pr) (phot-result-zpmag-err pr)
	    (phot-result-zpmag-source pr))
    
    (format stream "   PHOTOMETRY:~%")
    (format stream "     PHOTOMETRY_TYPE: ~A~%" (phot-result-photometry-type phot-result))
    
    (when (eq (phot-result-photometry-type phot-result) :sextractor)
      (format stream "     MAG_AUTO: ~,3F +/- ~,4F~%"
	      (phot-result-mag-auto pr)
	      (phot-result-mag-auto-err pr)))
    (format stream "     MAG_APER:~%")
    (loop for ap in (phot-result-apertures-pix pr)
	  for ap/as in (phot-result-apertures-arcsec pr)
	  for mag in (phot-result-mag-ap pr)
	  for mag-err in (phot-result-mag-ap-err pr)
	  do (format stream
		     "       APER_DIAM[~,1F pix, ~,1F\"]:~38,0T  ~,3F +/- ~,4F~%"
		     ap ap/as mag mag-err))))





			   
  


