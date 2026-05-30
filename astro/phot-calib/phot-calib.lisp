#|

Photometric calibration

The most useful routine uses MAG_AUTO. It is also possible to develop
a curve of growth, or used fixed apertures, but these are not as robust.


 (defun calibrate-image-with-catalog-type/mag-auto
    (catalog-type fits-file 
     &key
       (filter nil)  
       (min-calib-stars 20)
       (max-catalog-mag 21)
       (min-catalog-mag 0)
       (min-obj-flux 20000)
       (sextractor-reject-flags #xFFFF)
       ;; sextractor configs
       (phot-autoparams '(5.5 5.5)) ;; wide phot-autoparams
       (md5-avoid-rerun t)
       (tol/arcsec 1.0)
       (satur-level 32000)
       ;; other config
       (catalog-cache astro-catalog::*default-catalog-cache*))
   ...)

FILTER is a standard filter like :GSDSS or :VJ.   The 
function INSTRUMENT-ID:GET-STANDARD-FILTER-FOR-FITS will be used
if this is NIL.

The PHOT_AUTOPARAMS have been verified to capture over 99% of the flux
in almost all cases, and MIN-OBJ-FLUX=20000 avoids the region
where PSFs are ill determined.



|#



(in-package phot-calib)



;; the structure returned, in case we want more than the headers
(defstruct phot-calib-result
  catalog
  zpmag         ;; zero point mag to add to -2.5 log10 (flux/ADU)
  zpmag-err     ;; error on this
  zpmag-inst    ;; mag giving 1e/s
  zpmag-inst-err;; err on this
  nstars        ;; number of stars
  ok            ;; is this fit OK (probability peak looks nice)
  std-filter    ;; standard filter this is assumed to be in
  mag-5-sigma   ;; mags at which errors are 5 and 10 sigma
  mag-10-sigma
  matches  ;; pairs of form ((pcobj-image pcobj-catalog) ...)
  match-file)


;; a calib-obj that has a magnitude error as well
(defstruct (pcobj (:include astro-obj:obj))
  ;; the mag error to use for final output, associoatd with pcobj-mag
  (mag-err 0.0   :type single-float) 
  ;; the mag error used for 5,10 sigma error bound of image, because the
  ;; above mag error may be inflated for extra-big apertures
  (mag-err/s 0.0 :type single-float) 
  (xpix    0.0   :type single-float)
  (ypix    0.0   :type single-float)
  (flux    most-positive-single-float :type single-float)
  (flag 0 :type fixnum))


;; compute the mean ra,dec of a set of points, and the radius
;; return (ra0 dec0 radius) to hold all the points.
(defun %find-radec-circle-for-pcobj-list (pcobj-list)
  (when (not pcobj-list)
    (error "Zero length list of objects in %find-radec-circle-for-pcobj-list"))
  (let ((3v0 (make-array 3 :element-type 'double-float :initial-element 0d0))
	(ra0 0d0) (dec0 0d0)
	(radius 0d0))
    ;; compute mean x,y,z
    (loop 
      with 1/n = (/ 1d0 (length pcobj-list))
      with 3vec = (three-vector:make-3vec)
      for pcobj in pcobj-list
      for i from 0
      do
	 (three-vector:3vector-from-spherical-coordinates 
	  (* #.(/ pi 180) (pcobj-delta pcobj))
	  (* #.(/ pi 180) (pcobj-alpha pcobj))
	  1d0
	  :vdest 3vec)
	 ;;
	 (incf (aref 3v0 0) (* 1/n (aref 3vec 0)))
	 (incf (aref 3v0 1) (* 1/n (aref 3vec 1)))
	 (incf (aref 3v0 2) (* 1/n (aref 3vec 2))))
    ;;
    (multiple-value-bind (phi theta r0) 
	(3vec:spherical-coordinates-from-3vector 3v0)
      (when (< r0 1d-2)
	(error "Object in list seem to have average position in center of sphere - should not happen."))
      (setf dec0 (* #.(/ 180 pi)  phi))
      (setf ra0  (* #.(/ 180 pi)  theta)))
    ;;
    ;; find the maximum radius from the average position
    (loop for pcobj in pcobj-list
	  do (setf radius
		   (max radius
			(astro-coords:sky-angle 
			 ra0 dec0 
			 (pcobj-alpha pcobj) (pcobj-delta pcobj)
			 :units :degrees))))
    ;; return center position and radius
    (values ra0 dec0 radius)))
				     

;; for now, demand that all FITS-FILE are a one-chip
(defun %insist-on-onechip (fits-file)
  (when (not (typep (instrument-id:identify-instrument fits-file)
		    'instrument-id:onechip))
    (error "Fits file ~A is not a ONECHIP fits file with one image extension.  
PHOT-CALIB currently supports only ONECHIP fits files." fits-file)))
	    
;; for each object in catalog for which a valid mag-translation exists,
;; return a PCOBJ
(defun %convert-catalog-to-pcobj-list (catalog mag-translation-function
					       &key (stars-only t))
  (declare (type astro-catalog:astro-catalog catalog)
	   (type (or symbol function) mag-translation-function))
  (loop with outlist = nil
	for i below (astro-catalog:astro-catalog-n catalog)
	when (or (not stars-only)
		 (eq (astro-catalog:object-type catalog i) :star))
	do (multiple-value-bind (mag mag-err)
	       (funcall mag-translation-function catalog i)
	     (when mag ;; successful call to mag-translation-function
	       (push (make-pcobj 
		      :alpha (astro-catalog:object-ra catalog i)
		      :delta (astro-catalog:object-dec catalog i)
		      :mag (float mag 1e0)
		      :mag-err (float mag-err 1e0)
		      :id (astro-catalog:object-id catalog i))
		     outlist)))
	finally (return (nreverse outlist))))
		      
  

;; project pcobjs in list around ra0,dec0 in arcsec units
(defun %project-pcobj-list (pcobj-list ra0 dec0)
  (loop for pcobj in pcobj-list
	do (multiple-value-bind (x y)
	       (sky-project:tan-project (pcobj-alpha pcobj)
					(pcobj-delta pcobj)
					ra0 dec0 :units :arcsec)
	     (setf (pcobj-x pcobj) x
		   (pcobj-y pcobj) y))))
  

;; return (values mag-offset mag-offset-err n-stars-used)
(defun %compute-median-mag-offset/median (pcobj-pair-list)
  (cond ((zerop (length pcobj-pair-list))
	 (values 0d0 0d0 0))  ;; dmag dmag-err nstars
	(t
	 (let* ((dmag-vec-raw
		  (map '(simple-array double-float (*))
		       (lambda (pcobj-pair)
			 (let ((pcobj-targ (first pcobj-pair))
			       (pcobj-cat  (second pcobj-pair)))
			  (* 1d0 (- (pcobj-mag pcobj-cat) (pcobj-mag pcobj-targ)))))
		       pcobj-pair-list))
		(dmag-vec (stats:z-score-clip-data dmag-vec-raw :cutoff 1d0)))

	   #+nil ;; plot a histogram as a reality check
	   (let ((med-raw (stats:median-of-elements dmag-vec-raw))
		 (med     (stats:median-of-elements dmag-vec))	
		 (mode    (stats:mode-of-elements dmag-vec-raw :fwindow 0.2))
		 (windowspan 1)
		 (p (pgplot:open-device :x11)))
	     (pgplot:hist p  dmag-vec-raw :xleft (- med windowspan) :xright (+ med windowspan)) 
	     (pgplot:hist p  dmag-vec :xleft (- med windowspan) :xright (+ med windowspan)
			  :set-window nil :color :red)
	     (pgplot:connect p (vector med-raw med-raw) (vector 0 10000) :line-style :dotted)
	     (pgplot:connect p (vector med med) (vector 0 10000) :line-style :dotted
			     :color :red)
	     (pgplot:connect p (vector mode mode) (vector 0 10000) :line-style :dotted
						     :color :green)
	     (pgplot:box p))

	   (multiple-value-bind (dmag dmag-err)
	       (bootstrap:resample-one-sided-median-dev/double-float
		dmag-vec
		(length pcobj-pair-list) :frac 0.68)
	     (values dmag dmag-err (length pcobj-pair-list) t))))))


				     


;; EXTRA-ERROR is a term that is added in quadrature to the
;; sum of the catalog mag error and the image mag error.
;; It serves to limit our confidence in any individual magnitude,
;; because we know we can't trust small reported photometric errors.   
;; In practice, a value of 0.01 stabilizes the zeropoint between
;; adjacent images in a test sample a bit.

(defun %compute-mag-offset/bayes (pcobj-pair-list &key (extra-error 0.01d0))

  (cond ((zerop (length pcobj-pair-list))
	 (values 0d0 0d0 0))  ;; dmag dmag-err nstars
	(t
	 (let*
	     ((dmag-vec
		(map '(simple-array double-float (*))
		     (lambda (pcobj-pair)
		       (let ((pcobj-targ (first pcobj-pair))
			       (pcobj-cat  (second pcobj-pair)))
			 (* 1d0 (- (pcobj-mag pcobj-cat) (pcobj-mag pcobj-targ)))))
		     pcobj-pair-list))
	      (dmag-err-vec
		(map '(simple-array double-float (*))
		     (lambda (pcobj-pair)
		       (let ((s1 (pcobj-mag-err (first  pcobj-pair)))
			     (s2 (pcobj-mag-err (second pcobj-pair))))
			 (sqrt (+ (expt s1 2) (expt s2 2) (expt extra-error 2)))))
		     pcobj-pair-list))
	      (bayes-result
		(bayes-outlier:bayes-outlier-estimate-mean 
		 dmag-vec dmag-err-vec 
		 ;; hardwire these values for the background; it shouldn't be sensitive
		 :s 0.15 :f 0.15))
	      (dmag (bayes-outlier:outlier-result-xm bayes-result))
	      (dmag-err (bayes-outlier:outlier-result-xm-err bayes-result)))

	   #+nil
	   (when (not (bayes-outlier:outlier-result-ok bayes-result))
	     (error "The peak in the likelihood for the magnitude catalog/field offset is not clean, according to BAYES-OUTLIER:BAYES-OUTLIER-ESTIMATE-MEAN."))
	   
	   (values dmag dmag-err (length pcobj-pair-list)
		   (bayes-outlier:outlier-result-ok bayes-result)
		   )))))


;; compute the 5 and 10 sigma mag offsets once the correct additive mag-offset
;; is computed - return (values mag-5-sigma mag-10-sigma) 
(defun compute-5+10-sigma-mags (pcobj-img-list mag-offset)
  (loop for obj in pcobj-img-list
	for mag =  (+ (pcobj-mag obj) mag-offset)
	for mag-err = (pcobj-mag-err/s obj) ;; use the  'S'igma mag-err, not the regular one
	when (<= 0.17 mag-err 0.23)
	  collect mag into 5-sigma-mag-list
	when (<= 0.08 mag-err 0.12)
	  collect mag into 10-sigma-mag-list
	finally
	   (let ((mag-5-sigma
		   (when 5-sigma-mag-list
		     (stats:median-of-elements 5-sigma-mag-list)))
		 (mag-10-sigma
		   (when 10-sigma-mag-list
		     (stats:median-of-elements 10-sigma-mag-list))))
	     (return (values mag-5-sigma mag-10-sigma)))))


(defun phot-calib-obj-list-using-catalog
    (pcobj-list catalog mag-translation-function
     &key
       (min-obj-flux 0.0)
       (tol/arcsec 1d0)
       (max-catalog-mag 22)
       (min-catalog-mag 0)
       (mag-offset-function #'%compute-mag-offset/bayes)
       (extra-error 0.01d0) 
       (stars-only t)
       (matches-file nil))
  
  (declare (type list pcobj-list)
	   (type astro-catalog:astro-catalog catalog)
	   (type (or symbol function) mag-translation-function))
  (when (not pcobj-list)
    (error "Zero length object list in phot-calib-obj-list-using-catalog"))
  
  (let (ra0 dec0 radius
	cat-obj-grid
	matching-pairs ;; pairs of (pcobj-targ pcobj-cat)
	;; make a copy because we set x,y values when projecting
	(pcobj-list-targ (mapcar 'copy-pcobj pcobj-list))
	(pcobj-list-cat (%convert-catalog-to-pcobj-list 
			 catalog mag-translation-function
			 :stars-only stars-only)))

    (multiple-value-setq (ra0 dec0 radius)
      (%find-radec-circle-for-pcobj-list pcobj-list))

    ;; project the pcobj-lists (and COPY the original one because we change X,Y values
    (%project-pcobj-list pcobj-list-targ ra0 dec0)
    (%project-pcobj-list pcobj-list-cat ra0 dec0)
    ;; bin the stars
    (setf cat-obj-grid (aobj:bin-objects pcobj-list-cat))
    ;; find matches
    (setf matching-pairs
	  (loop with tol = (float tol/arcsec 1d0)
		for pcobj-targ in pcobj-list-targ
		for pcobj-cat = (aobj:get-nearest-object 
				 pcobj-targ cat-obj-grid tol 1d100)
		when (and pcobj-cat
			  (> (pcobj-flux pcobj-targ) min-obj-flux)
			  (< (pcobj-mag pcobj-cat) max-catalog-mag)
			  (> (pcobj-mag pcobj-cat) min-catalog-mag))
		  collect (list pcobj-targ pcobj-cat)))
    ;;
    ;; write out a file of the matches - should be a CSV
    (when matches-file
      (with-open-file (sout matches-file 
				    :direction :output 
				    :if-does-not-exist :create
				    :if-exists :supersede)
	(format sout "# Matches from catalog type ~A - last columns with \%-sign are catalog magnitudes~%" (type-of catalog))
	(format 
	 sout
	 "ObjN,  ObjMag,    ObjMagErr, ObjFlag,  Cat_ID,            CatMag,     CatMagErr,   Dmag,      DmagErr,  RA,       DEC,         Xpix,    Ypix")
	;; print out rest of mag names
	(loop with mag-list = (astro-catalog:available-mags catalog)
	      for mag-sym in mag-list
	      for mag-name = (concatenate 'string "%" (string-downcase (string mag-sym)))
	      do (format sout ", ~7@A, ~7@A" mag-name (concatenate 'string (string-downcase (string mag-name)) "_err")))
	(terpri sout)
	
	
	(loop
	  ;; a hash table that makes catalog ID to catalog number
	  with chash = (loop with h = (make-hash-table :test 'equalp)
			     for i below (astro-catalog:astro-catalog-n catalog)
			     for id = (astro-catalog:object-id catalog i)
			     do (setf (gethash id h) i)
			     finally (return h))
	  for pair in matching-pairs
	  for pcobj-img = (first pair) and pcobj-cat = (second pair)
	  for dmag = (- (pcobj-mag pcobj-cat) (pcobj-mag pcobj-img))
	  for dmag-err = (sqrt (+ (expt (pcobj-mag-err pcobj-cat) 2)
				  (expt (pcobj-mag-err pcobj-img) 2)))
	 
	  do (format 
	      sout 
	      "~4@A,  ~7,3F,   ~7,3F,  ~4D,  ~20@A,  ~7,3F,   ~7,3F,      ~7,3F,   ~7,3F, ~8,4F, ~8,4F,  ~7,1F, ~7,1F"
	      (pcobj-id pcobj-img)
	      (pcobj-mag pcobj-img) (pcobj-mag-err pcobj-img)
	      (pcobj-flag pcobj-img)
	      (pcobj-id pcobj-cat)
	      (pcobj-mag pcobj-cat) (pcobj-mag-err pcobj-cat)
	      dmag dmag-err
	      (pcobj-alpha pcobj-cat) (pcobj-delta pcobj-cat)
	      (pcobj-xpix pcobj-img)  (pcobj-ypix pcobj-img))

	     ;; write out all the catalog mags and errors
	     (loop with mag-list = (astro-catalog:available-mags catalog)
		   with icat = (gethash (pcobj-id pcobj-cat) chash) ;; the number in the catalog
		   for mag-name in mag-list
		   do (multiple-value-bind (mag mag-err)
			  (astro-catalog:object-mag catalog mag-name icat :error-if-not-exist nil)
			(if (not (< 1 mag 100))
			    (setf mag nil))
			(if mag
			    (format sout ", ~7,3F, ~7,3F" mag mag-err)
			    (format sout ",        ,        "))))
	     (terpri sout))	;;
	)) ;; end (when matches-file

    (multiple-value-bind (dmag dmag-err n is-ok)
	(funcall mag-offset-function matching-pairs :extra-error extra-error)
      ;; as a bonus compute the 5 and 10 sigma mag limits
      (multiple-value-bind (mag-5-sigma mag-10-sigma)
	  (compute-5+10-sigma-mags pcobj-list dmag)
	;;; DELETE ME - save original the mags but adjusted
	(defparameter *pcolist* (mapcar
				 (lambda (pco)
					(let ((pco2 (copy-pcobj pco)))
					  (incf (pcobj-mag pco2) (float dmag 1.0))
					  pco2))
				 pcobj-list))
	(values dmag dmag-err n  matching-pairs is-ok mag-5-sigma mag-10-sigma
		matching-pairs)))))
      

		 
  

(defun transfer-phot-calib-headers (fits-from fits-to)
  "Copy phot-calib headers from one fits to another; useful for calibrating
shifted stacks with static sky stacks."
  (let* ((extension (instrument-id:get-image-extension-for-onechip-fits
		     fits-from))
	 (headers
	   (append
	    '("PCZPMAG" "PCEZPMAG")
	    (mapcar 'first
		    (cf:read-fits-header-list
		     fits-from
		     :extension extension
		     :keyword-wildcard "PHOTCALIB.*")))))
    (cf:with-open-fits-file (fits-from ffin :mode :input)
      (cf:with-open-fits-file (fits-to ffout :mode :io)
	(cf:move-to-extension ffin extension)
	(cf:move-to-extension ffout extension)
	(cf:write-fits-header fits-to
			      "PHOTCALIB.CALIB-TRANSFER-FROM"
			      (file-io:file-minus-dir fits-from))
	(loop for key in headers
	      do (multiple-value-bind (value comment keyname)
		     (cf:read-fits-header ffin key)
		   (when keyname ;; header was present
		     (cf:write-fits-header ffout key value
					   :comment comment))))))))
					    
  

	  
(defun delete-all-photcalib-headers (fits)
  "Delete all headers like PHOTCALIB.*"
  (let ((extension (instrument-id:get-image-extension-for-onechip-fits fits)))
    (cf:delete-fits-header fits "PHOTCALIB.*" :extension extension)
    ;; delete old-style convenience headers too
    (cf:delete-fits-header fits "PCZPMAG" :extension extension)
    (cf:delete-fits-header fits "PCEZPMAG" :extension extension)))
    
    
;; compute the center and radius of a fits file using the wcs
;; return (values ra0 dec0 radius) where radius is grown
;; from actual field radius by EXPANSION-FACTOR
(defun %get-fits-bounds (fits-file &key (expansion-factor 1.5))
  (cf:with-open-fits-file (fits-file ff :mode :input)
    (cf:move-to-extension ff (instrument-id:get-image-extension-for-onechip-fits fits-file))
    (let ((wcs (or (cf:read-wcs ff)
		   (error "WCS not found in ~A" fits-file)))
	  (naxis1 (or (cf:read-fits-header ff "NAXIS1")
		      (error "NAXIS1 not found in ~A fits-file" fits-file)))
	(naxis2 (or (cf:read-fits-header ff "NAXIS2")
		    (error "NAXIS2 not found in ~A fits-file" fits-file)))
	  ra0 dec0 ra-corner dec-corner rad)
      (multiple-value-setq (ra0 dec0)
	(wcs:wcs-convert-pix-xy-to-ra-dec wcs (* 0.5 naxis1) (* 0.5 naxis2)))
      (multiple-value-setq (ra-corner dec-corner)
	(wcs:wcs-convert-pix-xy-to-ra-dec wcs 1 1))
      (setf rad (astro-coords:sky-angle ra0 dec0 ra-corner dec-corner :units :degrees))
      (values
       ra0 dec0
       (* expansion-factor rad)))))



(defun calibrate-image-using-catalog/cog
    (catalog fits-file
     &key
       (filter nil)
       (cog-apertures terapix::*curve-of-growth-apertures*)
       (phot-aperture nil)
       (phot-aperture-dmag 0.07)
       (min-cog-stars 20)
       (min-calib-stars 20)
       (min-cog-flux 5000)
       (max-catalog-mag 21)
       (min-catalog-mag 0)
       (min-obj-flux 10000)
       (sextractor-reject-flags #xFFFF)
       (stars-only t)
       (extra-error 0.01d0)
       ;; sextractor configs
       (md5-avoid-rerun t)
       (tol/arcsec 1.0)
       (satur-level 50000) ;; WARNING - doesn't always work in sextractor
       (peakflux-max 40000)   ;; backup for satur-level
       (write-headers t))
  "Uses ASTRO-CATALOG:CATALOG to calibrate FITS-FILE in FILTER (a
member of *ALLOWED-FILTERS*) with aperture photometry.

COG-APERTURES is the apertures to compute the curve of growth.
PHOT-APERTURE is the aperture to use for the photometry - it
                 must be one of COG-APERTURES.  If NIL, use
                 PHOT-APERTURE-DMAG to pick the first aperture that
                 contains less than PHOT-APERTURE-DMAG magnitudes 
PHOT-APERTURE-DMAG is (if PHOT-APERTURE isn't specified) the 
                   dmag along the curve of growth that is used to
                   pick the aperture for performing photometry.
MIN-COG-STARS is the minimal number of stars for computing curve-of-growth
MIN-CALIB-STARS is the minimal number of stars for performing a calibration
MAX-CATALOG-MAG is the dimmest catalog object to use for calibration
MIN-CATALOG-MAG is the brightest catalog object to use for calibration
EXTRA-ERROR     is an extra error contribution to overcome excessively 
                optimistic input catalogs.

The process is

       1. compute a sextractor catalog using the COG-APERTURES.
       2. compute a curve of growth
          a. insert into catalog
          b. insert into fits file (this will mess up MD5 used by
             sextractor for computing whether to re-run catalog)
       3. compute set of PCOBJ using corrected ap magitudes from curve  
          of growth
       4. compute a magnitude offset for the full corrected mag
          and write into fits header for file, and catalog
             PHOTCALIB.ZPMAG    - the zeropoint corrected to large aperture
             PHOTCALIB.ZPMAGERR - the error on zeropoint
             PHOTCALIB.NSTARS   - the number of stars used"

  (%insist-on-onechip fits-file) ;; must be onechip fits file
  (when (and phot-aperture (not (member phot-aperture cog-apertures)))
    (error "The primary PHOT-APERTURE=~A must be one of curve-of-growth COG-APERTURES=~A"
	   phot-aperture cog-apertures))
  
  (when (not (or phot-aperture phot-aperture-dmag))
    (error "One of PHOT-APERTURE or PHOT-APERTURE-DMAG must be set."))
  

  (when (zerop (astro-catalog:astro-catalog-n catalog))
    (error "Catalog ~A has no stars to use as calibrators." catalog))

  (let* ((std-filter (or filter
			 (instrument-id:get-standard-filter-for-fits fits-file)
			 (error "Filter can't be inferred for ~A" fits-file)))
	 (working-dir (terapix:get-fits-directory 
		       fits-file :if-does-not-exist t))
	 (phot-calib-catalog-file "sex_phot_calib_ap_cog.cat")
	 (phot-calib-catalog-file-fullpath
	   (format nil "~A/~A" working-dir phot-calib-catalog-file))
	 (matches-file (format nil "sex_phot_calib_ap_cog_matches_~A.csv"
			       (type-of catalog)))
	 (matches-file-fullpath
	   (format nil "~A/~A" working-dir matches-file))
	 (phot-calib-cog-file "sex_phot_calib_curve_of_growth.dat")
	 (mag-translation-function 
	   (or (get-mag-trans-func-for-catalog catalog std-filter)
	       (error "Cannot translate catalog of type ~A to magnitude ~A" 
		      (type-of catalog) filter)))
	 (cog-apertures (sort cog-apertures  '< )) ;; just in case
	 (scog ;; curve of growth object
	   (terapix:compute-aperture-curve-of-growth-for-fits-file 
	    fits-file
	    :phot-apertures cog-apertures
	    :min-flux min-cog-flux 
	    :deblend-mincont 1.0 
	    :display-errors nil 
	    :satur-level satur-level :md5-avoid-rerun md5-avoid-rerun
	    :output-table-file phot-calib-cog-file
	    :catalog-name phot-calib-catalog-file
	    :write-cog-to-catalog-headers t
	    :write-cog-to-image-headers t)))
    ;;
    (when (< (terapix:curve-of-growth-nstars scog) min-cog-stars)
      (error "Not enough stars for curve of growth: MIN-COG-STARS=~A but we have just ~A"
	     min-cog-stars (terapix:curve-of-growth-nstars scog)))

    (let* ((shash (terapix:add-neighbor-dist-to-sextractor-catalog
		   (terapix:read-sextractor-catalog 
		    phot-calib-catalog-file-fullpath)))
	   (apertures (gethash "%APERTURES" shash))
	   (ra-vec (gethash "ALPHA_J2000" shash))
	   (dec-vec (gethash "DELTA_J2000" shash))
	   (num-vec (gethash "NUMBER" shash))
	   (flag-vec (gethash "FLAGS" shash))
	   (xpixvec  (gethash "XWIN_IMAGE" shash))
	   (ypixvec  (gethash "YWIN_IMAGE" shash))
	   (peakflux-max-vec (gethash "FLUX_MAX" shash))
	   ;;
	   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	   ;; a bit of messy code to find the right phot-aperture and
	   ;; its index NAP
	   (%phot-aperture-and-nap-pair
	     (or 
	      ;; use supplied aperture
	      (when phot-aperture
		   (list phot-aperture
			   (position phot-aperture apertures :test 'eql)))
	      ;; or find the one that has no more than PHOT-APERTURE-DMAG
	      ;; of flux beyond it
	      (loop 
		for ap across (terapix::curve-of-growth-aperture-vec/pix
			       scog)
		for dmag across (terapix::curve-of-growth-dmag-vec
				 scog)
		for iap from 0
		when (<= (abs dmag) phot-aperture-dmag)
		  do (return  (list ap iap))
		finally ;; shouldn't happen
		(error "Could not find aperture for PHOT-APERTURE-DMAG=~A in CurveOfGrowth=~A" phot-aperture-dmag scog))))
	   (%phot-aperture (first %phot-aperture-and-nap-pair))
	   (nap (or (second %phot-aperture-and-nap-pair)
		    (error "Could not find NAP for PHOT-APERTURE=~A%. Should not happen." %phot-aperture)))
	   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	   ;;
	   (mag-array (gethash "MAG_APER" shash))
	   (magerr-array (gethash "MAGERR_APER" shash))
	   (mag-auto-vec (gethash "MAG_AUTO" shash))
	   (flux-auto-vec (gethash "FLUX_AUTO" shash))
	   (fits-pcobj-list nil)
	   (mb-map-list nil) ;; list of MAG_AUTO-MAG_AP_CORRECTED
	   (mag-auto-correction -100)) ;; amount to add to mag-auto to make
                                      ;; it match MAG_AP_CORRECTED
      
      (loop with neighbor-dist-vec = (gethash "%NEIGHBOR-DIST" shash)
	    for neighbor-dist across neighbor-dist-vec
	    for i below (length ra-vec)
	    for mag = (float (aref mag-array i nap) 1d0)
	    for mag-auto across mag-auto-vec
	    for flux-auto across flux-auto-vec
	    ;; correct for curve of growth
	    for cormag = (terapix:correct-aperture-mag-using-curve-of-growth 
			  scog mag %phot-aperture :units :pixel)
	    when (and (zerop (logand sextractor-reject-flags 
				     (aref flag-vec i)))
		      (< (aref peakflux-max-vec i) peakflux-max)
		      (> neighbor-dist (* 1.5 (* 0.5 %phot-aperture))))
	      do
		 (push (make-pcobj 
			:id (aref num-vec i)
			:alpha (float (aref ra-vec i) 1d0)
			:delta (float (aref dec-vec i) 1d0)
			:xpix (float (aref xpixvec i) 1.0)
			:ypix (float (aref ypixvec i) 1.0)
			:mag (float 1.0 cormag)
			:flux (float flux-auto 1.0)
			:flag (aref flag-vec i)
			:mag-err (float (aref magerr-array i nap) 1e0)
			:mag-err (float (aref magerr-array i nap) 1e0)
			;; mag-error for 5,10 sigmas is same as main one
			:mag-err/s (float (aref magerr-array i nap) 1e0)
			)
		       fits-pcobj-list)
		 (when (> flux-auto 5000) 
		   (push (- cormag mag-auto) mb-map-list)))
      
      (when (plusp (length mb-map-list))
	(setf mag-auto-correction (stats:median-of-elements mb-map-list)))

      (when (not fits-pcobj-list)
	(error "No acceptable objects found for calibration."))
      
      (multiple-value-bind (zp zperr nstars dummy is-ok
			    mag-5-sigma mag-10-sigma
			    matching-pairs)
	  (phot-calib-obj-list-using-catalog fits-pcobj-list
					     catalog mag-translation-function
					     :min-obj-flux min-obj-flux
					     :tol/arcsec tol/arcsec
					     :extra-error extra-error
					     :max-catalog-mag max-catalog-mag
					     :min-catalog-mag min-catalog-mag
					     :stars-only stars-only
					     :matches-file matches-file-fullpath)
	(declare (ignorable dummy))
	(when (< nstars min-calib-stars)
	  (error "Found ~A catalog calibration stars but MIN-CALIB-STARS=~A" 
		 nstars min-calib-stars))

	(let ((zptel
		(ignore-errors
		 (+ zp
		    (* -2.5 (log (instrument-id:get-exptime-for-fits
				  fits-file)
				 10))
		    (* 2.5 (log (instrument-id:get-gain-for-fits
				 fits-file)
				10))))))
	  ;; 
	  (loop for hfits
		  in (if write-headers
			 `(,fits-file ,phot-calib-catalog-file-fullpath)
			 `(,phot-calib-catalog-file-fullpath))
		for extension
		  in (if write-headers
			 `(,(instrument-id:get-image-extension-for-onechip-fits
			     fits-file)
			   1)
			 '(1))
		do
		   (cf:write-fits-header 
		    hfits "PHOTCALIB.MAGTYPE" "MAG_AP_COG" 
		    :comment "MAG_AP corrected for COG"
		    :extension extension)
		   (cf:write-fits-header
		    hfits "PHOTCALIB.MAGAUTOCOR" mag-auto-correction
		    :comment "fix MAG_AUTO for BRIGHT obj"
		    :extension extension)
		   (cf:write-fits-header
		    hfits "PHOTCALIB.ZPMAG" zp 
		    :comment "ZP to add to M=-2.5log10(flux/ADU)"
		    :extension extension)
		   (cf:write-fits-header 
		    hfits "PHOTCALIB.ZPMAGERR" zperr
		    :comment "Error on PHOTCALIB.ZPMAG"
		    :extension extension)
		   (cf:write-fits-header
		    hfits "PCZPMAG" zp 
		    :comment "Synonym for PHOTCALIB.ZPMAG"
		    :extension extension)
		   (cf:write-fits-header
		    hfits "PCEZPMAG" zperr 
		    :comment "Synonym for PHOTCALIB.ZPMAGERR"
		    :extension extension)
		   (cf:write-fits-header 
		    hfits "PHOTCALIB.NSTARS" nstars 
		    :comment "No. of stars used for PHOTCALIB.ZPMAG"
		    :extension extension)
		   (cf:write-fits-header 
		    hfits "PHOTCALIB.APERTURE" %phot-aperture
		    :comment "Aperture (pix) for PHOTCALIB.ZPMAG"
		    :extension extension)
		   (cf:write-fits-header 
		    hfits "PHOTCALIB.FILTER" std-filter 
		    :comment "Assumed filter in PHOTCALIB package"
		    :extension extension)
		   (cf:write-fits-header 
		    hfits "PHOTCALIB.CATALOGTYPE" (type-of catalog)
		    :extension extension)
		   (cf:write-fits-header 
		    hfits "PHOTCALIB.OK" is-ok
		    :comment "Do we trust this calib?"
		    :extension extension)
		   ;;
		   (when zptel
		     (cf:write-fits-header
		      hfits "PHOTCALIB.ZPTEL"  zptel
		      :comment "Mag giving 1e-/sec"
		      :extension extension))
		   ;;
		   (when mag-5-sigma
		      (cf:write-fits-header
		      hfits "PHOTCALIB.MAG-5-SIGMA" mag-5-sigma
		      :comment "Mag giving 5 sigma err"
		      :extension extension))
		   (when mag-10-sigma
		     (cf:write-fits-header
		      hfits "PHOTCALIB.MAG-10-SIGMA" mag-10-sigma
		      :comment "Mag giving 10 sigma err"
		      :extension extension))
		)
	  	  ;;
	  (make-phot-calib-result
	   :catalog (type-of catalog)
	   :zpmag zp
	   :zpmag-err zperr
	   :nstars nstars
	   :zpmag-inst  zptel
	   :zpmag-inst-err zperr
	   :mag-5-sigma mag-5-sigma
	   :mag-10-sigma mag-10-sigma
	   :ok is-ok
	   :std-filter std-filter
	   :matches matching-pairs
	   :match-file matches-file-fullpath))))))



(defun calibrate-image-with-catalog-type/cog
    (catalog-type fits-file 
     &key
       (filter nil)
       (cog-apertures terapix::*curve-of-growth-apertures*)
       (phot-aperture nil)
       (phot-aperture-dmag 0.07)
       (min-cog-stars 20)
       (min-calib-stars 20)
       (max-catalog-radius 1.0)  ;; limit the catalog radius to this many deg
       (min-cog-flux 5000)
       (max-catalog-mag 21)
       (min-catalog-mag 0)
       (min-obj-flux 10000)
       (sextractor-reject-flags #xFFFF)
       (stars-only t)
       (extra-error 0.01d0)
       ;; sextractor configs
       (md5-avoid-rerun t)
       (tol/arcsec 1.0)
       (satur-level 50000)
       (peakflux-max 40000)
       ;; other config
       (catalog-cache astro-catalog::*default-catalog-cache*)
       (write-headers t))
  "Like CALIBRATE-IMAGE-USING-CATALOG (see documentation) except that it takes CATALOG-TYPE
instead of CATALOG, computes the fits bounds, and downloads the catalog.

FILTER must be a standard filter keyword like :VJ or :GSDSS. If it 
is NIL, then INSTRUMENT-ID:GET-STANDARD-FILTER-FOR-FITS
is used to infer the filter.

CATALOG-TYPE must be one of ASTRO-CATALOG:*ALLOWED-CACHE-CATALOG-TYPES*"

  (%insist-on-onechip fits-file) ;; must be onechip fits file
  (when (not (member catalog-type astro-catalog:*allowed-cache-catalog-types*))
    (error "CATALOG-TYPE=~A is not one of ~A" 
	   catalog-type astro-catalog:*allowed-cache-catalog-types*))
  (when write-headers (delete-all-photcalib-headers fits-file))
  (multiple-value-bind (ra0 dec0 radius)
      (%get-fits-bounds fits-file)
    (let ((catalog (astro-catalog:get-cached-catalog-object 
		    ra0 dec0 (min max-catalog-radius radius)
		    catalog-type
		    :catalog-cache catalog-cache)))
      (calibrate-image-using-catalog/cog 
       catalog
       fits-file
       :filter filter
       :cog-apertures cog-apertures
       :phot-aperture phot-aperture
       :phot-aperture-dmag phot-aperture-dmag
       :min-cog-stars min-cog-stars
       :min-calib-stars min-calib-stars
       :min-cog-flux min-cog-flux
       :max-catalog-mag max-catalog-mag
       :min-catalog-mag min-catalog-mag
       :md5-avoid-rerun md5-avoid-rerun
       :min-obj-flux min-obj-flux
       :sextractor-reject-flags sextractor-reject-flags
       :stars-only stars-only
       :extra-error extra-error
       :tol/arcsec tol/arcsec
       :satur-level satur-level
       :peakflux-max peakflux-max
       :write-headers t))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; a version that uses MAG_AUTO for images for which we can't get COG
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun calibrate-image-using-catalog/mag-auto
    (catalog fits-file 
     &key
       (filter nil)
       (min-calib-stars 20)
       (max-catalog-mag 21)
       (min-catalog-mag 0)
       (min-obj-flux 20000)
       (sextractor-reject-flags #xFFFF)
       (stars-only t)
       (extra-error 0.01d0)
       (use-corrected-apertures t)
       ;; sextractor configs
       (md5-avoid-rerun t)
       (phot-autoparams '(5.5 5.5)) ;; wide phot-autoparams
       (phot-apertures  '(5 10 15 20 25 30 40)) ;; do some ap photometry (used for optional correction)
       (tol/arcsec 1.0)
       (satur-level 50000)   ;; WARNING - doesn't always work in sextractor
       (peakflux-max    40000)
       (write-headers t))  ;; backup for satur-level
  "Uses ASTRO-CATALOG:CATALOG to calibrate FITS-FILE in FILTER (a
member of *ALLOWED-FILTERS*) with aperture photometry.

FILTER must be a standard filter keyword like :VJ or :GSDSS. If it 
is NIL, then INSTRUMENT-ID:GET-STANDARD-FILTER-FOR-FITS
is used to infer the filter.

Uses sextractor MAG_AUTO instead of computing a curve of growth.

If USE-CORRECTED-APERTURES is set, then a pseudo- curve of growth is
used to correct the smallest aperture MAG_APER with most of the flux,
rather than using MAG_AUTO.   

MIN-CALIB-STARS is the minimal number of stars for performing a calibration
MAX-CATALOG-MAG is the dimmest catalog object to use for calibration
MIN-CATALOG-MAG is the brightest catalog object to use for calibration

The process is

       1. compute set of PCOBJ using MAG_AUTO
       2. compute a magnitude offset for the full corrected mag
          and write into fits header for file, and catalog
             PHOTCALIB.ZPMAG    - the zeropoint corrected to large aperture
             PHOTCALIB.ZPMAGERR - the error on zeropoint
             PHOTCALIB.NSTARS   - the number of stars used


Note that sextractor PHOT_AUTOPARAMS is set to 5.5, 5.5 which is much
larger than the default 2.5,3.5.  This is necessary to capture all the
night and stabilize the result.  It is possible to make it smaller, at
the cost of having more badly calibrated frames.

Tests show that values of PHOT_AUTOPARAMS larger than 5,5 capture over
99% of the light.  See tests in phot-calib-tests/mag-auto-expand.lisp
"

  (%insist-on-onechip fits-file) ;; must be onechip fits file
  (when (zerop (astro-catalog:astro-catalog-n catalog))
    (error "Catalog ~A has no stars to use as calibrators." catalog))
  (when write-headers (delete-all-photcalib-headers fits-file))
  (let* ((std-filter (or filter
			 (instrument-id:get-standard-filter-for-fits fits-file)
			 (error "Filter can't be inferred for ~A" fits-file)))
	 (ext (instrument-id:get-image-extension-for-onechip-fits fits-file))
	 (gain (or (instrument-id:get-gain-for-fits fits-file)
		   (error "Could not get GAIN for ~A" fits-file)))
	 (working-dir (terapix:get-fits-directory 
		       fits-file :if-does-not-exist t
		       :extension ext))
	 (phot-calib-catalog-file "sex_phot_calib_mag_auto.cat")
	 (phot-calib-catalog-file-fullpath
	   (format nil "~A/~A" working-dir phot-calib-catalog-file))
	 (matches-file (format nil "sex_phot_calib_mag_auto_matches_~A.csv"
			       (type-of catalog)))
	 (matches-file-fullpath
	   (format nil "~A/~A" working-dir matches-file))
	 (mag-translation-function 
	   (or (get-mag-trans-func-for-catalog catalog std-filter)
	       (error "Cannot translate catalog of type ~A to magnitude ~A" 
		      (type-of catalog) std-filter))))
    
    (terapix:run-sextractor 
     fits-file
     :extension ext
     :conf-suffix "_PHOTCALIB_MAGAUTO"
     :display-errors nil ;; too much output
     :output-catalog phot-calib-catalog-file
     :md5-avoid-rerun md5-avoid-rerun
     :satur-level satur-level
     :deblend-mincont 1.0 ;; don't deblend (eg trails)
     :gain gain
     :phot-autoparams phot-autoparams
     :phot-apertures phot-apertures) 
    ;;
    (let* ((shash (terapix:read-sextractor-catalog 
		   phot-calib-catalog-file-fullpath))		   
	   (ra-vec  (gethash "ALPHA_J2000" shash))
	   (dec-vec (gethash "DELTA_J2000" shash))
	   (num-vec (gethash "NUMBER" shash))
	   (flag-vec (gethash "FLAGS" shash))
	   (peakflux-max-vec (gethash "FLUX_MAX" shash))
	   (mag-vec (gethash "MAG_AUTO" shash))
	   (flux-vec (gethash "FLUX_AUTO" shash))
	   (xpixvec  (gethash "XWIN_IMAGE" shash))
	   (ypixvec  (gethash "YWIN_IMAGE" shash))
	   (magerr-vec (gethash "MAGERR_AUTO" shash))

	   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	   ;; Block below fills vector MAGERR/S-VEC with magnitude errors from
	   ;; aperture that is just larger than 2x the FWHM. This is to provide
	   ;; a good estimate for 5,10 sigma errors of image without using inflated
	   ;; aperture.
	   ;;
	   ;; fwhm to pick a suitable aperture to use for 5,10 sigma
	   ;; estimate - can be NIL
	   (fwhm (%estimate-fwhm-pix-from-a-img
		  (gethash "A_IMAGE" shash)
		  magerr-vec))
	   ;; find the index of theaperture that is the first one more than 2x the fwhm
	   ;; to use as an error
	   (aperture-index-for-sigmas ;; can be NIL if failed
	     (when fwhm
	       (loop for ap in phot-apertures  ;; all are in pix units
		     for iap from 0
		     when (>= ap (* 2.0 fwhm))
		       do (return iap)
		     finally (return nil))))
	   (magerr-aper-arr (gethash "MAGERR_APER" shash)) ;; vector of array depending on #apertures
	   ;; a vector of mag errors to use for computing 5,10 sigma limits of image
	   (magerr/s-vec ;; can be NIL
	     (when (and aperture-index-for-sigmas magerr-aper-arr)
	       (let* ((nd (length (array-dimensions magerr-aper-arr))) ;; now many dims (1 or 2)
		      (npts (array-dimension magerr-aper-arr  0)) ;; how many objects
		      (naper (if (= nd 2) ;; how many apertures we see
				 (array-dimension magerr-aper-arr 1)
				 1))
		      (merrout (make-array npts)))
		 (when (not (= naper (length phot-apertures)))
		   (error "This should not happen - number of apertures in MAGERR_APER of sextractor file is less than length of PHOT-APERTURES"))
		 (loop for i below npts
		       do (setf (aref merrout i)
				(cond ((= nd 1)
				       (aref magerr-aper-arr i))
				      (t ;; two d array
				       (aref magerr-aper-arr i aperture-index-for-sigmas)))))
		 merrout)))
	   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
			  
				      
				      
	   ;;
	   (mag-vec-final mag-vec) (magerr-vec-final magerr-vec)
	   fits-pcobj-list)

      #+nil
      (format t "Using aperture ~A, index ~A for fwhm=~A~%"
	      (nth  aperture-index-for-sigmas phot-apertures)
	      aperture-index-for-sigmas
	      fwhm)

      ;; use corrected aperture mags (using MAG_AUTO) instead of
      ;; MAG_AUTO itself, if possible
      (when use-corrected-apertures
	(multiple-value-bind (mag-vec-cor magerr-vec-cor
			      ;; how many mags were fixed using aperture correction
			      noriginal ncorrected)
	    (compute-corrected-mag-auto-vector shash)
	  (declare (ignorable noriginal ncorrected)) ;; these are for diagnostics
	  (setf mag-vec-final mag-vec-cor)
	  (setf magerr-vec-final magerr-vec-cor)))
	  

      (setf fits-pcobj-list 
	    (loop for i below (length ra-vec)
		  when (and (zerop (logand sextractor-reject-flags
					   (aref flag-vec i)))
			    (< (aref peakflux-max-vec i) peakflux-max))
		    collect (make-pcobj 
			     :id (aref num-vec i)
			     :alpha (float (aref ra-vec i) 1d0)
			     :delta (float (aref dec-vec i) 1d0)
			     :xpix (float (aref xpixvec i) 1.0)
			     :ypix (float (aref ypixvec i) 1.0)
			     :mag (float (aref mag-vec-final i) 1e0)
			     :flux (float (aref flux-vec i) 1.0)
			     :mag-err (float (aref magerr-vec-final i) 1e0)
			     ;; mag error for computing 5,10 sigma image mags - if we have
			     ;; a good aperture use that instead of magerr-vec-final
			     :mag-err/s (float (aref (or magerr/s-vec magerr-vec-final) i) 1e0)
			     )))


      (when (not fits-pcobj-list)
	(error "No acceptable objects found for calibration."))
	 
      
      (multiple-value-bind (zp zperr nstars dummy is-ok
			    mag-5-sigma mag-10-sigma
			    matching-pairs)
	  (phot-calib-obj-list-using-catalog fits-pcobj-list
					     catalog mag-translation-function
					     :tol/arcsec tol/arcsec
					     :extra-error extra-error
					     :min-obj-flux min-obj-flux
					     :max-catalog-mag max-catalog-mag
					     :min-catalog-mag min-catalog-mag
					     :stars-only stars-only
					     :matches-file matches-file-fullpath)
	(declare (ignorable dummy))
	(when (< nstars min-calib-stars)
	  (error "Found ~A catalog calibration stars but MIN-CALIB-STARS=~A" 
		 nstars min-calib-stars))

	(let ((zptel (ignore-errors
		      (+ zp
			 (* -2.5 (log (instrument-id:get-exptime-for-fits fits-file) 10))
			 (* 2.5 (log (instrument-id:get-gain-for-fits fits-file) 10))))))
	  ;; write headers to both image and catalog file if asked
	  (loop for hfits
		  in (if write-headers
			 `(,fits-file ,phot-calib-catalog-file-fullpath)
			 `(,phot-calib-catalog-file-fullpath))
		for extension
		  in (if write-headers
			 `(,(instrument-id:get-image-extension-for-onechip-fits
			     fits-file)
			   1)
			 '(1))
		do
		   (cf:write-fits-header 
		    hfits "PHOTCALIB.MAGTYPE" "MAG_AUTO"
		    :comment "sextractor MAG_AUTO"
		    :extension extension)
		   (cf:write-fits-header
		    hfits "PHOTCALIB.MAGAUTOCOR" 0.0
		    :comment "Addit. cor. for MAG_AUTO"
		    :extension extension)
		   (cf:write-fits-header
		    hfits "PHOTCALIB.ZPMAG" zp 
		    :comment "ZP to add to M=-2.5log10(flux/ADU)"
		    :extension extension)
		   (cf:write-fits-header 
		    hfits "PHOTCALIB.ZPMAGERR" zperr
		    :comment "Error on PHOTCALIB.ZPMAG"
		    :extension extension)
		   (cf:write-fits-header
		    hfits "PCZPMAG" zp 
		    :comment "Synonym for PHOTCALIB.ZPMAG"
		    :extension extension)
		   (cf:write-fits-header
		    hfits "PCEZPMAG" zperr 
		    :comment "Synonym for PHOTCALIB.ZPMAGERR"
		    :extension extension)
		   (cf:write-fits-header 
		    hfits "PHOTCALIB.NSTARS" nstars 
		    :comment "No. of stars used for PHOTCALIB.ZPMAG"
		    :extension extension)
		   (cf:write-fits-header 
		    hfits "PHOTCALIB.FILTER" std-filter 
		    :comment "Assumed filter in PHOTCALIB package"
		    :extension extension)
		   (cf:write-fits-header 
		    hfits "PHOTCALIB.CATALOGTYPE" (type-of catalog)
		    :extension extension)
		   (cf:write-fits-header 
		    hfits "PHOTCALIB.OK" is-ok
		    :comment "Do we trust this calib?"
		    :extension extension)
		   (when zptel
		     (cf:write-fits-header
		      hfits "PHOTCALIB.ZPTEL"  zptel
		      :comment "Mag giving 1e-/sec"
		      :extension extension))
		   ;;
		   (when mag-5-sigma
		      (cf:write-fits-header
		      hfits "PHOTCALIB.MAG-5-SIGMA" mag-5-sigma
		      :comment "Mag giving 5 sigma err"
		      :extension extension))
		   (when mag-10-sigma
		     (cf:write-fits-header
		      hfits "PHOTCALIB.MAG-10-SIGMA" mag-10-sigma
		      :comment "Mag giving 10 sigma err"
		      :extension extension))
		)
	  ;;
	  (make-phot-calib-result
	   :catalog (type-of catalog)
	   :zpmag zp
	   :zpmag-err zperr
	   :nstars nstars
	   :zpmag-inst  zptel
	   :zpmag-inst-err zperr
	   :mag-5-sigma mag-5-sigma
	   :mag-10-sigma mag-10-sigma
	   :ok is-ok
	   :std-filter std-filter
	   :matches matching-pairs
	   :match-file matches-file-fullpath))))))


(defun calibrate-image-with-catalog-type/mag-auto
    (catalog-type fits-file
     &key
       (filter nil)
       (min-calib-stars 20)
       (max-catalog-radius 1.0)  ;; limit the catalog radius to this many deg
       (max-catalog-mag 21)
       (min-catalog-mag 0)
       (min-obj-flux 20000)
       (sextractor-reject-flags #xFFFF)
       (stars-only t)
       (extra-error 0.01d0)
       (use-corrected-apertures t)
       ;; sextractor configs
       (phot-autoparams '(5.5 5.5)) ;; wide phot-autoparams
       (phot-apertures  '(5 10 15 20 25 30 40)) ;; do some ap photometry (not used)
       (md5-avoid-rerun t)
       (tol/arcsec 1.0)
       (satur-level 50000)
       (peakflux-max 40000)
       ;; other config
       (catalog-cache astro-catalog::*default-catalog-cache*)
       (write-headers t))
  "Like CALIBRATE-IMAGE-USING-CATALOG/MAG-AUTO (see documentation)
except that it takes CATALOG-TYPE instead of CATALOG, computes the
fits bounds, and downloads the catalog.

FILTER must be a standard filter keyword like :VJ or :GSDSS. If it 
is NIL, then INSTRUMENT-ID:GET-STANDARD-FILTER-FOR-FITS
is used to infer the filter.

CATALOG-TYPE must be one of ASTRO-CATALOG:*ALLOWED-CACHE-CATALOG-TYPES*"

  (%insist-on-onechip fits-file) ;; must be onechip fits file
  (when (not (member catalog-type astro-catalog:*allowed-cache-catalog-types*))
    (error "CATALOG-TYPE=~A is not one of ~A" 
	   catalog-type astro-catalog:*allowed-cache-catalog-types*))
  (multiple-value-bind (ra0 dec0 radius)
      (%get-fits-bounds fits-file)
    (let ((catalog (astro-catalog:get-cached-catalog-object 
		    ra0 dec0 (min max-catalog-radius radius)
		    catalog-type
		    :catalog-cache catalog-cache)))

      (calibrate-image-using-catalog/mag-auto 
       catalog fits-file 
       :filter filter
       :min-obj-flux    min-obj-flux
       :sextractor-reject-flags sextractor-reject-flags
       :stars-only stars-only
       :min-calib-stars min-calib-stars
       :max-catalog-mag max-catalog-mag
       :min-catalog-mag min-catalog-mag
       :phot-autoparams phot-autoparams
       :phot-apertures phot-apertures
       :md5-avoid-rerun md5-avoid-rerun
       :tol/arcsec tol/arcsec
       :extra-error extra-error
       :use-corrected-apertures use-corrected-apertures
       :satur-level satur-level
       :peakflux-max peakflux-max
       :write-headers write-headers))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; a version that uses fixed MAG_APER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun calibrate-image-using-catalog/ap
    (catalog fits-file 
     &key
       (filter nil)
       (aperture 20) ;; aperture diameter in pix
       (min-calib-stars 20)
       (max-catalog-mag 21)
       (min-catalog-mag 0)
       (min-obj-flux 20000)
       (sextractor-reject-flags #xFFFF)
       (stars-only t)
       (extra-error 0.01d0)
       ;; sextractor configs
       (md5-avoid-rerun t)
       (tol/arcsec 1.0)
       (satur-level 50000)   ;; WARNING - doesn't always work in sextractor
       (peakflux-max    40000)
       (write-headers t))  ;; backup for satur-level
  "Uses ASTRO-CATALOG:CATALOG to calibrate FITS-FILE in FILTER (a
member of *ALLOWED-FILTERS*) with aperture photometry. 

FILTER must be a standard filter keyword like :VJ or :GSDSS. If it 
is NIL, then INSTRUMENT-ID:GET-STANDARD-FILTER-FOR-FITS
is used to infer the filter.

Uses sextractor MAG_APER at fixed diameter aperture APERTURE pixels instead of
computing a curve of growth.

MIN-CALIB-STARS is the minimal number of stars for performing a calibration
MAX-CATALOG-MAG is the dimmest catalog object to use for calibration
MIN-CATALOG-MAG is the brightest catalog object to use for calibration

The process is

       1. compute set of PCOBJ using MAG_AUTO
       2. compute a magnitude offset for the full corrected mag
          and write into fits header for file, and catalog
             PHOTCALIB.ZPMAG    - the zeropoint corrected to large aperture
             PHOTCALIB.ZPMAGERR - the error on zeropoint
             PHOTCALIB.NSTARS   - the number of stars used"
  
  (%insist-on-onechip fits-file) ;; must be onechip fits file
  (when (zerop (astro-catalog:astro-catalog-n catalog))
    (error "Catalog ~A has no stars to use as calibrators." catalog))
  (when write-headers (delete-all-photcalib-headers fits-file))
  (let* ((std-filter (or filter
			 (instrument-id:get-standard-filter-for-fits fits-file)
			 (error "Filter can't be inferred for ~A" fits-file)))
	 (ext (instrument-id:get-image-extension-for-onechip-fits fits-file))
	 (working-dir (terapix:get-fits-directory 
		       fits-file :if-does-not-exist t
		       :extension ext))
	 (phot-calib-catalog-file "sex_phot_calib_mag_ap.cat")
	 (matches-file (format nil "sex_phot_calib_mag_ap_matches_~A.csv"
			       (type-of catalog)))
	 (matches-file-fullpath
	   (format nil "~A/~A" working-dir matches-file))
	 (phot-calib-catalog-file-fullpath
	   (format nil "~A/~A" working-dir phot-calib-catalog-file))
	 (mag-translation-function 
	   (or (get-mag-trans-func-for-catalog catalog std-filter)
	       (error "Cannot translate catalog of type ~A to magnitude ~A" 
		      (type-of catalog) std-filter))))
    
    (terapix:run-sextractor 
     fits-file
     :extension ext
     :conf-suffix "_PHOTCALIB_MAGAP"
     :display-errors nil
     :output-catalog phot-calib-catalog-file
     :md5-avoid-rerun md5-avoid-rerun
     :deblend-mincont 1.0 ;; don't deblend (eg trails)
     :satur-level satur-level
     :phot-apertures (list aperture))
    ;;
    (let* ((shash (terapix::add-neighbor-dist-to-sextractor-catalog
		   (terapix:read-sextractor-catalog 
		    phot-calib-catalog-file-fullpath)))
	   (ra-vec (gethash "ALPHA_J2000" shash))
	   (dec-vec (gethash "DELTA_J2000" shash))
	   (num-vec (gethash "NUMBER" shash))
	   (flag-vec (gethash "FLAGS" shash))
	   (mag-vec (gethash "MAG_APER" shash))
	   (flux-vec (gethash "FLUX_APER" shash))
	   (peakflux-max-vec (gethash "FLUX_MAX" shash))
	   (xpixvec  (gethash "XWIN_IMAGE" shash))
	   (ypixvec  (gethash "YWIN_IMAGE" shash))
	   (magerr-vec (gethash "MAGERR_APER" shash))
	   (fits-pcobj-list 
	     (loop 
	       with neighbor-dist-vec = (gethash "%NEIGHBOR-DIST" shash)
	       for neighbor-dist across neighbor-dist-vec
	       for i below (length ra-vec)
	       when (and (zerop (logand sextractor-reject-flags
					(aref flag-vec i)))
			 (< (aref peakflux-max-vec i) peakflux-max)
			 (> neighbor-dist (* 1.1 (* 0.5 aperture))))
		 collect (make-pcobj 
			  :id (aref num-vec i)
			  :alpha (float (aref ra-vec i) 1d0)
			  :delta (float (aref dec-vec i) 1d0)
			  :xpix (float (aref xpixvec i) 1.0)
			  :ypix (float (aref ypixvec i) 1.0)
			  :mag (float (aref mag-vec i) 1.0)
			  :flux (float (aref flux-vec i) 1.0)
			  :mag-err (float (aref magerr-vec i) 1e0)
			  ;; for computing 5,10 sigma limits use this one aperture
			  :mag-err/s (float (aref magerr-vec i) 1e0)
			  ))))
      
      (when (not fits-pcobj-list)
	(error "No acceptable objects found for calibration."))
      
      (multiple-value-bind (zp zperr nstars dummy is-ok
			    mag-5-sigma mag-10-sigma
			    matching-pairs)
	  (phot-calib-obj-list-using-catalog fits-pcobj-list
					     catalog mag-translation-function
					     :tol/arcsec tol/arcsec
					     :extra-error extra-error
					     :stars-only stars-only
					     :min-obj-flux min-obj-flux
					     :max-catalog-mag max-catalog-mag
					     :min-catalog-mag min-catalog-mag
					     :matches-file matches-file-fullpath)
	(declare (ignorable dummy))
	(when (< nstars min-calib-stars)
	  (error "Found ~A catalog calibration stars but MIN-CALIB-STARS=~A" 
		 nstars min-calib-stars))
	;; 
	(let ((zptel
		(ignore-errors
		 (+ zp
		    (* -2.5
		       (log (instrument-id:get-exptime-for-fits
			     fits-file)
			    10))
		    (* 2.5
		       (log (instrument-id:get-gain-for-fits
			     fits-file)
			    10))))))
	  ;; write headers to both image and catalog file if asked
	  ;; zeropoint giving 1e/sec
	  (loop for hfits
		  in (if write-headers
			 `(,fits-file ,phot-calib-catalog-file-fullpath)
			 `(,phot-calib-catalog-file-fullpath))
		for extension
		  in (if write-headers
			 `(,(instrument-id:get-image-extension-for-onechip-fits
			     fits-file)
			   1)
			 '(1))
		do
		   (cf:write-fits-header 
		    hfits "PHOTCALIB.MAGTYPE" "MAG_AP"
		    :comment "fixed aperture mag"
		    :extension extension)
		   (cf:write-fits-header 
		    hfits "PHOTCALIB.APERTURE" aperture)
		   ;; will we want to to put this in?
		   ;;(cf:write-fits-header
		   ;; hfits "PHOTCALIB.MAGAUTOCOR" 0.0
		   ;; :comment "Addit. cor. for MAG_AUTO")
		   (cf:write-fits-header
		    hfits "PHOTCALIB.ZPMAG" zp 
		    :comment "ZP to add to M=-2.5log10(flux/ADU)"
		    :extension extension)
		   (cf:write-fits-header 
		    hfits "PHOTCALIB.ZPMAGERR" zperr
		    :comment "Error on PHOTCALIB.ZPMAG"
		    :extension extension)
		   (cf:write-fits-header
		    hfits "PCZPMAG" zp 
		    :comment "Synonym for PHOTCALIB.ZPMAG"
		    :extension extension)
		   (cf:write-fits-header
		    hfits "PCEZPMAG" zperr 
		    :comment "Synonym for PHOTCALIB.ZPMAGERR"
		    :extension extension)
		   (cf:write-fits-header 
		    hfits "PHOTCALIB.NSTARS" nstars 
		    :comment "No. of stars used for PHOTCALIB.ZPMAG"
		    :extension extension)
		   (cf:write-fits-header 
		    hfits "PHOTCALIB.FILTER" std-filter 
		    :comment "Assumed filter in PHOTCALIB package"
		    :extension extension)
		   (cf:write-fits-header 
		    hfits "PHOTCALIB.CATALOGTYPE" (type-of catalog)
		    :extension extension)
		   (cf:write-fits-header 
		    hfits "PHOTCALIB.OK" is-ok
		    :comment "Do we trust this calib?"
		    :extension extension)
		   (when zptel
		     (cf:write-fits-header
		      hfits "PHOTCALIB.ZPTEL"  zptel
		      :comment "Mag giving 1e-/sec"
		      :extension extension))
		   ;;
		   (when mag-5-sigma
		     (cf:write-fits-header
		      hfits "PHOTCALIB.MAG-5-SIGMA" mag-5-sigma
		      :comment "Mag giving 5 sigma err"
		      :extension extension))
		   (when mag-10-sigma
		     (cf:write-fits-header
		      hfits "PHOTCALIB.MAG-10-SIGMA" mag-10-sigma
		      :comment "Mag giving 10 sigma err"
		      :extension extension))
		)
		   
	  ;;
	  (make-phot-calib-result
	   :catalog (type-of catalog)
	   :zpmag zp
	   :zpmag-err zperr
	   :nstars nstars
	   :zpmag-inst  zptel
	   :zpmag-inst-err zperr
	   :mag-5-sigma mag-5-sigma
	   :mag-10-sigma mag-10-sigma
	   :ok is-ok
	   :std-filter std-filter
	   :matches matching-pairs
	   :match-file matches-file-fullpath))))))


(defun calibrate-image-with-catalog-type/ap
    (catalog-type fits-file
     &key
       (filter nil)
       (aperture 20)
       (min-calib-stars 20)
       (max-catalog-radius 1.0)  ;; limit the catalog radius to this many deg
       (max-catalog-mag 21)
       (min-catalog-mag 0)
       (min-obj-flux 20000)
       (sextractor-reject-flags #xFFFF)
       (stars-only t)
       (extra-error 0.01d0)
       ;; sextractor configs
       (md5-avoid-rerun t)
       (tol/arcsec 1.0)
       (satur-level 50000)
       (peakflux-max 40000)
       ;; other config
       (catalog-cache astro-catalog::*default-catalog-cache*)
       (write-headers t))
  "Like CALIBRATE-IMAGE-USING-CATALOG/AP (see documentation)
except that it takes CATALOG-TYPE instead of CATALOG, computes the
fits bounds, and downloads the catalog.

FILTER must be a standard filter keyword like :VJ or :GSDSS. If it 
is NIL, then INSTRUMENT-ID:GET-STANDARD-FILTER-FOR-FITS
is used to infer the filter.

CATALOG-TYPE must be one of ASTRO-CATALOG:*ALLOWED-CACHE-CATALOG-TYPES*"
  (%insist-on-onechip fits-file) ;; must be onechip fits file
  (when (not (member catalog-type astro-catalog:*allowed-cache-catalog-types*))
    (error "CATALOG-TYPE=~A is not one of ~A" 
	   catalog-type astro-catalog:*allowed-cache-catalog-types*))
  (multiple-value-bind (ra0 dec0 radius)
      (%get-fits-bounds fits-file)
    (let ((catalog (astro-catalog:get-cached-catalog-object 
		    ra0 dec0 (min max-catalog-radius radius)
		    catalog-type
		    :catalog-cache catalog-cache)))
      (calibrate-image-using-catalog/ap
       catalog fits-file 
       :filter filter
       :aperture aperture
       :min-obj-flux    min-obj-flux
       :sextractor-reject-flags sextractor-reject-flags
       :stars-only stars-only
       :extra-error extra-error
       :min-calib-stars min-calib-stars
       :max-catalog-mag max-catalog-mag
       :min-catalog-mag min-catalog-mag
       :md5-avoid-rerun md5-avoid-rerun
       :tol/arcsec tol/arcsec
       :satur-level satur-level
       :peakflux-max peakflux-max
       :write-headers write-headers))))





