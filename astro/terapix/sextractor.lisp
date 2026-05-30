

(in-package terapix)


(defparameter *default-sextractor-read-columns* 
  '("NUMBER" 
    "X_IMAGE" "Y_IMAGE"  "XWIN_IMAGE" "YWIN_IMAGE"
    "XPEAK_IMAGE"  "YPEAK_IMAGE"
    "A_IMAGE" "B_IMAGE" "THETA_IMAGE" ;; rms in pixel coords; 
    "A_WORLD" "B_WORLD" "THETA_WORLD" ;; THETA_XXX is CCW of X (or E)
    ;; positions and errors^2
    "ALPHA_J2000" "DELTA_J2000"  "ERRX2_WORLD" "ERRY2_WORLD" "ERRXY_WORLD"
    ;; and windowed version
    "ALPHAWIN_J2000" "DELTAWIN_J2000"
    "ALPHAPEAK_J2000" "DELTAPEAK_J2000"
    "ERRX2WIN_WORLD" "ERRY2WIN_WORLD" "ERRXYWIN_WORLD"
    ;;
    "MAG_APER" "MAGERR_APER"   "FLUX_APER" "FLUXERR_APER"
    "MAG_BEST" "MAGERR_BEST"   "FLUX_BEST" "FLUXERR_BEST" 
    "MAG_AUTO" "MAGERR_AUTO"   "FLUX_AUTO" "FLUXERR_AUTO" 
    "FWHM_IMAGE"  "FWHM_WORLD"
    "SNR_WIN" ;; signal to noise
    "FLUX_MAX"
    "FLAGS" ;; can also contain our own post-processed flags (eg cosmic ray)
    ))

;; we define our own flags above sextractor's 8 bits (1 to 128)
;; because it is a signed 16 bit, we can use only 2^8,9,10,11,12,13
;; for our own flag values
(defconstant +cosmic-ray-flag+ (expt 2 13))



;; read the multiline headers APERTURES.PARTn from catalog-file - it gets
;; written into it by 
(defun %read-apertures-from-sextractor-catalog (catalog-file)
  (let ((apertures-string (cf:read-multiline-header catalog-file "APERTURES")))
    (when apertures-string
      (setf apertures-string
	    (string-trim '(#\space #\tab #\cr #\lf) apertures-string))
      (when (every (lambda (c) (or (find c "0123456789d. ") 
				   (member c '(#\cr #\lf))))
		   (subseq apertures-string 1 (1- (length apertures-string))))
	(when (and (char= (aref apertures-string 0) #\( )
		   (char= (aref apertures-string 
				(1- (length apertures-string)))))
	  (read-from-string apertures-string))))))
			      
(defun read-sextractor-catalog
    (catalog-file 
     &key 
       (columns *default-sextractor-read-columns*)
       (extra-columns nil))
  "Given a sextractor fits file, reads the sextractor columns
  specified in COLUMNS plus EXTRA-COLUMNS and returns
     (VALUES DATA-HASH BOOL-HASH) 
   where DATA-HASH is a hash table of KEY=ColumnName, VAL=DataVector
   and   BOOL-HAH  is a hash table of KEY=ColumnName, VAL=BooleanVector
      where the boolean is T if a non-NULL value was present in the table.

EXTRA-COLUMNS is a convenient way to add columns to the default columns in
 *DEFAULT-SEXTRACTOR-READ-COLUMNS* 

The entry %APERTURES is a special one, containing the apertures, representing the
columns of the apertures matrix.  As a general rule, supplemental columns
begin with %XXXX."
  (cf:with-open-fits-file (catalog-file ff)
    (cf:move-to-extension ff 3)
    (loop with data-hash = (make-hash-table :test 'equalp)
	  with bool-hash = (make-hash-table :test 'equalp)
	  with all-columns = (append columns extra-columns)
	  for column in all-columns 
	  do (multiple-value-bind (data-vec bool-vec)
		 (cf:read-column-from-fits-table ff column)
	       (setf (gethash column data-hash) data-vec)
	       (setf (gethash column bool-hash) bool-vec))
	  finally 
	     ;; read the apertures if present to help interpret
	     ;; MAG_APER and FLUX_APER
	     (let ((apertures
		     (%read-apertures-from-sextractor-catalog catalog-file)))
	       (when apertures 
		 (setf (gethash "%APERTURES" data-hash) apertures)))
	     ;;
	     (return (values data-hash bool-hash)))))


(defun find-nearest-index-in-sextractor-catalog (cat-hash x y &key (x-col "XPEAK_IMAGE") (y-col "YPEAK_IMAGE"))
  "Returns (VALUES INDEX DISTANCE/PIX KEY-VAL-ALIST) of the object closest to X,Y
using columns specified, using brute force search."
  (loop with xvec of-type vector = (gethash x-col cat-hash)
	with yvec of-type vector = (gethash y-col cat-hash)
	with best-dist2 = most-positive-double-float
	with best-i = 0
	for i from 0
	for xx across xvec and yy across yvec
	for dist2 =  (+ (expt (- x xx) 2) (expt (- y yy) 2)) 
	when (< dist2 best-dist2)
	  do (setf best-dist2 dist2)
	     (setf best-i i)
	finally
	   (let ((fields (loop for key being the hash-key of cat-hash
			       for val being the hash-value of cat-hash
			       when (not (equal #\% (aref key 0))) ;; not a special col
				 collect (list key
					       (cond ((vectorp val) ;; a simple column
						      (aref val best-i))
						     (t ;; a matrix column
						      (loop for icol below (array-dimension val 1)
							    collect (aref val best-i icol))))))))
	     (return (values best-i (sqrt best-dist2) fields)))))	



(defun add-neighbor-dist-to-sextractor-catalog
    (cat-hash &key (match-dist 100) (dmag 5.0))
  "Insert two new columns into sextractor catalog hash 
 (see function READ-SEXTRACTOR-CATALOG)

 1. %NEIGHBOR-INDEX - the vector index of the closest object in the 
     catalog, or NIL if none within MATCH-DIST (arcsec) and 
     brighter than DMAG difference from the object
 2. %NEIGHBOR-DIST  - the distance in arcsec to the nearest 
    object, or MOST-POSITIVE-SINGLE-FLOAT if no neighbor 
    within MATCH-DIST.

If there is no neighbor (a field with one object) then the %NEIGHBOR-DIST
is NIL, and the %NEIGHBOR-INDEX is NIL."
  (let* ((nstars (length (gethash "ALPHA_J2000" cat-hash)))
	 (%dmag (float dmag 1d0))
	 (aobj-list
	   (loop with ravec = (gethash "ALPHA_J2000" cat-hash)
		 with decvec = (gethash "DELTA_J2000" cat-hash)
		 with magvec = (gethash "MAG_BEST" cat-hash)
		 ;; project around first object - it's good enough
		 with ra0 = (aref ravec 0) and dec0 = (aref decvec 0)
		 for index from 0
		 for ra across ravec and dec across decvec and mag across magvec
		 for obj = (aobj:make-obj :alpha ra :delta dec 
					  :id index :mag (float mag 1e0))
		 do (multiple-value-bind (x y)
		       (sky-project:tan-project ra dec ra0 dec0 :units :arcsec)
		      (setf (aobj:obj-x obj) x)
		      (setf (aobj:obj-y obj) y))
		 collect obj))
	 (bins (aobj:bin-objects aobj-list))
	 (neighbor-id-vec (make-array nstars :element-type t :initial-element nil))
	 (neighbor-dist-vec (make-array nstars  :element-type t :initial-element nil)))
    (when (> nstars 1)
      (loop for obj of-type aobj:obj in aobj-list
	    for neighbor-obj = (aobj:get-nearest-object 
				obj bins (float match-dist 1d0)
				most-positive-double-float 
				0d0 0d0 t
				;; function to test if neighbor obj
				;; (nobj) is brighter than current obj
				;; plus dmag
				(lambda (nobj) 
				  (declare (type aobj:obj nobj))
				  (< (aobj:obj-mag nobj)
				     (+ %dmag (aobj:obj-mag obj)))))
	    for dist = (if neighbor-obj ;; should always be true
			   (astro-coords:sky-angle (aobj:obj-alpha neighbor-obj)
						   (aobj:obj-delta neighbor-obj)
						   (aobj:obj-alpha obj)
						   (aobj:obj-delta obj)
						   :units :arcsec))
	    do (when neighbor-obj
		 (setf (aref neighbor-id-vec (aobj:obj-id obj))
		       (aobj:obj-id neighbor-obj))
		 (setf (aref neighbor-dist-vec (aobj:obj-id obj))
		       (float dist 1.0)))))
    (setf (gethash "%NEIGHBOR-INDEX" cat-hash) neighbor-id-vec)
    (setf (gethash "%NEIGHBOR-DIST" cat-hash) neighbor-dist-vec)
    cat-hash))
	     
			 
	 
  

;(defparameter *fwhm-vec* nil)



#+nil ;; old version with FWHM_WORLD
(defun estimate-seeing-fwhm-from-sextractor-catalog
  (sxhash &key (min-fwhm 0.2) (max-fwhm 15.0)
	    (mag-error-cutoff 0.03))
  "NEW VERSION of FWHM estimator because old one was failing.
This simply collects the FWHM_WORLD from a sextractor catalog,
keeps those with FLAG=0, MAGERR_AUTO<MAG-ERROR-CUTOFF, and computes
the mode of remaining FWHM_WORLD.

Returns (VALUES FWHM-EST N-GOOD-STARS-USED)."
    (let* ((fwhm-vec (map 'vector (lambda (x) (* x 3600))
			  (gethash "FWHM_WORLD" sxhash)))
	   (mag-err-vec (gethash "MAGERR_AUTO" sxhash))
	   (flag-vec (gethash "FLAGS" sxhash))
	   (fwhm-good-vec
	     (coerce
	      (loop for fwhm across fwhm-vec
		    for merr across mag-err-vec
		    for flag across flag-vec
		    when (and (zerop flag)
			      (< merr mag-error-cutoff)
			      (<= min-fwhm fwhm max-fwhm))
		      collect fwhm)
	      'vector)))
      (values (stats:mode-of-elements fwhm-good-vec :fwindow 0.05)
	      (length fwhm-good-vec))))

;; new version with B_WORLD to get short axis of trailing - we assume
;; this is the Gaussian short-axis sigma
(defun estimate-seeing-fwhm-from-sextractor-catalog
  (sxhash &key (min-fwhm 0.2) (max-fwhm 15.0)
	    (mag-error-cutoff 0.03))
  "NEW VERSION of FWHM estimator because old one was failing.
This simply collects the B_WORLD from a sextractor catalog,
keeps those with FLAG=0, MAGERR_AUTO<MAG-ERROR-CUTOFF, and computes
the mode of remaining FWHM_WORLD.

Returns (VALUES FWHM-EST N-GOOD-STARS-USED)."
  (let* ((fwhm-vec (map 'vector
			(lambda (x) (* x
				       3600  ;; to arcsec
				       2.35482)) ;; sigma (rms) to Gaussian
			;; B_WORLD is the rms of the FWHM, but we pretend
			;; it is Gaussian sigma
			(gethash "B_WORLD" sxhash)))
	   (mag-err-vec (gethash "MAGERR_AUTO" sxhash))
	   (flag-vec (gethash "FLAGS" sxhash))
	   (fwhm-good-vec
	     (coerce
	      (loop for fwhm across fwhm-vec
		    for merr across mag-err-vec
		    for flag across flag-vec
		    when (and (zerop flag)
			      (< merr mag-error-cutoff)
			      (<= min-fwhm fwhm max-fwhm))
		      collect fwhm)
	      'vector)))
      (values (stats:mode-of-elements fwhm-good-vec :fwindow 0.05)
	      (length fwhm-good-vec))))


(defun %guesstimate-fwhm-peak (fwhm-vec fcutoff)
  (multiple-value-bind (fwvec fracvec)
      (stats:cumulative-probability fwhm-vec)
    (loop for ff across fracvec and fw across fwvec
	  when (> ff fcutoff)
	    do (return fw))))


;; this produces cruddy results
(defun estimate-seeing-fwhm-from-sextractor-catalog/old
    (sxhash
     &key 
       (min-fwhm 0.3)
       (max-fwhm 10.0)
       (mode-window 0.1)
       (mode-clip 0.5)
       (min-flux 3000)
       (max-flux 1e5)
       (min-stars 20)
       (heuristic-fwhm-max-frac 0.15)
       (heuristic-fwhm-max-factor 3))
						    
  "Estimate seeing FWHM in arcseconds from a sextractor catalog 
in SXHASH, by

  1. taking all FLAG=0 detections with flux between MIN-FLUX and
     MAX-FLUX and FWHM_WORLD (converted to arcsec) between MIN-FWHM
     and MAX-FWHM  (discarding unrealistic values)
  2. taking mode of this fwhm (get rid of galaxies)
  3. keeping only those stars within MODE-CLIP fraction of this mode
     (eg MODE-CLIP = 0.8 means between 0.8 and 1.2 of this mode
     Additionally assume that an another upper bound for fwhm is
     HEURISTIC-FWHM-MAX-FACTOR times the HEURISTIC-FWHM-MAX-FRAC
     fraction of the fwhm max distribution.  Eg, by default we assume
     that the real fwhm can't be more than 3 times the 15th fwhm
     percentile of all objects, because we guess that by the time we
     get into the 15th percentile, we must be seeing real stars.
  4. returning
      (VALUES MEDIAN-FWHM MODE-FWHM SIGMA-FWHM N-STARS-USED)

WARNING -- If sextractor was not run with correct :PIXEL-SCALE,
 then FWHM_WORLD is meaningless. 

Note that there can be secondary peaks of FWHM above the real one, so
that MAX-FWHM should be kept small, and the heuristics should be used
to limit allowed range of FWHM.

As a sanity check, the mode and median should be compared."
  (let* ((fwhm-vec (map 'vector (lambda (x) (* x 3600))
			(gethash "FWHM_WORLD" sxhash)))
	 (flux-vec (gethash "FLUX_BEST" sxhash))
	 (flag-vec (gethash "FLAGS" sxhash))
	 ;; first get rid of all stars outside fwhm, flux range
	 (fwhm-good-vec-pass1
	   (let ((fvec
		   (coerce 
		    (loop for fwhm across fwhm-vec
			  for flux across flux-vec
			  for flag across flag-vec
			  when (and (zerop flag)
				    (<= min-flux flux max-flux)
				    (<= min-fwhm fwhm max-fwhm))
			    collect fwhm)
		    'vector)))
	     (when (< (length fvec) min-stars)
	       (error "Not enough stars after first pass of flux selection."))
	     fvec))
	 ;; find where the cumulative distribution ramps up, and call this 
	 ;; a crude guesstimate of the max plausible
	 (fwhm-max-guesstimate 
	   (* heuristic-fwhm-max-factor
	      (%guesstimate-fwhm-peak fwhm-good-vec-pass1 heuristic-fwhm-max-frac)))
	 ;; pick stars by heuristic fwhm cutoff
	 (fwhm-good-vec 
	   (let ((fvec
		   (coerce 
		    (loop for fwhm across fwhm-good-vec-pass1
			  when (<= fwhm fwhm-max-guesstimate)
			    collect fwhm)
		    'vector)))
	     (when (< (length fvec) min-stars)
	       (error "Not enough stars after second pass of flux selection."))
	     fvec))
	 
	 (fwhm-mode (stats:mode-of-elements fwhm-good-vec :fwindow mode-window))
	 (fwhm-star-vec
	   (coerce 
	    (loop 
	      with min-fwhm = (* (- 1d0 mode-clip) fwhm-mode)
	      with max-fwhm = (* (+ 1d0 mode-clip) fwhm-mode)
	      for fwhm across fwhm-good-vec
	      when (<= min-fwhm fwhm max-fwhm)
		collect fwhm)
	    'vector)))
;;      (setf *fwhm-vec* fwhm-good-vec-pass1)
    #+nil
    (progn
      (setf *fwhm-vec* fwhm-good-vec)
      (plot:plot-vectors fwhm-vec flux-vec :xmin 0.0 :xmax 10.0
					   :ymin 1000 :ymax 10000 :connect nil :ptype :point)
      (plot:plot-hist fwhm-good-vec :toplabel "fwhm-good-vec")
      (plot:plot-hist fwhm-star-vec :toplabel "fwhm-star-vec")
      
      (plot:plot-hist (map 'vector (lambda (f) (log (max f 1d-5))) fwhm-good-vec)
		      :xleft -3 :xright 3 :toplabel "log(fwhm-good-vec"))
    
    (when (< (length fwhm-star-vec) min-stars)
      (error "Fewer than ~D stars after finally mode-clipping" min-stars))

    (values  (stats:median-of-elements fwhm-star-vec)
	     fwhm-mode
	     (stats:sigma-of-elements fwhm-star-vec)
	     (length fwhm-star-vec))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun sextractor-insert-cosmic-rays-into-imflags (fits catalog-file &key extension)
  "Mark cosmic rays in CATALOG column IMAFLAGS_ISO with +cosmic-ray-flag+.  Extension
is normal [1]-based scheme."
  (let* ((hash (read-sextractor-catalog catalog-file))
	 (xpix-vec (gethash "XPEAK_IMAGE" hash)) ;; use peak for cosmics
	 (ypix-vec (gethash "YPEAK_IMAGE" hash))
	 (flag-vec (gethash "FLAGS" hash))
	 (imsec (cf:read-image-section
		 fits
		 :extension
		 (or extension
		     (instrument-id:get-image-extension-for-onechip-fits fits))))
		
	 (img (cf:image-section-data imsec)))
    (loop with scratch = (make-array 121 :element-type 'single-float)
	  for i from 0
	  for x across xpix-vec
	  for y across ypix-vec
	  when (imutils:is-cosmic-ray (float x 1.0) (float y 1.0) img :scratch scratch)
	    do (setf (aref flag-vec i)
		     (logior (aref flag-vec i) +cosmic-ray-flag+)))
    (cf:with-open-fits-file (catalog-file ff :mode :io)
      (cf:write-fits-header ff "FCOSMIC" +cosmic-ray-flag+
			    :comment (format nil "Cosmic rays flagged in FLAGS as ~A"
					     +cosmic-ray-flag+))
      (cf:move-to-extension ff 3)
      (cf:write-column-to-fits-table ff "FLAGS" flag-vec))))
    
	  
    
    
    
    
	    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; built in flags
(defun sextractor-flag-neighbor (flag)
  (not (zerop (logand flag 1))))

(defun sextractor-flag-blended (flag)
  (not (zerop (logand flag 2))))

(defun sextractor-flag-saturated (flag)
  (not (zerop (logand flag 4))))

(defun sextractor-flag-truncated (flag)
  (not (zerop (logand flag 8))))

(defun sextractor-flag-aperture-data-bad (flag)
  (not (zerop (logand flag 16))))

(defun sextractor-flag-isophotal-data-bad (flag)
  (not (zerop (logand flag 32))))

(defun sextractor-flag-memory-overflow-deblending (flag)
  (not (zerop (logand flag 64))))

(defun sextractor-flag-memory-overflow-extraction (flag)
  (not (zerop (logand flag 128))))
    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; our own flags in FLAGS
(defun private-flag-cosmic-ray (flag)
  (not (zerop (logand flag +cosmic-ray-flag+))))
