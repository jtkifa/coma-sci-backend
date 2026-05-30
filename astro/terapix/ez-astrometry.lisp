

#|

use terapix for easy astrometry of simple images

|#




(in-package terapix)



(defun do-nonlinear-astrometry 
    (fits-file 
     &key
       (extension nil)
       (write-wcs t)
       (nstars-min 12) 
       (rms-max 0.50)
       (md5-avoid-rerun t) ;; for sextractor
       (catalog-postproc-func nil) ;; function to post-proc sextractor cat
       ;; sextractor params
       (detect-threshold 5.0)  ;; too many faint stars confuse scamp, it seems
       (analysis-threshold 5.0)
       (gain 1.0)
       (satur-level 1e6) ;; very high - use saturated stars
       (pixel-scale :from-fits) ;; not important, but catalog is then useful for 
       (make-object-checkimage nil) ;; make an object checkimage?
       ;;
       ;; scamp params 
       (astref-catalog "2mass")       (astref-catalog-filename nil)
       (create-ldac-file  nil) ;; create an intermediate LDAC file
       (scamp-save-refcatalog "N") ;; this is SCAMP's version, unrelated to created-ldac-file
       (match-flipped nil)
       (astref-mag-low 10) (astref-mag-high 22) ;; catalog mag limits
       (fwhm-threshold-low 1) ;; in pix, for good detection
       (fwhm-threshold-high 50) 
       (pixscale-maxerr 1.03)
       (posangle-maxerr 5.0)
       (position-maxerr 2.0) ;; arcmin
       (distort-degrees 3)
       (crossid-radius  3.0)
       (flags-mask #x00fc)   ;; for scamp: 00fc is default mask, keeps deblends, rejects other glitches
       (allow-saturated-stars nil) ;; set 4s bit of flag mask
       (file-suffix "WCSFIT") ;; suffix for internal files
       (display-errors nil))
       

  "Given a single-extension FITS-FILE, fit the WCS to DISTORT-DEGREES
order (1 for linear) using swarp and scamp, and if WRITE-WCS is set
write it to the file, after backing up the original wcs in the
headers.

 (CATALOG-POSTPROC-FUNC SEXTRACTOR-CATALOG 
                        FITS-FILE 
                        OUTPUT-SEXTRACTOR-CATALOG) 
is an optional function that takes the sextractor catalog and fits image, and 
generates a new catalog.  For example, it
might be (a closure of) TRAILED-IMAGE-ASTROMETRY:IMPROVE-SEXTRACTOR-CATALOG

Return (VALUES WCS NSTARS RMS NSTARS-HI-SN RMS-HI-SN FWHM)

EXTENSION is [1]-based, not [0]-based like Astrometric and Iraf.  This
changed in 2025/12.

The FWHM is returned even for failure, so it can be used to adjust subsequent
WCS fits."

  (when (and write-wcs
	     (not
	      (ignore-errors
	       (cf:with-open-fits-file (fits-file ff :mode :io)
		 t))))
    (error "Fits file ~A is not writable but WRITE-WCS is true in do-nonlinear-astrometry" fits-file))
  
  (let* (xml-list 
	 nstars rms-vec nstars-hi-sn rms-vec-hi-sn
	 rms rms-hi-sn
	 fwhm ;; estimate fwhm while we're at it
	 ;; fix flags-mask to accept flags-mas
	 (flags-mask-fixed (if allow-saturated-stars
			       (logandc2 flags-mask 4) ;; set 4s bit to 0
			       flags-mask))
	 ;; use basenames because we will add either .cat or .head
	 (extension-tag (if (not extension) ;; the extension tag denotes the extension
			    ""
			    (format nil "_ext-~A" extension)))
	 (cat-basename (format nil "sex_~A~A" file-suffix extension-tag))
	 (cat-basename-fixed (format nil "sex_~A~A_FIXED" file-suffix extension-tag))
	 (cat-basename-used cat-basename) ;; the one we use
	 sextractor-catalog
	 wcs-list wcs workdir checkimage-types-and-names
	 ldac-file ;; if we write catalog to ldac file
	 (extension-to-use  (or extension
				   (ignore-errors ;; could be non-instrument-id fits
				    (instrument-id:get-image-extension-for-onechip-fits
				      fits-file))))
	 )
    
    (setf workdir (ensure-fits-directory fits-file :extension extension-to-use))
    (setf checkimage-types-and-names
	  (if make-object-checkimage
	      (list (list "OBJECTS"
			  (format nil "~A/checkimage_object_~A.fits" workdir file-suffix)))
	      nil))
    ;; 
    (run-sextractor fits-file
		    :extension extension-to-use
		    :conf-suffix (format nil "_~A" file-suffix)
		    :output-catalog (format nil "~A.cat" cat-basename)
		    :md5-avoid-rerun md5-avoid-rerun
		    :deblend-mincont 0.01
		    :deblend-nthresh 1
		    ;; NOTE - we don't have the ability to switch conv files - it's hardwired
		    ;; :conv-filename "gauss_5.0_9x9.conv" ;; fat convolve
		    :detect-threshold detect-threshold 
		    :analysis-threshold analysis-threshold
		    :gain gain :pixel-scale pixel-scale
		    :satur-level satur-level
		    :checkimage-types-and-names checkimage-types-and-names
		    :display-errors display-errors)

    (setf sextractor-catalog (format nil "~A/~A.cat"
					  workdir cat-basename))
    (when catalog-postproc-func
      (setf cat-basename-used cat-basename-fixed)
      (setf sextractor-catalog
	    (funcall catalog-postproc-func
		     sextractor-catalog
		     fits-file
		     (format nil "~A/~A.cat" workdir cat-basename-fixed))))


    
    ;; compute fwhm if we're writing stuff to headers
    (when write-wcs
      (ignore-errors ;; don't let fwhm failure stop fit
       (let ((sxhash (read-sextractor-catalog sextractor-catalog)))
	(setf fwhm (estimate-seeing-fwhm-from-sextractor-catalog
		    sxhash)))))

    ;; refcat is a special case
    (when (equalp astref-catalog "refcat")
      (setf create-ldac-file t))
    
    ;; if we are using an intermediate ldac-file, then write it to a file
    (when create-ldac-file
      (setf ldac-file
	    (%generate-ldac-catalog-file-for-fits
	     fits-file
	     astref-catalog 
	     :extension extension-to-use))) 
    
    ;; 
    (setf xml-list
	  (run-scamp   
	   fits-file
	   :extension extension-to-use
	   :xml-filename (format nil "scamp_~A.xml" file-suffix)
	   ;; use either the orig or fixed catalog
	   :sextractor-catalog-base cat-basename-used
	   :conf-suffix (concatenate 'string "_" file-suffix)
	   :copy-head-up nil
	   ;; if we're using an ldac file, then don't give scamp the catalog
	   ;; type, but give it the file we just made
	   :astref-catalog (if create-ldac-file nil astref-catalog)
	   :astref-catalog-filename (or ldac-file astref-catalog-filename)
	   ;;
	   :scamp-save-refcatalog scamp-save-refcatalog
	   :astref-mag-low astref-mag-low :astref-mag-high astref-mag-high
	   :pixscale-maxerr pixscale-maxerr
	   :posangle-maxerr posangle-maxerr
	   :position-maxerr position-maxerr
	   :crossid-radius  crossid-radius
	   :fwhm-threshold-low fwhm-threshold-low
	   :fwhm-threshold-high fwhm-threshold-high
	   :distort-degrees distort-degrees
	   :flags-mask flags-mask-fixed
	   :match-flipped (if match-flipped "Y" "N")
	   :display-errors display-errors))
    
    ;; note - these are ALL stars, not the high S/N stars
    (setf nstars (second (assoc "NDeg_Reference" xml-list :test 'equalp)))
    (setf rms-vec (second (assoc "AstromSigma_Reference"
				 xml-list :test 'equalp)))
    (setf nstars-hi-sn (second (assoc "NDeg_Reference_HighSN"
				      xml-list :test 'equalp)))
    (setf rms-vec-hi-sn (second (assoc "AstromSigma_Reference_HighSN"
				       xml-list :test 'equalp)))
  
    (when (not (and nstars rms-vec nstars-hi-sn rms-vec-hi-sn))
      (error "NDeg_Reference, AstromSigma_Reference, NDeg_Reference_HighSN, AstromSigma_ReferenceHighSN, not found in scamp XML output."))
    
    (setf rms
	  (sqrt (+ (expt (aref rms-vec 0) 2)
		   (expt (aref rms-vec 1) 2))))
    (setf rms-hi-sn
	  (sqrt (+ (expt (aref rms-vec-hi-sn 0) 2)
		   (expt (aref rms-vec-hi-sn 1) 2)))) 
    
    (when (< nstars nstars-min)
      (error "Too few stars for ~A: wanted >~A but got ~A"
	     (file-io:file-minus-dir fits-file) nstars-min nstars))

    ;; we use the HI-SN stars for the RMS cutoff if we have enough of them
    (cond ((>= nstars-hi-sn nstars-min)
	   (when (> rms-hi-sn rms-max)
	     (error "Rms too big for ~A: wanted < ~F but got ~,4F for ~D HIGH-SN stars"
		    (file-io:file-minus-dir fits-file) rms-max rms-hi-sn nstars-hi-sn)))
	  (t ;; too few hi-sn stars
	   (when (> rms rms-max)
	     (error "Rms too big for ~A: wanted < ~F but got ~,4F for ~S TOTAL stars"
		    (file-io:file-minus-dir fits-file) rms-max rms nstars))))


    (setf wcs-list
	  (parse-scamp-wcs-from-headfile
	   (concatenate 'string workdir
			(format nil "/~A.head" cat-basename-used))))
	  
  (when (not wcs-list)
    (error "Could not read a wcs from sex.head after running scamp."))
  (when (> (length wcs-list) 1)
    (error "Too many WCS in sex.head.  Perhaps this fits file had multiple extensions"))
  (setf wcs (first wcs-list))


  (when write-wcs
    ;; back up old wcs to names beginning X
    (cf:with-open-fits-file (fits-file ff :mode :io)
      ;; just delete the old PV headers - don't bother backing up
      ;;
      ;; write to the first fits IMAGE extension we find, so move to next image ext
      (loop for iext from 1 to (cf:fits-file-num-hdus ff)
	    do (cf:move-to-extension ff iext)
	       (when (= (length (cf:fits-file-current-image-size ff)) 2)
		 (return))
	    finally ;; no image extension? HOW?
	       (error "do-nonlinear-astrometry: Could not find an image extension to write wcs."))
      ;;
      (loop for i from 1 to 2
	    do (loop for j from 0 to 100
		     for key = (format nil "PV~D_~D" i j)
		     do (cf:delete-fits-header ff key)))
      ;; if this isn't the first time running wcsfit, don't back up so
      ;; we preserve original values
      (when (not (cf:read-fits-header ff "WCSFIT"))
	(loop
	  ;;
	  for header in '("CTYPE1" "CTYPE2" "CRVAL1" "CRVAL2" 
			"CRPIX1" "CRPIX2" "CD1_1" "CD1_2" 
			  "CD2_1" "CD2_2" "EQUINOX")
	  for new-header = (concatenate 'string "x" header)
	  for old-value = (cf:read-fits-header ff header)
	  when old-value
	    do (cf:write-fits-header ff new-header old-value)))
      ;;
      ;;  write fwhm
      (when fwhm
	(cf:write-fits-header
	 ff "FWHMEST" fwhm
	 :comment "short axis seeing FWHM [arcsec] from WCS-FIT"))
      ;;
      (cf:write-wcs wcs ff)
      (cf:write-fits-header ff "WCSFIT" t
			    :comment "wcs is from wcsfit")
      (cf:write-fits-header ff "ASTRMTHD" "scamp"
			    :comment "method used for astrometry")
      (cf:write-fits-header ff "WCSFIT.CATALOG"
			    (or astref-catalog
				(file-io:file-minus-dir (or ldac-file astref-catalog-filename)))
			    :comment "catalog used for astrometry")
      (cf:write-fits-header ff "WCSFITOK" t
			    :comment "Have an OK wcs fit")
      (cf:write-fits-header ff "NASTRON" nstars
			    :comment "Number of stars for astrometry")
      (cf:write-fits-header ff "RMSFIT" rms
			    :comment "RMS of astrometry fit")))
  (values wcs nstars rms nstars-hi-sn rms-hi-sn fwhm)))


(defun restore-original-wcs-headers (fits-file &key extension)
  "Restore the original headers, with an x suffix. If EXTENSION is NIL,
do all extensions where backups exist; if EXTENSION is given, limit
restoration to this extension."
  (cf:maybe-with-open-fits-file (fits-file ff :mode :io)
    (loop for iext from 1 to (cf:fits-file-num-hdus ff)
	  do (cf:move-to-extension ff iext)
	     (when (and (= (length (cf:fits-file-current-image-size ff)) 2)
			;; limit to extension if given
			(or (not extension)
			    (eql iext extension)))
	       ;; if this isn't the first time running wcsfit, don't back up so
	       ;; we preserve original values
	       (when (cf:read-fits-header ff "WCSFIT")
		 (loop for header in '("WCSFIT" "ASTRMTHD" "WCSFIT.CATALOG" "WCSFITOK"
				       "NASTRON"  "RMSFIT")
		       do
			  (cf:delete-fits-header ff header)))
	       
	       (loop
		 ;;
		 for header in '("CTYPE1" "CTYPE2" "CRVAL1" "CRVAL2" 
				 "CRPIX1" "CRPIX2" "CD1_1" "CD1_2" 
				 "CD2_1" "CD2_2" "EQUINOX")
		 for bak-header = (concatenate 'string "x" header)
		 for bak-value = (cf:read-fits-header ff bak-header)
		 when bak-value
		   do (cf:write-fits-header ff header bak-value
					    :comment "backup of original header"))))))



(defun do-linear-astrometry 
    (fits-file 
     &key
       (extension nil)
       (write-wcs t)
       (nstars-min 12) 
       (rms-max 0.50)
       (md5-avoid-rerun t) ;; for sextractor
       (catalog-postproc-func nil) ;; function to post-proc sextractor cat
       ;; sextractor params
       (detect-threshold 2.0)
       (analysis-threshold 2.0)
       (gain 1.0)
       (satur-level 1e6)
       (pixel-scale :from-fits) 
       (make-object-checkimage nil) ;; make an object checkimage?
       ;;
       ;; scamp params 
       (astref-catalog "2mass")
       (astref-catalog-filename nil)
       (create-ldac-file nil)
       (scamp-save-refcatalog "N")
       (match-flipped nil)
       (astref-mag-low 10) (astref-mag-high 22) ;; catalog mag limits
       (fwhm-threshold-low 1) 
       (fwhm-threshold-high 50) ;; in pix, for good detection
       (pixscale-maxerr 1.03)
       (posangle-maxerr 5.0)
       (position-maxerr 2.0) ;; arcmin
       (crossid-radius  3.0)
       (flags-mask #x00fc) ;; default value for scamp
       (allow-saturated-stars nil)
       ;;
       ;; run sextractor and scamp verbosely
       (file-suffix "WCSFIT")
       (display-errors nil))

  "Given a single-extension FITS-FILE, fit the WCS to first order
using swarp and scamp, and if WRITE-WCS is set write it to the file,
after backing up the original wcs in the headers.

(CATALOG-POSTPROC-FUNC SEXTRACTOR-CATALOG FITS-FILE) is an optional
function that takes the sextractor catalog and fits image, and returns
the pathname to a new one that is fixed in some way.  For example, it
might be (a closure of)
TRAILED-IMAGE-ASTROMETRY:IMPROVE-SEXTRACTOR-CATALOG

Return (VALUES WCS NSTARS RMS NSTARS-HI-SN RMS-HI-SN)"

  (do-nonlinear-astrometry
    fits-file
    :extension extension
    :write-wcs write-wcs
    :nstars-min nstars-min
    :rms-max rms-max
    :md5-avoid-rerun md5-avoid-rerun
    :catalog-postproc-func catalog-postproc-func
    :detect-threshold detect-threshold
    :analysis-threshold analysis-threshold
    :gain gain
    :satur-level satur-level
    :pixel-scale pixel-scale
    :make-object-checkimage make-object-checkimage
    :astref-catalog astref-catalog
    :astref-catalog-filename astref-catalog-filename
    :create-ldac-file create-ldac-file
    :scamp-save-refcatalog scamp-save-refcatalog
    :match-flipped match-flipped
    :astref-mag-low astref-mag-low :astref-mag-high astref-mag-high
    :fwhm-threshold-low fwhm-threshold-low
    :fwhm-threshold-high fwhm-threshold-high
    :pixscale-maxerr pixscale-maxerr
    :position-maxerr position-maxerr
    :posangle-maxerr posangle-maxerr
    :crossid-radius crossid-radius
    :file-suffix file-suffix
    :display-errors display-errors
    ;; this makes it linear
    :distort-degrees 1
    :flags-mask flags-mask
    :allow-saturated-stars allow-saturated-stars
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun %get-astro-catalog-type-for-scamp-key (key)
  (cond ((equalp key "2mass") 'astro-catalog:2mass-point-source-catalog)
	((equalp key "usno-a1") 'astro-catalog:usno-b1-catalog)
	((equalp key "sdss-r7") 'astro-catalog:sdss7-catalog)
	((equalp key "sdss-r8") 'astro-catalog:sdss8-catalog)
	((equalp key "sdss-r9") 'astro-catalog:sdss9-catalog)
	((equalp key "ps1") 'astro-catalog:psps-3pi-mean-psf-mag-catalog)
	((equalp key "refcat") 'astro-catalog:refcat-catalog)
	(t
	 nil)));; no such catalog
	
	 

(defun %generate-ldac-catalog-file-for-fits (fits astref-catalog-type &key extension)
  (let* ((ext (or extension (instrument-id:get-image-extension-for-onechip-fits fits)))
	 (wcs (or (cf:read-wcs fits :extension ext)
		  (instrument-id:get-initial-wcs-for-fits fits :extension ext)
		  (error "Could not get initial WCS for FITS ~A to generate ldac catalog" fits)))
	 (mjd (instrument-id:get-mjd-mid-for-fits fits))
	 
	 (naxis1 (cf:read-fits-header
		  fits "NAXIS1"
		  :extension ext))
	 (naxis2 (cf:read-fits-header
		  fits "NAXIS2"
		  :extension ext))
	 (radius/deg 0d0)
	 ;; catalog type according to astref-catalog package
	 (catalog-type
	   (or (%get-astro-catalog-type-for-scamp-key astref-catalog-type)
	       (error "Cannot create an astro-catalog catalog from SCAMP catalog of type ~A"
		      astref-catalog-type))))
    ;;
    (multiple-value-bind (ra0 dec0)
	(wcs:wcs-convert-pix-xy-to-ra-dec wcs (* naxis1 0.5) (* naxis2 0.5))
      ;; compute corner distance and use that to compute radius of catalog
      (multiple-value-bind (ra1 dec1)
	  (wcs:wcs-convert-pix-xy-to-ra-dec wcs 0.0 0.0)
	(setf radius/deg
	      (* 2.0 (astro-coords:sky-angle ra0 dec0 ra1 dec1 :units :degrees))))

     (let ((catalog (astro-catalog:get-cached-catalog-object ra0 dec0 radius/deg
							      catalog-type))
	   (file (format nil "~A/astref-~A.cat"
			 (get-fits-directory fits :if-does-not-exist :create
					     :extension extension)
			 astref-catalog-type)))
       (astro-catalog:write-catalog-to-fits-ldac
	catalog file
	;; FIXME - what if it exists?
	:overwrite t
	;; minimum allowed ra,dec error - GAIA small values may cause SCAMP to fail
	:position-error-floor 0.05
	:mjd mjd ;; apply proper motions
	:mag-to-use nil ;; any available mag
	:use-tmpfile-for-speed t)

       file))))
