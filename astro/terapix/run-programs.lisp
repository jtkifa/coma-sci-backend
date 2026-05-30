

(in-package terapix)

;; by default we create a new directory for each fits file
;; and we do all our work there


(defvar *terapix-dir-not-found-warning*
 "Terapix binary directory not found.
You need the set the variable 
 terapix:*terapix-directory*
      or
 (pconfig:set-config \"TERAPIX:TERAPIX-DIRECTORY\" ...) 
      or
  set the environment variable TERAPIX_DIRECTORY ahead of time")

;; TERAPIX-DIRECTORY can be a colon-delimited list
;; of directories
(defun get-terapix-dirs (&key (throw-error t)) ;; return a LIST of directories
  (let ((dir-string
	  (or (and (boundp 'terapix:*terapix-directory*)
		   (symbol-value '*terapix-directory*))
	      (pconfig:get-config "TERAPIX:TERAPIX-DIRECTORY")
	      (uiop:getenv "TERAPIX_DIRECTORY")
	      (when throw-error
		(error *terapix-dir-not-found-warning*)))))
    (if dir-string   (string-utils:split-string dir-string ":"))))


(eval-when (:load-toplevel)
  (when (not (get-terapix-dirs :throw-error nil))
    (format t "WARNING: ~A~%" *terapix-dir-not-found-warning*)))



(defun get-sextractor-program ()
  (loop for dir in (get-terapix-dirs)
	for file
	  = (or
	     ;; modern macports version
	     (probe-file (concatenate 'string dir "/source-extractor"))
	     (probe-file (concatenate 'string dir "/sextractor")) ;;linux
	     (probe-file (concatenate 'string dir "/sex")))
	when file
	  do (return (namestring file))
	finally   
	   (error "Terapix Sextractor program not found.")))

(defun get-swarp-program ()
   (loop for dir in (get-terapix-dirs)
	for file
	  = (or
	     (probe-file (concatenate 'string dir "/swarp"))
	     (probe-file (concatenate 'string dir "/SWarp")))
	when file
	  do (return (namestring file))
	finally   
	   (error "Terapix Swarp program not found.")))


(defun get-scamp-program ()
  (loop for dir in (get-terapix-dirs)
	for file
	  = (or
	     (probe-file (concatenate 'string dir "/scamp")))
	when file
	  do (return (namestring file))
	finally
	   (error "Terapix SCAMP program not found.")))


	       
;; for the working directory, for /a/b/c/foo.fits  (or fit,flt)
;; we use /a/b/c/foo_DIR, but for /a/b/c/foo.xx we use /a/b.c/foo.xx_DIR
;; if EXTENSION is specified, it is foo_DIR/extN
(defun get-fits-directory (fits-file &key (extension nil) (if-does-not-exist nil))
  "Returns the terapix processing directory for FITS-FILE or NIL if it
does not exist.  If IF-DOES-NOT-EXIST does not exist, then return 
the directory path even if does not exist."
  (let* ((suffix (file-io:file-suffix fits-file))
	 (is-fits-suffix ;; clip off only a suffix that indicates fits
	   ;; problem - what about fitz.gz? fz?  ugh - should have kept
	   ;; the whole fits name and appended _DIR
	   (member suffix '("fit" "fits" "flt") :test 'equalp))
	  (base (if is-fits-suffix
		   (file-io:file-basename fits-file)
		   fits-file))
	 (dir  (concatenate 'string
			    (if is-fits-suffix base fits-file)
			    "_DIR"
			    (if extension (format nil "/ext~3,'0D" extension) "")
			    )))
    (if (or if-does-not-exist
	    (cl-fad:directory-exists-p dir))
      dir
      nil)))

(defun ensure-fits-directory (fits-file &key (extension nil))
  (let* ((suffix (file-io:file-suffix fits-file))
	 (is-fits-suffix  (member suffix '("fit" "fits" "flt") :test 'equalp))
	 ;; clip off suffix only if CLEARLY fits suffix - otherwise keep suffix
	 (base (if is-fits-suffix
		   (file-io:file-basename fits-file)
		   fits-file))
	 (dir  (concatenate 
		'string base "_DIR"
		(if extension (format nil "/ext~3,'0D" extension) "")))
	 (topdir (file-io:dir-of-file fits-file)) ;; clip filename
	 (filebase (file-io:file-minus-dir base)))
     (ensure-directories-exist 
      (concatenate 'string dir "/#IGNORE#"))
     (values dir base topdir filebase)))

     
(defmacro run-program-and-check (error-message &body body)
  `(let ((%proc (progn ,@body)))
	(when (not (zerop (jutils:process-exit-code %proc)))
	  (error ,error-message))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SEXTRACTOR
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; compute an md5 signature for a sextractor run
(defun %compute-sextractor-run-md5
    (fits-file extension  param-filename conf-filename conv-filename nnw-filename
     make-badpix-weight-image)
  (when (and (probe-file param-filename)
	     (probe-file conv-filename)
	     (probe-file conf-filename)
	     (probe-file nnw-filename))
    (flet ((tostring (md5vec) ;; md5 vector to hex string
	     (with-output-to-string (s)
	       (loop for x across md5vec do (format s "~X" x))))) 
      (tostring
       (md5:md5sum-string
	(concatenate 'string
		     (if make-badpix-weight-image "BP" "")
		     (tostring (md5:md5sum-file fits-file))
		     ;; we use old 0-based extension for compatibility with old runs
		     (tostring (md5:md5sum-string (format nil "~A" (1- extension))))
		     (tostring (md5:md5sum-file param-filename))
		     (tostring (md5:md5sum-file conv-filename))
		     (tostring (md5:md5sum-file conf-filename))
		     (tostring (md5:md5sum-file nnw-filename))))))))
       


(defun %build-checkimage-file-pairs (dir checkimage-types-and-names)
   (mapcar (lambda (thing)
	     (cond 
	       ((stringp thing)
		(list thing (format nil "~A/~A.fits" dir thing)))
	       ((listp thing)
		(when (find #\/ (second thing))
		  (error "FITS files in CHECKIMAGE-TYPES-AND-NAMES should not have directories, because diagnostic images are placed into default directory"))
		(list (first thing) 
		      (format nil "~A/~A" dir (second thing))))))
	   checkimage-types-and-names))


(defun %get-pixel-scale-for-sextractor (fits-file inst
					extension ;; 1-based
					pixel-scale)
  (cond ((member pixel-scale '(:from-wcs :from-fits))
	 (or
	  ;; 
	  (if (eq pixel-scale :from-fits)
	      (ignore-errors (instrument-id:get-pixel-scale-for-fits
			      fits-file
			      :extension extension)))
	  (wcs:get-pixel-scale-for-wcs
	   (or (cf:read-wcs
		fits-file
		:extension
		(or extension
		    (if inst
			(instrument-id:get-image-extension-for-onechip-fits
			 fits-file)
			1))) ;; if we can't identify this instrument, try extension 1
	       (error
		"WCS not found in ~A but PIXEL-SCALE=~A"
		fits-file pixel-scale)))))
	 ((numberp pixel-scale)
	  pixel-scale)
	 (t (error
	     "Invalid PIXEL-SCALE. Must be :FROM-WCS or a number."))))

(defun make-badpix-weight-image (fits-file &key (extension nil) instrument
					     (expand-cosmics-by-one t)
					     (if-exists :do-nothing))
  (declare (type (member :do-nothing :recreate) if-exists))
  (multiple-value-bind (dir base parent-dir shortbase)
      (ensure-fits-directory fits-file)
    (declare (ignorable base parent-dir))
    (let* ((inst (or instrument (instrument-id:identify-instrument fits-file)))
	   (ext (or extension
		    (if (typep inst 'instrument-id:onechip)
			(instrument-id:get-image-extension-for-onechip-instrument inst fits-file)
			(error "make-badpix-weight-image: No extension specified for multichip instrument"))))
	   (badpix-file
	     (format
	      nil
	      "~A/~A_BadpixWeight~A.fits" dir shortbase
	      (if (not (typep inst 'instrument-id:onechip))  ;; specify extension only if more than one
		  (format nil "_ext_~A" ext)
		  "")))
	   (file-exists (and (probe-file badpix-file)
			     (eql (cf:read-fits-header ;; rough check if file OK
				   fits-file
				   "NAXIS1" :extension ext)
				  (ignore-errors (cf:read-fits-header
						  badpix-file
						  "NAXIS1" :extension ext)))
			     (eql (cf:read-fits-header
				   fits-file
				   "NAXIS2" :extension extension)
				  (ignore-errors (cf:read-fits-header
						  badpix-file
						  "NAXIS2" :extension ext))))))
      (when (or (not file-exists)
		(eq if-exists :recreate))
	(let* ((im (cf:image-section-data
		    (cf:read-image-section fits-file :extension ext)))
	       (imbit (imutils:bad-rowcol-filter-image 
		       im
		       :modify-input-image nil
		       :filter-cosmic-rays t
		       :grow-cosmics t
		       :min-cosmic-flux 50.0
		       :expand-cosmics-by-one expand-cosmics-by-one
		       :mark-saturated t :saturation-value :auto 
		       :invert-bitmap t)))
	  (cf:write-2d-image-to-new-fits-file imbit badpix-file :overwrite t
								:type :byte)))
      badpix-file)))

(defun run-sextractor
    (fits-file 
     &key
       (extension nil) ;; now cfitsio-compatible 1-based
       (output t) (display-errors t)
       (output-catalog "sex.cat")
       (conf-suffix "") ;;additional suffix to add to conf files 
       (md5-avoid-rerun t)
       (detect-threshold 1.5) (analysis-threshold 1.5)
       (gain "GAIN")
       (mag-zeropoint 0.0)
       (pixel-scale :FROM-FITS) ;; or FROM-WCS or a number
       (deblend-mincont 0.005) ;; 1 for no deblending
       (deblend-nthresh 32)
       (phot-apertures '(10.0)) ;; diameters of apertures
       (phot-autoparams '(2.5 3.5))
       (satur-level 20000)
       (back-size 256)   ;; size of backd mesh
       (back-filtersize 3) ;; filtering over backd mesh
       (verbose-type "NORMAL")
       ;; eg (("OBJECTS" "FOO_OBJECTS.fits") ...)
       ;; or just '("OBJECTS" ...)
       ;; these are put into the default directory
       (checkimage-types-and-names '())
       (nthreads 0)
       (flag-cosmic-rays t)
       (weight-image nil)
       (make-badpix-weight-image nil)
       (weight-type "NONE") ;; NONE, BACKGROUND (no flag image), MAP_WEIGHT, MAP_RMS, MAP_VAR
       (flag-image nil) ;; user input flags, 2-32 bit integers
       (flag-type  "OR") ;; OR AND MIN MAX MOST
       ;; additional output parameters
       (extra-output-parameters nil))
  "Run sextractor on FITS-FILE (eg foo.fits) and leave file
sex.cat in foo.DIR/sex.cat.

EXTENSION is cfitsio style image extension, which starts at 1
To get image.fits[0], use EXTENSION=1.
This is a change from past behavior.

By default, PIXEL-SCALE is :FROM-WCS, so sextractor will try to determine
the pixel scale from the wcs.

Uses an MD5 signature based on input fits and config files to avoid
re-running sextractor unless :MD5-AVOID-RERUN is disabled."
  (multiple-value-bind (dir base parent-dir shortbase)
      (ensure-fits-directory fits-file :extension extension)
    (declare (ignorable base parent-dir shortbase)
	     (type (or null (integer 1)) extension))
    (when (and weight-image make-badpix-weight-image)
      (error "Cannot specify both WEIGHT-IMAGE and MAKE-BADPIX-WEIGHT-IMAGE"))
    (let* ((inst (instrument-id:identify-instrument fits-file)) ;; use it if it is there
	   ;; from [0] based to [1] based
	   (full-fits-file ;; fits plus extension, if given
 	     (concatenate 
	      'string fits-file
	      ;; note conversion of [1] to [0]
	      (if extension (format nil "[~A]" (1- extension)) "")))
	   (extension/hdu ;; [1] based as is now standar
	     (or extension
		 (if (typep inst 'instrument-id:onechip)
		     (the (integer 1)
			  (instrument-id:get-image-extension-for-onechip-fits fits-file)))
		 1)) ;; [1] extension by default
	   (pixel-scale
	     (%get-pixel-scale-for-sextractor fits-file inst extension/hdu pixel-scale))
	   (param-filename (format nil "~A/sex~A.param" dir conf-suffix))
	   (conv-filename (format nil "~A/sex~A.conv" dir conf-suffix))
	   (nnw-filename (format nil "~A/sex~A.nnw" dir conf-suffix))
	   (conf-filename (format nil "~A/sex~A.conf" dir conf-suffix))
	   (Catalog-filename (format nil "~A/~A" dir output-catalog))
	   ;; change checkimage Fits files to be in the default directory
	   (checkimage-types-and-names
	     (%build-checkimage-file-pairs dir checkimage-types-and-names))
	   (md5-signature nil)
	   (%gain (cond ((numberp gain)
			 gain)
			(inst
			 (instrument-id:get-gain-for-instrument
			  inst fits-file :extension extension/hdu))
			((stringp gain)
			 (or (cf:read-fits-header fits-file gain :extension extension/hdu)
			     (error "Could not read GAIN keyword '~A' from ~A"
				    gain full-fits-file)))))
	   (old-output-exists-using-md5 nil))

      (when make-badpix-weight-image
	(setf weight-type "MAP_WEIGHT")
	(setf weight-image (make-badpix-weight-image
			    fits-file
			    :extension extension/hdu
			    :instrument inst :if-exists :do-nothing)))

      (%make-sextractor-param-file
       :param-filename param-filename 
       :n-phot-apertures (length phot-apertures)
       :extra-output-parameters extra-output-parameters)
      (%make-sextractor-conv-file   :conv-filename conv-filename)
      (%make-sextractor-nnw-file    :nnw-filename nnw-filename)
      (%make-sextractor-conf-file  
       :conf-filename conf-filename 
       :param-filename param-filename
       :nnw-filename nnw-filename
       :conv-filename conv-filename
       :catalog-filename catalog-filename
       :detect-threshold detect-threshold
       :back-size back-size
       :back-filtersize back-filtersize
       :analysis-threshold analysis-threshold
       :checkimage-types-and-names checkimage-types-and-names
       :mag-zeropoint mag-zeropoint
       :gain %gain
       :deblend-nthresh deblend-nthresh
       :deblend-mincont deblend-mincont
       :conv-filename conv-filename
       :phot-apertures phot-apertures
       :phot-autoparams phot-autoparams
       :satur-level satur-level 
       :verbose-type verbose-type
       :pixel-scale pixel-scale
       :weight-image weight-image :weight-type weight-type 
       :flag-image  flag-image    :flag-type   flag-type
       :nthreads nthreads)

      ;; append a note to conf filename so that md5 signature reflects it
      (when flag-cosmic-rays
	(with-open-file (sc conf-filename :direction :output :if-exists :append)
	  (format sc "# NOTE for MD5 signature - flagging cosmic rays~%")))

      ;; always compute signature, because it MAY be used to check
      ;; for need to re-run, and WILL be put in output
      (setf md5-signature (%compute-sextractor-run-md5
			   fits-file extension/hdu param-filename conf-filename conv-filename 
			   nnw-filename make-badpix-weight-image))
      ;;
      (setf old-output-exists-using-md5 
	    (and (probe-file catalog-filename)
		 md5-signature
		 (equalp (cf:read-fits-header catalog-filename "SEXTRMD5")
			 md5-signature)))
      
      (when (or (not md5-avoid-rerun)
		(not old-output-exists-using-md5))
	;;
	(prog1
	    (run-program-and-check
		"Sextractor process exited with failure"
	      (jutils:run-program   
	       (get-sextractor-program)
	       (list full-fits-file "-c" conf-filename)
	       :wait t 
	       :output output 
	       ;; stderr to stdout only if requested
	       :error (if display-errors t nil) ))
	  ;;
	  (when flag-cosmic-rays
	    (sextractor-insert-cosmic-rays-into-imflags fits-file 
							catalog-filename
							:extension extension/hdu))
	  ;;
	  (cf:write-fits-header catalog-filename "SEXTRMD5" md5-signature
				:comment "sextractor config md5 signature")
	  (when phot-apertures
	    (cf:write-multiline-header
	     catalog-filename
	     "APERTURES" (format nil "~A" phot-apertures)
	     :comment "Sextractor apertures for MAG_APER, FLUX_APER"))
	  ))
      )))
     
 
    

    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SCAMP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
  

(defun run-scamp
    (fits-file &key
		 (extension nil)
		 (output *standard-output*)
		 (sextractor-catalog-base "sex") ;; for "sex.cat"
		 (xml-filename "scamp.xml")
		 (conf-suffix "") 
		 (display-errors t)
		 (header-suffix "head")	   ;; output headers
		 (aheader-suffix "ahead")  ;; extra input headers
		 (copy-head-up t)
		 (astref-mag-low 10) (astref-mag-high 22) ;; catalog mag limits
		 (pixscale-maxerr 1.03)
		 (posangle-maxerr 5.0)
		 (position-maxerr 2.0)	;; arcmin
		 (crossid-radius  3.0)	;; arcsec
		 (astref-catalog "2mass")
		 (astref-catalog-filename nil)
		 (catalog-server  *default-vizier-catalog-server*)
		 (refout-catpath nil)
		 (scamp-save-refcatalog "N")
		 (match-flipped "N") ;; allow flipped axes
		 (distort-degrees 1)
		 (flags-mask #x0fc) ;; default; reject saturated and corrupted
		 (fwhm-threshold-low 1.0)
		 (fwhm-threshold-high 40.0)
		 (checkplot-dev "NULL") ;; none
		 (verbose-type "NORMAL"))
  "Run scamp on fits file, assuming that the sextractor catalog 
FITSFILE_DIR/sex.cat exists already.  If COPY-HEAD is T, then
copy the headfile created, sex.head, to FITSFILE.head so it can
be used further."
  
  (when (and astref-catalog astref-catalog-filename)
    (error "Both astref-catalog=~A and astref-catalog-filename=~A are defined. Only one of these may be used so that EITHER a remote catalog is used, or a local file."
	   astref-catalog astref-catalog-filename))

  (when (and astref-catalog-filename
	     (not (probe-file astref-catalog-filename)))
    (error "Can't find astref-catalog-filename file '~A'" astref-catalog-filename))    
    
  
  (multiple-value-bind (dir base topdir)
      (ensure-fits-directory fits-file :extension extension)
    (let ((catalog-filename
	    (format nil "~A/~A.cat" dir sextractor-catalog-base))
	  (scamp-conf-filename (format nil "~A/scamp~A.conf" dir conf-suffix))
	  ;; put catalogs next to the file
	  (astrometric-catalog-path
	    (or refout-catpath
		(concatenate 'string
			     topdir 
			     "scamp-astrometric-catalogs"))))
      (when (not (probe-file catalog-filename))
	(error "~A sextractor catalog does not exist" catalog-filename))
      (when (equalp scamp-save-refcatalog "Y")
	(ensure-directories-exist
	 (concatenate 'string astrometric-catalog-path "/#IGNORE")))
      (%make-scamp-conf-file      
       :conf-filename scamp-conf-filename
       :xml-filename xml-filename
       :header-suffix header-suffix :aheader-suffix aheader-suffix
       :output-dir (format nil "~A/" dir) ;; weird scamp behavior wrt plots
       :astref-catalog (or astref-catalog "FILE")
       :astrefcat-name (or astref-catalog-filename "NONE")
       :catalog-server catalog-server
       :checkplot-dev checkplot-dev
       :astref-mag-low astref-mag-low :astref-mag-high astref-mag-high
       :scamp-save-refcatalog scamp-save-refcatalog
       :astrometric-catalog-path astrometric-catalog-path
       :pixscale-maxerr pixscale-maxerr
       :position-maxerr position-maxerr
       :posangle-maxerr posangle-maxerr
       :crossid-radius crossid-radius
       :match-flipped  match-flipped
       :distort-degrees distort-degrees
       :flags-mask flags-mask
       :fwhm-threshold-low fwhm-threshold-low
       :fwhm-threshold-high fwhm-threshold-high
       :verbose-type verbose-type)
      
      (run-program-and-check
	  "Scamp process exited with failure"
	    (jutils:run-program   
	     (get-scamp-program)
	     (list catalog-filename "-c" scamp-conf-filename)
	     :wait t 
	     :output output 
	     ;; stderr to stdout only if requested
	     :error (if display-errors t nil) ))
      (when copy-head-up 
	(file-io:copy-file 
	 (format nil "~A/~A.head" dir sextractor-catalog-base)
	 (format nil "~A.head" base)
	 :overwrite t))
      (read-scamp-xml-file (format nil "~A/~A" dir xml-filename))

      )))





(defun write-swarp-header-file (head-file-name entries
				&key wcs naxis1 naxis2
				set-cr-values)
  "Write a header file for WCS, NAXIS1 and NAXIS2, and with ENTRIES being a list of
additional legal ((\"KEYWORD\" <VALUE> [comment]) ...) pairs.  :TRUE
or :FALSE may be used for boolean T/F but T/NIL also work.
Any of ENTRIES, WCS, NAXIS1, NAXIS2 can be NIL (and not written).

Unless SET-CR-VALUES is true, then CRPIX1,2 and CRVAL1,2 are not written,
to allow SWARP's default centering to take effect.

Header file is termined by an END statement."
  (with-open-file (sout head-file-name :direction :output
				       :if-exists :supersede
				       :if-does-not-exist :create)

    
    (when naxis1 (format sout "NAXIS1  = ~D~%" naxis1))
    (when naxis2 (format sout "NAXIS2  = ~D~%" naxis2))
    (when wcs
      (when (not (typep wcs 'wcs:wcs-radec-tan))
	(error "WCS ~A is not WCS:WCS-RADEC-TAN" wcs))
      (format sout "CTYPE1  = 'RA---TAN'~%") 
      (format sout "CTYPE2  = 'DEC--TAN'~%") 
      (format sout "RADESYS = 'ICRS'~%")
      (format sout "EQUINOX = ~,8F~%" (wcs:wcs-radec-tan-equinox wcs))
      (when set-cr-values
	(format sout "CRVAL1  = ~,8F~%" (wcs:wcs-radec-tan-crval1 wcs))
	(format sout "CRVAL2  = ~,8F~%" (wcs:wcs-radec-tan-crval2 wcs))
	(format sout "CRPIX1  = ~,5F~%" (wcs:wcs-radec-tan-crpix1 wcs))
	(format sout "CRPIX2  = ~,5F~%" (wcs:wcs-radec-tan-crpix2 wcs)))
      (format sout "CD1_1   = ~,8,,,,,'eE~%" (wcs:wcs-radec-tan-cd1_1 wcs))
      (format sout "CD1_2   = ~,8,,,,,'eE~%" (wcs:wcs-radec-tan-cd1_2 wcs))
      (format sout "CD2_1   = ~,8,,,,,'eE~%" (wcs:wcs-radec-tan-cd2_1 wcs))
      (format sout "CD2_2   = ~,8,,,,,'eE~%" (wcs:wcs-radec-tan-cd2_2 wcs))
      (format sout "CUNIT1  = ~A~%" "'deg'")
      (format sout "CUNIT2  = ~A~%" "'deg'"))

    (loop for (key val comment) in entries
	  do
	     (format sout "~8A= " key)
	     (cond ((stringp val)
		    (format sout "'~A'" val))
		   ((floatp val)
		    (format sout "~,10F" val))
		   ((integerp val)
		    (format sout "~D" val))
		   ((or (eq val nil) (eq val :false))
		    (format sout "F"))
		   ((or (eq val :true) (eq val t))
		    (format sout "T"))
		   (t
		    (error "Cannot write header for value type ~A:  ~S=~A"
			   (type-of val) key val)))
	     (when comment
	       (format sout " / ~A" comment))
	     (terpri sout))
    ;;
    (format sout "END      ~%")))
		    

#+nil ;; hightlights a compiler warning bug - (eq val :true) supposedly unused
(defun blah (val)               
  (cond ((or (eq val nil) (eq val :false))
	 "F")
	((or (eq val t)  (eq val :true))
	 "T")))

(defun run-swarp
    (fits-file-or-files imageout-base
     &key 
       (output t) ;; FIXME - output should be *standard-out*?
       (conf-suffix "")
       (display-errors t)
       (combine "Y")
       (weight-type "NONE")
       (weight-suffix ".weight.fits")
       (weight-image nil)
       (make-badpix-weight-image nil)
       (combine-type "MEDIAN")
       (center-type "ALL")
       (center "00:00:00.0, +00:00:00.0") ;; string, list, or vector
       (pixel-scale NIL) ;; overrides PIXEL-SCALE-TYPE if given
       (pixel-scale-type "MEDIAN")
       (image-size 0) ;; 0=automatic
       (resampling-type "LANCZOS3")
       (fscale-default 1.0)
       (gain-default 0.0)
       (blank-badpixels "Y")
       (satlev-keyword "SATURATE")
       (satlev-default 50000)
       (subtract-back "Y") 
       (header-suffix "head")
       (delete-tmpfiles "N")
       (verbose-type "NORMAL")
       (nthreads 0)
       (copy-keywords :auto)
       (output-headers nil)
       (wcs nil)
       (center-on-wcs nil)
       )

       
  "Run Swarp on a file or list of files in FITS-FILE-OR-FILES, creating
imageout-base.fits, imageout-base.weight.fits, and a working directory
imageout_DIR.  If PIXEL-SCALE is set, it overrides PIXEL-SCALE-TYPE,
setting the pixel scale to a constant value.  SUBTRACT-BACK and
DELETE-TMPFILES can be Y/N or T/NIL.

IMAGEOUT-BASE, if not an absolute path, is defind relative to
CL::*DEFAULT-PATHNAME-DEFAULTS*

If COPY-KEYWORDS is :AUTO then use INSTRUMENT-ID:GET-CRITICAL-KEYWORDS-FOR-FITS
to determine which keywords to copy.

If OUTPUT-HEADERS, a list of ((\"KEY\" value [comment]) ..) entries, is specified,
then a header file IMAGEOUT-BASE.header-suffix is made.
If WCS is specified, then WCS is written into the header to constrain the output
image to have this output WCS.
NAXIS1 and NAXIS2 can be specified using :IMAGE-SIZE #(NAXIS1 NAXIS2).
Any previous head file is overwritten.

If CENTER-ON-WCS is set and WCS is given, then other centering
arguments are overridden, and CENTER-TYPE=MANUAL and CENTER=#(CRVAL1,
CRVAL1).

Returns (VALUES IMAGE-OUT WEIGHT-OUT)

NOTE: CRVAL1,2 and CRPIX1,2 are never transferred from WZS, so the WCS
is really used only for CD_ij matrix."
  
  ;; first force the imageout-base to obey the LISP default directory if 
  ;; it isn't absolute
  (let ((imageout-base-abs (file-io:make-filename-absolute-using-default-pathname-defaults
			    imageout-base)))
    (multiple-value-bind (dir base topdir)
	(ensure-fits-directory imageout-base-abs)
      (declare (ignorable topdir))
      (let ((swarp-conf-filename (format nil "~A/swarp~A.conf" dir conf-suffix))
	    (fitslist (if (listp fits-file-or-files)
			  fits-file-or-files
			  (list fits-file-or-files)))
	    (subtract-back (if (member subtract-back '(t "Y") :test 'equalp)
			       "Y" "N"))
	    (delete-tmpfiles (if (member delete-tmpfiles '(t "Y") :test 'equalp)
				 "Y" "N"))
	    (imageout (format nil "~A.fits" base))
	    (weightout (format nil "~A.weight.fits" base))
	    (headerfile (format nil "~A.~A" base header-suffix))
	    (auto-image-size (eql image-size 0))
	    (naxis1 (cond ((integerp image-size) image-size)
			  ((typep image-size 'sequence) (elt image-size 0))
			  (t (error "Invalid IMAGE-SIZE: ~A" image-size))))
	    (naxis2 (cond ((integerp image-size) image-size)
			  ((and (typep image-size 'sequence)
				(= (length image-size) 2))
			   (elt image-size 1))
			  (t (error "Invalid IMAGE-SIZE: ~A" image-size))))
	    
	    ;; override pixel-scale-type by pixel scale
	    (pixel-scale-type (if (not pixel-scale) pixel-scale-type "MANUAL"))
	    (center (cond ((stringp center) center)
			  ((typep center 'sequence)
			   (format nil "~A, ~A" (elt center 0) (elt center 1))))))

	;; avoid making dangerously large NAXIS1,2
	(when (not auto-image-size)  ;; but this lets terapix make it dangerously big
	  (when (or (and naxis1 (not (and (integerp naxis1)
					  (<= 1 naxis1 (expt 2 16)))))
		    (and naxis2 (not (and (integerp naxis2)
					  (<= 1 naxis2 (expt 2 16))))))
	    (error "NAXIS1,2 invalid or out of range: NAXIS1=~A  NAXIS2=~A" naxis1 naxis2)))

	(when (and wcs center-on-wcs)
	  (setf center-type "MANUAL")
	  (setf center (vector (wcs:wcs-radec-tan-crval1 wcs)
			       (wcs:wcs-radec-tan-crval2 wcs))))

	(when (or wcs output-headers)
	  (write-swarp-header-file headerfile output-headers
				   :wcs wcs
				   :naxis1 naxis1
				   :naxis2 naxis2
				   ;; let swarp figure out CRVAL, CRPIX
				   ;; using CENTER
				   :set-cr-values nil))

	(when make-badpix-weight-image
	  (when weight-image
	    (error "Cannot specify WEIGHT-IMAGE and MAKE-BADPIX-WEIGHT-IMAGE"))
	  (setf weight-type "MAP_WEIGHT")
	  (loop with weight-image-list = nil
		for fits-file in fitslist
		for inst = (instrument-id:identify-instrument fits-file)
		do
		   (when (not (typep inst 'instrument-id:onechip))
		     (error "Cannot yet make a badpix-weight-image for a fits file that is not a onechip."))
		   
		   (let ((this-weight-image (make-badpix-weight-image
					     fits-file 
					     :extension nil ;; auto for onechip
					     :expand-cosmics-by-one nil ;; judgment call
					     :instrument inst
					     :if-exists :do-nothing)))
		     (push this-weight-image weight-image-list))
		finally (setf weight-image weight-image-list)))
	
	(%make-swarp-conf-file :conf-filename swarp-conf-filename 
			       :weight-type weight-type
			       :weight-suffix weight-suffix
			       :weight-image weight-image
			       :imageout-name imageout
			       :weightout-name weightout
			       :header-suffix header-suffix
			       :combine combine
			       :combine-type combine-type
			       :center-type center-type
			       :center center
			       :pixel-scale-type pixel-scale-type
			       :pixel-scale (or pixel-scale 0)
			       :image-size image-size
			       :resample-dir dir
			       :resampling-type resampling-type
			       :fscale-default fscale-default
			       :gain-default gain-default
			       :blank-badpixels blank-badpixels
			       :satlev-keyword satlev-keyword
			       ;; we can't use instrument-id:saturation-level because
			       ;; images might be from different instruments
			       ;; WARNING - satlev might not do anything!!
			       :satlev-default satlev-default
			       :subtract-back subtract-back
			       :delete-tmpfiles delete-tmpfiles
			       ;; we have to transfer the headers manually below
			       :copy-keywords nil
			       :verbose-type verbose-type
			       :nthreads nthreads)
	;;
	(run-program-and-check
	 "Swarp process exited with failure"
	 (jutils:run-program   
	  (get-swarp-program)
	  `("-c" ,swarp-conf-filename ,@fitslist)
	  :wait t 
	  :output output 
	  ;; stderr to stdout only if requested
	  :error (if display-errors t nil) ))
	
	;; unfortunately, swarp doesn't handle HIERARCH headers nicely, so
	;; we manually transfer the headers, extension by extension

	(transfer-headers (first fitslist) imageout
			  (if (eq copy-keywords :auto)
			      (instrument-id:get-critical-headers-for-fits
			       (first fitslist))
			      copy-keywords)
			  ;; a swarp output is a single-extension fits file, but
			  ;; our input might be a onechip with 1,2 extensions.
			  :put-all-in-first-extension t
			  ;; don't do more than 2 extensions - this is because
			  ;; we think all input images will contain important
			  ;; info in first two headers
			  :n-ext-max 2)
	;;
	(values imageout weightout)))))
      



;; return T if STR consists of PREFIX plus a number
(defun %prefix+num? (prefix str)
  (declare (type string prefix str))
  (and (eql (search prefix str) 0)
       (> (length str) (length prefix))
       (loop for i from (length prefix) below (length str)
	     when (not (digit-char-p (aref str i)))
	       do (return nil)
	     finally (return t))))




;; by default, ignore any WCS headers because we assume that Swarp writes its
;; own
(defun transfer-headers (fits-in fits-out header-list
			 &key
			   (ignore-wcs t)
			   ;; Swarp produdes one-header images, so we put all
			   ;; the headers in the first header
			   (put-all-in-first-extension nil)
			   ;; max number of extensions to do
			   (n-ext-max nil))
			 
  (cf:with-open-fits-file (fits-in ffin :mode :input)
    (cf:with-open-fits-file (fits-out ffout :mode :io)
      (loop for iext from 1 to (cf:fits-file-num-hdus ffin)
	    ;; don't do more than maximum number of extensions - for Swarp run
	    ;; this is 2
	    while (or (not n-ext-max)
		      (<= iext n-ext-max))
	    do (cf:move-to-extension ffin iext)
	       (when (not put-all-in-first-extension)
		 (cf:move-to-extension ffout iext))
	       (loop for key in header-list
		     when (and (stringp key)
			       (or (not ignore-wcs)
				 (not (cf:wcs-header-p key))))
		       do (multiple-value-bind (value comment keyname)
			      (cf:read-fits-header ffin key)
			    (when keyname ;; header was present
			      (cf:write-fits-header ffout key value
						    :comment comment))))))))
	  
