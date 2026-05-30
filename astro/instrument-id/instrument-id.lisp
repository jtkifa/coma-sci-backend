

(in-package instrument-id)





(defclass %anychips ()
  ())

(defclass multichip (%anychips)
  ())

(defclass onechip (%anychips)
  ;; chip-id is an identifier for a onechip instrument that might be taken
  ;; from an array
  ((chip-id :initarg :chip-id :initform t :accessor chip-id)))


;; parent of all instrument
(defclass instrument ()
  ((name :initarg :name  :accessor instrument-name) ;; a string
   (observatory :initarg :observatory  :accessor instrument-observatory)))
  


(defclass imaging-instrument (instrument)
  ;; for saturation level
  ((saturation-level :initarg :saturation-level ;; in ADU
		     :accessor saturation-level
		     :initform 32000)
   (non-linear-level :initarg :non-linear-level ;; in ADU
		     :accessor non-linear-level
		     :initform 32000)
   (aperture :initarg :aperture
	     :accessor aperture
	     :initform NIL) ;; NIL = unknown aperture
   ;; these are mainly for MPC XML-form reporting
   (detector-type   :initarg :detector-type
		    :accessor detector-type
		    :initform "CCD")
   (telescope-design  :initarg :telescope-design
		      :accessor telescope-design
		      :initform "Reflector")
   ;; does this instrument have one or more overscan regions
   ;; suitable for processing?  NIL by default
   (has-overscan :initarg :has-overscan
		 :accessor has-overscan
		 :initform nil)   
   ))

;; instrument mixin for a fzipped file - most instruments do not have this written yet.
;; See subaru-hypersuprime-cam or cfht-megacam
(defclass fzipped ()
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mixin for a preprocessed image, like images from processed survey
;; NOTE - this MIXIN must go FIRST, or the methods must be repeated
(defclass preprocessed ()
  ((preprocessed-p :initarg :preprocessed-p :initform t :accessor preprocessed-p) ;; bias and flat
   (wcs-origin    ;; origin of wcs if present
    :initarg :wcs-origin :initform "unknown" :accessor preproc-wcs-origin)
   (phot-calib-origin ;; origin of phot calib if present
    :initarg :phot-calib-origin :initform "unknown" :accessor preproc-phot-calib-origin)))

;; by default, instruments without a PREPROCESSED mixin have  preproc-xxx-calib-origin = NIL
(defmethod preprocessed-p ((inst imaging-instrument))
  (if (typep inst 'preprocessed) ;; this complexity so that secondary parent class propagates
      (call-next-method)
      nil))
(defmethod preproc-wcs-origin ((inst imaging-instrument))
  (if (typep inst 'preprocessed)
       (call-next-method)
       nil))
(defmethod preproc-phot-calib-origin ((inst imaging-instrument))
  (if (typep inst 'preprocessed)
      (call-next-method)
      nil))
(defun preprocessed-p-for-fits (fits-file &key (instrument nil) (extension nil))
  (declare (ignore extension))
  (preprocessed-p (or instrument (identify-instrument fits-file))))
(defun preproc-wcs-origin-for-fits (fits-file &key (instrument nil) (extension nil))
  (declare (ignore extension))
  (preproc-wcs-origin (or instrument (identify-instrument fits-file))))
(defun preproc-phot-calib-origin-for-fits (fits-file &key (instrument nil) (extension nil))
  (declare (ignore extension))
  (preproc-phot-calib-origin (or instrument (identify-instrument fits-file))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmethod fzipped-p ((inst instrument))
  (typep inst 'fzipped))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgeneric get-info-extension (instrument)
  (:documentation "Get the informative extension for an instrument"))

(defmethod get-info-extension ((inst imaging-instrument))
  (cond
    ;; by default, fzipped single chip file has all its info in extension 2 if 
    ((and (fzipped-p inst)
	  (typep inst 'onechip))
     2)
    ;; but multipchip has it in normal place, extension 1, because it already had many
    ;; exts before it was compressed
    ((and (fzipped-p inst)
	   (typep inst 'multichip))
     1)
    ;; and non-fzipped instruments have it in ext 1
    (t
     1)))
    
  



;; instrument equality indicates if images can be combined together
;; meaning 'same chip' for onchip instruments
(defgeneric instruments-combinable-p (inst1 inst2)
  (:documentation " if images can be combined together
meaning 'same chip' for onchip instruments."))

(defmethod instruments-combinable-p ((inst1 %anychips) (inst2 %anychips))
  (and (eq (type-of inst1) (type-of inst2))
       (or (not (typep inst1 'onechip))
	   (equalp (chip-id inst1) (chip-id inst2)))))


;; These can be overriden by individual instruments.  The are not
;; part of the object, because the keyword doesn't always exist,
;; eg when gain is known from external source, not header.
(defgeneric instrument-gain-keyword (inst))
(defgeneric instrument-exptime-keyword (inst))
(defmethod instrument-gain-keyword ((inst instrument))  "GAIN")
(defmethod instrument-exptime-keyword ((inst instrument))  "EXPTIME")

;; a macro to define a specific instrument that pushes the instrument onto 
;; *KNOWN-INSTRUMENTS* - should use only for true instruments, not
;; abstact classes and mixins
(defvar *known-instruments* nil)
(defmacro defclass/inst (instrument &body body)
  `(progn
     (pushnew  ',instrument *known-instruments*)
     (defclass ,instrument ,@body)))
  

  

;; helper function for later on - by default, reads first header - this should be gradually
;; superseded by %gethead-info
(defun %gethead-or-error (fits header &key (space-trim-strings t) (extension 1)
			       (throw-error t))
  (multiple-value-bind (val comment keyword status)
      (cf:read-fits-header fits header :extension extension)
    (declare (ignore comment))
    (when (and throw-error (or (not keyword) (not (zerop status))))
      (error "Header ~A not found in fits file ~A" header
	     (if (stringp fits) 
		 fits
		 (format nil "~A[~A]" (cf:fits-file-filename fits)
			 (1- (cf:fits-file-current-hdu-num fits))))))
    ;;
    (when (and space-trim-strings (stringp val))
      (setf val (string-trim '(#\space #\tab) val)))
    ;;
    val))


;; helper macro to throw error if a multichip function is called without
;; EXTENSION keyword set, unless FITS-FILE is a CF:FITS_FILE, in which
;; case the current extension can be used. Assumes existence of variables
;; EXTENSION and FITS-FILE
(defmacro %require-extension (where)
  `(when (not (or (cf:fits-file-p fits-file)
		  extension)) ;; for case of filename
     (error ,(format nil "ERROR in ~A - must specify EXTENSION for multichip." where))))



;; helper to get header from the 'info' header which is generally header 1 or 2,
;; using method  (get-info-header inst)
(defun %gethead-info (inst fits header &key (space-trim-strings t) (throw-error t))
   (multiple-value-bind (val comment keyword status)
       (cf:read-fits-header fits header
			    :extension (get-info-extension inst))
    (declare (ignore comment))
    (when (and throw-error (or (not keyword) (not (zerop status))))
      (error "Header ~A not found in fits file ~A" header
	     (if (stringp fits) 
		 fits
		 (format nil "~A[~A]" (cf:fits-file-filename fits)
			 (1- (cf:fits-file-current-hdu-num fits))))))
    ;;
    (when (and space-trim-strings (stringp val))
      (setf val (string-trim '(#\space #\tab) val)))
    ;;
    val))
  


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (instrument-id::make-instrument-for-name 'cfht-megacam-one-chip-ext)
;;    ==> #<instrument-id:cfht-megacam-one-chip-ext {100CE85D93}>
(defun make-instrument-for-name (name)
  "Given a NAME (symbol or string), generate the instrument object
corresponding to it, or NIL.  Name is allowed to have colons in it,
specifying a package, and anything before the colons is stripped out."
  (declare (type (or symbol string) name))
  (let ((str (if (symbolp name)
		 (string name) ;; this strips the package name
		 ;; strip package name from string if present
		 (let ((n (position #\: name :from-end t)))
		   (when (eql n (1- (length name)))
		     (return-from make-instrument-for-name nil))
		   (if n
		       (subseq name (1+ n))
		       name)))))
    (nth-value 0
	       (ignore-errors
		(make-instance
		 (find-symbol (string-upcase str) '#:instrument-id))))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; the following functions are defined for all instruments and throw an error if 
;; no value is returned.  They have AROUND methods to fall back on
;; STDHEADS if we have inserted them.
;;

(defgeneric test-if-image-at-extension-for-instrument
    (imaging-instrument fits-file &key extension)
  (:documentation "Return T if the current extension (for EXTENSION=NIL) or
named extension (if EXTENSION is not NIL) is an image extension for
which various image parameters can be defined."))


(defgeneric get-badpix-function-for-instrument
    (imaging-instrument fits-file &key extension)
  (:documentation "Return a function (lambda (ix iy) that returns T
if 1-indexed pixel ix,iy is bad"))

(defgeneric get-critical-headers-for-instrument (imaging-instrument fits-file)
  (:documentation "get the unique critical headers needed for this instrument,
for example for image duplication"))

(defgeneric get-standard-filter-for-instrument (imaging-instrument fits-file)
  (:documentation "get the image symbol (:uj :bj .. :usdss etc) for a fits file"))

(defgeneric get-exptime-for-instrument (instrument fits-file)
  (:documentation "Get exposure time in sec for instrument for a fits file"))

(defgeneric get-object-for-instrument (instrument fits-file)
  (:documentation "Get object name for instrument for a fits file"))

(defgeneric get-object-type-for-instrument (instrument fits-file)
  (:documentation "Get one of (:BIAS :FLAT :OBJECT :OTHER) for fits file."))

(defgeneric get-mjd-start-for-instrument (instrument fits-file)
  (:documentation "Get MJD for instrument for a fits file"))

(defgeneric get-mjd-mid-for-instrument (instrument fits-file)
  (:documentation "Get MJD of the center of the exposure for instrument for a fits file"))

(defgeneric get-gain-for-instrument (instrument fits-file &key extension)
  (:documentation "Get GAIN for instrument for a fits file, EXTENSION"))

;; we sometimes need to write a new gain
(defgeneric write-gain-for-instrument (instrument fits-file gain
				       &key extension)
  (:documentation "Write GAIN for instrument for a fits file at extension"))

(defgeneric get-chip-id-for-instrument (imaging-instrument fits-file
					&key extension)
  (:documentation "Get the current chip ID for an instrument"))

(defgeneric get-datasec-for-instrument (imaging-instrument fits-file
					&key extension)
  (:documentation "Get DATESEC as a vector #(nx1 nx2 ny1 ny2) for an
image.  This is the section of the BINNED image that contains image
data. The indices start at 1.  If this returns NIL, then the image is
not to be trimmed."))

(defgeneric get-trimsec-for-instrument (imaging-instrument fits-file
					&key extension)
  (:documentation "Get TRIMSEC as a vector #(nx1 nx2 ny1 ny2) for an
image.  This is the section of the BINNED image that contains GOOD
image data. The indices start at 1.  If this returns NIL, then the
image is not to be trimmed."))

(defgeneric get-overscans-for-instrument (imaging-instrument fits-file
					  &key extension)
  (:documentation "Get a list of overscan sections as vectors #(nx1 nx2 ny1 ny2)
for animage.  This is the section of the BINNED image that contains GOOD
image data. If this returns NIL, then there is no overscan.  By default,
an instrument has no overscan."))


(defgeneric get-statsec-for-instrument (imaging-instrument fits-file
					&key extension)
  (:documentation "Get statistics section as a vector #(nx1 nx2 ny1
ny2) for an image.  This is the section of the BINNED image that
contains image data good for computing image statistics (mainly,
unvignetted). The indices start at 1, and it is in UNTRIMMED
coordinates."))

(defgeneric get-initial-wcs-for-instrument (imaging-instrument fits-file
					    &key extension)
  (:documentation "Get the initial WCS guess for an instrument"))

(defgeneric insert-initial-wcs-for-instrument (imaging-instrument fits-file
					       &key extension)
  (:documentation "Insert an initial guess for a WCS into a one-chip instrument"))


(defgeneric get-bright-flux-limit-for-instrument (imaging-instrument fits-file)
  (:documentation "Get the bright flux limit - min(saturation, nonlinear) - for
an instrument"))

(defgeneric get-pixel-scale-for-instrument (imaging-instrument fits-file &key extension)
  (:documentation "Get the pixel scale in arcsec/pix for an instrument."))

(defgeneric get-observatory-for-instrument (imaging-instrument fits-file)
  (:documentation "Get the observatory for an instrument"))




(defgeneric get-image-extension-for-onechip-instrument (onechip fits-file)
  (:documentation "Get the extension (HDU) in which the image lives,
for a one-chip image.  Note that HDU 1 is usually denoted as
image.fits[0]."))

(defgeneric get-calibrated-zeropoint-for-instrument
    (imaging-instrument fits-file &key extension
				    flux-units
				    exposure)  
  (:documentation "Get the zeropoint calibration (if it exist) for an image 
(mainly useful for calibrated surveys)
FLUX-UNITS can be :ADU or :ELECTRONS
EXPOSURE   can be :ONE-SECOND or :EXPTIME

Return (VALUES ZP ZP-ERR) or NIL if no zeropoint available."))

(defgeneric is-image-reduced-for-instrument
    (imaging-instrument fits-file &key extension)
  (:documentation "Is FITS-FILE reduced, using various piplines?  Returns NIL
or a keyword representing the method used."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; by default, return first 2d image in a fits file
(defmethod get-image-extension-for-onechip-instrument ((inst onechip) fits-file)
  (if (fzipped-p inst)
      2  ;; fzipped file put everything into ext 2 unless it began with multiple exts
      ;; else search for first 2d image to account for possible primary header
      (cf:maybe-with-open-fits-file (fits-file ff)
	(loop for i from 1 to (cf:fits-file-num-hdus ff)
	      do (cf:move-to-extension ff i)
		 (when (and (cf:fits-file-current-hdu-type ff) :image
			    (eql 2 (cf:fits-file-current-image-ndims ff)))
		   (return i))))))

;; common critical keywords
(defparameter *default-critical-keywords*
  '("OBJECT" "OBJ" "DATE" "DATE-OBS" "UT" "UT-OBS" "UTDATE" "RA" "DEC" "AIRMASS"
    "HA" "ST" "ZD" "OBSTYPE" "INSTRUMENT" "FILTER" "FILT" "FILTER1" "FILTER2"
    "MJD" "MJD-DATE"
    "MJD-OBS" "MJDATE" "EXPTIME" "LATITUDE" "LONGITUD" "TELESCOP" "INSTRUME"
    "DETECTOR" "IMAGETYP" "EPOCH" "EQUINOX"  "EXTNAME" "DET-ID"
    "EPOMJD"
    "CTYPE1" "CTYPE2" "CRVAL1" "CRVAL2" "CRPIX1" "CRPIX2"
    "CD1_1" "CD1_2" "CD2_1" "CD2_2"))
    

(defmethod get-critical-headers-for-instrument ((inst imaging-instrument) fits-file)
  (declare (ignore inst fits-file))
  *default-critical-keywords*)

;; by default, there are no zero point calibs, but we look if our photcalib package has
#|
 For future reference, here is conversion
 
   mag = ZP_adu -  2.5 log(adu)    and adu=electrons/gain
       = ZP_adu - [ 2.5 log(electrons/gain) ]
       = ZP_adu - [ 2.5 log(electrons) - 2.5 log(gain) ]

   Therefore ZP_e = ZP_adu + 2.5 log (gain)


  simlarly, the magnitude for 1s is more negative than for the entire exposure
  so ZP_1sec = ZP_exptime - 2.5 log (exptime)

|# 
(defmethod get-calibrated-zeropoint-for-instrument ((inst imaging-instrument) fits-file
						    &key extension
						      (flux-units :adu)
						      (exposure   :one-second))
  (declare (type (member :adu :electrons) flux-units)
	   (type (member :one-second :exptime) exposure))
  ;;
  (let* ((ext (if (typep inst 'onechip)
		  (get-image-extension-for-onechip-instrument inst fits-file)
		  (or extension (error "Need to provide EXTENSION for multi-chip"))))
	
	 (zp-adu-whole-exp (or
			    (cf:read-fits-header fits-file "HIERARCH PHOTCALIB.ZPMAG" 
						 :extension ext)
			    ;; sometimes we fake our ZPMAG as in PS1 data
			    (cf:read-fits-header fits-file "PCZPMAG" 
						 :extension ext)))
	 (zp-err  (or
		   (cf:read-fits-header fits-file "HIERARCH PHOTCALIB.ZPMAGERR" 
					:extension ext)
		   (cf:read-fits-header fits-file "PCEZPMAG" 
					:extension ext)))
	 (gain (if (eq flux-units :electrons)
		   (get-gain-for-instrument inst fits-file :extension ext)
		   t)) ;; return T='have it' for check below
	 (exptime (if (eq exposure :one-second)
		      (get-exptime-for-instrument inst fits-file)
		      t))) ;; return T='have it' for check below
    ;;
    (when (and zp-adu-whole-exp zp-err gain exptime)
      (let ((zp
	      (+ zp-adu-whole-exp
		 ;;
		 (cond ((eq flux-units :adu)
			0)
		       ((eq flux-units :electrons)
			(* +2.5d0 (log gain 10)))
		       (t (error "Invalid flux-units ~A" flux-units)))
		 ;;
		 (cond ((eq exposure :exptime)
			0)
		       ((eq exposure :one-second)
			(* -2.5d0 (log exptime 10)))
		       (t (error "Invalid exposure ~A" exposure))))))
	;;
	(values zp zp-err)))))
   
	
(defmethod is-reduced-for-instrument ((inst imaging-instrument) fits-file)
  (declare (ignorable inst))
  (or (preprocessed-p inst)
      (%is-image-reduced-using-common-methods fits-file)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

   
(defun %id-instrument-or-fail (fits-file &key (instrument nil))
  (or instrument (identify-instrument fits-file)
      (error "Can't identify instrument for fits file ~A" fits-file)))


(defun test-if-image-at-extension-for-fits (fits-file &key (instrument nil)
							extension)
  "Test if the current extnsion, or EXTENSION, is an actual image"
  (test-if-image-at-extension-for-instrument
   (%id-instrument-or-fail fits-file :instrument instrument)
   fits-file :extension extension))

(defun get-badpix-function-for-fits (fits-file &key (instrument nil)
						 extension)
  (get-badpix-function-for-instrument
    (%id-instrument-or-fail fits-file :instrument instrument)
    fits-file :extension extension))

;; an assertion that is placed where we attempt to do image ops
(defun err-if-not-image-at-extension (instrument fits where extension)
  (when (not (test-if-image-at-extension-for-instrument 
	      instrument fits :extension extension))
    (error "Attempted to perform a image-based operation '~A' at a non-image extension ~A of ~A"
	   where
	   ;; figure out which extension we're using for this call
	   extension
	   (if (cf:fits-file-p fits)
	       (cf:fits-file-filename fits)
	       fits))))
		       

(defun aperture-for-fits (fits-file  &key (instrument nil))
  "Get the aperture (meters) or NIL for a fits file."
  (aperture (%id-instrument-or-fail fits-file :instrument instrument)))

(defun detector-type-for-fits (fits-file  &key (instrument nil))
  "Get the detector type (usually :CCD) for an fits-file."
  (detector-type (%id-instrument-or-fail fits-file :instrument instrument)))
							
(defun get-critical-headers-for-fits (fits-file &key (instrument nil))
  "Get the standard (keyword) filter for a fits file."
  (get-critical-headers-for-instrument
   (%id-instrument-or-fail fits-file :instrument instrument)
				      fits-file))

(defun get-standard-filter-for-fits (fits-file &key (instrument nil))
  "Get the standard (keyword) filter for a fits file."
  (get-standard-filter-for-instrument
   (%id-instrument-or-fail fits-file :instrument instrument)
				      fits-file))

(defun get-exptime-for-fits (fits-file  &key (instrument nil))
  "Get the exposure time for an fits file."
  (get-exptime-for-instrument
   (%id-instrument-or-fail fits-file :instrument instrument)
			      fits-file))

(defun get-object-for-fits (fits-file  &key (instrument nil))
  "Get the object name for fits file."
  (get-object-for-instrument 
   (%id-instrument-or-fail fits-file :instrument instrument)
   fits-file))

(defun get-object-type-for-fits (fits-file  &key (instrument nil))
  "Get the type (:BIAS :FLAT :OBJECT :OTHER) for a fits file."
  (get-object-type-for-instrument 
   (%id-instrument-or-fail fits-file :instrument instrument)
				  fits-file))

(defun get-mjd-start-for-fits (fits-file  &key (instrument nil))
  "Get the starting MJD for a fits file."
  (get-mjd-start-for-instrument 
   (%id-instrument-or-fail fits-file :instrument instrument)
   fits-file))

(defun get-mjd-mid-for-fits (fits-file  &key (instrument nil))
  "Get the midpoint MJD for a fits file."
  (get-mjd-mid-for-instrument 
   (%id-instrument-or-fail fits-file :instrument instrument)
			      fits-file))

(defun get-gain-for-fits (fits-file  &key (instrument nil) (extension nil))
  "Get the gain for a fits file, possibly at EXTENSION."
  (get-gain-for-instrument
   (%id-instrument-or-fail fits-file :instrument instrument)
   fits-file
   :extension extension))

;; by default, write for current extension
(defun write-gain-for-fits (fits-file gain
			    &key (instrument nil) (extension nil))
  "Write the gain for a fits file, possibly at EXTENSION."
  (write-gain-for-instrument
   (%id-instrument-or-fail fits-file :instrument instrument)
   fits-file gain
   :extension extension))

(defun get-chip-id-for-fits (fits-file  &key (instrument nil) (extension nil))
  "Get the chip-id for a fits file, possibly at EXTENSION."
  (get-chip-id-for-instrument
   (%id-instrument-or-fail fits-file :instrument instrument)
   fits-file :extension extension))

(defun get-datasec-for-fits (fits-file  &key (instrument nil) (extension nil))
  "Get the data section #(x1 x2 y1 y2) for a fits file, possibly at EXTENSION."
  (get-datasec-for-instrument
   (%id-instrument-or-fail fits-file :instrument instrument)
   fits-file :extension extension))

(defun get-trimsec-for-fits (fits-file  &key (instrument nil) (extension nil))
  "Get the trim section #(x1 x2 y1 y2) for a fits file, possibly at EXTENSION.
This is a subset of the DATASEC with good data."
  (get-trimsec-for-instrument
   (%id-instrument-or-fail fits-file :instrument instrument)
   fits-file :extension extension))

(defun get-overscans-for-fits (fits-file  &key (instrument nil) (extension nil))
  "Get a list of overscans #(x1 x2 y1 y2) for a fits file, possibly at EXTENSION.
May return NIL; by default instruments don't have an overscan."
  (get-overscans-for-instrument
   (%id-instrument-or-fail fits-file :instrument instrument)
   fits-file :extension extension))


(defun get-statsec-for-fits (fits-file  &key (instrument nil) (extension nil))
  "Get the a section #(x1 x2 y1 y2) suitable for computing image
 statistics for a fits file, possibly at EXTENSION.  This is a subset
 of the TRIMSEC with unvignetted data.  It is in UNTRIMMED coordinates."
  (get-statsec-for-instrument
   (%id-instrument-or-fail fits-file :instrument instrument)
   fits-file :extension extension))


(defun convert-imagesec-to-trimmed-imagesec (imagesec trimsec)
  "Give IMAGESEC (section good for image statistics, in untrimmed
image) and TRIMSEC, both as #(x1 x2 y1 y2), compute what the IMAGESEC
would be for the trimmed image.  

This is a helper function when performing statistics on trimmed images."
  (declare (type (array t (4)) imagesec trimsec))
  (let ((x0 (1- (aref trimsec 0)))
	(y0 (1- (aref trimsec 2))))
    (vector (- (aref imagesec 0) x0)
	    (- (aref imagesec 1) x0)
	    (- (aref imagesec 2) y0)
	    (- (aref imagesec 3) y0))))
	    

(defun get-initial-wcs-for-fits (fits-file
				 &key (instrument nil) (extension nil))
  "Get the initial WCS for a fits file, possibly at EXTENSION."
  (get-initial-wcs-for-instrument
   (%id-instrument-or-fail fits-file :instrument instrument)
   fits-file :extension extension))

(defun get-pixel-scale-for-fits (fits-file &key (instrument nil)
					     (extension nil))
  "Get the pixel scale for a fits file, possibly at EXTENSION."
  (get-pixel-scale-for-instrument
   (%id-instrument-or-fail fits-file :instrument instrument)
   fits-file :extension extension))

(defun insert-initial-wcs-for-fits (fits-file
				    &key (instrument nil) (extension nil))
  "Insert an initial WCS for a fits file as WCS headers, possibly at EXTENSION."
  (insert-initial-wcs-for-instrument
   (%id-instrument-or-fail fits-file :instrument instrument)
   fits-file :extension extension))

(defun get-bright-flux-limit-for-fits (fits-file &key (instrument nil))
  "Get the maximum valid ADU for a fits image."
  (get-bright-flux-limit-for-instrument
   (%id-instrument-or-fail fits-file :instrument instrument)
   fits-file))

(defun get-observatory-for-fits (fits-file  &key (instrument nil))
  "Get the observatory at which fits-file was taken."
  (get-observatory-for-instrument
   (%id-instrument-or-fail fits-file :instrument instrument)
   fits-file))


(defun get-image-extension-for-onechip-fits (fits-file &key (onechip nil))
  "Get the extension in which a onechip image lives."
  (get-image-extension-for-onechip-instrument
   (or onechip (instrument-id:identify-instrument fits-file))
   fits-file))

(defun get-calibrated-zeropoint-for-fits (fits-file &key (instrument nil)
						      extension 
						      (flux-units :adu)
						      (exposure   :one-second))
  "Get the zeropoint calibration (if it exist) for an image 
 (mainly useful for calibrated surveys)
FLUX-UNITS can be :ADU or :ELECTRONS
EXPOSURE   can be :ONE-SECOND or :EXPTIME.

Return (VALUES ZP ZP-ERR) or NIL if no zeropoint available."
  (get-calibrated-zeropoint-for-instrument
   (or instrument (identify-instrument fits-file))
   fits-file
   :extension extension
   :flux-units flux-units
   :exposure exposure))


(defun is-reduced-for-fits (fits-file &key (instrument nil))
  "Test if an image is reduced, and return a keyword for the method, or NIL."
  (is-reduced-for-instrument
   (%id-instrument-or-fail fits-file :instrument instrument)
   fits-file))

;; method defined for onechip always
(defmethod get-gain-for-instrument ((inst onechip) fits-file &key extension)
  (declare (ignore extension))
  (let ((extension (get-image-extension-for-onechip-instrument inst fits-file)))
    (err-if-not-image-at-extension inst fits-file "get-gain" extension)
    (if (instrument-gain-keyword inst)
	(cf:read-fits-header fits-file (instrument-gain-keyword inst)
			     :extension extension)
	(error "GAIN keyword undefined for instrument ~A" inst)))) 

;; method defined for multichip only when opened to a particular fits extension
(defmethod get-gain-for-instrument ((inst multichip) fits-file
				    &key extension)
  (%require-extension "get-gain-for-instrument")
  (err-if-not-image-at-extension inst fits-file "get-gain" extension)
  (if (instrument-gain-keyword inst)
      (cf:read-fits-header fits-file (instrument-gain-keyword inst)
			   :extension extension)
      (error "GAIN keyword ~A undefined for instrument ~A."
	     (instrument-gain-keyword inst)
	     inst)))

;; get naxis1,naxis2 but allow for possibility that this
;; is a compressed image with no such headers, so use-value
;; fits file size.  If EXTENSION is NIL then current
;; extension is used in case of FITS-FILE=FF (cf:fits-file struct)
(defun %get-naxis1-naxis2 (fits-file &key extension)
  (flet ((%gn1n2-internal (ff)
	   ;; internal function that will always move to extension
	   ;; if specified
	   (cf:with-fits-extension (ff extension) ;; does nothing for NIL extension
	     (when (and (eq (cf:fits-file-current-hdu-type ff) :image)
			(eql (cf:fits-file-current-image-ndims ff) 2))
	       (let ((naxis1 (cf:read-fits-header fits-file "NAXIS1"))
		     (naxis2 (cf:read-fits-header fits-file "NAXIS2")))
		 ;; we COULD just rely on CURRENT-IMAGE-SIZE
		 (cond ((and naxis1 naxis2)
			(values naxis1 naxis2))
		       (t ;; must be commpressed?
			(values
			 (aref (cf:fits-file-current-image-size ff) 0)
			 (aref (cf:fits-file-current-image-size ff) 1)))))))))
    ;; if we just did 'maybe-with-open-fits-file' then extension would be reset
    ;; to first one, which we don't want for the case of a multpichip
    (cond ((typep fits-file 'cfitsio:fits-file)
	   (%gn1n2-internal fits-file)) ;; don't change extension unless EXTENSION  is set
	  (t
	   (cf:with-open-fits-file (fits-file ff)
	     (%gn1n2-internal ff))))))
	  

(defmethod test-if-image-at-extension-for-instrument
    ((inst imaging-instrument) fits-file &key  extension)
  (when (typep inst 'multichip)
    (%require-extension "test-if-image-at-extension-for-instrument"))
  (not (not (%get-naxis1-naxis2 fits-file :extension extension))))

      

(defmethod get-badpix-function-for-instrument 
    ((inst imaging-instrument) fits-file &key  extension)
  (declare (ignore inst fits-file extension))
  ;; by default, just say that all pixels are good
  (lambda (ix iy)
    (declare (type (unsigned-byte 20) ix iy)
	     (ignore ix iy)
	     (optimize speed))
    nil))
  
  
  
(defmethod write-gain-for-instrument ((inst onechip) fits-file gain
				      &key extension)
  (err-if-not-image-at-extension inst fits-file "write-gain" extension)
  (if (instrument-gain-keyword inst)
      (cf:write-fits-header fits-file (instrument-gain-keyword inst) gain
			    :extension extension)
      (error "GAIN keyword undefined for instrument ~A" inst)))

(defmethod write-gain-for-instrument ((inst multichip) fits-file gain
				      &key extension)
  (%require-extension "write-gain-for-instrument")
  (err-if-not-image-at-extension inst fits-file "write-gain" extension)
  (if (instrument-gain-keyword inst)
      (cf:write-fits-header fits-file (instrument-gain-keyword inst) gain
			    :extension extension)
      (error "GAIN keyword undefined for instrument ~A" inst)))

(defmethod write-gain-for-instrument ((inst onechip) fits-file gain
				      &key extension)
  (declare (ignore extension))
  (let ((extension (get-image-extension-for-onechip-instrument inst fits-file)))
    (err-if-not-image-at-extension inst fits-file "write-gain" extension)
    (if (instrument-gain-keyword inst)
	(cf:write-fits-header fits-file (instrument-gain-keyword inst) gain
			      :extension extension)
	(error "GAIN keyword undefined for instrument ~A" inst))))




;; override gain with standard header
(defmethod get-gain-for-instrument :around ((inst multichip) fits-file
					    &key extension)
  (%require-extension "get-gain-for-instrument :before method")
  (err-if-not-image-at-extension inst fits-file "get-gain" extension)
  (or (get-standard-header fits-file :gain :extension extension)
      (call-next-method inst fits-file :extension extension)))

(defmethod get-gain-for-instrument :around ((inst onechip) fits-file
					    &key extension)
  (declare (ignore extension))
  (let ((extension (get-image-extension-for-onechip-instrument inst fits-file)))
    (err-if-not-image-at-extension inst fits-file "get-gain" extension)
    (or (get-standard-header fits-file :gain :extension extension)
	(call-next-method inst fits-file :extension extension))))


  

(defmethod get-exptime-for-instrument ((inst instrument) fits-file)
  (if (instrument-exptime-keyword inst)
      (cf:read-fits-header fits-file (instrument-exptime-keyword inst)
			   :extension (get-info-extension inst))
      (error "EXPTIME undefined for instrument ~A" inst)))

(defmethod get-exptime-for-instrument :around ((inst instrument) fits-file)
  (or (get-standard-header fits-file :exptime :extension 1) 
      (call-next-method)))


;;

(defmethod get-mjd-mid-for-instrument ((inst instrument) fits-file)
  (+ (get-mjd-start-for-instrument inst fits-file)
     (* 0.5d0 (/ (get-exptime-for-instrument inst fits-file) #.(* 24d0 3600)))))

(defmethod get-mjd-mid-for-instrument :around ((inst instrument) fits-file)
  (or (get-standard-header fits-file :mjd-mid)
      (call-next-method)))

;;

(defmethod get-chip-id-for-instrument ((inst onechip) fits-file
				       &key extension)
  (declare (ignore extension))
  (or (cf:read-fits-header fits-file "EXTNAME")
      1)) ;; call it chip 1 if not named




(defmethod get-chip-id-for-instrument ((inst multichip) fits-file
				       &key extension)
  (%require-extension "get-chip-id-for-instrument")
  (err-if-not-image-at-extension inst fits-file "get-chip-id" extension)
  (or (cf:read-fits-header fits-file "EXTNAME" :extension extension)
      (error "Cannot get EXTNAME for extension ~A of ~A"
	     (or extension ;; if we gave it
		 (if (cf:fits-file-p fits-file) ;; if using current HDU
		     (cf:fits-file-current-hdu-num fits-file))
		 ;; we must be using extension 1 if fits-file not of
		 ;; type cf:fits-file
		 1)
	     fits-file)))
		 


(defmethod get-datasec-for-instrument ((inst multichip)
				       fits-file &key extension)
  (%require-extension "get-dataset-for-instrument")
  (err-if-not-image-at-extension inst fits-file "get-datasec" extension)
  (let ((datasec-header (cf:read-fits-header fits-file
					     "DATASEC"
					     :extension extension)))
    (if datasec-header
	(cf:parse-image-section-string datasec-header)
	;; else just make the whole image the DATASEC
	(multiple-value-bind (naxis1 naxis2)
	    (%get-naxis1-naxis2 fits-file :extension extension)
	  (vector 1 naxis1 1 naxis2)))))

;; for onechip, make the extension be the right one
(defmethod get-datasec-for-instrument ((inst onechip) fits-file
				       &key extension)
  (declare (ignore extension))
  (let ((extension (get-image-extension-for-onechip-instrument inst fits-file)))
    (let ((datasec-header (cf:read-fits-header fits-file "DATASEC")))
      (if datasec-header
	  (cf:parse-image-section-string datasec-header)
	  ;; else just make the whole image the DATASEC
	  (multiple-value-bind (naxis1 naxis2)
	      (%get-naxis1-naxis2 fits-file :extension extension)
	    (vector 1 naxis1 1 naxis2))))))


;; if imagesec is non-null, parse it into a vector, otherwise return NIL
(defun %maybe-parse-imagesec (imagesec)
  (when imagesec
    (cf:parse-image-section-string imagesec)))

(defmethod get-datasec-for-instrument :around ((inst multichip) fits-file
					       &key extension)
  (%require-extension "get-datasec-for-instrument :around method")
  (or (%maybe-parse-imagesec
       (get-standard-header fits-file :datasec :extension extension))
      (call-next-method)))

(defmethod get-datasec-for-instrument :around ((inst onechip) fits-file
					       &key extension)
  (declare (ignore extension))
  (let ((extension (get-image-extension-for-onechip-instrument inst fits-file)))
    (or (%maybe-parse-imagesec
	 (get-standard-header fits-file :datasec :extension extension))
	(call-next-method))))

;;;;;;;;;;;;;;;; by default, trimsec is datasec

(defmethod get-trimsec-for-instrument ((inst multichip)
				       fits-file &key extension)
  (%require-extension "get-trimsec-for-instrument")
  (err-if-not-image-at-extension inst fits-file "get-trimsec" extension)
  (get-datasec-for-instrument inst fits-file :extension extension))


;; for onechip, make the extension be the right one
(defmethod get-trimsec-for-instrument ((inst onechip) fits-file
				       &key extension)
  (declare (ignore extension))
  (get-datasec-for-instrument inst fits-file
			      :extension (get-image-extension-for-onechip-instrument
					  inst fits-file)))


(defmethod get-trimsec-for-instrument :around ((inst multichip) fits-file
					       &key extension)
  (%require-extension "get-trimsec-for-instrument :around method")
  (or (%maybe-parse-imagesec
       (get-standard-header fits-file :trimsec :extension extension))
      (call-next-method)))

(defmethod get-trimsec-for-instrument :around ((inst onechip) fits-file
					       &key extension)
  (or
   (%maybe-parse-imagesec
    (get-standard-header fits-file :trimsec :extension extension))
   (call-next-method)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; by default, the overscans are the part of image outside datasec
;; if the instrument supports overscans - processed images should
;; have a dataset that is the whole image, hence no overscans.
(defmethod get-overscans-for-instrument ((inst multichip)
					 fits-file &key extension)
  (%require-extension "get-overscans-for-instrument")
  (when (has-overscan inst)
    (cf:maybe-with-open-fits-file (fits-file ff)
      (err-if-not-image-at-extension inst ff "get-trimsec" extension)
      (%get-overscans-from-datasec inst ff extension))))

(defmethod get-overscans-for-instrument ((inst onechip)
					 fits-file &key extension)
  (declare (ignorable extension))
  (when (has-overscan inst)
    (let ((extension (get-image-extension-for-onechip-instrument inst fits-file)))
      (cf:maybe-with-open-fits-file (fits-file ff)
	(err-if-not-image-at-extension inst ff "get-trimsec" extension)
	(%get-overscans-from-datasec inst ff extension)))))

(defun %get-overscans-from-datasec (inst ff extension)
  (cf:with-fits-extension (ff extension)
    (let* ((datasec (get-datasec-for-instrument
		     inst ff :extension extension))
	   (nx1 (aref datasec 0))
	   (nx2 (aref datasec 1))
	   (ny1 (aref datasec 2))
	   (ny2 (aref datasec 3))
	   (naxis1 (aref (cf:fits-file-current-image-size ff) 0))
	   (naxis2 (aref (cf:fits-file-current-image-size ff) 1))
	   (overscans nil))
      ;; left overscan
      (when (> nx1 1)
	(push (vector 1 (1- nx1) 1 naxis2)
	      overscans))
      ;; right overscan
      (when (< nx2 naxis1)
	(push (vector (1+ nx2) naxis1 1 naxis2)
	      overscans))
      ;; top overscan, minus top corners from x overscans
      (when (> ny1 1)
	(push (vector nx1 nx2 1 (1- ny1))
	      overscans))
      ;; bottom overscan, minus bottom corners from x overscans
      (when (< ny2 naxis2)
	(push (vector nx1 nx2 (1+ ny2) naxis2)
	      overscans))
      ;;
      (reverse overscans))))
      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; note that STATSEC (statistics section) is in the original, untrimmed image

(defmethod get-statsec-for-instrument  ((inst multichip)
					fits-file &key extension)
  (%require-extension "get-statsec-for-instrument")
  (err-if-not-image-at-extension inst fits-file "get-statsec" extension)
  (get-trimsec-for-instrument inst fits-file :extension extension))

(defmethod get-statsec-for-instrument ((inst onechip) fits-file
				       &key extension)
  (declare (ignore extension))
  (get-trimsec-for-instrument
   inst fits-file
   :extension
   (get-image-extension-for-onechip-instrument inst fits-file)))



(defmethod get-statsec-for-instrument :around ((inst multichip) fits-file
					       &key extension)
  (%require-extension "get-statsec-for-instrument :around method")
  (or (%maybe-parse-imagesec
       (get-standard-header fits-file :statsec :extension extension))
      (call-next-method)))

(defmethod get-statsec-for-instrument :around ((inst onechip) fits-file
					       &key extension)
  (or
   (%maybe-parse-imagesec
    (get-standard-header fits-file :statsec :extension extension))
   (call-next-method)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod get-initial-wcs-for-instrument ((inst %anychips) fits-file
					   &key extension)
  (when (typep inst 'multichip)
    (%require-extension "get-initial-wcs-for-instrument"))
  (err-if-not-image-at-extension inst fits-file "get-initial-wcs" extension)
  (cf:read-wcs fits-file :extension extension))

(defmethod get-initial-wcs-for-instrument ((inst onechip) fits-file
					   &key extension)
  (declare (ignore extension))
  (call-next-method inst fits-file :extension
		    (get-image-extension-for-onechip-instrument
		     inst fits-file)))




;; by default, 
(defmethod insert-initial-wcs-for-instrument
    ((inst %anychips) fits-file
     &key extension)
  (when (typep inst 'multichip)
      (%require-extension "insert-initial-wcs-for-instrument"))
  (err-if-not-image-at-extension inst fits-file "insert-wcs" extension)
  (when (not (cf:read-wcs fits-file :extension extension))
    (let ((wcs-new (instrument-id:get-initial-wcs-for-instrument
		    inst fits-file :extension extension)))
      (if wcs-new
	  (cf:write-wcs wcs-new fits-file :extension extension)
	  (error "ERROR in INSERT-INITIAL-WCS-FOR-INSTRUMENT - no WCS found in ~A extension ~A and GET-INITIAL-WCS-FOR-INSTRUMENT did not return a WCS." fits-file extension))
  t)))


(defmethod insert-initial-wcs-for-instrument ((inst onechip) fits-file
					   &key extension)
  (declare (ignore extension))
  (call-next-method inst fits-file :extension
		    (get-image-extension-for-onechip-instrument
		     inst fits-file)))


(defmethod  get-pixel-scale-for-instrument ((inst %anychips) fits-file
					    &key extension)
  (when (typep inst 'multichip)
    (%require-extension "get-pixel-scale-for-instrument"))
  (err-if-not-image-at-extension inst fits-file "get-pixel-scale" extension)
  (let ((wcs
	  (or (ignore-errors ;; some instruments have bogus wcs
	       (cf:read-wcs fits-file :extension extension))
	      (get-initial-wcs-for-instrument inst fits-file)
	      (error "Cannot find WCS for instrument."))))
    (wcs:get-pixel-scale-for-wcs wcs)))


(defmethod get-pixel-scale-for-instrument ((inst onechip) fits-file
					   &key extension)
  (declare (ignore extension))
  (call-next-method inst fits-file :extension
		    (get-image-extension-for-onechip-instrument
		     inst fits-file)))

(defmethod get-mjd-start-for-instrument :around ((inst instrument) fits-file)
  (or (get-standard-header fits-file :mjd-start)
      (call-next-method)))


(defmethod get-standard-filter-for-instrument :around ((inst instrument)
						       fits-file)
  (or
   ;; checks validity and converts to keyword automatically
   (get-standard-header fits-file :filter) 
   (call-next-method)))

 
(defmethod get-chip-id-for-instrument :around ((inst instrument) fits-file
					       &key extension)
  (when (typep inst 'multichip)
    (%require-extension "get-chip-id-for-instrument"))
  (or (get-standard-header fits-file :chip-id :extension extension)
      (call-next-method)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; these will probably never have to be specialized
(defmethod get-bright-flux-limit-for-instrument ((inst imaging-instrument)
						 fits-file)
  (declare (ignorable fits-file))
  (min (saturation-level inst)
       (non-linear-level inst)))

(defmethod get-observatory-for-instrument ((inst instrument) fits-file)
  (instrument-observatory inst))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; a list of function-symbols for functions (func fits-file) that
;; either return "INSTRUMENT-NAME" or NIL
(defvar *instrument-id-funcs* nil)

(defun %add-instrument-id-function (func-sym)
  (pushnew func-sym *instrument-id-funcs*))


(defgeneric identify-instrument (fits-file)
  (:documentation "Identify the instrument in FITS-FILE.  If this is
a string (filename) then identify the primary extension. If this
is a CFITSIO:FITS-FILE then identify the current extension."))



(defun fits-file-is-fzipped (fits-file &key (on-mixed :error))
  "Return T if images in FITS are compressed.   Examines only
image extensions with NAXIS>0

ON-MIXED is what to do if some images are compressed and some are
not:
   - :ERROR means throw an error
   - T means return T, meaning fzipped
   - NIL means NIL, meaning non fzipped

Returns (VALUES T/NIL N-FZIPPED N-NOT-FZIPPED)"
  (cf:maybe-with-open-fits-file (fits-file ff)
    (loop with num-fzipped = 0
	  with num-not-fzipped = 0
	  for ihdu from 1 to (cf:fits-file-num-hdus ff)
	  do (cf:move-to-extension ff ihdu)
	     (let* ((naxis (cf:read-fits-header ff "NAXIS"))
		    (compressed (if naxis
				    (equalp
				     (cf:read-fits-header ff "TTYPE1")
				     "COMPRESSED_DATA"))))
	       (cond ((or (not naxis)      ;; not an image, ignore
			  (eql naxis 0)    ;; a primary header, ignore
			  (not (integerp naxis))) ;; this would be weird
		      nil) ;; not an image
		     (compressed
		      (incf num-fzipped))
		     (t
		      (incf num-not-fzipped))))
	  finally
	     (let ((mixed (and (plusp num-fzipped) (plusp num-not-fzipped))))
	       (when (and (eq on-mixed :error) mixed)
		 (error "Fits file has mixed fzipped and non-fzipped images, and throw-error-on-mixed is set."))
	     (return
	       (values
		(if on-mixed ;; one zipped means whole file is zipped
		    (plusp num-fzipped) ;; one non-zipped means while file is non-zipped
		    (plusp num-not-fzipped))))))))
		
		
		    
		 
		      
    
#+nil
(defun fits-filename-is-fzipped (filename)
  (string-utils:string-ends-with filename ".fz"))
#+nil
(defun fits-file-is-fzipped (fits-file)
  (cond ((stringp fits-file)
	 (fits-filename-is-fzipped fits-file))
	((pathnamep fits-file)
	 (fits-filename-is-fzipped (namestring fits-file)))
	((typep fits-file 'cf:fits-file)
	 (fits-filename-is-fzipped (cf:fits-file-filename fits-file)))
	(t
	 (error "Invalid type ~A in fits-file-is-fzipped - must be string or fits-file or path"  (type-of fits-file)))))

(defmethod identify-instrument ((ff cf:fits-file))
  (let ((ext0 (cf:fits-file-current-hdu-num ff)))
    (unwind-protect ;; reset the original HDU
	 ;; if compressed, the primary extension is empty
	 (let ((extension
		 (if (and (fits-file-is-fzipped ff)
			  ;; ATLAS can be compressed but 1 extension
			  (> (cf:fits-file-num-hdus ff) 1))
		     2 1)))
	   (cf:with-fits-extension (ff extension)
	     (loop 
	       for func in *instrument-id-funcs*
	       for inst = (progn
			    ;; always reset extension in case FUNC
			    ;; changes it 
			    (cf:move-to-extension ff 1)
			    (funcall func ff))
	       when inst
		 collect inst into instr-list
	       finally 
		  (when (> (length instr-list) 1)
		    (error "Fits file ~A matches more than one instrument: ~A" 
			   (cf:fits-file-filename ff)
			   instr-list))
		  (return (car instr-list)))))
      (progn ;; reset the HDU
	(cf:move-to-extension ff ext0)))))

(defmethod identify-instrument ((fits-file string))
  (cf:with-open-fits-file (fits-file ff)
    (identify-instrument ff)))
	
  



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; validation function to see if functions work on a particular fits file
(defun validate-fits-file (fits-file &key (extension :first-image)
				       (output :print)
				       (require-wcs t))
  "Validate whether the functions of instrument-id package work on a fits image.
OUTPUT can be :PRINT to print to console, or :LIST to generate a list of failures.

EXTENSION is :FIRST-IMAGE by default."
  (declare (type (member :print :list) output))
  (let* ((error-list nil) ;; outputs if OUTPUT is LIST
	 (inst nil)
	 %extension)

    (cf:with-open-fits-file (fits-file ff)

      (setf inst (when ff
		   (or (identify-instrument fits-file)
		       (progn (if (eq output :print)
				  (format t "Failed to identify instrument for ~A"
					  fits-file)
				  (push :failed-to-identify error-list))
			      nil))))
    
      (when inst
	(when (eq output :print) (format t "Identified instrument ~A~%" inst))


      ;; for special EXTENSION=:FIRST-IMAGE value set to the ONCHIP default or 1 
      (when (eq extension :first-image)
	(cond ((typep inst  'onechip)
	       (setf %extension (get-image-extension-for-onechip-instrument
				 inst fits-file)))
	      (t ;; multipchip
	       (loop for i from 1 to (cf:fits-file-num-hdus ff)
		     do (cf:move-to-extension ff i)
			(when (and (eq :image (cf:fits-file-current-hdu-type ff))
				   (= 2 (cf:fits-file-current-image-ndims ff)))
			  (setf %extension i)
			  (return))))))

      ;;(format t "Set EXTENSION=~A~%" %extension) (force-output)
      
	(macrolet ((do-test (command-form thing)
		     `(let ((%result (ignore-errors (progn ,command-form))))
			(if (eq output :print)
			    (if %result
				(format t "success accessing ~A~%" ,thing)
				(format t "ERROR attempting to access ~A~%" ,thing))
			    (if (not %result)
				(push (intern ,thing :keyword) error-list))))))
	
	  (when (test-if-image-at-extension-for-fits ff :extension %extension)
	    (when (eq output :print)
	      (format t "There is an image extension here; running image functions~%~%"))
	    (when require-wcs
	      (do-test
		  (get-initial-wcs-for-fits ff :extension %extension) "WCS"))
	    (do-test
		(get-gain-for-fits ff :extension %extension) "GAIN")
	    (do-test
		(get-chip-id-for-fits ff :extension %extension) "CHIP-ID")
	    (do-test
		(get-trimsec-for-fits ff :extension %extension) "TRIMSEC")
	    (do-test
		(get-datasec-for-fits ff :extension %extension) "DATASEC")
	    (do-test
		(get-statsec-for-fits ff :extension %extension) "STATSEC")
	    (do-test
		(get-pixel-scale-for-fits ff :extension %extension) "PIXEL-SCALE"))
	
	  ;; the following tests are valid for all types of inst
	  (do-test 
	      (get-standard-filter-for-fits ff) "FILTER")
	  (do-test
	      (get-exptime-for-fits ff) "EXPTIME")
	  (do-test
	      (get-observatory-for-fits ff) "OBSERVATORY")
	  (do-test
	      (get-object-type-for-fits ff) "IMAGE-TYPE")
	  (do-test
	      (get-object-for-fits ff) "OBJECT-NAME")
	  (do-test
	      (get-mjd-mid-for-fits ff) "MJD-MID")
	  (do-test
	      (get-mjd-start-for-fits ff) "MJD-START")))
    
      ;;
      (if (eq output :list) (values inst error-list)))))
      
      
      
      
      
      
      
	

