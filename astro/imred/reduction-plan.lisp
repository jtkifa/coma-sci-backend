

(in-package imred)



;; structure describing instrument setup and what to run
(defclass reduction-plan ()
  ((target-dir  ;; output directory for reduced objects
    :initarg :target-dir 
    :initform "./" 
    :accessor reduction-plan-target-dir)
   ;; if not NIL, calib-dir is the directory to use for biases
   ;; and flats
   (calib-dir 
    :initarg :calib-dir  
    :initform nil
    :accessor reduction-plan-calib-dir)
   (logger ;; logger as defined in jlib/logger
    :initarg :logger
    :initform nil ;; no logging
    :accessor reduction-plan-logger)
   (reduced-suffix
    :initarg :reduced-suffix ;; foo.fits --> foor.fits
    :initform "r"
    :accessor reduction-plan-reduced-suffix)
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;e;;;;;;;;;;;;;;;;;;;;;;;;
   ;; to limit the images to one class of INSTRUMENT-ID types,
   ;; set this
   (inst-id-type
    :initarg :inst-id-type
    :initform nil
    :accessor reduction-plan-inst-id-type)
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; to set invalid pixels when we read raw pixels
   (saturation-value ;; in ADU in raw frame
    :initarg :saturation-value
    :initform 65535 ;; generally, 16 bit
    :accessor reduction-plan-saturation-value)
   ;; function of single-float that returns T if a pixel is invalid
   (invalid-pixel-function
    :initarg :invalid-pixel-function
    :initform (lambda (x) (declare (type single-float x)) (= x 65535.0))
    :accessor reduction-plan-invalid-pixel-function)
   ;; output value for null (no information) pixels
   (output-null-pixel-value  
    :initarg :output-null-pixel-value
    :initform  (make-single-float-nan)
    :accessor reduction-plan-output-null-pixel-value)
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;
   (delete-intermediate-files 
    :initarg :delete-intermediate-files
    :initform t
    :accessor reduction-plan-delete-intermediate-files)
   ;;
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; default overscan subtraction uses a single median value from
   ;; overscan regions - anything fancier should be in per-instrument
   ;; preproc step
   (overscan-subtract
    :initarg :overscan-subtract
    :initform nil  ;; generally don't overscan subtract
    :accessor reduction-plan-overscan-subtract)
   ;; NIL, list of vectors, or function of EXTDESC
   ;; generally, should use instrument-id, so use NIL
   (overscans ;; LIST of sections, not just one
    :initarg :overscans
    :initform nil ;; generally, use INSTRUMENT-ID
    :accessor reduction-plan-overscans)
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; TRIM parameters
   ;;
   ;; if TRIM is T, do trimming.  If TRIMSEC is NIL, try to use
   ;; image's ccdsec and biassec to determine trim region
   ;; otherwise, if TRIMSEC is a vector, use this.  Otherwise,
   ;; if TRIMSEC is funcallable, call with the current EXTDESC
   (trim 
    :initarg :trim 
    :initform t
    :accessor reduction-plan-trim)
   ;; NIL, vector, or function of EXTDESC - resolved in func
   ;; trim-image - generally, should use INSTRUMENT-ID, so use NIL
   (trimsec  
    :initarg :trimsec
    :initform NIL ;; trust instrument ID to compute it by default
    :accessor reduction-plan-trimsec)
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; BIAS parameters
   ;;
   (make-zero 
    :initarg :make-zero 
    :initform t
    :accessor reduction-plan-make-zero)
   (max-zero-frames ;; no more than this many bias frames
    :initarg max-zero-frames
    :initform 15
    :accessor reduction-plan-max-zero-frames)
   (recreate-zero 
    :initarg :recreate-zero
    :initform nil
    :accessor reduction-plan-recreate-zero)
   ;; the maximum deviation of a particular input bias' median flux
   ;; relative to median of all input biases, for discarding bad biases
   (max-zero-fractional-deviation
    :initarg :max-zero-fractional-deviation
    :initform 0.03
    :accessor reduction-plan-max-zero-fractional-deviation)
   (zero-basename 
    :initarg :zero-basename
    :initform "BIAS"
    :accessor reduction-plan-zero-basename)
   ;; (funcall bias-group-func fits) gives the bias name - eg
   ;; when there's more than one chip to an instrument flavor
   ;; we could have BIAS-CHIPn.fits
   (bias-group-func
    :initarg :bias-group-func
    :initform (lambda (fits) (declare (ignore fits)) "")
    :accessor reduction-plan-bias-group-func)
   ;;
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; FLAT parameters
   ;;
   (make-flats 
    :initarg :make-flats 
    :initform t
    :accessor reduction-plan-make-flats)
   (min-flat-frames ;; no fewer than this flat frames per filter
    :initarg :min-flat-frames
    :initform 3
    :accessor reduction-plan-min-flat-frames)
   (max-flat-frames ;; no more than this many flat frames per filter
    :initarg :max-flat-frames
    :initform 15
    :accessor reduction-plan-max-flat-frames)
   (recreate-flats 
    :initarg :recreate-flats
    :initform nil
    :accessor reduction-plan-recreate-flats)
   (min-flat-counts  ;; in ADU, not in gain-normalized photons
    :initarg :min-flat-counts
    :initform 5000
    :accessor reduction-plan-min-flat-counts)
   (max-flat-counts  ;; in ADU, not in gain-normalized photons
    :initarg :max-flat-counts
    :initform 55000
    :accessor reduction-plan-max-flat-counts)
   ;; parameters for object (dark sky) flats
   (min-objflat-counts  ;; in ADU, not in gain-normalized photons
    :initarg :min-objflat-counts
    :initform 1000 ;; make this lower
    :accessor reduction-plan-min-objflat-counts)
   (max-objflat-counts  ;; in ADU, not in gain-normalized photons
    :initarg :max-objflat-counts
    :initform 30000
    :accessor reduction-plan-max-objflat-counts)
   (flat-basename 
    :initarg :flat-basename
    :initform "FLAT"
    :accessor reduction-plan-flat-basename)
   ;; (funcall flat-group-func fits) gives the flat-group-name 
   (flat-group-func
    :initarg :flat-group-func
    :initform '%safe-get-filter-for-fits
    :accessor reduction-plan-flat-group-func)
   ;; allow object images throughout night instead of dedicated flats
   (allow-object-images-for-flats 
    :initarg :allow-object-image-flats
    :initform t
    :accessor reduction-plan-allow-object-images-for-flats)
    ;; mandate object image flats even if normal flats possible
   (mandate-object-images-for-flats 
    :initarg :mandate-object-image-flats
    :initform nil
    :accessor reduction-plan-mandate-object-images-for-flats)
   ;; max number of darksky (object) flat input images to use, to avoid using an entire night's worth
   (maximum-number-objflat-images
    :initarg :maximum-number-objflat-images
    :initform 51 ;; uses a lot of memory, but sometimes needed for good quality.
                 ;; If this is a problem, will need to introduce chunking
    :accessor reduction-plan-maximum-number-objflat-images)
   ;; any values in final flat outside this range are set to
   ;; output-null-pixel-value
   (min-final-flat-value
    :initarg :min-final-flat-value
    :initform 0.01
    :accessor reduction-plan-min-final-flat-value)
   (max-final-flat-value
    :initarg :max-final-flat-value
    :initform 100.0
    :accessor reduction-plan-max-final-flat-value)
   ;;
   ;;
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; FRINGE parameters
   (make-fringes 
    :initarg :make-fringes 
    :initform t
    :accessor reduction-plan-make-fringes)
   (max-fringe-frames ;; no more than this many fringe frames per filter
    :initarg :max-fringe-frames
    :initform 20
    :accessor reduction-plan-max-fringe-frames)
   ;; if we're making fringes, then fringes with fringeset-group EQUALP
   ;; to the STRING of these get made
   (desired-fringe-groups
    :initarg :desired-fringe-groups
    :initform '(:isdss :zsdss :ic :y :ips1 :zps1 :wps1)
    :accessor reduction-plan-desired-fringe-groups)
   (defringed-suffix
    :initarg :defringed-suffix ;; foor.fits --> foor_frcor.fits
    :initform "_frcor"
    :accessor reduction-plan-defringed-suffix)
   ;; List of which values of FRINGE-GROUP-FUNC (see below) to make
   ;; fringe files for.  These would typically be filter names for this
   ;; instrument.
   (fringe-groups
    :initarg :fringe-groups
    :initform nil
    :accessor reduction-plan-fringe-groups)
   (recreate-fringes 
    :initarg :recreate-fringes
    :initform nil
    :accessor reduction-plan-recreate-fringes)
   (fringe-name 
    :initarg :fringe-name
    :initform "FRINGEFIELD" ;; filename indicating output fringe
    :accessor reduction-plan-fringe-name)
   (min-fringe-counts  ;; in ADU
    :initarg :min-fringe-counts
    :initform 1000
    :accessor reduction-plan-min-fringe-counts)
   (max-fringe-counts  ;; in ADU
    :initarg :max-fringe-counts
    :initform 55000
    :accessor reduction-plan-max-fringe-counts)
   ;;
   ;; parameters for cleaning input fringe frames to get rid of stars
   (fringe-prefilter-input-images
    :initarg :fringe-prefilter-input-images
    :initform t
    :accessor reduction-plan-fringe-prefilter-input-images)
   (fringe-filter-r1 ;; interior ring-median radius, pixels
    :initarg  :fringe-filter-r1
    :initform 15.0
    :accessor reduction-plan-fringe-filter-r1)
   (fringe-filter-r2 ;; exterior ring-median radius, pixels
    :initarg  :fringe-filter-r2
    :initform 17.0
    :accessor reduction-plan-fringe-filter-r2)
   (fringe-filter-max-dev ;; maximum allowed deviation of img over ring median
    :initarg :fringe-filter-max-dev
    :initform 0.20 ;; fringes can be DEEP
    :accessor reduction-plan-fringe-filter-max-dev)
   ;; 
   (fringe-basename 
    :initarg :fringe-basename
    :initform "FRINGE"
    :accessor reduction-plan-fringe-basename)
   (fringe-group-func ;; the function that gets a fits-file's fringe-group
    :initarg :fringe-group-func
    :initform '%safe-get-filter-for-fits
    :accessor reduction-plan-fringe-group-func)
   ;; any values in final fringe outside this range are set to
   ;; output-null-pixel-value
   (min-final-fringe-value
    :initarg :min-final-fringe-value
    :initform -1e4
    :accessor reduction-plan-min-final-fringe-value)
   (max-final-fringe-value
    :initarg :max-final-fringe-value
    :initform 1e4
    :accessor reduction-plan-max-final-fringe-value)
   ;;
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; OBJECT parameters
   ;;
   (ccdproc-objects 
    :initarg :ccdproc-objects
    :initform t
    :accessor reduction-plan-ccdproc-objects)
   (recreate-objects 
    :initarg :recreate-objects
    :initform nil
    :accessor reduction-plan-recreate-objects)
   ;;
   (gain-normalize ;; set GAIN=1 in final objects
    :initarg :gain-normalize
    :initform t
    :accessor reduction-plan-gain-normalize)
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; initial patch function that is run on input files to fill in crucial missing
   ;; headers
   (input-fits-patch-function
    :initarg :input-fits-patch-function
    :initform nil
    :accessor reduction-plan-input-fits-patch-function)
   ;; final patch function (func input-fits) that 
   ;; is run on the output file to make it nice (eg, put in mjd)
   (output-fits-patch-function
    :initarg :output-fits-patch-function
    :initform nil
    :accessor reduction-plan-output-fits-patch-function)
   ))


;; this is just for debugging - things like keywords won't be right
(defparameter *default-reduction-plan* (make-instance 'reduction-plan))

;; get filter, but accept no-filter for BIAS frames
(defun %safe-get-filter-for-fits (fits)
  (format nil
	  "~A"
	  (or 
	   (instrument-id:get-standard-filter-for-fits fits)
	   ;; sometimes bias doesn't have a filter, which is OK
	   (if (not (eq (instrument-id:get-object-type-for-fits fits)
			:bias))
	       :unknown)))) ;; try to survive no filter situation

;;	       (error "Could not get filter for ~A" fits)))))


;; helper function for later on
(defun %gethead-or-error (fits header &key (space-trim-strings t))
  (multiple-value-bind (val comment keyword status)
      (cf:read-fits-header fits header)
    (declare (ignorable comment status))
    (when (not keyword)
      (error "Header ~A not found in fits file ~A" header fits))
    ;;
    (when (and space-trim-strings (stringp val))
      (setf val (string-trim '(#\space #\tab) val)))
    ;;
    val))
      

(defstruct flatset
  name ;; flat-name + flat-group, eg "FLAT_rsdss"
  group
  flat-list        ;; the final flat set
  flat-list/flat   ;; possible flats of type FLAT
  flat-list/obj    ;; possible flats type OBJECT
  flat-type        ;; either :FLAT or :OBJECT
  ) ;; the fits files that go into in this group


(defstruct fringeset
  name ;; fring-name + flat-group, eg "FRINGE_isdss"
  group
  fringe-obj-list) ;; the fits files that go into this group


;; flatset name in form safe for filenames, because 
;; filters sometimes have weird names.  Spaces
;; are replaced by underscores and other characters are hexified
(defun fileset-name-filesafe (flatset-name)
  (declare (type string flatset-name))
  (with-output-to-string (s)
      (loop for c across flatset-name 
	    do (cond ((or (alphanumericp c)
			  (member c '(#\- #\_)))
		      (write-char c s))
		     ((char= c #\space)
		      (write-char #\_ s))
		     (t
		      (format s "%~2,'0x" (char-code c)))))))


(defun flatset-name-filesafe (fileset-name)
  (fileset-name-filesafe fileset-name))

(defun fringeset-name-filesafe (fileset-name)
  (fileset-name-filesafe fileset-name))

(defstruct fileset
  bias-list               ;; list of input biases
  flatsets                ;; flatets of input flat frames
  fringesets              ;; fringesets of input file
  object-list             ;; list of input object images
  ;; 
  output-fits-list        ;; all reduced object files in one list
  output-bias-list        ;; produced bias files
  output-flat-list        ;; produced flats
  output-fringe-list      ;; produced fringe frames
  output-reduced-obj-list     ;; produced reduced fits files
  output-fringecorrected-obj-list  ;; produced fringe corrected reduced files
  )

(defun get-flatset-or-create (fileset group flat-name)
  (or (find group (fileset-flatsets fileset) :test 'equal :key 'flatset-group)
      (car
       (push
	(make-flatset :name (format nil "~A~A" flat-name (or group ""))
		      :group group)
	(fileset-flatsets fileset)))))

(defun get-fringeset-or-create (fileset group fringe-name)
  (or (find group (fileset-fringesets fileset) :test 'equal :key 'fringeset-group)
      (car
       (push
	(make-fringeset :name (format nil "~A~A" fringe-name (or group ""))
			:group group)
	(fileset-fringesets fileset)))))
		      
	
(defun %get-type-list-of-fits-files (fits-list)
  (remove-duplicates
   (mapcar 'type-of
	   (mapcar 'instrument-id:identify-instrument fits-list))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-fileset-from-fits-list (fits-list reduction-plan) 
  "Breaks up fits files into biases, objects, and flats by group (filter)
Returns a FILESET"
  
  (declare (type list fits-list)
	   (optimize debug)
	   (type reduction-plan reduction-plan))

  (let ((missing (loop for fits in fits-list 
		    when (not (probe-file fits))
		    collect fits)))
    (when missing 
      (error "Missing fits files: ~{ ~A~}" missing)))

  (let ((type-list (%get-type-list-of-fits-files fits-list)))
    (when (or (member nil type-list)
	      (> (length type-list) 1))
      (imred-log-error 
       "Some images are not identifiable, or more than one image type present."
       :die t)) ; fatal error
    (when (and (reduction-plan-inst-id-type reduction-plan)
	       (not (subtypep (first type-list)
			      (reduction-plan-inst-id-type reduction-plan))))
      (imred-log-error
       (format nil "Images not of type ~A as required by REDUCTION-PLAN"
	       (reduction-plan-inst-id-type reduction-plan))
       :die t)))		 
  
  (when (not (all-images-compatible-p fits-list :reduction-plan reduction-plan))
    (imred-log-error
     (format nil "Images are not all compatible (typically, different size images or different TRIMSEC):  ~{ ~A~}" fits-list)
     :die t))

  (let ((fileset (make-fileset)))
    
    ;; use just the first header and pray
    (loop 
       for fits in fits-list
       for extdesc-list = (build-extdesc-list-for-fits
			   fits
			   :reduction-plan reduction-plan)
       ;; the ones that are actually reducable
       for extdesc-list-red = (loop for ed in extdesc-list
				 when (extdesc-reduce-p ed)
				 collect ed)
       for extdesc1
	 = (or (first extdesc-list-red)
	       (imred-log-error
		(format
		 nil
		 "make-fileset-from-fits-list: No reducable image extensions in ~A"
		 fits)
		:die t)) ;; fatal
       do
	 ;(print extdesc1)
	 (let* ((obj-type (extdesc-object-type extdesc1))
		(flatgroup (extdesc-flat-group extdesc1))
		(fringegroup (extdesc-fringe-group extdesc1))
		(gain (extdesc-gain extdesc1))
		(is-flat (eq :flat obj-type))
		(is-bias (eq :bias obj-type))
		(is-obj  (eq :object obj-type))
		(flatset
		 (when (and (or is-flat is-obj) flatgroup)
		   (get-flatset-or-create 
		    fileset 
		    flatgroup 
		    (reduction-plan-flat-basename reduction-plan))))
		(fringeset
		  (when (and fringegroup is-obj)
		    (get-fringeset-or-create
		     fileset
		     fringegroup
		     (reduction-plan-fringe-basename reduction-plan)))))
	   ;;
	   (when (and (not gain)
		      (reduction-plan-gain-normalize reduction-plan))
	     (imred-log-error
	      (format nil "Told to normalize gain but can't find gain in ~A"
		      fits)
	      :die t))
	   
	   ;;
	   (when is-bias
	     (push (fullfile fits) (fileset-bias-list fileset)))
	   ;;
	   ;;
	   ;; must have a flatgroup if reduction plan specifies it
	   (when (and is-flat 
		      (not (reduction-plan-mandate-object-images-for-flats
			    reduction-plan))
		      ;; we push this onto the flat list if 1) there's no group
		      ;; distinction or 2) we've found a flat group
		      (or (not (reduction-plan-flat-group-func reduction-plan))
			  flatgroup))
	     (push (fullfile fits) (flatset-flat-list/flat flatset)))
	   
	   ;; if doing objects as flats, then put those in the flatset's flat-list/obj
	   (when (and is-obj
		      (or (reduction-plan-allow-object-images-for-flats reduction-plan)
			  (reduction-plan-mandate-object-images-for-flats reduction-plan))
		      (or (not (reduction-plan-flat-group-func reduction-plan))
			  flatgroup))
	     (push (fullfile fits) (flatset-flat-list/obj flatset)))

	   ;; if doing fringes, put this obect into the fringe group
	   (when (and is-obj fringegroup)
	     (push (fullfile fits) (fringeset-fringe-obj-list fringeset)))

	   ;;
	   (when is-obj
	     (push (fullfile fits) (fileset-object-list fileset)))))


    ;; go through the flatsets and decide whether to use flat or object (darksky) flats
    (loop for flatset in (fileset-flatsets fileset)
	  do
	     (setf (flatset-flat-type flatset) :flat) ;; start with this default
	     (cond
	       ;; if we must use dark sky flats, use the object frames
	       ((reduction-plan-mandate-object-images-for-flats reduction-plan)
		    (setf (flatset-flat-type flatset) :object)
		    (setf (flatset-flat-list flatset)
			  (flatset-flat-list/obj flatset)))
	       ;; if dark sky flats are allowed, then use them if too few flat images
	       ((and (reduction-plan-allow-object-images-for-flats reduction-plan)
		     (< (length (flatset-flat-list/flat flatset))
			(reduction-plan-min-flat-frames reduction-plan)))
		 (setf (flatset-flat-type flatset) :object)
		    (setf (flatset-flat-list flatset)
			  (flatset-flat-list/obj flatset)))
	       ;; otherwise, we're using normal flatss
	       (t
		(setf (flatset-flat-type flatset) :flat)
		(setf (flatset-flat-list flatset)
		      (flatset-flat-list/flat flatset)))))
	       

 
    fileset))
	     
	     
				  

;; function that tries to get a reduction play class from an instrument-id class
(defgeneric get-reduction-plan-for-instrument (inst)
  (:documentation "Given an INST (a symbol for an
  INSTRUMENT-ID:INSTRUMENT, or an INSTRUMENT object, return
  a symbol that can be turned into a reduction plan"))

(defmethod get-reduction-plan-for-instrument ((inst t))
  (error "GET-REDUCTION-PLAN-FOR-INSTRUMENT called with invalid type ~A" (type-of inst)))

;; method when a definition is missing
(defmethod get-reduction-plan-for-instrument ((inst instrument-id:imaging-instrument))
  (error "GET-REDUCTION-PLAN-FOR-INSTRUMENT is not defined for inst of type ~A"
	 (type-of inst)))
  
