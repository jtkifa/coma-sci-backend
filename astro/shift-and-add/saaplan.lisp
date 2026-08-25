(in-package shift-and-add)


;; structure describing the plan for this set of operations
(defclass saaplan ()
  ((imageout-base
    :initform "stack_image"
    :initarg :imageout-base
    :accessor saaplan-imageout-base)
   (output-directory
    :initform "./"
    :initarg :output-directory
    :accessor saaplan-output-directory)
   (logger ;; a LOGGER:LOGGER structure
    :initform t ;; standard output/error according to LOGGER package
    :initarg :logger
    :accessor saaplan-logger)
   (verbose-type 
    :initform "QUIET" ;; terapix verbosity for sextractor,scamp, swarp
    :initarg :verbose-type
    :accessor saaplan-verbose-type)
   ;;
   ;; this fits keyword having a non-nil value indicates a bad fits image
   ;; to be ignored
   (bad-fits-keyword
    :initform "BADIMG"
    :initarg :bad-fits-keyword
    :accessor saaplan-bad-fits-keyword)
   ;;
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; clean up some intermediate files
   (cleanup
    :initform t
    :initarg :cleanup
    :accessor saaplan-cleanup)
   ;;
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; a limit on the max distance (arcsec) between the center of the
   ;; first file and the other files, to avoid huge stacks that fill
   ;; the disk or memory
   (max-sky-spread
    :initform 1800
    :initarg :max-sky-spread
    :accessor saaplan-max-sky-spread)
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   (swarp-combine-type
    :initform :median
    :initarg :swarp-combine-type
    :accessor saaplan-swarp-combine-type)
   (swarp-resampling-method
    :initform "LANCZOS3"
    :initarg :swarp-resampling-type
    :accessor saaplan-swarp-resampling-type)
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; a locator object that returns the ra,dec of an object in a fits file
   ;; see object-locators.lisp
   (locator
    :initform nil 
    :initarg :locator
    :accessor saaplan-locator)
   ;;
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; the following revert to INSTRUMENT-ID package values if not defined
   (object-keyword  ;; fits keyword that denotes unique object name
    :initform nil 
    :initarg :object-keyword
    :accessor saaplan-object-keyword)
   (object-name  ;; the name of the object
    :initform nil 
    :initarg :object-name
    :accessor saaplan-object-name)
   (mjd-keyword
    :initform nil
    :initarg :mjd-keyword
    :accessor saaplan-mjd-keyword)
   ;; observatory parameters
   (observatory ;; as in slalib-ephem::observatory, or a name
    :initform nil
    :initarg :observatory
    :accessor saaplan-observatory)
   ;;
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; pre-processes the image - if not NIL, original
   ;; images are first copied before running it.
   ;; Inovoked as as
   ;; (run-image-preproc image-preproc saaplan fits-list) => new-fits-list
   (image-preproc ;; object of type image-preproc
    :initform nil
    :initarg :image-preproc
    :accessor saaplan-image-preproc)
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; function to make weight images (eg, badpix masks)
   ;; takes argument as (run-weight-generation image-weighter saaplan image-list)
   ;; currently, the only one uses INSTRUMENT-ID:GET-BADPIX-FUNCTION
   (image-weighter
    :initform nil
    :initarg :image-weighter
    :accessor saaplan-image-weighter)
   ;;
   ))
   

(defparameter *default-saaplan* (make-instance 'saaplan))

(defun saaplan-log-format (saaplan &rest args)
  (logger:writelog (saaplan-logger saaplan)
		   (apply #'format nil args)
		   :log-level 5
		   :log-type :log))
