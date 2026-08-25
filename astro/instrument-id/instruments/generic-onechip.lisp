
;; template for instrument id of a new instrument.
;; replace all instances of generic-onechip and fix methods appropriately

#|

Definition for a generic one-chip instrument.  The fits file should
have the following headers
 
  INSTRUME  = "GENERIC_ONECHIP"
     or 
  GINSTRUM  = "GENERIC_ONECHIP"  (takes precedence)

  WCS       ... has a valid WCS guess
  OBSERVTR  = "MKO"   (or other code we know about)
  GAIN      = xx 
  EXPTIME   = xx
  FILTER    = U,B,V,R,I,u,g,r,i,z,w
    or
  GFILTER   = U,B,V,R,I,u,g,r,i,z,w    (takes precedence)

  OBJECT    = "objname"
  OBSTYPE   = BIAS, FLAT, OBJECT
  MJD-MID   = xxxxx     
  SATURLVL  = xxxx (optional saturation level - defaults to 60K)


|#



(in-package instrument-id)

(defclass/inst generic-onechip (imaging-instrument onechip)
    ((name :initform "generic-onechip") ;; a string
     (observatory :initform "generic-onechip" :initarg :observatory)
     ;; optional
     (saturation-level :initform 60000)
     (non-linear-level :initform 60000)
     (gain-keyword :initform "GAIN"
		   :accessor instrument-gain-keyword)))

(defun %to-generic-filter (filt-keyword) 
  (case
      filt-keyword
    (:uj "U")
    (:bj "B")
    (:vj "V")
    (:rc "R")
    (:ic "I")
    ;;
    (:usdss "u")
    (:gsdss "g")
    (:rsdss "r")
    (:isdss "i")
    (:zsdss "z")
    ;;
    (:gps1 "g")
    (:rps1 "r")
    (:ips1 "i")
    (:zps1 "z")
    (:yps1 "y")
    ;; all wide filters treated the same
    ((:gri :wps1 :gri-cfht-megacam :open :vr) "wide")
    ;;
    (otherwise "unknown")))
 
(defun add-generic-headers-using-template (fits template-fits &key
								(do-wcs nil)
								(do-exptime t)
								(do-object t)
								(do-filter t)
								(do-obstype t)
								(do-mjd t)
								(do-gain t))
  
  "Given a fits file, insert headers to make it generic using a template
file - ie, insert following headers
  GINSTRUM, OBSERVTR, GAIN, EXPTIME, GFILTER, OBJECT, OBSTYPE, MJD-MID
based on the template.  This works only on onchip files.

Keywords DO-WCS, DO-EXPTIEM, DO-OBJECT, DO-OBSTYPE, DO-FILTER
         DO-MJD, DO-GAIN
allow some fields to not be transferred  All but DO-WCS are T by default.
WCS is not transferred by default because a correct WCS is often present from
stacking.

It is useful, for example, when making stacks or shift-and-add files."
  (cf:with-open-fits-file (template-fits ff-template)
    (cf:with-open-fits-file (fits ff-out :mode :io)
      (let ((comment (format nil "Generic from template ~A"
			     (type-of
			      (instrument-id:identify-instrument ff-template)))))
	(cf:write-fits-header ff-out "GINSTRUM" "GENERIC_ONECHIP"
			      :comment comment)
	(cf:write-fits-header ff-out "INSTRUME" "GENERIC_ONECHIP"
			      :comment comment))
      (when do-wcs
	(let ((wcs (or (cf:read-wcs ff-template)
		       (error "DO-WCS is true and cannot read WCS from template ~A" ff-template))))
	  (cf:write-wcs ff-out wcs)))
      (cf:write-fits-header ff-out "OBSERVTR"
			    (get-observatory-for-fits ff-template))
      (when do-gain
	(cf:write-fits-header ff-out "GAIN"
			      (get-gain-for-fits ff-template)))
      (when do-exptime
	(cf:write-fits-header ff-out "EXPTIME"
			      (get-exptime-for-fits ff-template)))
      (when do-filter
	(cf:write-fits-header ff-out "GFILTER"
			      (%to-generic-filter
			       (get-standard-filter-for-fits ff-template))))
      (when do-object
	(cf:write-fits-header ff-out "OBJECT"
			      (get-object-for-fits ff-template)))
      (when do-obstype
	(cf:write-fits-header ff-out "OBSTYPE"
			      (string (or (get-object-type-for-fits ff-template)
					  "UNKNOWN"))))
      (when do-mjd
	(cf:write-fits-header ff-out "MJD-MID"
			      (get-mjd-mid-for-fits ff-template))))))
    
    




 
(defun %generic-onechip-identify-instrument (fits-file)
  (when
      (or (equalp (cf:read-fits-header fits-file "GINSTRUM" :extension 1) "GENERIC_ONECHIP")
	  (equalp (cf:read-fits-header fits-file "INSTRUME" :extension 1) "GENERIC_ONECHIP")
	  (equalp (ignore-errors
		   (cf:read-fits-header fits-file "GINSTRUM" :extension 2))
		  "GENERIC_ONECHIP")
	  (equalp (ignore-errors
		   (cf:read-fits-header fits-file "INSTRUME" :extension 2))
		  "GENERIC_ONECHIP"))
    (make-instance
     'generic-onechip
     :observatory (or (cf:read-fits-header fits-file "OBSERVTR" :extension 1)
		      (ignore-errors (cf:read-fits-header fits-file "OBSERVTR" :extension 2)))
     :saturation-level (or (cf:read-fits-header fits-file "SATURLVL" :extension 1)
			   (ignore-errors (cf:read-fits-header fits-file "SATURLVL" :extension 2))
			   60000))))

(%add-instrument-id-function '%generic-onechip-identify-instrument)

(defun %get-generic-header-in-primary-or-img (header inst fits-file &key (throw-error t))
  (or (cf:read-fits-header fits-file header  :extension 1)
      (cf:read-fits-header fits-file header
			   :extension (get-image-extension-for-onechip-instrument
				       inst fits-file))
      (when throw-error
	(error "Header ~A not found in primary or image extension." header))))


(defmethod get-standard-filter-for-instrument ((inst generic-onechip) fits-file) 
  (let ((filter (or (%get-generic-header-in-primary-or-img "GFILTER" inst fits-file :throw-error nil)
		    (%get-generic-header-in-primary-or-img "FILTER" inst fits-file))))
    (cond
      ((string= filter "U") :uj) ;; case sensitive, so string=
      ((string= filter "B") :bj)
      ((string= filter "V") :vj)
      ((string= filter "R") :rc)
      ((string= filter "I") :ic)
      ((string= filter "u") :usdss)
      ((string= filter "g") :gsdss)
      ((string= filter "r") :rsdss)
      ((string= filter "i") :isdss)
      ((string= filter "z") :zsdss)
      ((string= filter "wide") :wide)
      (t NIL))))





;; EXPTIME is default
(defmethod get-exptime-for-instrument ((inst generic-onechip) fits-file)
  (%get-generic-header-in-primary-or-img "EXPTIME" inst fits-file))
    
    

;; OBJECT
(defmethod get-object-for-instrument ((inst generic-onechip) fits-file)
  (%get-generic-header-in-primary-or-img "OBJECT" inst fits-file))
  

(defmethod get-object-type-for-instrument ((inst generic-onechip) fits-file)
  (%get-generic-header-in-primary-or-img "OBSTYPE" inst fits-file))

(defmethod get-mjd-start-for-instrument ((inst generic-onechip) fits-file)
  (-  (%get-generic-header-in-primary-or-img "MJD-MID" inst fits-file)
      (* 0.5d0
	 (/ (%get-generic-header-in-primary-or-img "EXPTIME" inst fits-file)
	    24 3600d0))))
  

(defmethod get-mjd-mid-for-instrument ((inst generic-onechip) fits-file)
  (%get-generic-header-in-primary-or-img "MJD-MID" inst fits-file))

(defmethod get-gain-for-instrument ((inst generic-onechip) fits-file
				    &key extension)
  (declare (ignorable extension))
  (%get-generic-header-in-primary-or-img "GAIN" inst fits-file))

(defmethod write-gain-for-instrument ((inst generic-onechip) fits-file gain
				      &key extension)
  (call-next-method))

(defmethod get-chip-id-for-instrument ((inst generic-onechip) fits-file
				       &key extension)
  (declare (ignorable extension))
  1) ;; always chip1

;; by default return the whole image
(defmethod get-datasec-for-instrument ((inst generic-onechip) fits-file
				       &key extension)
  (call-next-method))

;; should have a WCS
(defmethod get-initial-wcs-for-instrument ((inst generic-onechip) fits-file
					   &key extension)
  (call-next-method))

;; should have a WCS
(defmethod insert-initial-wcs-for-instrument ((inst generic-onechip) fits-file
					      &key extension)
  (call-next-method))

;; use WCS pixel scale
(defmethod get-pixel-scale-for-instrument ((inst generic-onechip) fits-file
					   &key extension)
  (call-next-method))


;; will return 1
#+nil
(defmethod get-image-extension-for-onechip-instrument
    ((inst generic-onechip) fits-file)
  (call-next-method))

;; test first and second extension
(defmethod get-image-extension-for-onechip-instrument
    ((inst generic-onechip) fits-file)
  (flet ((test-extension (k)
	   (ignore-errors ;; might not be an extension K
	    (let ((naxis (cf:read-fits-header fits-file "NAXIS" :extension k))
		  (naxis1 (cf:read-fits-header fits-file "NAXIS1" :extension k))
		  (naxis2 (cf:read-fits-header fits-file "NAXIS2" :extension k)))
	     (when (and naxis (plusp naxis)
			naxis1 (plusp naxis1)
			naxis2 (plusp naxis2))
	       k)))))
    (or (test-extension 1)
	(test-extension 2)
	(error "Can't find image extension for generic onechip instrument ~A" inst))))
	       
		 


;; default should work
(defmethod test-if-image-at-extension-for-instrument
    ((inst generic-onechip) fits-file &key  extension)
  (call-next-method))
