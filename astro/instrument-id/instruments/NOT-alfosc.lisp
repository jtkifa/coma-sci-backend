;; WARNING - not well validated, especially wrt DATASEC


(in-package instrument-id)


(defclass/inst not-alfosc (imaging-instrument onechip)
  ((name :initform "NOT-ALFOSC")
   (observatory :initform "Z23") ;; NOT code
   (aperture :initform 2.495) ;; 72 inches
   (saturation-level :initform 65535)
   (non-linear-level :initform 60000)))


  

(defun %not-alfosc-identify-instrument (fits-file)
  (when (and (equalp (cf:read-fits-header fits-file "TELESCOP") "NOT")
	     (equalp (cf:read-fits-header fits-file "INSTRUME")
		     "ALFOSC_FASU"))
    (make-instance 'not-alfosc)))

(%add-instrument-id-function '%not-alfosc-identify-instrument)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod get-critical-headers-for-instrument ((inst not-alfosc) fits-file)
  (declare (ignore inst fits-file))
  (remove-duplicates
   (append
    '("ALFLTNM" "IMAGETYP")
    (call-next-method))
   :test 'equalp))

;; see https://www.not.iac.es/instruments/filters/filters.php - the filters
;; are a bit confusing
(defmethod get-standard-filter-for-instrument ((inst not-alfosc) fits-file)
  (let* ((filter (%gethead-or-error fits-file "ALFLTNM")))
    (flet ((filter-starts-with (str)
	     (eql 0 (search str filter)))
	   (filter-is (str)
	     (string= filter str)))
      (cond
	;; non-string-filter
	((not (stringp filter))
	 nil)
	;;
	((filter-starts-with "U_Bes") :uj)
	((filter-starts-with "B_Bes") :bj)
	((filter-starts-with "V_Bes") :vj)
	((filter-starts-with "R_Bes") :rc)
	((filter-starts-with "I_") :ic) ;; guess this is normal I?  who knows!
	;;
	((filter-starts-with "u'_SDSS") :usdss)
	((filter-starts-with "g'_SDSS") :gsdss)
	((filter-starts-with "r'_SDSS") :rsdss)
	((filter-starts-with "i'_SDSS") :isdss)
	((filter-starts-with "z'_SDSS") :zsdss)
	
	;; a set of interference filters
	((filter-starts-with "i_int") :isdss) ;; various
	((filter-starts-with "I_int") :ic) 
	;;
	))))
	    

;; generic version OK
;(defmethod get-exptime-for-instrument ((inst not-alfosc) fits-file) ... )

(defmethod get-object-for-instrument ((inst not-alfosc) fits-file)
  (%gethead-or-error fits-file "OBJECT"))

(defmethod get-object-type-for-instrument ((inst not-alfosc) fits-file)
  (let ((obj-type (%gethead-or-error fits-file  "IMAGETYP")))
    (cond ((equalp obj-type "BIAS") :BIAS)
	  ((equalp obj-type "FLAT") :FLAT)
	  ((equalp obj-type "OBJECT") :OBJECT)
	  (T :OTHER))))

(defmethod get-mjd-start-for-instrument ((inst not-alfosc) fits-file)
  (let* ((date-string  (%gethead-or-error fits-file "DATE-OBS"))
	 (mjd-start
	   (multiple-value-bind (year month day hour min sec)
	       (astro-time:parse-ut-date-and-time-string date-string)
	     (astro-time:calendar-date-to-mjd year month day hour min sec))))
    mjd-start))


;; generic version OK
;(defmethod get-mjd-mid-for-instrument ((inst not-alfosc) fits-file) ...  )

(defmethod get-gain-for-instrument ((inst not-alfosc) fits-file
				    &key (extension nil))
  (%gethead-or-error fits-file "GAIN"))

;(defmethod get-chip-id-for-instrument ((inst not-alfosc) fits-file) ..)

#+nil ;; UNKNOWN
(defmethod get-datasec-for-instrument ((inst not-alfosc) fits-file &key extension)
  (declare (ignore extension))
  (vector 54 2101 2 2063))

#+nil ;; just read initial wcs
(defmethod get-initial-wcs-for-instrument
    ((inst not-alfosc) fits-file &key (extension))
  )

#+nil
(defmethod  get-pixel-scale-for-instrument ((inst not-alfosc) fits-file
					    &key extension)
 )



(defmethod insert-initial-wcs-for-instrument ((inst not-alfosc) fits-file
					      &key extension)
  (declare (ignore extension))
  (cf:write-wcs (get-initial-wcs-for-instrument inst fits-file) fits-file))



