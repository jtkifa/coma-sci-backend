#|



|#

(in-package instrument-id)

(defclass %ctio-decam-compressed-mixin () ()) ;; mixin for compression

;; parent class of DECAM
(defclass %ctio-decam (imaging-instrument)
  ((name :initform "CTIO-DECAM") ;; a string
   (aperture :initform 4.0)  ;; Victor M. Blanco telescope
   (observatory :initform "W84"))) ;; correct obscode for DECam

(defclass/inst %ctio-decam-onechip (%ctio-decam onechip)
  ())

;; one extension - as from archives
(defclass/inst ctio-decam-onechip-one-ext (%ctio-decam-onechip)
  ())

;; two extensions - as from using custom code to copy one extension
(defclass/inst ctio-decam-onechip-two-ext (%ctio-decam-onechip)
  ())


;; one chip, one ext with compression - probably won't see this much
(defclass/inst ctio-decam-onechip-compressed-one-ext
  (ctio-decam-onechip-one-ext %ctio-decam-compressed-mixin) 
  ())

;; one chip, two ext, with compression - probably won't see this much
(defclass/inst ctio-decam-onechip-compressed-two-ext
  (ctio-decam-onechip-two-ext %ctio-decam-compressed-mixin) 
  ())

;; presumably compressed
(defclass/inst ctio-decam-array (%ctio-decam multichip
					     %ctio-decam-compressed-mixin)
  ())

;; mixin for pre-processed decam, with wcs but no phot-calib
(defclass %decam-processed/oo (preprocessed)
  ((wcs-origin :initform "DECam-DCP")
   (phot-calib-origin :initform nil)))


;; mixin for WCS+phot processed decam
(defclass %decam-processed-full (preprocessed)
  ((wcs-origin :initform "DECam-DCP")
   (phot-calib-origin :initform "DECam-DCP")))

;; the ooi proc class with wcs but no phot-calib or re-projection
(defclass/inst ctio-decam-array-proc/oo (ctio-decam-array %decam-processed/oo)
  ())

;; FIXME - we need to better subclassify processing types, plus single
;; and array

(defmethod get-critical-headers-for-instrument ((inst %ctio-decam) fits-file)
  (declare (ignore inst fits-file))
  (remove-duplicates
   (append
    '("CCDNUM" "OPENSHUT" "GAINA" "GAINB")
    (call-next-method))
   :test 'equalp))

(defmethod get-image-extension-for-onechip-instrument
    ((inst  ctio-decam-onechip-one-ext) fits-file)
  (declare (ignore inst fits-file))
  1)  ;; 1st ext has the imag

(defmethod get-image-extension-for-onechip-instrument
    ((inst ctio-decam-onechip-two-ext) fits-file)
  (declare (ignore inst fits-file))
  2)  ;; 2nd ext has the image
 
(defun %ctio-decam-identify-instrument (fits-file)
  (when (equalp (ignore-errors
		 (cf:read-fits-header fits-file "INSTRUME" :extension 1))
		"DECAM" )
    (let* ((nextensions
	     (number-of-extensions fits-file))
	   (next-use (if (= nextensions 1) 1 2)) ;; use this extension
	   (naxis (%gethead-or-error fits-file "NAXIS"  :extension next-use))
	   ;; this is awful - need real multi-ext image
	   (nextend  (cf:read-fits-header fits-file "NEXTEND" :extension 1))
	   (compressed (equalp (cf:read-fits-header fits-file "TTYPE1" :extension
						    next-use)
			       "COMPRESSED_DATA")))
    (cond ((equal nextend 70)
	   (make-instance 'ctio-decam-array))
	  ((member nextensions '(61 62)) ;; weirdly NEXTEND=2 for proc mosaic
	   ;; need to handle full proc possibility
	   (make-instance 'ctio-decam-array-proc/oo)) 
	  ((and naxis compressed)
	   (cond
	     ((= next-use 1)
	      (make-instance 'ctio-decam-onechip-compressed-one-ext
			     :chip-id (%gethead-or-error
				       fits-file
				       "CCDNUM" :extension next-use)))
	     ((= next-use 2)
	      (make-instance 'ctio-decam-onechip-compressed-two-ext
			     :chip-id (%gethead-or-error
				       fits-file
				       "CCDNUM" :extension  next-use)))))
	     
	  (naxis
	    (cond
	     ((= next-use 1)
	      (make-instance 'ctio-decam-onechip-one-ext
			     :chip-id (%gethead-or-error
				       fits-file
				       "CCDNUM" :extension  next-use)))
	     ((= next-use 2)
	      (make-instance 'ctio-decam-onechip-two-ext
			     :chip-id (%gethead-or-error
				       fits-file
				       "CCDNUM" :extension  next-use)))))
	  (t
	   (error "Unknown DECAM fits file ~A" fits-file))))))

(%add-instrument-id-function '%ctio-decam-identify-instrument)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod get-standard-filter-for-instrument ((inst %ctio-decam) fits-file)
  (let ((filter (%gethead-or-error fits-file "FILTER" :extension 1)))
    (when (or (not filter) (not (stringp filter))  (< (length filter) 1))
      (error "FILTER keyword not found in image ~A" fits-file))
    (cond ((eql 0 (search "Y " filter)) :y) ;; strings like "r DECam SDSS c0002 ...."
	  ((eql 0 (search "g " filter)) :gsdss)
	  ((eql 0 (search "r " filter)) :rsdss)
	  ((eql 0 (search "i " filter)) :isdss)
	  ((eql 0 (search "z " filter)) :zsdss)
	  ;; the undocumented wideband filter spanning g,r,i
	  ((eql 0 (search "gri." filter)) :gri-ctio-decam)
	  ;; this filter goes from about 500 to 750 nm
	  ((eql 0 (search "VR DECam" filter)) :vr-ctio-decam)
	  ;; fixme - there are more filters
	  (t NIL))))

;; generic version OK
;(defmethod get-exptime-for-instrument ((inst %ctio-decam) fits-file) ... )

(defmethod get-object-for-instrument ((inst %ctio-decam) fits-file)
  (%gethead-or-error fits-file "OBJECT" :extension 1))

(defmethod get-object-type-for-instrument ((inst %ctio-decam) fits-file)
  (let ((obj-type (%gethead-or-error fits-file  "OBSTYPE" :extension 1)))
    (cond ((equalp obj-type "BIAS") :BIAS)
	  ((equalp obj-type "FLAT") :FLAT)
	  ((equalp obj-type "OBJECT") :OBJECT)
	  (T :OTHER))))

(defmethod get-mjd-start-for-instrument ((inst %ctio-decam) fits-file)
  ;; almost same as MJD-OBS, but difference of 0.03s
  (let* ((open-ut (cf:read-fits-header fits-file "OPENSHUT" :extension 1))) 
    (when open-ut
      (ignore-errors (astro-time:parse-ut-date-and-time-string-to-mjd open-ut)))))


;; apparently this is the most accurate version in the headers
(defmethod get-mjd-mid-for-instrument ((inst %ctio-decam) fits-file)
  (+ (get-mjd-start-for-instrument inst fits-file)
     (* 0.5d0 
	(/ (%gethead-or-error fits-file "EXPTIME" :extension 1)
	   (* 24 3600)))))
	       

(defmethod instrument-gain-keyword ((inst %ctio-decam))
  (declare (ignore inst))
  "GAINA") ;; GAINA and GAINB are close


;(defmethod get-gain-for-instrument ((inst %ctio-decam) fits-file)   )

;; megacam has two trimsecs, one for each amp, that have no separation between them.
(defmethod get-datasec-for-instrument ((inst %ctio-decam) fits-file
				       &key extension)
  (declare (ignore extension))
  (call-next-method))


;(defmethod get-chip-id-for-instrument ((inst ctio-decam-onechip) fits-file) ..)


(defmethod patch-extracted-extension-headers ((inst ctio-decam-array) fits-file extname/extnum output-fits-file)
  (cf:with-open-fits-file (output-fits-file ff-out :mode :io)
    (let ((extension (cf:fits-file-num-hdus ff-out)))
      (when (equalp (cf:read-fits-header  ff-out "WCSCAL" :extension 1) ;; it's in primary
		    "Successful")
	(cf:write-fits-header ff-out "WCSFITOK" t
			      :comment "from WCSCAL='Successful'"
			      :extension extension)))))

#+nil ;; obsolete now
(defmethod extract-one-image-from-mosaic-for-instrument
    ((inst  ctio-decam-array) fits-file  extname/extnum output-fits-file
     &key (decompress nil))

  ;; how to handle decompression?  On the fly?  Make temp file?
  (when decompress (error "Decompression not yet supported."))
  (let ((chip-number nil)
	(headers-to-copy nil)) ;; KEY NAME COMMENT
    (cf:with-open-fits-file (fits-file ff-in)

      ;; the headers we need to copy from primary
      (setf headers-to-copy
	    (loop
	      with hlist = (cf:read-fits-header-list ff-in)
	      for h in '("SIMPLE" "NAXIS" "NEXTEND")
	      do (setf hlist (remove h hlist :test 'equalp :key 'first))
	      finally (return hlist)))
      
      
      (when extname/extnum
	(cf:move-to-extension ff-in extname/extnum))

      (cf:extract-fits-extension  fits-file output-fits-file
				  :extension (cf:fits-file-current-hdu-num ff-in)
				  :overwrite t)
      (cf:with-open-fits-file (output-fits-file ff-out :mode :io)
	(cf:move-to-extension ff-out 1)
	(cf:write-fits-header ff-out "EXTRACTED.CHIP"
			      (format nil "ccd~2,'0D" chip-number))
	(cf:write-fits-header ff-out "EXTRACTED.CHIPNUM" CHIP-NUMBER)
	;; convert DECam wcs fit to a WCSFITOK
	(when (equalp (cf:read-fits-header  ff-out "WCSCAL") "Successful")
	  (cf:write-fits-header ff-out "WCSFITOK" t
				:comment "from WCSCAL='Successful'"))
	;; put some useful keys into the primary header
	(loop for (key val comment) in headers-to-copy
	      for key2 = (nth-value 2 (cf:read-fits-header ff-out key))
	      when (not key2) ;; avoid duplicated key
		do
		   (cf:write-fits-header ff-out key val
					 :comment comment))))))
