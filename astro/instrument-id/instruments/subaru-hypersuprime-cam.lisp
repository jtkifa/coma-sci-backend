

(in-package instrument-id)

(defclass %subaru-hypersuprime-cam (imaging-instrument)
  ((name :initform "Subaru HyperSuprimeCam") ;; a string
   (aperture :initform 8.2)
   (observatory :initform "T09")))

(defclass/inst subaru-hypersuprime-cam-one-chip
  (%subaru-hypersuprime-cam onechip)
  ())

(defclass/inst subaru-hypersuprime-cam-one-chip/fzip
  (subaru-hypersuprime-cam-one-chip fzipped)
  ())


(defclass/inst subaru-hypersuprime-cam-array
  (%subaru-hypersuprime-cam multichip)
  ())

(defclass/inst subaru-hypersuprime-cam-array/fzip
  (subaru-hypersuprime-cam-array)
  ())

;; tangent-projected version by our HSC package
(defclass/inst subaru-hypersuprime-cam-one-chip-tanproj
  (%subaru-hypersuprime-cam onechip)
  ((saturation-level :initform 180000) ;; much higher saturation level
   (non-linear-level :initform 180000)))

(defclass/inst subaru-hypersuprime-cam-one-chip-tanproj/fzip
  (subaru-hypersuprime-cam-one-chip-tanproj)
  ())


(defun %hypersuprime-cam-identify-instrument (fits-file)
  (cond ((not (fits-file-is-fzipped fits-file))
	 (when (equalp (cf:read-fits-header fits-file "INSTRUME" :extension 1)
		       "Hyper Suprime-Cam")
	   (let ((naxis (%gethead-or-error fits-file "NAXIS"))
		 (tanproj (cf:read-fits-header fits-file "SCTPROJ")))
	     (cond ((and (equalp naxis 0))
		    (make-instance 'subaru-hypersuprime-cam-array))
		   ((equalp naxis 2)
	   (let* ((inst (make-instance
			 (if tanproj
			     'subaru-hypersuprime-cam-one-chip-tanproj
			     'subaru-hypersuprime-cam-one-chip)))
		  (chipid (get-chip-id-for-instrument inst fits-file)))
	     (setf (chip-id inst) chipid)
	     inst))
		   (t
		    (error "Unknown Hyper-SuprimeCam fits file ~A" fits-file))))))
	;; else it is fzipped
	(t
	 (let* ((instr-ext1 (cf:read-fits-header fits-file "INSTRUME" :extension 1))
		(instr-ext2 (ignore-errors
			     (cf:read-fits-header fits-file "INSTRUME" :extension 2)))
		(ext (if instr-ext1 1 2))
		(instr (or instr-ext1 instr-ext2)))
	   (when (equalp instr "Hyper Suprime-Cam")
	      (let ((naxis (%gethead-or-error fits-file "NAXIS" :extension ext))
		 (tanproj (cf:read-fits-header fits-file "SCTPROJ" :extension ext)))
	     (cond ((and (equalp naxis 0))
		    (make-instance 'subaru-hypersuprime-cam-array/fzip))
		   ((equalp naxis 2)
		    (let* ((inst (make-instance
				  (if tanproj
				      'subaru-hypersuprime-cam-one-chip-tanproj/fzip
				      'subaru-hypersuprime-cam-one-chip/fzip)))
			   (chipid (get-chip-id-for-instrument inst fits-file)))
		      (setf (chip-id inst) chipid)
		      inst))
		   (t
		    (error "Unknown Hyper-SuprimeCam fits file ~A" fits-file)))))))))
	   
	   

(%add-instrument-id-function '%hypersuprime-cam-identify-instrument)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod get-critical-headers-for-instrument ((inst %subaru-hypersuprime-cam)
						fits-file)
  (declare (ignore inst fits-file))
  (remove-duplicates
   (append
    '("FILTER01" "DATA-TYP" "MJD-END" "DET-ID")
    (call-next-method))
   :test 'equalp))




(defmethod get-standard-filter-for-instrument ((inst %subaru-hypersuprime-cam) fits-file)
  (let ((filter (%gethead-info inst fits-file "FILTER01")))
    (when (or (not filter) (not (stringp filter))  (< (length filter) 1))
      (error "FILTER keyword not found in image ~A" fits-file))
    (cond ((equalp filter "HSC-g") :gsdss)
	  ((equalp filter "HSC-r") :rsdss)
	  ((equalp filter "HSC-i") :isdss)
	  ((equalp filter "HSC-z") :zsdss)
	  ((equalp filter "HSC-Y") :ysdss)
	  ;;
	  ;; https://www.subarutelescope.org/Observing/Instruments/HSC/sensitivity.html
	  ;; i2,r2 replaced i,r around Jul 2018 (r) or Feb 2018 (i)
	  ((equalp filter "HSC-r2") :rsdss) 
	  ((equalp filter "HSC-i2") :isdss)
	  ;;
	  (t NIL))))

;; generic version OK
;(defmethod get-exptime-for-instrument ((inst %subaru-hypersuprime-cam) fits-file) ... )

(defmethod get-object-for-instrument ((inst %subaru-hypersuprime-cam) fits-file)
  (%gethead-info inst fits-file "OBJECT"))

(defmethod get-object-type-for-instrument ((inst %subaru-hypersuprime-cam) fits-file)
  (let ((obj-type (%gethead-info inst fits-file  "DATA-TYP")))
    (cond ((equalp obj-type "BIAS") :BIAS)
	  ((equalp obj-type "SKYFLAT") :FLAT) 
	  ((equalp obj-type "DOMEFLAT") :FLAT) 
	  ((equalp obj-type "OBJECT") :OBJECT)
	  (T :OTHER))))

;; D.Tholen says "MJD-END - exptime but
;; MJD seems very close to this 
(defmethod get-mjd-start-for-instrument ((inst %subaru-hypersuprime-cam) fits-file)
  (%gethead-info inst fits-file "MJD")) 

 
(defmethod get-mjd-mid-for-instrument ((inst %subaru-hypersuprime-cam) fits-file)
  (let ((mjd-end (%gethead-info inst fits-file "MJD-END" :throw-error nil))
	(exptime (%gethead-info inst fits-file "EXPTIME" :throw-error nil)))
    (when (not (and mjd-end exptime))
      (error "MJD-END not found in HyperSuprimeCam header."))
      ;; D.Tholen says MJD-END - exptime
      (- 
       mjd-end
       (* 0.50
	  exptime
	  (/ 1d0 3600 24)))))


;(defmethod get-gain-for-instrument ((inst %subaru-hypersuprime-cam) fits-file)   )



(defmethod get-chip-id-for-instrument
    ((inst subaru-hypersuprime-cam-one-chip) fits-file &key (extension nil))
  (%gethead-info inst fits-file "DET-ID"))

(defmethod get-initial-wcs-for-instrument
    ((inst subaru-hypersuprime-cam-one-chip) fits-file &key (extension nil))
  (cf:read-wcs fits-file
	       :extension (or extension
			      (get-info-extension inst))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


