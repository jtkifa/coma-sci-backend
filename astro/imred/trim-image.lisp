
(in-package imred)



(defun compute-median-overscan-value (ff overscans)
  ;; loop over overscans, read them, compute median
  (let* ((os-arrays ;; list of oversns as single-float arrays
	   (loop for os in overscans
		 for fp = (vector (aref os 0) (aref os 2))
		 for lp = (vector (aref os 1) (aref os 3))
		 for imsec = (cf:read-image-section ff :fp fp :lp lp :type :float)
		 for arr = (cf:image-section-data imsec)
		 collect arr))
	 (npix-good ;; cound good pixels
	   (loop with kgood = 0 
		 for arr in os-arrays
		 do (loop for i below (array-total-size arr)
			  for x = (row-major-aref arr i)
			  when (not (float-utils:single-float-nan-or-infinity-p x))
			    do (incf kgood))
		 finally (return kgood)))
	 (arrtot ;; combined array of good pixels
	   (make-array npix-good :element-type 'single-float)))
    ;;
    (when (zerop npix-good)
      (error "Found zero good pixels in overscan regions ~A in extension ~A of file ~A"
	     overscans (cf:fits-file-current-hdu-num ff)
	     (cf:fits-file-filename ff)))
    ;; collect good pixels
    (loop with kgood = 0 
	  for arr in os-arrays
	  do (loop for i below (array-total-size arr)
		   for x = (row-major-aref arr i)
		   when (not (float-utils:single-float-nan-or-infinity-p x))
		     do (setf (aref arrtot kgood) x)
			(incf kgood)))
    ;; and return median
    (fastmedian:fast-single-float-1d-array-median arrtot)))
    
    
	
	
;; optionally overscan-subtract and optionally trim one extension
(defun trim/os-one-extension (ff ff-out extdesc write-type
			   &key trim trimsec overscan-subtract overscans)
  (let* ((image-size (cf:fits-file-current-image-size ff))
	 ;; if not trimming, then set the trimsec to whole image
	 (trimsec* (if trim
		       trimsec
		       image-size))
	 (fp (vector (aref trimsec* 0) (aref trimsec* 2)))
	 (lp (vector (aref trimsec* 1) (aref trimsec* 3)))
	 ;; get current extension from input file FF because FF-OUT
	 ;; may be in indeterminate state
	 (extnum (cf:fits-file-current-hdu-num ff))
	 ;; we always read to float because of bzero issue
         (read-type :float)
	 (overscan-value  (if overscan-subtract
			      (compute-median-overscan-value ff overscans)
			      0.0))
	 (imsec
	  (cf:read-image-section  ff :fp fp :lp lp  :type read-type))
	 (data (cf:image-section-data imsec))
	 (bzero (float (or (cf:read-fits-header ff "BZERO") 0.0) 1.0))
	 (bscale (float (or (cf:read-fits-header ff "BSCALE") 1.0) 1.0))
	 (naxes (vector (array-dimension data 1) (array-dimension data 0)))
	 (output-image-is-float (member write-type '(:float :double))))

    (declare (type (simple-array single-float (* *)) data)
	     (type single-float bzero bscale overscan-value))

    (when overscan-subtract
      (loop for i of-type fixnum below (array-total-size data)
	    for x = (row-major-aref data i)
	    when (not (float-utils:single-float-nan-or-infinity-p x))
	      do (setf (row-major-aref data i)
		       (- x overscan-value))))

    ;; we have to fix the BZERO,BSCALE issue before writing float data
    ;; back, but for float images we just get rid of BZERO,BSCALE
    (when (not output-image-is-float)
      (when (not (and (= bzero 0.0) (= bscale 1.0)))
	(locally (declare (optimize speed))
	  (loop 
	    with k = (array-total-size data)
	    for i of-type fixnum below k 
	    do
	       (setf (row-major-aref data i)
		     (/ (- (row-major-aref data i) bzero)
			bscale))))))
    
    (cf:add-image-to-fits-file      
     ff-out write-type    
     naxes
     :create-data  data)

    ;; THEN copy headers
    (copy-headers ff ff-out
		  :exclude (append
			    '("DATASEC" "BIASSEC" 
			      "NAXIS" "NAXIS1" "NAXIS2" "BITPIX")
			    ;; get rid of BZERO,BSCALE for float
			    ;; output images
			    (if output-image-is-float
				'("BZERO" "BSCALE"))))
    ;;
    (cf:write-fits-header ff-out "TRIM" 
			  (format 
			   nil "~A  ~A" 
			   (sec-to-string trimsec*)
			   (astro-time:ut-to-date-string (get-universal-time))))

    (when overscan-subtract
      (cf:write-fits-header ff-out "IMRED.OVERSCANSUBTRACT" t
			    :comment "IMRED subtracted one median overscan value")
      (cf:write-fits-header ff-out "IMRED.OVERSCANVALUE"
			    overscan-value
			    :comment "Single overscan value used"))
    

    (instrument-id:set-standard-header ff-out
				       :trimmed "YES" 
				       :extension extnum)

    ;; write the new DATASEC and STATSEC into the headers, as standard headers
    (let* ((full-imsec (vector 1 (aref naxes 0) 1 (aref naxes 1)))
	   (full-imsec-string (format nil "[~A:~A,~A:~A]"
				     (aref full-imsec 0) (aref full-imsec 1)
				     (aref full-imsec 2) (aref full-imsec 3)))
	   ;; we computed the statsec in the extdesc to be the TRIMMED statsec
	   (statsec-trimmed (extdesc-statsec extdesc))
	   (statsec-trimmed-string
	     (format nil "[~A:~A,~A:~A]"
		     (aref statsec-trimmed 0) (aref statsec-trimmed 1)
		     (aref statsec-trimmed 2) (aref statsec-trimmed 3))))
      (instrument-id:set-standard-header ff-out :datasec full-imsec-string
						:extension extnum)
      (instrument-id:set-standard-header ff-out :statsec statsec-trimmed-string
						:extension extnum)
      ;; also write ordinary TRIMSEC header because others might expect them
      (cf:write-fits-header ff-out "TRIMSEC" full-imsec-string)
      (cf:write-fits-header ff-out "DATASEC" full-imsec-string))
      
      
    
    (when trim (cf:write-fits-header ff-out "TRIMSEC" (sec-to-string trimsec)))

    #+nil ;; no longer preserve ccdsec and biassec
    (progn
      (cf:write-fits-comment    
       ff-out
       (format nil "Original ccdsec=~A"
	       (if ccdsec (sec-to-string ccdsec) "NONE")))
      (cf:write-fits-comment    
       ff-out
       (format nil "Original biassec=~A"
	       (if biassec (sec-to-string biassec) "NONE"))))
			   
    ;; fix WCS cripix - it looks like the convention is to define crpix on the
    ;; image array coordinate system, ignoring BIASSEC etc
    (let ((crpix1 (cf:read-fits-header ff "CRPIX1"))
	  (crpix2 (cf:read-fits-header ff "CRPIX2")))
      (when crpix1  (cf:write-fits-header ff-out "CRPIX1" (1+ (- crpix1 (aref fp 0)))))
      (when crpix2  (cf:write-fits-header ff-out "CRPIX2" (1+ (- crpix2 (aref fp 1))))))
    ff-out))

    
    

 

(defun trim/os-image (fits fits-out reduction-plan
		      &key
			(write-type :auto)
			(if-exists :error))
  "If REDUCTION-PLAN-OVERSCAN-SUBTRACT is true, then do a simple overscan subtraction, using
the median pixel value.

If REDUCTION-PLAN-TRIM is true, then trim the image.

The TRIMSEC and OVERSCANS is obtained from the REDUCTION-PLAN.

Overscan subtraction is very simple using just the medians of the
overscan regions.

WRITE-TYPE is the output image type, by default :AUTO, which means
to use the EXTDESC type unless we are overscan subtracting, in which
case use float.

It may not be possible to read and write a SHORT image because of
BZERO issues."
  (declare (type (member :error :supersede) if-exists))

  (when (probe-file fits-out)
    (if (eq if-exists :error)
	(error "Output file ~A exists" fits-out)
	(delete-file fits-out)))

  (let ((trim (reduction-plan-trim reduction-plan))
	(overscan-subtract (reduction-plan-overscan-subtract reduction-plan))
	(fits (fullfile fits))
	(fits-out (fullfile fits-out :create t :delete t))) ;; see fullfile in utils.lisp
    
    (when (equal fits fits-out)
      (error "output file ~A would overwrite input file ~A" fits-out fits))

    
    (with-temporary-output-file (fits-out fits-out-tmp :extra-suffix "_TMP")
      (cf:with-new-fits-file   (fits-out-tmp ff-out :overwrite t :make-primary-headers nil)
	(cf:with-open-fits-file (fits ff :mode :input)
	  (loop 
	    with extdesc-list = (build-extdesc-list-for-fits fits)
	    for ihdu from 1 to (cf:fits-file-num-hdus ff)
	    for extdesc in extdesc-list
	    ;; for the write type, force FLOAT if overscan subtract 
	    for write-type-final = (cond ((eq write-type :auto)
					  (if overscan-subtract
					      :float
					      (extdesc-image-type extdesc)))
					 (t
					  write-type))
	    do 
	       (cf:move-to-extension ff ihdu)
	       (cond 
		 ;; just copy any non-image extension
		 ((not (extdesc-reduce-p extdesc))
		  (cfitsio:copy-current-extension ff ff-out))
		 ;; is if reducible, get overscans and trimsec if needed
		 ;; and process 
		 (t
		  (trim/os-one-extension 
		   ff ff-out extdesc write-type-final
		   :trim trim
		   :trimsec (extdesc-trimsec extdesc)
		   :overscan-subtract overscan-subtract
		   :overscans (extdesc-overscans extdesc)
		   )))))))))
	   
