


(defpackage suprime-cam-fix
    (:use :cl)
    (:export 
     #:process-overscan-strips
     #:merge-chips
     #:merge-chips-in-directory
     #:convert-wcs-to-zpn
     ))

(in-package suprime-cam-fix)


;; final gain, roughly same as input gain, so we can assess saturation
(defparameter *final-gain* 3.33)
(defconstant +saturation-value+ 65535)

(defstruct sregion
  (n 0 :type (unsigned-byte 29))
  (xovmin 0 :type (unsigned-byte 29)) (xovmax 0 :type (unsigned-byte 29))
  (yovmin 0 :type (unsigned-byte 29)) (yovmax 0 :type (unsigned-byte 29))
  (ximmin 0 :type (unsigned-byte 29)) (ximmax 0 :type (unsigned-byte 29))
  (yimmin 0 :type (unsigned-byte 29)) (yimmax 0 :type (unsigned-byte 29))
  (gain  0.0 :type single-float))


(defun %get-overscan-and-image-regions-from-header (ff nregion)
  (flet ((get-header (format-name)
	   (let ((full-name (format nil format-name nregion)))
	     (multiple-value-bind (val comment name code)
		 (cf:read-fits-header ff full-name)
	     (declare (ignore comment name))
	       (when (not (zerop code))
		 (error "Fits header ~A not found" full-name))
	       val))))
    (make-sregion
     :n nregion
     :xovmin   (get-header "S_OSMN~d1")
     :xovmax   (get-header "S_OSMX~d1")
     :yovmin   (get-header "S_OSMN~d2")
     :yovmax   (get-header "S_OSMX~d2")
     :ximmin   (get-header "S_EFMN~d1")
     :ximmax   (get-header "S_EFMX~d1")
     :yimmin   (get-header "S_EFMN~d2")
     :yimmax   (get-header "S_EFMX~d2")
     :gain     (float (get-header "S_GAIN~D") 1.0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; compute overscan array using vertical overscan strip
(defun %make-y-overscan-vector (data sr)
  (declare (type (simple-array single-float (* *)) data)
	   (type sregion sr)
	   (optimize speed))
  (let* ((ny (array-dimension data 0))
	 (nx (1+ (- (sregion-xovmax sr) (sregion-xovmin sr))))
	 (vtmp (make-array nx :element-type 'single-float :initial-element 0.0))
	 (vmed (make-array ny :element-type 'single-float :initial-element 0.0)))
    (loop for iy below ny
	  do (loop for ix from (1- (sregion-xovmin sr)) to (1- (sregion-xovmax sr))
		   for i of-type (signed-byte 29) from 0
		   do (setf (aref vtmp i) (aref data iy ix)))
	     (setf (aref vmed iy) (fastmedian:fast-single-float-1d-array-median vtmp)))
    vmed))
	     
;; do the vertical overscans 
(defun %subtract-y-overscans (data sr-vec)
    (declare (type (simple-array single-float (* *)) data)
	     (type simple-vector sr-vec)
	     (optimize speed))
  (loop for sr of-type sregion across sr-vec  
	for vos of-type (simple-array single-float *) = (%make-y-overscan-vector data sr)
	do ;(print sr)
	   ;; we subtract overscan vector from ENTIRE height of chip, including the horizontal
	   ;; overscan region
	   (loop for iy below (array-dimension data 0)
	      do (loop for ix from (1- (sregion-ximmin sr)) to (1- (sregion-ximmax sr))
		    for xmed of-type single-float = (aref vos iy)
		    for pixval of-type single-float = (aref data iy ix)
		    for saturated = (>= pixval +saturation-value+)
		    for xresult of-type single-float = (if saturated
							   (float +saturation-value+ 1.0)
							   (- pixval xmed))
		    do (setf (aref data iy ix) xresult)))))
     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; note that vertical overscan and horizontal is not treated quite the same.  The idea is to
;; first subtract the y overscan from the x overscan region, then do a 2nd sweep using the x (horiz) 
;; overscan region.


;; compute overscan array using horizontal overscan strip - the vector
;; is the whole length of chip, but mostly zeros
(defun %make-x-overscan-vector (data sr)
  (declare (type (simple-array single-float (* *)) data)
	   (type sregion sr)
	   (optimize speed))
  (let* ((nx (array-dimension data 1))
	 (ny (1+ (- (sregion-yovmax sr) (sregion-yovmin sr))))
	 (vtmp (make-array ny :element-type 'single-float :initial-element 0.0))
	 (vmed (make-array nx :element-type 'single-float :initial-element 0.0)))
    (loop for ix from (1- (sregion-ximmin sr)) to (1- (sregion-ximmax sr))
	  do (loop for iy from (1- (sregion-yovmin sr)) to (1- (sregion-yovmax sr))
		   for i of-type (signed-byte 29) from 0
		   do (setf (aref vtmp i) (aref data iy ix)))
	     (setf (aref vmed ix)  (fastmedian:fast-single-float-1d-array-median vtmp)))
    vmed))


;; do the vertical overscans
(defun %subtract-x-overscans (data sr-vec)
    (declare (type (simple-array single-float (* *)) data)
	     (type simple-vector sr-vec)
	     (optimize speed))
  (loop for sr of-type sregion across sr-vec  
	for vos of-type (simple-array single-float *) = (%make-x-overscan-vector data sr)
	do ;(print sr)
	   (loop for ix from (1- (sregion-ximmin sr)) to (1- (sregion-ximmax sr))
	      do (loop 
		    for iy from (1- (sregion-yimmin sr)) to (1- (sregion-yimmax sr))
		    for xmed of-type single-float = (aref vos ix)
		    for pixval of-type single-float = (aref data iy ix)
		    for saturated = (>= pixval +saturation-value+)
		    for xresult of-type single-float = (if saturated
							   (float +saturation-value+ 1.0)
							   (- pixval xmed))
		    do (setf (aref data iy ix) xresult)))))

;; get rid of overscan regions, placing into output image, and turning gain to 3.33
(defun %rebin-regions (data-in data-out sr-vec &key (final-gain *final-gain*))
  (declare (type (simple-array single-float (* *)) data-in data-out)
	   (type (simple-array T (*))  sr-vec)
	   (type single-float final-gain)
	   (optimize speed))
  ;; starting x index in final array for the sregions
  (let ((ixstart (loop with v  = (make-array 4 :element-type '(signed-byte 29)   :initial-element 0)
		       for i from 1 to 3
		       for j from 0 to 2
		       for sr  = (aref sr-vec j)
		       do (setf (aref v i) ;; add width of last sregion each time
				(+ (aref v j) (1+ (- (sregion-ximmax sr) (sregion-ximmin sr) ))))
		       finally (return v))))
    (loop for sr of-type sregion across sr-vec
	  for istart of-type fixnum across ixstart ;; start in final array
	  do (loop with gain of-type single-float = (sregion-gain sr)
		   for ix of-type (unsigned-byte 29) from (1- (sregion-ximmin (the sregion sr)))
		     to 
		     (1- (sregion-ximmax (the sregion sr)))
		   for ix-out of-type (unsigned-byte 29) from istart
		   do (loop 
			 for iy-out of-type (unsigned-byte 29)  from 0
			 for iy of-type (unsigned-byte 29) from (1- (sregion-yimmin sr)) 
			  to (1- (sregion-yimmax sr))
			 for pixval of-type single-float = (aref data-in iy ix)
			 for saturated = (>= pixval +saturation-value+)
			 for outval = (if saturated
					  (float +saturation-value+ 1.0)
					  (* (/ gain final-gain) (aref data-in iy ix)))
			 do (setf (aref data-out iy-out ix-out) outval))))))





(defun process-overscan-strips (fits-in fits-out &key (subtract-overscan t) (append nil) (final-gain *final-gain*))
"For a single Suprime-Cam (version 2) image file, remove the overscan
regions, make the amplifiers contiguous, and by default perform
overscan subtraction.  Also multiply by each amplifier's GAIN keyword.

Overscan subtraction is performed by first subtracting vertical
overscan region (including from horizontal overscan region); then
subtracting horizontal overscan region. In practice, the 2nd step is very
close to zero.

The final image still has steps because GAIN keywords are not precise, but 
flattening should remove them.

APPEND means add to an existing fits file.

FITS-OUT can be a CF:FITS-FILE, a pathname, or a string, but APPEND must be T
if a CF:FITS:FILE."
  (declare (type (or string pathname) fits-in)
	   (type (or string pathname cf:fits-file) fits-out))
  

  (when (equalp fits-in fits-out)
    (error "input file ~A and output file ~A must be different" fits-in fits-out))

  (when (not (probe-file fits-in))
    (error "input fits file ~A does not exist" fits-in))


  (when (and (or (stringp fits-out) (pathnamep fits-out))
	     (probe-file fits-out)
	     (not append))
    (error "Output file ~A already exists and APPEND is NIL." fits-out))

  (when (and (or (stringp fits-out) (pathnamep fits-out))
	     (not (probe-file fits-out))
	     append)
    (error "Output file ~A does not exists and APPEND is true." fits-out))

  (when (and (typep fits-out 'cf:fits-file)
	     (not append))
    (error "FITS-OUT is a CF:FITS-FILE but APPEND is NIL."))

  (cf:with-open-fits-file ((namestring fits-in) ffin)
    (let* ((imsec-in (cf:read-image-section ffin :type :single-float))
	   (det-n (cf:read-fits-header ffin "DET-ID"))
	   (extnam (format nil "CHIP~D" det-n))
	   (data-in (cf:image-section-data imsec-in))
	   (sr-vec 
	    (sort (map 'vector (lambda (n) (%get-overscan-and-image-regions-from-header ffin n))
		       #(1 2 3 4))
		  '<
		  :key 'sregion-ximmin))
	   (xsize (loop for sr across sr-vec 
			sum (1+ (- (sregion-ximmax sr) (sregion-ximmin sr) ))))
	   (sr1 (aref sr-vec 0))
	   (ysize  (1+ (- (sregion-yimmax sr1) (sregion-yimmin sr1))))
	   (data-out (make-array (list ysize xsize) :element-type 'single-float)))

      ;;
      ;; first subtract y (vert) overscan from both image and x (horiz, bottom) overscan region,
      ;; then subtract the modified x (horiz) overscan regiona
      (when subtract-overscan
	(%subtract-y-overscans data-in sr-vec);; this seems to work
	(%subtract-x-overscans data-in sr-vec))
      (%rebin-regions data-in data-out sr-vec)


      #+nil ;; test code to make gradient images
      (loop for ix below xsize
	   do (loop for iy below ysize
		   for yboost = iy
		 do (setf (aref data-out iy ix) (+ 1.0 yboost ix))))

      (flet ((add-image (ffout)
	       ;; this must go FIRST
	       (cf:add-image-to-fits-file 
		ffout :float 
		(vector (array-dimension data-out 1) (array-dimension data-out 0))
		:create-data data-out)
	       ;;(cf:write-fits-header ffout "SIMPLE" T)
	       (cf:write-fits-header ffout "EXTNAME" extnam)
	       (loop with hlist = (cf:read-fits-header-list ffin)
		     for header in hlist
		     for key = (first header) and val = (second header) and comment = (third header)
		     do (cond
			  ;; we changed gain to FINAL-GAIN
			  ((equalp key "GAIN")
			   (cf:write-fits-header ffout "GAIN" FINAL-GAIN
						 :comment "Typical gain"))
			  ;; ignore these headers because the image will put them in
			  ((member key '("BZERO" "BSCALE" "BITPIX" 
					 "SIMPLE" "EXTEND" "PCOUNT" "GCOUNT"
					 "NAXIS" "NAXIS1" "NAXIS2") :test 'equalp)
			   nil)
			  ((equalp key "CRPIX1")
			   (cf:write-fits-header ffout "CRPIX1" (- val (sregion-ximmin sr1)) 
						 :comment comment))
			  ((equalp key "CRPIX2")
			   (cf:write-fits-header ffout "CRPIX2" (- val (sregion-yimmin sr1))
						 :comment comment))
			  (t
			   (cf:write-fits-header ffout key val :comment comment))))
	       ;;
	       
	       ))
	
	(cond ((and append (or (stringp fits-out) (pathnamep fits-out)))
	       (cf:with-open-fits-file ((namestring fits-out) ffout :mode :io)
		 (add-image ffout)))
	      ((or (stringp fits-out) (pathnamep fits-out))
	       (cf:with-new-fits-file (fits-out ffout)
		 (add-image ffout)))
	      (t ;; append=T, FITS-OUT is CF:FITS-FILE
	       (add-image fits-out)))))))

    


(defun merge-chips (fits-name &key (subtract-overscan t) (outfile nil) (overwrite nil) (outdir nil))
  "Given a fits file like SUPA1234567n.fits for chip n, find all the other chips denoted by n=0...9"
  (when outdir
    (ensure-directories-exist (format nil "~A/#dummy#" (namestring outdir))))
  (when (not (probe-file fits-name))
    (error "Fits file ~A does not exist" fits-name))
  (let* ((dir (file-io:dir-of-file fits-name))
	 (outdir (or outdir dir))
	 (filename (or (file-io:file-minus-dir fits-name)
		       (error "Could not parse filename in ~A" fits-name)))
	 (file-head (subseq filename 0 11))
	 (file-list (loop for i below 10 collect (format nil "~A~D.fits" file-head i)))
	 (full-file-list (loop for file in file-list collect (format nil "~A~A" dir file)))
	 (outfilename (or outfile
			  (format nil "~A/~A.fits" 
				  (string-right-trim "/" outdir )
				  file-head))))
    ;;
    (loop for file in full-file-list 
	  when (not (probe-file file))
	    do (error "File ~A does not exist" file))
    ;;
    (cf:with-new-fits-file (outfilename ffout :overwrite overwrite)
      (cf:write-fits-header ffout "SIMPLE" t)
      (cf:write-fits-header ffout "BITPIX" 16)
      (cf:write-fits-header ffout "NAXIS" 0)
      (cf:write-fits-comment ffout "Combined file of 10 Suprime-Cam chips")
      (cf:write-fits-comment ffout 
			     (if subtract-overscan 
				 "Overscan subtr. using S_xx hdrs by suprime-cam-fix.lisp" 
				 "Overscan NOT subtracted"))
      (cf:with-open-fits-file (fits-name ffin)
	(flet ((copy-hdr (key &optional new-key)
	       (multiple-value-bind (val comment) 
		   (cf:read-fits-header ffin key)
	       (cf:write-fits-header ffout (or new-key key) val :comment comment))))
	  (loop for key in '("DATA-TYP" "OBJECT" "DATE-OBS" "UT" "EXPTIME" "RA" "DEC" 
			     "FILTER01" "MJD" "LST" "HST" "AIRMASS" "ALTITUDE" "AZIMUTH" "INSTRUME" "INST-PA")
		do (copy-hdr key))
	  (copy-hdr "FILTER01" "FILTER")
	(cf:write-fits-header ffout "UTDATE" 
			      (format nil "~AT~A" 
				      (cf:read-fits-header ffin "DATE-OBS")
				      (cf:read-fits-header ffin "UT")) :comment "Starting UT")))
      (loop for fits in full-file-list
	 do (process-overscan-strips fits ffout :append t :subtract-overscan subtract-overscan)))
    ;;
    outfilename))

(defun merge-chips-in-directory (dir &key (subtract-overscan t)  (overwrite nil) (outdir nil))
  "Run MERGE-CHIPS on all of the files in the directory."
  (let ((fits-chip-0-list 
	 (mapcar 'namestring 
		 (directory
		  (format nil "~A/~A" (string-right-trim "/" dir) "SUPA???????0.fits")))))
    (loop for fits in fits-chip-0-list 
	  do (merge-chips fits 
			  :subtract-overscan subtract-overscan 
			  :overwrite overwrite 
			  :outdir outdir))))




(defun convert-wcs-to-zpn (fits-file)
  "Insert WCS ZPN headers with PV2_3=400, to allow rebinning 
 (eg using SCAMP) to an undistorted tangent projection."
  (let ((pv23 400.0)
	(instr (instrument-id:identify-instrument fits-file)))
    (flet ((write-wcs (ff)
	     (cf:write-fits-header ff "CTYPE1" "RA---ZPN")
	     (cf:write-fits-header ff "CTYPE2" "DEC--ZPN")
	     (cf:write-fits-header ff "PV2_1" 1)
	     (cf:write-fits-header ff "PV2_3" pv23)))
      
      (cond ((typep instr 'instrument-id:subaru-suprime-cam-one-chip)
	     (write-wcs fits-file))
	    ((typep instr 'instrument-id:subaru-suprime-cam-array)
	     (cf:with-open-fits-file (fits-file  ff :mode :io)
	       (loop for i from 0 below 10
		     for ext = (format nil "CHIP~D" i)
		     do (cf:move-to-extension ff ext)
			(write-wcs ff))))
	    (t
	     (error "Not a SUPRIMECAM instrument type: ~A" instr))))))
	  


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+nil
(defun make-test-image-from-template (template-fits out-fits)
  "Generate a test image for overscan subtraction.  This image has
  111,222,333,444 as the image values, 11,22,33,44 as the horizontal
  overscan, and 1,2,3,4 as the vertical overscan.  The edge pixels
  of the image regions are incremented by 1 (112,223,334,445)."
  (let* ((ff (cf:create-fits-file out-fits :template template-fits :overwrite t)))
    (cf:close-fits-file ff)
    (cf:with-open-fits-file (out-fits ff :mode :io)
      (cf:move-to-extension ff 1)
      (let* ((imsec (cf:read-image-section ff))
	     (a  (cf:image-section-data imsec))
	     (ny (array-dimension a 0))
	     (nx (array-dimension a 1)))

	;; now for each image section mark out section and overscan
	(loop for i below (array-total-size a) do (setf (row-major-aref a i) 1000.0))

	(loop 
	   for i from 1 to 4
	   for pval = (* i 111.0)
	   for oval-vert = (* i 1.0)
	   for oval-horiz = (* i 10.0)
	   for sr = (%get-overscan-and-image-regions-from-header ff i)
	   do 
	     (loop for ix from (sregion-ximmin sr) to (sregion-ximmax sr)
		do (loop for iy from (sregion-yimmin sr) to (sregion-yimmax sr)
		      for edge = (or (= ix (sregion-ximmin sr))  (= ix (sregion-ximmax sr))
				     (= iy (sregion-yimmin sr))  (= iy (sregion-yimmax sr)))
		      ;; add 1 to edges to check they are preserved
		      for edgeboost = (if edge 1.0 0.0)
		      do (setf (aref a (1- iy) (1- ix)) (+ edgeboost pval))))
	     (loop for ix from (sregion-ximmin sr) to (sregion-ximmax sr)
		do (loop for iy from (sregion-yovmin sr) to (sregion-yovmax sr)
		      do (setf (aref a (1- iy) (1- ix)) oval-horiz)))
	     (loop for ix from (sregion-xovmin sr) to (sregion-xovmax sr)
		do (loop for iy from (sregion-yimmin sr) to (sregion-yimmax sr)
		      do (setf (aref a (1- iy) (1- ix)) oval-vert))))
	(loop for i from 1 to 4 do (cf:write-fits-header ff (format nil "S_GAIN~D" i) 1))
	(cf:write-back-image-section imsec )))))

#+nil
(defun make-sloped-test-image-from-template (template-fits out-fits)
"Generate a sloped test image that should overscan subtract to all 100"
  (let* ((ff (cf:create-fits-file out-fits :template template-fits :overwrite t)))
    (cf:close-fits-file ff)
    (cf:with-open-fits-file (out-fits ff :mode :io)
      (cf:move-to-extension ff 1)
      (let* ((imsec (cf:read-image-section ff))
	     (a  (cf:image-section-data imsec))
	     (ny (array-dimension a 0))
	     (nx (array-dimension a 1)))

	(loop for i below (array-total-size a) do (setf (row-major-aref a i) 0.0))

	;; introduce a slope
	(loop for ix below nx
	     do (loop for iy below ny
		     do (setf (aref a iy ix) (float (+ 1.0 iy ix) 1.0))))

	(loop 
	   for i from 1 to 4
	   for pval = (* i 111.0)
	   for oval-vert = (* i 1.0)
	   for oval-horiz = (* i 10.0)
	   for sr = (%get-overscan-and-image-regions-from-header ff i)
	   do 
	     (loop for ix from (sregion-ximmin sr) to (sregion-ximmax sr)
		do (loop for iy from (sregion-yimmin sr) to (sregion-yimmax sr)
		      do (incf (aref a (1- iy) (1- ix)) 10000.0))))
	(loop for i from 1 to 4 do (cf:write-fits-header ff (format nil "S_GAIN~D" i) 1))
	(cf:write-back-image-section imsec )))))


