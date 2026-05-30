

(in-package astro-catalog)

(defgeneric %default-ldac-test-function (catalog index)
  (:documentation "Generic function to pick 'good' objects from a catalog 
suitable for LDAC fits file for astrometry"))

(defmethod %default-ldac-test-function ((catalog astro-catalog) i)
  (declare (ignore catalog i))
  t)

;; for PSPS, we demand 3 or more detections
(defmethod  %default-ldac-test-function ((catalog psps-3pi-catalog) i)
  (> (astro-catalog:get-value  catalog :ndet i) 3))


(defun %get-fits-type-for-vector (vec)
  (cond ((typep vec '(simple-array single-float (*)))
	 :float)
	((typep vec '(simple-array double-float (*)))
	 :double-float)
	((typep vec '(simple-array (signed-byte 64)))
	 :long-long)
	((typep vec '(simple-array (unsigned-byte 64)))
	 :long-long)
	((typep vec '(simple-array (signed-byte 32)))
	 :long)
	((typep vec '(simple-array (unsigned-byte 32)))
	 :long)
	((typep vec '(simple-array (signed-byte 16)))
	 :short)
	((typep vec '(simple-array (unsigned-byte 16)))
	 :short)
	((typep vec '(simple-array (unsigned-byte 8)))
	 :byte)
	;;
	((every 'stringp vec)
	 (loop with len = 0 for thing across vec
	       do (setf len (max len (length thing)))
	       finally (return (list :string len))))
	((every (lambda (x) (or (eq x nil) (eq x t))) vec)
	 :logical)
	;; maybe this is a lisp without typed arrays or
	;; this is an untyped vector
	((every (lambda (x) (typep x 'single-float)) vec)
	 :float)
	((every (lambda (x) (typep x 'double-float)) vec)
	 :double-float)
	((every (lambda (x) (typep x '(signed-byte 16))) vec)
	 :short)
	((every (lambda (x) (typep x '(signed-byte 32))) vec)
	 :long)
	((every (lambda (x) (typep x '(signed-byte 32))) vec)
	 :long-long)
	(t
	 (error "Could not get suitable fits table for vector ~A" vec))))
	

;; make a table descriptor suitable for cf:add-table-to-fits-file
(defun %make-fits-table-vec-for-astro-catalog (astro-catalog)
  (declare (type astro-catalog astro-catalog))
  (let* ((data (astro-catalog:astro-catalog-data astro-catalog))
	 (types (map 'vector '%get-fits-type-for-vector data))
	 (names (map 'vector (lambda (keyword) (format nil "%~A" (string-upcase keyword)))
		     (astro-catalog-fields astro-catalog))))
    (map 'vector
	 ;; a bit complicated to hand fact that strng is (:string NNN)
	 (lambda (name type)
	   (append (list name) (if (listp type) type (list type))))
	 names types)))
	 
	 
    

(defun write-catalog-to-fits-ldac
    (catalog outfile
     &key
       (overwrite nil)
       (mjd nil)
       (test-function '%default-ldac-test-function)
       (position-error-floor 0.0d0)
       (mag-to-use)
       (use-tmpfile-for-speed t)) ;; for HUGE improvement in speed on
				  ;; crossmounted disks
  
  "Writes an ASTRO-CATALOG object to an LDAC fits file.

MAG-TO-USE is the symbol of the filter/color to use for the MAG field
of the FITS table.

MJD will propagate the RA,DEC to this time, if the catalog supports it
with proper motions.

 (TEST-FUNCTION CATALOG i) is a function that returns T if an object
is to be selected.

POSITION-ERROR-FLOOR (arcsec) is the a floor to position error to put
into fields ERRA_WORLD ERRB_WORLD.  This is useful for SCAMP which 
fails when this is too small, as for GAIA.

The original fields of the catalog are written into columns starting
with '%' to allow recovery of the catalog from the FITS file."
  (declare (type astro-catalog catalog))
  
  (let ((filter mag-to-use))
    (when (and filter (not (astro-catalog:get-astro-catalog-vector catalog filter)))
      (error "Can't get data in catalog ~A for filter ~A" catalog filter)))

  (when (and (not overwrite) (probe-file outfile))
    (error "Outfile ~A exists but overwrite is NIL" outfile))

  (let* ((ntot (astro-catalog-n catalog))
         (igood-list (loop for i below ntot
                           when (or (not test-function)
                                    (funcall test-function catalog i))
			     collect i))
         (ngood (length igood-list)))

    (when (zerop ngood)
      (error "Will not create an LDAC file for a zero-element catalog ~A because zero-row fits tables seem to generate errors."
	     catalog))
    
    (flet ((make-the-file (true-outfile overwrite-outfile)
	     (cf:with-new-fits-file (true-outfile
				     ff
				     ;; overwrite if using tmpfile
				     :overwrite overwrite-outfile 
				     :make-primary-headers t)
	       (cf:write-fits-header ff "CAT_TYPE" (type-of catalog))
	       (cf:write-fits-header ff "RA" (ra-center catalog) :comment "Center RA")
	       (cf:write-fits-header ff "DEC" (dec-center catalog) :comment "Center Dec")
	       (cf:write-fits-header ff "MJD-CAT" mjd
				     :comment "RA,Dec at MJD using proper motion if avail")
	       (cf:write-fits-header ff "EQUINOX" 2000d0)
	       (cf:write-fits-header ff "RADIUS" (radius-deg catalog)
				     :comment "Radius of field in degrees")
	       (cf:write-fits-header ff "NOBJ" ngood
				     :comment "Number of objects")
	       
	       (cf:write-fits-header ff "MAG_TYPE"
				     (or mag-to-use "ANY-AVAIL")
				     :comment "Magnitude in MAG field")
	       (cf:write-fits-comment 
		ff
		(format nil "RA,Dec proper motion corrected to MJD=~,1F (~A)      where possible"
			mjd (astro-time:mjd-to-ut-string  mjd :frac-seconds nil)))
				      
    
      
     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	       ;; write the first extension's table, containing just one
	       ;; giant line line with all the (non-existant) headers concatenated
	       ;; together
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	       (cf:add-table-to-fits-file 
		ff #(("Field Header Card" :string 1680)))
	       (let* ((text "SIMPLE  =                    T / This is a FITS file")
		      (string (concatenate
			       'string text
			       (make-string (- 1680 (length text)) 
					    :initial-element #\space))))
		 (cf:write-column-to-fits-table
		  ff "Field Header Card"  `#(,string)))
	       (cf:write-fits-header ff "DATE"
				     (astro-time:ut-to-date-string (get-universal-time)))
	       (cf:write-fits-header ff "TDIM1" "(80,21)")
	       (cf:write-fits-header ff "EXTNAME" "LDAC_IMHEAD")
	     
	       ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	       ;; write the 2nd extension with the data 
	       ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	     
	       (let* ((filter mag-to-use)
		      (mag-vec (make-array ngood))
		      (magerr-vec (make-array ngood))
		      (ra-vec (make-array ngood))
		      (dec-vec (make-array ngood))
		      (raerr-vec (make-array ngood))
		      (decerr-vec (make-array ngood))
		      ;; FIXME - can we put in correct date obs?
		      (obsdate-vec (make-array ngood :element-type 'double-float
						     :initial-element 2000d0))
		      (verbatim-table-column-descs
			(%make-fits-table-vec-for-astro-catalog catalog)))
	       
	       
	       
	       
		 (loop with pos-err-floor/deg = (/ position-error-floor 3600d0) ;; arcsec->deg
		       for i in igood-list
		       for j from 0
		       for err-ra ;; astroemtric errors in degrees
			 = (sqrt (+
				  (expt pos-err-floor/deg 2)
				  (expt (object-ra-err catalog i) 2)))
		       for err-dec
			 = (sqrt (+
				  (expt pos-err-floor/deg 2)
				  (expt (object-dec-err catalog i) 2)))
		       do (setf (aref raerr-vec j) (* 1d0 err-ra))
			  (setf (aref decerr-vec j) (* 1d0 err-dec))
			  (multiple-value-bind (mag mag-err)
			      (if filter ;; if asking for exact filter, use this
				  (object-mag catalog filter j)
				  ;; otherwse, for the mag, use the first filter available
				  (loop for filter in (available-mags catalog)
					for mag = (astro-catalog:object-mag catalog filter j)
					when (and mag (< 1 mag 35)) ;; -99 is typically the 'no data' value
					  do (return ;; need mag-error, so run it again
					       (astro-catalog:object-mag catalog filter j))
					     ;; somehow we didn't get a mag, so return dummy
					finally (return (values -99.0 -99.0))))
			  
			    (setf (aref mag-vec j) (* 1d0 mag))
			    (setf (aref magerr-vec j) (* 1d0 mag-err)))
			  ;; maybe use proper motions to propagate RA,DEC
			  (multiple-value-bind (ra dec)
			      (object-ra-dec catalog i :mjd mjd)
			    (setf (aref ra-vec j) ra)
			    (setf (aref dec-vec j) dec)))
		 ;;
		 (cf:add-table-to-fits-file
		  ff
		  ;;
		  (concatenate
		   'vector
		   #(("X_WORLD" :double-float)
		     ("Y_WORLD" :double-float)
		     ("ERRA_WORLD" :double-float)
		     ("ERRB_WORLD" :double-float)
		     ("MAG" :double-float)
		     ("MAGERR" :double-float)
		     ("OBSDATE" :double-float))
		   verbatim-table-column-descs)
		  ;;
		  :extname "LDAC_OBJECTS"
		  :tunit-vec (concatenate
			      'vector
			      #("deg" "deg" "deg" "deg" "mag" "mag" "yr")
			      (make-array (length verbatim-table-column-descs)
					  :initial-element "void"))
		  :nrows (astro-catalog-n catalog))
		 ;;
		 (cf:write-fits-comment ff "X_WORLD, Y_WORLD are RA_J2000, DEC_J2000")
		 (cf:write-fits-comment
		  ff "ERRA_WORLD, ERRB_WORLD are major/minor axis errors on RA,DEC")
		 (cf:write-fits-comment ff "Fields starting with '%' are verbatim catalog columns")
		 (loop for magname in (available-mags catalog)
		       for i from 1
		       do (cf:write-fits-header
			   ff (format nil "MAG~d" i) magname
			   :comment (format nil "Col name for mag ~D" i)))
		 (cf:write-column-to-fits-table ff "X_WORLD" ra-vec)
		 (cf:write-column-to-fits-table ff "Y_WORLD" dec-vec)
		 (cf:write-column-to-fits-table ff "ERRA_WORLD" raerr-vec)
		 (cf:write-column-to-fits-table ff "ERRB_WORLD" decerr-vec)
		 (cf:write-column-to-fits-table ff "MAG" mag-vec)
		 (cf:write-column-to-fits-table ff "MAGERR" magerr-vec)
		 (cf:write-column-to-fits-table ff "OBSDATE" obsdate-vec)
		 ;;
		 (flet ((select-from-igood (vector)
			  (loop with vout = (make-array
					     ngood
					     :element-type (array-element-type vector))
				for i of-type fixnum in igood-list
				for j of-type fixnum from 0
				do (setf (aref vout j) (aref vector i))
				finally (return vout))))
		 
		   (loop for colvec across (astro-catalog-data catalog)
			 for cdesc across verbatim-table-column-descs
			 for colname = (first cdesc)
			 do (cf:write-column-to-fits-table
			     ff colname
			     (select-from-igood colvec))))))))

	   (if use-tmpfile-for-speed
	       (cl-fad:with-open-temporary-file (stmp)
		 (let ((tmpname (namestring (pathname stmp))))
		   (make-the-file tmpname t) ;; T = force overwrite for tmpfile
		   (cl-fad:copy-file tmpname outfile :overwrite overwrite)))
	       (make-the-file outfile nil)))))

    
     
				   
    
(defun read-catalog-from-fits-ldac (fits-ldac-file)
  (cf:with-open-fits-file (fits-ldac-file ff :mode :input)
    (let* ((cat-type (or (cf:read-fits-header ff "CAT_TYPE")
			 (error "CAT-TYPE header not found")))
	   (cat-class (find-symbol (string-upcase cat-type) "ASTRO-CATALOG"))
	   (ra0  (or (cf:read-fits-header ff "RA") (error "RA header not found")))
	   (dec0  (or (cf:read-fits-header ff "DEC") (error "DEC header not found")))
	   (radius (or (cf:read-fits-header ff "RADIUS") (error "RADIUS header not found")))
	   (nobj (or (cf:read-fits-header ff "NOBJ") (error "NOBJ header not found")))
	   fields
	   data
	   available-mags)

      (when (not cat-class)
	(error "Catalog type ~A is unknown" cat-type))

      (when (not (ignore-errors (cf:move-to-extension ff "LDAC_OBJECTS")))
	(error "LDAC_OBJECTS extension not found."))

      (setf available-mags
	    (loop for i from 1
		  for key = (format nil "MAG~D" i)
		  for val = (cf:read-fits-header ff key)
		  until (not val)
		  collect (find-symbol val :keyword)))

      (loop 
	with flist = nil and clist = nil
	for col across (cf:fits-file-current-table-columns ff)
	when (char= #\% (aref col 0)) ;; tagged with %
	  do (let ((field-sym (find-symbol (string-left-trim "%" col) :keyword))
		   (data (cf:read-column-from-fits-table ff col)))
	       (push field-sym flist)
	       (push data clist))
	finally
	   (setf fields (reverse flist))
	   (setf data   (coerce (reverse clist) 'vector)))
		   
      
      (make-instance cat-class
		     :ra-center ra0 :dec-center dec0
		     :available-mags available-mags
		     :fields fields :data data
		     :%map (make-map fields)
		     :radius-deg radius :n nobj))))
	  
