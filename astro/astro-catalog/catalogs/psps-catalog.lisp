
;; routines to download astronomical catalogs



(in-package astro-catalog)



;; the minimum number of detections PS1 must have
;; in a color to view the magnitude as valid
(defparameter *ps1-nstars-min* 2)



(defparameter *psps-stackpsf-field-ids*
  '((:id "objID")
    (:ra  "raStack") (:dec "decStack")
    (:ra-err "raStackErr") (:dec-err "decStackErr") ;; arcsec
    (:epoch-mean "epochMean") ;; mean epoch and baseline for proper motion
    (:pmra  "pmra")       (:pmdec "pmdec")  ;; not sure if proper motion returned for all datasets
    (:g  "gStackPSFMag") (:g-err "gStackPSFMagErr")
    (:r  "rStackPSFMag") (:r-err "rStackPSFMagErr")
    (:i  "iStackPSFMag") (:i-err "iStackPSFMagErr")
    (:z  "zStackPSFMag") (:z-err "zStackPSFMagErr")
    (:y  "yStackPSFMag") (:y-err "yStackPSFMagErr"))) ;; no w-band in PSPS

    

(defparameter *psps-meanpsf-field-ids*
  '((:id "objID")
    (:ra  "raMean") (:dec "decMean")
    (:ra-err "raMeanErr") (:dec-err "decMeanErr")  ;; arcsec
    (:pmra  "pmra")       (:pmdec "pmdec") ;; proper motion returned for SOME stars here (mas/yr)
    (:epoch-mean "epochMean") ;; mean epoch and baseline for proper motion
    (:g  "gMeanPSFMag") (:g-err "gMeanPSFMagErr")
    (:r  "rMeanPSFMag") (:r-err "rMeanPSFMagErr")
    (:i  "iMeanPSFMag") (:i-err "iMeanPSFMagErr")
    (:z  "zMeanPSFMag") (:z-err "zMeanPSFMagErr")
    (:y  "yMeanPSFMag") (:y-err "yMeanPSFMagErr"))) ;; no w-band in PSPS

(defparameter *psps-stackkron-field-ids*
  '((:id "objID")
    (:ra  "raStack") (:dec "decStack")
    (:ra-err "raStackErr") (:dec-err "decStackErr")  ;; arcsec
    (:pmra  "pmra")       (:pmdec "pmdec") ;; not sure if proper motion returned for all datasets
    (:epoch-mean "epochMean") ;; mean epoch and baseline for proper motion
    (:g  "gStackKronMag") (:g-err "gStackKronMagErr")
    (:r  "rStackKronMag") (:r-err "rStackKronMagErr")
    (:i  "iStackKronMag") (:i-err "iStackKronMagErr")
    (:z  "zStackKronMag") (:z-err "zStackKronMagErr")
    (:y  "yStackKronMag") (:y-err "yStackKronMagErr"))) ;; no w-band in PSPS


(defparameter *psps-meankron-field-ids*
  '((:id "objID")
    (:ra  "raMean") (:dec "decMean")
    (:ra-err "raMeanErr") (:dec-err "decMeanErr")  ;; arcsec
    (:pmra  "pmra")       (:pmdec "pmdec") ;; not sure if proper motion returned for all datasets
    (:epoch-mean "epochMean") ;; mean epoch and baseline for proper motion
    (:g  "gMeanKronMag") (:g-err "gMeanKronMagErr")
    (:r  "rMeanKronMag") (:r-err "rMeanKronMagErr")
    (:i  "iMeanKronMag") (:i-err "iMeanKronMagErr")
    (:z  "zMeanKronMag") (:z-err "zMeanKronMagErr")
    (:y  "yMeanKronMag") (:y-err "yMeanKronMagErr"))) ;; no w-band in PSPS


(defparameter *psps-extra-kron-field-ids*
  '((:gkron  "gMeanKronMag") (:gkron-err "gMeanKronMagErr")
    (:rkron  "rMeanKronMag") (:rkron-err "rMeanKronMagErr")))

;; extra fields containing the difference between psf and Kron magnitudes
(defparameter *psps-psf-minus-kron-ids*
  '((:r-psf-minus-kron "rPSF_Minus_Kron")
    (:g-psf-minus-kron "gPSF_Minus_Kron")
    (:g-ndet "ng") (:r-ndet "nr") (:i-ndet "ni")
    (:z-ndet "nz") (:y-ndet "ny") 
    (:ndet  "ndet")))



#+nil 
(defun read-psps-3pi-catalog/once (ra-deg dec-deg radius-deg  
				   &key (mag-source :mean)
				     (mag-type :psf)
				     (verbose nil)
				     (query-type :quick))
  (declare (type (member :mean :stack) mag-source)
	   (type (member :psf :kron) mag-type)
	   (type (member :quick :slow) query-type))
  (let* ((field-type-list
	   (append 
	    *psps-psf-minus-kron-ids*
	    (cond ((eq mag-type :psf)
		   (cond 
		     ((eq mag-source :stack) *psps-stackpsf-field-ids*)
		     ((eq mag-source :mean) *psps-meanpsf-field-ids*)))
		  ((eq mag-type :kron)
		   (cond 
		     ((eq mag-source :stack) *psps-stackkron-field-ids*)
		     ((eq mag-source :mean) *psps-meankron-field-ids*))))))
	 (query-function
	   (cond ((eq query-type :slow) 'psps:do-full-executeslowjob)
		 ((eq query-type :quick) 'psps:do-full-executequickjob))))
    (multiple-value-bind (hash error)
	(funcall query-function
	    (psps:make-sql-query-all-object-mags-in-cone 
	     ra-deg dec-deg radius-deg
	     :mag-source mag-source :mag-type mag-type)
	  :verbose verbose)
      (cond ((not hash) ;; psps failed
	     (values nil error))
	    (t
	     (let 
	     ;; vector with the appropriate data vector in each element
		 ((data (map 'vector (lambda (pair) (gethash (second pair) hash))
			     field-type-list))
		  ;; list of fields in order
		  (fields-tags (mapcar 'first field-type-list)))
	       ;; fixme 
	       ;;(%fix-psps-data data fields-tags)
	       (values data fields-tags)))))))


(defun build-url-for-ps1-catalog-search/stsci (ra dec radius/deg
					 &key (mag-source :mean)
					   (data-release :dr2)
					   (max-objects 1000000)
					   (ndet-min 3))
  (declare (type (member :mean :stack) mag-source)
	   (type (member :dr1 :dr2) data-release))
  (format
   nil
   "https://catalogs.mast.stsci.edu/api/v0.1/panstarrs/~A/~A.csv?ra=~,4F&dec=~,4F&radius=~,4F&pagesize=~A&nDetections.gte=~A"
   (string-downcase (string data-release))
   (string-downcase (string mag-source))
   ra dec radius/deg
   max-objects
   ndet-min))
   
   
;; parse datum as integer, float string in that order
(defun %psps-parse-datum (str key)
  (declare (type string str))
  (or
   ;; don't parse ID as number because too long for normal integer
   (and (eq key :id) str)
   ;; try integer
   (ignore-errors (parse-integer str))
   ;; then float
   (and (jk-parse-float:validate-float-string
	 str :ignore-leading-whitespace t)
	(jk-parse-float:parse-float str))
   ;; fall back on string
   str))
  
(defun read-psps-catalog/stsci  (ra-deg dec-deg radius-deg  
			   &key (mag-source :mean)
			     (mag-type :psf)
			     (data-release :dr2)
			     (max-objects 1000000)
			     (ndet-min 3))
  (declare (type (member :mean :stack) mag-source)
	   (type (member :psf :kron) mag-type))
  (multiple-value-bind (csv-data http-code)
      (drakma:http-request
       (build-url-for-ps1-catalog-search/stsci
	ra-deg dec-deg radius-deg
	:mag-source mag-source
	:data-release data-release
	:max-objects max-objects
	:ndet-min ndet-min))
    (when (not (= http-code 200)) ;; HTTP-OK
      (error "HTTP ERROR CODE ~A accessing MAST.STSCI.EDU for PS1 catalog."
	     http-code))
    (let* ((csv-list (with-input-from-string (s csv-data)
		       (fare-csv:read-csv-stream s)))
	   ;; the translation table
	   (field-type-list
	     (append 
	      *psps-psf-minus-kron-ids*
	      (cond ((eq mag-type :psf)
		     (cond 
		       ((eq mag-source :stack)
			(append *psps-stackpsf-field-ids*
				;; add kron for star/galaxy separation
				*psps-extra-kron-field-ids*))
		       ((eq mag-source :mean)
			(append *psps-meanpsf-field-ids*
				;; add kron for star/galaxy separation
				*psps-extra-kron-field-ids*))))
		    ((eq mag-type :kron)
		     (cond 
		       ((eq mag-source :stack) *psps-stackkron-field-ids*)
		       ((eq mag-source :mean) *psps-meankron-field-ids*))))))
	   (field-strings   (first csv-list))
	   ;; field pairs is (index symbol) pairs for each returned field,
	   ;; if this is one of the ones we want
	   (field-pairs
	     (loop for fstr in field-strings
		   for i from 0
		   for sym = (car
			      (find fstr field-type-list
				    :key 'second :test 'equalp))
		   when sym
		     collect (list i sym)))
	   (fields (mapcar 'second field-pairs))
	   (map-hash (make-hash-table :test 'eql))
	   (index-hash (make-hash-table :test 'eql))
	   (data-list  (cdr csv-list))
	   ;; add 3 fields for r
	   (nfields (+ 3 (length field-pairs)))
	   (ndata (length data-list))
	   (output-vecs ;; array of arrays, one for each column
	     (make-array nfields
			 :initial-contents
			 (loop for i below nfields
			       collect (make-array ndata)))))
      (loop for (index sym) in field-pairs
	    do (setf (gethash index index-hash) sym)
	       (setf (gethash sym map-hash)     index))


      ;; parse the data using only those fields we've identified
      (loop for j from 0 ;; index jth object
	    for data-line in data-list
	    do (loop with k = 0 ;; increment index in data only if we used it
		     for datum in data-line
		     for i from 0 ;; index ith field
		     for key = (gethash i index-hash)
		     when key ;; this field has a used entry
		       do
			  (setf (aref (aref output-vecs k) j)
				(%psps-parse-datum datum key))
			  (incf k)))
      ;;
      ;; fill in the missing fields
      (let* ((ndet-vec (aref output-vecs  (+ -3 nfields)))
	     (r-rkron-vec (aref output-vecs (+ -2 nfields)))
	     (g-gkron-vec (aref output-vecs (+ -1 nfields)))
	     (r-index (position :r fields))
	     (r-vec (when r-index
		      (aref output-vecs r-index)))
	     (rkron-index (position :rkron fields))
	     (rkron-vec (when rkron-index
			  (aref output-vecs rkron-index)))
	     (g-index (position :g fields))
	     (g-vec (when g-index
		      (aref output-vecs g-index)))
	     (gkron-index (position :gkron fields))
	     (gkron-vec (when gkron-index
			  (aref output-vecs gkron-index))))
	;; count total detections
	(loop for idat below ndata
	      do
		 (loop
		   with ndet = 0
		   for field in fields
		   for ifield from 0
		   when (member field '(:g-ndet :r-ndet :i-ndet :z-ndet :w-ndet))
		     do (incf ndet
			      (aref (aref output-vecs ifield) idat))
		   finally
		      (setf (aref ndet-vec idat) ndet)))
	;; fill in r minus rkron, etc
	(fill r-rkron-vec -999d0)
	(fill g-gkron-vec -999d0)
	;; compute r - rkron
	(when (and r-vec rkron-vec)
	  (loop for idat below ndata
		for r across r-vec
		for rk across rkron-vec
		when (and (< (abs r) 99)
			  (< (abs rk) 99))
		  do (setf (aref r-rkron-vec idat)
			   (- r rk))))
      ;; compute g - gkron
      (when (and g-vec gkron-vec)
	(loop for idat below ndata
	      for g across g-vec
	      for gk across gkron-vec
	      when (and (< (abs g) 99)
			(< (abs gk) 99))
		do (setf (aref g-gkron-vec idat)
			 (- g gk)))))
      (setf fields
	    (append fields '(:ndet :r-psf-minus-kron :g-psf-minus-kron)))

      ;; get rid of spaces as designators of no-valid-proper motion, and replace
      ;; with *invalid-dbl-value
      (%badvalify-field-dbl-vector :pmra fields output-vecs)
      (%badvalify-field-dbl-vector :pmdec fields output-vecs)
	

      ;; convert all output vecs to float types
      (setf output-vecs
	    (map 'vector
		 (lambda (vec)
		   (cond ((every (lambda (x) (typep x 'double-float)) vec)
			  (coerce vec '(simple-array double-float (*))))
			 ((every (lambda (x) (typep x 'single-float)) vec)
			  (coerce vec '(simple-array single-float (*))))
			 ((every (lambda (x) (typep x 'fixnum)) vec)
			  (coerce vec '(simple-array fixnum (*))))
			 (t
			  vec)))
		 output-vecs))
		       
	   
      
      (values output-vecs fields))))
	  
  
  




				
;; try it several times before throwing an 
(defun read-psps-3pi-catalog  (ra-deg dec-deg radius-deg 
			       &key
				 (mag-source :mean) (mag-type :psf)
				 (ntries 5))
  (loop with error = nil
	for i below ntries
	do
	   (multiple-value-bind (data fields-tags)
	       (read-psps-catalog/stsci
		ra-deg dec-deg radius-deg
		:mag-source mag-source :mag-type mag-type)
	     (setf error fields-tags)
	     (when data
	       (return (values data fields-tags)))
	     (sleep 5)) ;;
	finally 
	   (error "READ-PSPS-3PI-CATALOG failed after ~A tries with error ~A"
		  ntries error))) 




;; a null class
(defclass psps-3pi-catalog (astro-catalog)
   ((defult-position-error :accessor default-position-error
    :initarg :default-position-error :initform #.(/ 0.10 3600d0))))

(defclass psps-3pi-mean-psf-mag-catalog (psps-3pi-catalog) ())
(defclass psps-3pi-mean-kron-mag-catalog (psps-3pi-catalog) ())
(defclass psps-3pi-stack-psf-mag-catalog (psps-3pi-catalog) ())
(defclass psps-3pi-stack-kron-mag-catalog (psps-3pi-catalog) ())

(defun %get-psps-catalog-class (mag-source mag-type)
  (cond ((eq mag-type :psf)
	 (cond ((eq mag-source :stack)
		'psps-3pi-stack-psf-mag-catalog)
	       ((eq mag-source :mean)
		'psps-3pi-mean-psf-mag-catalog)))
	((eq mag-type :kron)
	 (cond ((eq mag-source :stack)
		'psps-3pi-stack-kron-mag-catalog)
	       ((eq mag-source :mean)
		'psps-3pi-mean-kron-mag-catalog)))))




(defmethod object-mag ((acat psps-3pi-catalog) mag-name i &key (error-if-not-exist t))
  (multiple-value-bind (mag err ndet)
      (cond ((eq mag-name :g) 
	     (values (get-value acat :g i) (get-value acat :g-err i)
		     (get-value acat :g-ndet i)))
	    ((eq mag-name :r)
	     (values (get-value acat :r i) (get-value acat :r-err i)
		     (get-value acat :r-ndet i)))
	    ((eq mag-name :i) 
	     (values (get-value acat :i i) (get-value acat :i-err i)
		     (get-value acat :i-ndet i)))
	    ((eq mag-name :z)
	     (values (get-value acat :z i) (get-value acat :z-err i)
		     (get-value acat :z-ndet i)))
	    ((eq mag-name :y)
	     (values (get-value acat :y i) (get-value acat :y-err i)
		     (get-value acat :y-ndet i)))
	    ;; no w-band in PSPS
	    (t
	     (if error-if-not-exist
		 (error "Magnitude type ~A unknown in ~A" mag-name acat)
		 nil)))
    (if mag
	(values mag err 
		;; invalid mags have a value of 999, OR fewer than
		;; *ps1-nstars-min* detections (we just don't trust PS1)
		(or (= (round mag) 999)
		    (< ndet *ps1-nstars-min*))))))
	


(defmethod object-ra-dec ((acat psps-3pi-catalog) (i fixnum)  &key mjd)
  (declare (ignorable mjd))
  (let* ((ra  (get-value acat :ra i))
	 (dec (get-value acat :dec i))
	 (dra/dt (get-value acat :pmra i))   ;; mas/yr
	 (ddec/dt (get-value acat :pmdec i)) ;; mas/yr
	 (epoch (get-value acat :epoch-mean i)) ;; different epoch for each obj
	 ;; not every detection has a proper motion
	 (good-proper-motion
	   (and (not (invalid-value-p dra/dt))
		(not (invalid-value-p ddec/dt))))
	 (adjust-by-pm (and mjd epoch good-proper-motion
			    t))) ;; final T to have T/NIL for return

    ;; shift to epoch mjd
    (when adjust-by-pm
      (let* ((years-difference (/ (- mjd epoch) 365.25d0))
	     (delta-ra  (* years-difference 1d-3 dra/dt))
	     (delta-dec (* years-difference 1d-3 ddec/dt)))
	(multiple-value-setq (ra dec)
	  (astro-coords:sky-angles-slew ra dec delta-ra delta-dec :units :arcsec))))
    
    (values ra dec 
	    (object-ra-err  acat i)
	    (object-dec-err  acat i)
	    adjust-by-pm))) ;; NIL means 'no adjustment by MJD made'




;; PS1 stsci returns arcsec errors, but -999 means 'bad value'
(defmethod object-ra-err ((acat psps-3pi-catalog) i)
  (let ((val (get-value acat :ra-err i)))
    (if (minusp val)   ;; unknown error becomes 1 arcsec
	(setf val 1d0))
    (max val  0.010))) ;; impose 10 mas error floor
;;
(defmethod object-dec-err ((acat psps-3pi-catalog) i)
  (let ((val (get-value acat :dec-err i)))
    (if (minusp val)   ;; unknown error becomes 1 arcsec
	(setf val 1d0))
    (max val  0.010))) ;; impose 10 mas error floor

;; PS returns mas/yr
(defmethod object-proper-motions ((acat psps-3pi-catalog) i)
  (let ((pmra  (get-value acat :pmra i))
	(pmdec (get-value acat :pmdec i)))
    (if (and (floatp pmra)
	     (floatp pmdec))
	(values pmra pmdec)
	(values nil nil))))


;; a rough guess of whether an object is a star or galaxy using the difference
;; between KRON and PSF mags.  Objects fainter than 20 are :UNKNOWN, otherwise
;; anything with a big difference (> 0.15) is a :GALAXY
(defmethod object-type ((acat psps-3pi-catalog) i)
  (let ((rdmag (get-value acat :r-psf-minus-kron i))
	(gdmag (get-value acat :g-psf-minus-kron i))
	(rmag  (get-value acat :r i))
	(gmag  (get-value acat :g i)))
    (cond ((or (> 998 gmag 21.5)(> 998 rmag 21.5)) ;; faint are unknown unless bad mags
	   :unknown)
	  ((and (= (round gmag) 999) (= (round rmag) 999)) ;; no photometry ==> :unknown
	   :unknown)
	  ((and (= rdmag 0.0) (= gdmag 0)) ;; exactly zero is suspect
	   :unknown)
	  ((or (> gdmag 0.10)  ;; either big g or r variation means galaxy
	       (< gdmag -0.15)
	       (> rdmag 0.10)
	       (< rdmag -0.15))
	   :galaxy)
	  (t
	   :star))))
	   

		
	   
	
				
					      
(defun read-psps-3pi-catalog-object (ra-deg dec-deg radius-deg &key
					       (mag-source :mean) (mag-type :psf))
  (multiple-value-bind (data fields)
      (read-psps-3pi-catalog ra-deg dec-deg radius-deg 
			     :mag-source mag-source :mag-type mag-type)
    (mark-astro-catalog-ok ;; use method to mark objects that are OK
     (make-instance (%get-psps-catalog-class mag-source mag-type)
		    :n (if data 
			   (length (aref data 0))
			  0)
		    :ra-center (float ra-deg 1d0) :dec-center (float dec-deg 1d0) 
		    :radius-deg (float radius-deg 1d0)
		    :data data
		    :fields fields
		    :available-mags '(:g :r :i :z :y)
		    :%map (make-map fields)))))

