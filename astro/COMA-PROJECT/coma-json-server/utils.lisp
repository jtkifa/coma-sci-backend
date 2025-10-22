
(in-package coma-json-server)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; default data location - can be overriden by COMA_JSON_SERVER_DATA_DIR
(defparameter *default-coma-data-dir*
  (jk-datadir:get-datadir-for-system (asdf:find-system "coma-json-server")))

;; define the data directory, preferring the environment var COMA-DATA-DIR
(defparameter *coma-data-directory*
  (or (uiop/os:getenv "COMA_JSON_SERVER_DATA_DIR")
      (namestring *default-coma-data-dir*)))

;; this is troublesome - won't work 
(defun make-dir-world-usable (dir)
  #+sbcl (ignore-errors (sb-posix:chmod  (namestring dir) #o777)))

;; create the working directory, make it world-rwx and put a global
;; .gitignore in it
(progn
  (when (not (cl-fad:directory-exists-p *coma-data-directory*))
    (ensure-directories-exist (format nil "~A/IGNORE" *coma-data-directory*)))
  (make-dir-world-usable *coma-data-directory*)
  ;; make the .gitignore ignore everything in the directory
  (let ((gitignore (format nil "~A/.gitignore" *coma-data-directory*)))
    (when (not (probe-file gitignore))
      (with-open-file (sout gitignore :direction :output)
	(write-line "*" sout)))))
      

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dblf (x) (float x 1d0))

;; maybe gethash, if hash is not NIL
(defun %mgethash (key hash &optional default)
  (if hash (gethash key hash default)))
    
;; turn certain things like symbols into JSON
;; This is WAY overkill for the use cases.
;; this is a DESTRUCTIVE operation, possibly modifying the
;; contents of hash tables and vectors, eg replacing symbols
;; with strings.
(defun json-ify (thingy)
  (labels ((%json-ify-internal (thing depth) ;; prevent excessive recursion
	     (when (> depth 30)
	       (error "Recursion too deep in json-ify"))
	     (cond ((or (eq thing nil)
			(floatp thing) (integerp thing) (stringp thing))
		    thing)
		   ((vectorp thing)
		    (map 'vector
			 (lambda (ding)
			   (%json-ify-internal ding (1+ depth)))
			 thing))
		   ((hash-table-p thing)
		    (loop for k being the hash-key of thing
			  for v being the hash-value of thing
			  for vnew = (%json-ify-internal v (1+ depth))
			  when (not (stringp k)) ;; bad json hash
			    do (error "Hash key ~A is not a string" k)
			  ;; when hash value changed by json-ify, fix it
			  when (not (equalp v vnew))
			    do (setf (gethash k thing) vnew)))
		   (t ;; eg a symbol
		    (format nil "~A" thing)))))
    (%json-ify-internal thingy 0)))

(defun json-bool (thing)
  "Turn a THING into a boolean - if it is NIL, the 'yason:false, else 'yason:true"
  (cond ((not thing)
	 'yason:false)
	(t
	 'yason:true)))


(defun utils-to-dbl-vec (v &key (where nil) (ignore-nil t))
  (when (not (and (not v) ignore-nil))
    (when (not (vectorp v))
      (error "quantity ~A to be converted is not a vector" where))
    (map '(simple-array double-float (*)) (lambda (x) (float x 1d0))  v)))


#+sbcl 
(defun get-slot-names (wcs-struct)
  (mapcar (lambda (def) (slot-value def 'sb-pcl::name))
	  (sb-mop:class-slots (class-of wcs-struct))))

#-sbcl ;; for non-sbcl we can't do MOP on structs
(defun hashify-wcs (wcs)
  (let ((h (make-hash-table :test 'equalp)))
    (setf (gethash "TYPE" h) "WCS") ;; the JSON object type
    (setf (gethash "WCS-TYPE" h)
	  (string-upcase (format nil "~A" (type-of wcs))))
    (when (typep wcs 'wcs:wcs-2d)
      (setf (gethash "CTYPE2" h) (wcs:wcs-2d-crval1 wcs))
      (setf (gethash "CRVAL1" h) (wcs:wcs-2d-crval1 wcs))
      (setf (gethash "CRVAL2" h) (wcs:wcs-2d-crval2 wcs))
      (setf (gethash "CRPIX1" h) (wcs:wcs-2d-crpix1 wcs))
      (setf (gethash "CRPIX2" h) (wcs:wcs-2d-crpix2 wcs))
      (setf (gethash "CD1_1" h) (wcs:wcs-2d-cd1_1 wcs))
      (setf (gethash "CD1_2" h) (wcs:wcs-2d-cd1_2 wcs))
      (setf (gethash "CD2_1" h) (wcs:wcs-2d-cd2_1 wcs))
      (setf (gethash "CD2_2" h) (wcs:wcs-2d-cd2_2 wcs))
      (setf (gethash "EQUINOX" h) (wcs:wcs-2d-equinox wcs)))
    (when (typep wcs 'wcs-tan)
      (setf (gethash "CTYPE1" h) "RA---TAN")
      (setf (gethash "CTYPE1" h) "DEC--TAN"))
    (when (typep wcs 'wcs-sin)
      (setf (gethash "CTYPE1" h) "RA---SIN")
      (setf (gethash "CTYPE1" h) "DEC--SIN"))
    ;; in case it is TANTPV
    (when (typep wcs 'wcs:wcs-radec-tan-pv)
      (setf (gethash "CTYPE1" h) "RA---TAN")
      (setf (gethash "CTYPE1" h) "DEC--TAN")
      (setf (gethash "PV1VEC" h) (wcs:wcs-radec-tan-pv-pv1vec wcs))
      (setf (gethash "PV2VEC" h) (wcs:wcs-radec-tan-pv-pv2vec wcs)))
    ;; in case it is TAN-TPV
    (when (typep wcs 'wcs:wcs-radec-tan-tpv)
      (setf (gethash "CTYPE1" h) "RA---TPV")
      (setf (gethash "CTYPE1" h) "DEC--TPV")
      (setf (gethash "PV1VEC" h) (wcs:wcs-radec-tan-tpv-pv1vec wcs))
      (setf (gethash "PV2VEC" h) (wcs:wcs-radec-tan-tpv-pv2vec wcs)))
    ;; in case it is TAN-SIP
    (when (typep wcs 'wcs:wcs-radec-tan-sip)
      (setf (gethash "CTYPE1" h) "RA---TAN-SIP")
      (setf (gethash "CTYPE1" h) "DEC--TAN-SIP")
      (setf (gethash "A" h) (wcs:wcs-radec-tan-sip-a wcs))
      (setf (gethash "B" h) (wcs:wcs-radec-tan-sip-b wcs))
      (setf (gethash "AP" h) (wcs:wcs-radec-tan-sip-ap wcs))
      (setf (gethash "BP" h) (wcs:wcs-radec-tan-sip-bp wcs)))
    ;; in case it is TAN-ZPN
    (when (typep wcs 'wcs:wcs-radec-zpn)
      (setf (gethash "CTYPE1" h) "RA---ZPN")
      (setf (gethash "CTYPE1" h) "DEC--ZPN")
      (setf (gethash "NP" h) (wcs:wcs-radec-zpn-np wcs))
      (setf (gethash "PVEC" h) (wcs:wcs-radec-zpn-pvec wcs)))
    ;;
    h))




;; WARNING - this does not handle all wcs types correctly
(defun json-to-wcs (json-hash)
  (let ((crval1 (dblf (or (gethash "CRVAL1" json-hash) 0d0)))
	(crval2 (dblf (or (gethash "CRVAL2" json-hash) 0d0)))
	(crpix1 (dblf (or (gethash "CRPIX1" json-hash) 0d0)))
	(crpix2 (dblf (or (gethash "CRPIX1" json-hash) 0d0)))
	(cd1_1  (dblf (or (gethash "CD1_1" json-hash) 0d0)))
	(cd1_2  (dblf (or (gethash "CD1_2" json-hash) 0d0)))
	(cd2_1  (dblf (or (gethash "CD2_1" json-hash) 0d0)))
	(cd2_2  (dblf (or (gethash "CD2_2" json-hash) 0d0)))
	(equinox (dblf (or (gethash "EQUINOX" json-hash) 2000d0)))
	(ctype1  (gethash "CTYPE1" json-hash))
	(ctype2  (gethash "CTYPE2" json-hash))
	(pv1 (utils-to-dbl-vec (gethash "PV1" json-hash)))
	(pv2 (utils-to-dbl-vec (gethash "PV2" json-hash))))
    (declare (ignore pv1 pv2))
    (cond
      ((and (equalp ctype1 "RA---TAN")
	    (equalp ctype2 "DEC--TAN"))
       (wcs:make-wcs-radec-tan
	:crval1 crval1 :crval2 crval2
	:crpix1 crpix1 :crpix2 crpix2
	:cd1_1  cd1_1 :cd1_2 cd1_2 :cd2_1 cd2_1 :cd2_2 cd2_2
	:equinox equinox))
      
      ((and (equalp ctype1 "RA---SIN")
	    (equalp ctype2 "DEC--SIN"))
       (wcs:make-wcs-radec-sin
	:crval1 crval1 :crval2 crval2
	:crpix1 crpix1 :crpix2 crpix2
	:cd1_1  cd1_1 :cd1_2 cd1_2 :cd2_1 cd2_1 :cd2_2 cd2_2
	:equinox equinox))
      (t
       (wcs::make-wcs-2d ;; just a generic 2d
	:crval1 crval1 :crval2 crval2
	:crpix1 crpix1 :crpix2 crpix2
	:cd1_1  cd1_1 :cd1_2 cd1_2 :cd2_1 cd2_1 :cd2_2 cd2_2
	:equinox equinox)))))

      
	
	
	
    

#+sbcl
(defun hashify-wcs (wcs)
  (let ((h (make-hash-table :test 'equalp))
	(slot-names (get-slot-names wcs)))
    (setf (gethash "TYPE" h) "WCS") ;; the JSON object type
    (setf (gethash "WCS-TYPE" h)
	  (string-upcase (format nil "~A" (type-of wcs))))
    (cond ((typep wcs 'wcs:wcs-radec-zpn)
	   (setf  (gethash "CTYPE1" h) "RA---ZPN")
	   (setf  (gethash "CTYPE2" h) "DEC--ZPN"))
	  ((typep wcs 'wcs:wcs-radec-tan-sip)
	   (setf  (gethash "CTYPE1" h) "RA---TAN-SIP")
	   (setf  (gethash "CTYPE2" h) "DEC--TAN-SIP"))
	  ((typep wcs 'wcs:wcs-radec-tan-tpv)
	   (setf  (gethash "CTYPE1" h) "RA---TPV")
	   (setf  (gethash "CTYPE2" h) "DEC--TPV"))
	  ((typep wcs 'wcs:wcs-radec-tan)
	   (setf  (gethash "CTYPE1" h) "RA---TAN")
	   (setf  (gethash "CTYPE2" h) "DEC--TAN"))
	  ((typep wcs 'wcs:wcs-radec-sin)
	   (setf  (gethash "CTYPE1" h) "RA---SIN")
	   (setf  (gethash "CTYPE2" h) "DEC--SIN")))
    (loop for sname in slot-names
	  for value = (slot-value wcs sname)
	  for key  = (string-upcase (format nil "~A" sname))
	  do (setf (gethash key h) value))
    h))


(defun is-integer-in (x xmin xmax)
  (and (integerp x) (<= xmin x xmax)))

(defun is-real-in (x xmin xmax)
  (and (realp x) (<= xmin x xmax)))

(defun delete-dir-for-fits-file (fits-file)
  (let ((dir (terapix:get-fits-directory fits-file)))
    (when (and dir (cl-fad:directory-exists-p dir))
      (ignore-errors (cl-fad:delete-directory-and-files dir)))))

      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun comet-elem-to-json (elem &key (extra-fields nil))
  (let ((h (make-hash-table :test 'equalp)))
    (loop for pair in extra-fields
	  do (setf (gethash (car pair) h) (cdr pair)))
    (setf (gethash "TYPE" h) "ORBIT")
    (setf (gethash "ID" h) (orbital-elements:comet-elem-id elem))
    (setf (gethash "EPOCH-MJD" h) (orbital-elements:comet-elem-epoch elem))
    (setf (gethash "TIME-PERI-MJD" h) (orbital-elements:comet-elem-time-peri elem))
    (setf (gethash "ORBINC" h) (orbital-elements:comet-elem-orbinc elem))
    (setf (gethash "ANODE" h) (orbital-elements:comet-elem-anode elem))
    (setf (gethash "PERIHELION" h) (orbital-elements:comet-elem-q elem))
    (setf (gethash "ARG-PERIHELION" h) (orbital-elements:comet-elem-perih elem))
    (setf (gethash "ECCENTRICITY" h) (orbital-elements:comet-elem-e elem))
    ;;
    (when (typep elem 'orbital-elements:comet-elem+err)
      (setf (gethash "TIME-PERI-MJD-ERR" h)
	    (orbital-elements:comet-elem+err-time-peri-err elem))
      (setf (gethash "ORBINC-ERR" h)
	    (orbital-elements:comet-elem+err-orbinc-err elem))
      (setf (gethash "ANODE-ERR" h)
	    (orbital-elements:comet-elem+err-anode-err elem))
      (setf (gethash "PERIHELION-ERR" h)
	    (orbital-elements:comet-elem+err-q-err elem))
      (setf (gethash "ARG-PERIHELION-ERR" h)
	    (orbital-elements:comet-elem+err-perih-err elem))
      (setf (gethash "ECCENTRICITY-ERR" h)
	    (orbital-elements:comet-elem+err-e-err elem)))
    ;;
    (when (orbital-elements:comet-elem-nongravs elem)
      (let ((hng (make-hash-table :test 'equalp))
	    (ng (orbital-elements:comet-elem-nongravs elem)))
	(setf (gethash "TYPE" hng) "NONGRAVS")
	(setf (gethash "DT" hng) (orbital-elements:nongravs-dt ng))
	(setf (gethash "A1" hng) (orbital-elements:nongravs-a1 ng))
	(setf (gethash "A2" hng) (orbital-elements:nongravs-a2 ng))
	(setf (gethash "A3" hng) (orbital-elements:nongravs-a3 ng))
	(setf (gethash "NONGRAVS" h) hng)))
    ;;
    (when (typep (orbital-elements:comet-elem-data elem)
		 'orbital-elements:comet-desc)
       (let ((hd (make-hash-table :test 'equalp))
	     (desc (orbital-elements:comet-elem-data elem)))
	 (setf (gethash "TYPE" hd) "COMET-ELEMENT-DATA")
	 (setf (gethash "NAME" hd) (orbital-elements:comet-desc-name desc))
	 (setf (gethash "SOURCE" hd) (orbital-elements:comet-desc-source desc))
	 (setf (gethash "COMET-DESC" h) hd)))
       ;;
    (when (typep (orbital-elements:comet-elem-data elem)
		 'orbital-elements:asteroid-desc)
       (let ((hd (make-hash-table :test 'equalp))
	     (desc (orbital-elements:comet-elem-data elem)))
	 (setf (gethash "TYPE" hd) "ASTEROID-ELEMENT-DATA")
	 (setf (gethash "NAME" hd) (orbital-elements:asteroid-desc-name desc))
	 (setf (gethash "SOURCE" hd) (orbital-elements:asteroid-desc-source desc))
	 (setf (gethash "NUMBER" hd) (orbital-elements:asteroid-desc-number desc))
	 (setf (gethash "H" hd) (orbital-elements:asteroid-desc-h desc))
	 (setf (gethash "PERIOD" hd) (orbital-elements:asteroid-desc-period desc))
	 (setf (gethash "ALBEDO" hd) (orbital-elements:asteroid-desc-albedo desc))
	 (setf (gethash "ASTEROID-DESC" h) hd)))  

    h))

;; this does only the main fields of comet elem, not errors, or descriptor
(defun json-to-comet-elem (json-hash)
  (let ((elem (orbital-elements:make-comet-elem)))
    (macrolet ((esetf (place hash-key &optional must-be-dbl)
		 `(let ((value (gethash ,hash-key json-hash)))
		    (when (not value)
		      (error "FIELD ~A not found in JSON representation of comet elements." ,hash-key))
		    ;; convert all reals to double float
		    (if ,must-be-dbl
			(setf ,place (dblf value))
			(setf ,place value)))))

      (esetf (orbital-elements:comet-elem-id elem) "ID")
      (esetf (orbital-elements:comet-elem-epoch elem) "EPOCH-MJD" t )
      (esetf (orbital-elements:comet-elem-time-peri elem) "TIME-PERI-MJD" t)
      (esetf (orbital-elements:comet-elem-orbinc elem) "ORBINC" t)
      (esetf (orbital-elements:comet-elem-anode elem) "ANODE" t)
      (esetf (orbital-elements:comet-elem-q elem) "PERIHELION" t)
      (esetf (orbital-elements:comet-elem-perih elem) "ARG-PERIHELION" t)
      (esetf (orbital-elements:comet-elem-e elem) "ECCENTRICITY" t))
    elem))




;; compute the RA,DEC of an object, using the method given, either an
;; orbit, or JPL-ORBIT, MPC-ORBIT, JPL-EPHEM
(defun get-ra-dec-with-method (&key object-name orbit method mjd observatory)
  (cond ((and (equalp method "ORBIT") orbit)
	 (ignore-errors
	  (slalib-ephem:compute-radecr-from-comet-elem-for-observatory
	   orbit mjd observatory :perturb t)))
	;;
	((equalp method "JPL-ORBIT")
	 (let ((elem (ignore-errors
		      (jpl-horizons:get-jpl-horizons-elements-with-caching
		       object-name :mjd mjd))))
	   (when elem
	     (ignore-errors
	      (slalib-ephem:compute-radecr-from-comet-elem-for-observatory
	       elem mjd observatory :perturb nil))))) ;; no perturb needed
	;;
	((equalp method "MPC-ORBIT")
	 (let ((elem (ignore-errors
		      (mpc:get-mpc-elements-with-caching object-name))))
	   (when elem
	     (ignore-errors
	      (slalib-ephem:compute-radecr-from-comet-elem-for-observatory
	       elem mjd observatory :perturb t))))) ;; no perturb needed
	;;
	((equalp method "JPL-EPHEM")
	 (ignore-errors (jpl-horizons:get-jpl-radecr-and-rates-for-observatory
			 object-name mjd observatory)))))


;; similar to above, but return pixel x and y
(defun get-pixel-xy-for-object-with-method
    (&key wcs object-name orbit method mjd observatory)
  (multiple-value-bind (ra dec)
      (get-ra-dec-with-method :object-name object-name :orbit orbit :method method
			      :mjd mjd :observatory observatory)
    (when ra
      (ignore-errors
       (wcs:wcs-convert-ra-dec-to-pix-xy wcs ra dec)))))


(defun get-orbit-using-method (orbit-thing object-name
			       &key (ntries 3) (sleep 10) mjd)
  "Try to get a ORBITAL-ELEMENTS:COMET-ELEM orbit if it is a JSON orbit
(parse it), or string 'MPC-ORBIT' or string 'JPL-ORBIT'

Returns (values NIL ERROR) on failure of orbit retrieval."
  (cond ((slalib-ephem:comet-elem-p orbit-thing)
	 orbit-thing)
	((hash-table-p orbit-thing) ;; its a JSON-orbit
	 (ignore-errors (json-to-comet-elem orbit-thing)))
	((equalp orbit-thing "MPC-ORBIT")
	 (loop for i below ntries
	       for orbit = (ignore-errors
			    (mpc:get-mpc-elements-with-caching
			     object-name))
	       when (plusp ntries) do (sleep sleep)
	       when orbit
		 do (return orbit)
	       finally
		  (return (values nil
				  (make-instance
				   'simple-error
				   :format-control (format  nil "Failed to retrieve MPC orbit for ~A"
							    object-name)
				   :format-arguments nil)))))
	((equalp orbit-thing "JPL-ORBIT")
	 (loop for i below ntries
	       for orbit = (ignore-errors
			    (jpl-horizons:get-jpl-horizons-elements-with-caching
			     object-name :mjd mjd))
	       when (plusp ntries) do (sleep sleep)
	       when orbit
		 do (return orbit)
	       finally
		  (return (values nil
				  (make-instance
				   'simple-error
				   :format-control (format nil "Failed to retrieve JPL orbit for ~A"
							   object-name)
				   :format-arguments nil)))))))
		    



(defun get-fits-naxes (fits-file &key extension)
  "Get #(NAXIS1 NAXIS2) for a fits file, accounting for  possibility it might be fzip."
  (cf:maybe-with-open-fits-file (fits-file ff) 
    (let ((im-ext (or extension
		      (instrument-id:get-image-extension-for-onechip-fits ff))))
      (cf:with-fits-extension (ff im-ext)
	(cf:fits-file-current-image-size ff)))))
	  




(defun compute-xypix-using-orbit-and-wcs
    (comet-elem mjd wcs 
	      &key
	      naxis1 naxis2
	      (perturb t) 
	      (observatory "geocenter"))
      "Given COMET-ELEM, MJD, and WCS, Give the XPIX,YPIX.   Returns NIL
on failure.  Also performs a fits bounds check if NAXIS1 and NAXIS2
are both provided, for cases where the object might be out of bounds.

Returns (VALUES XPIX YPIX RA DEC  OUT-OF-BOUNDS
         OUT-OF-BOUNDS-DIST/DEG  ;; how far out of bounds; negative if in-bounds
         ERROR)

where OUT-OF-BOUNDS-DIST/DEG is NIL if in bounds, or the number of
degrees the object is outside the circle cirumscribing the image denoted
by WCS,NAXIS1,NAXIS2."
  (when (not (wcs:wcs-2d-p wcs))
    (error "WCS=~A not a valid 2d WCS object." wcs))
  
  (let (ra dec ra-cent dec-cent ra-corn dec-corn
	xpix ypix
	(pix-scale/deg (/ (wcs:get-pixel-scale-for-wcs wcs) 3600d0))
	field-radius object-dist
	out-of-bounds-dist/deg err)
  
    (multiple-value-setq (ra dec)
      (slalib-ephem:compute-radecr-from-comet-elem-for-observatory
       comet-elem mjd observatory :perturb perturb))
    
    ;; perform additional bounds checking 
    (when (and naxis1 naxis2)
      (multiple-value-setq (ra-cent dec-cent) ;; center ra,dec
	(wcs:wcs-convert-pix-xy-to-ra-dec wcs (* 0.5d0 naxis1) (* 0.5d0 naxis2)))
      (multiple-value-setq (ra-corn dec-corn) ;; corner ra,dec
	(wcs:wcs-convert-pix-xy-to-ra-dec wcs 0d0 0d0))
      ;; field radius is a bit bogus because field is a rectangle, but good enough
      (setf field-radius (astro-coords:sky-angle ra-corn dec-corn ra-cent dec-cent :units :degrees))
      ;; dist from center to obj
      (setf object-dist (astro-coords:sky-angle ra dec ra-cent dec-cent :units :degrees))
      (if (> object-dist field-radius)
	  (setf out-of-bounds-dist/deg (- object-dist field-radius))))
    
    ;; we ignore the errors when conveting RA,DEC to pix xy because this may result
    ;; from a non-linear WCS system.
    (multiple-value-setq (xpix ypix)
      (ignore-errors (wcs:wcs-convert-ra-dec-to-pix-xy wcs ra dec)))
    (when (not xpix)
      (setf err ypix)
      (setf ypix nil))
    
    (flet ((linedist (x1 y1 x2 y2) ;; distance of line connecting x1,y1 to x2,y2 to x0,y0 in deg
	     (let ((x0 xpix)
		   (y0 ypix))
	       (* pix-scale/deg
		  (/ (abs (-
			   (* (- x2 x1) (- y1 y0))
			   (* (- x1 x0) (- y2 y1))))
		     (sqrt
		      (+ (expt (- x2 x1) 2)
			 (expt (- y2 y1) 2))))))))
      ;; now set out-of-bounds-dist/deg using xpix,ypix, if not set already, to
      ;; the closest distance of xpix,ypix to the boundary of the image.
      (when (and
	     naxis1 naxis2 ;; not always given
	     xpix ypix (not out-of-bounds-dist/deg)
	     (or (< xpix 0) (> xpix naxis1)
		 (< ypix 0) (> ypix naxis2)))
	
	(setf out-of-bounds-dist/deg
	      (min
	       (linedist 0      0        0      naxis2)
	       (linedist 0      0        naxis1 0)
	       (linedist naxis1 0        naxis1 naxis2)
	       (linedist 0      naxis2   naxis1 naxis2))))
      
      ;; check how far safely in the image the object is
      (when (and
	     naxis1 naxis2 ;; not always given
	     xpix ypix
	     (not out-of-bounds-dist/deg)
	(setf out-of-bounds-dist/deg
	      (* -1 ;; negative means in-bounds
		 (min
		  (linedist 0      0        0      naxis2)
		  (linedist 0      0        naxis1 0)
		  (linedist naxis1 0        naxis1 naxis2)
		  (linedist 0      naxis2   naxis1 naxis2)))))))
    
    
    (values xpix ypix ra dec out-of-bounds-dist/deg err)))
  


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; define position angle as UP=1,0 90=0,1, -90=0,-1
;; (0,1)->0; (1,0)->90; (-1,0)->-90; (-1,-1)->45; (1,-1)->-45
(defun position-angle-from-xy (x y)
  (let ((pa (* (/ 180 pi) (atan (* 1d0 x) (* 1d0 y)))))
    ;; PA is always in top 2 quadrants, so in [-90,90]
    (when (> pa 90)
      (decf pa 180))
    (when (< pa -90)
       (incf pa 180))
    pa))

(defun position-angle-pixels-to-sky (theta-pixels wcs)
  "Convert an angle (0 is +Y, rotating into +X) into position angle on the sky"
  (let ((dx (sin (* (/ pi 180) theta-pixels))) ;; because theta=0 is +Y, not +X
	(dy (cos (* (/ pi 180) theta-pixels))))
    (multiple-value-bind (dx-sky dy-sky)
	(wcs:wcs-convert-pix-xy-to-world-xy
	 wcs
	 (+ dx (wcs:wcs-2d-crpix1 wcs))
	 (+ dy (wcs:wcs-2d-crpix2 wcs)))
      (position-angle-from-xy dx-sky dy-sky))))
      

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun error-object-to-string (err)  ;; remove excess spaces from error string
  (let ((string (format nil "~A" err)))
    (with-output-to-string (s)
      (loop with i-space = 0
	    for c across string
	    do
	       (cond ((eql c #\space)
		      (incf i-space)
		      (when (< i-space 2) (write-char c s)))
		     ((eql c #\newline)
		      (when (zerop i-space)
			(write-char #\space s)
			(setf i-space 1)))
		     (t
		      (setf i-space 0)
		      (write-char c s)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun build-error-hash-json (&key name desc)
  (alexandria:alist-hash-table
   `(("TYPE" . "ERROR")
     ("ERROR" . ,name)
     ("DESCRIPTION" . ,desc))
   :test 'equalp))


(defun subtract-mag-from-totmag (mag-tot mag-extra)
  "Given MAG-TOT and some extra mag MAG-EXTRA (like a nucleus),
return the mag after subtracting the flux of MAG-EXTRA."
  (let* ((flux-tot   (expt 10 (/ mag-tot -2.5)))
	 (flux-extra (expt 10 (/ mag-extra -2.5)))
	 (flux-final (- flux-tot flux-extra)))
    (* -2.5 (log flux-final 10))))


;; 
(defun equatorial-xyz-to-ecliptic (x y z)
  (let ((v (make-array 3 :element-type 'double-float)))
    (setf (aref v 0) (* 1d0 x))
    (setf (aref v 1) (* 1d0 y))
    (setf (aref v 2) (* 1d0 z))
    (let ((vv (astro-coords:equatorial-to-ecliptic-xyz v)))
      (values (aref vv 0)
	      (aref vv 1)
	      (aref vv 2)))))
