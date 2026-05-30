
;; class and generic methods for an astro catalog

(in-package astro-catalog)

(defclass astro-catalog ()
  ((n :accessor astro-catalog-n  :initarg :n :initform 0)
   (ra-center :accessor ra-center :initarg :ra-center :initform 0d0)
   (dec-center :accessor dec-center :initarg :dec-center :initform 0d0)
   (radius-deg :accessor radius-deg :initarg :radius-deg :initform 0d0)
   ;; data is a vector of vectors, each containing a the data for one field
   (data :accessor astro-catalog-data :initarg :data)
   ;; fields is a list of keywords representing the fields available
   (fields :accessor astro-catalog-fields :initarg :fields)
   ;; which magnitudes (eg :vj, :usdss) are available
   (available-mags :accessor available-mags :initarg :available-mags)
   ;; the default position error for objects in a catalog
   (defult-position-error :accessor default-position-error
			  :initarg :default-position-error
			  :initform #.(/ 0.35 3500d0))
   ;; %map is a hash, where KEY=FIELD_KEYWORD VAL=Index-in-data
   ;;  eg, (gethash :usdss %map)==>3 and (aref data 3) is the usdss mag vector
   (%map :accessor %map :initarg :%map)))


(defmethod print-object ((cat astro-catalog) stream)
  (format stream "#<~A N=~A RA=~,4F Dec=~,4F R=~,3F deg>" (type-of cat) 
	  (astro-catalog-n cat)
	  (ra-center cat)
	  (dec-center cat)
	  (radius-deg cat)))

(defun make-map (field-list)
  (loop with h = (make-hash-table :test 'eq)
	for field in field-list
	for i from 0
	do (setf (gethash field h) i)
	finally (return h)))


(defgeneric get-astro-catalog-vector (catalog field &key error-if-not-exist))

(defmethod get-astro-catalog-vector ((acat astro-catalog) field &key (error-if-not-exist t))
  (let ((k (gethash field (%map acat))))
    (if (not k)
	(if error-if-not-exist
	    (error "Field ~A not found in ~A" field acat)
	    nil)
	(aref (astro-catalog-data acat) k)))) 

 
(defgeneric get-value (catalog field i &key error-if-not-exist default-value))

(defmethod get-value ((acat astro-catalog) 
		      field i 
		      &key (error-if-not-exist t) (default-value nil) )
  (let ((k (gethash field (%map acat))))
    (if (and (not k) error-if-not-exist)
	(error "Field ~A not found in ~A" field acat))
    (if (not k)
	default-value
	(aref (aref (astro-catalog-data acat) k) i))))
     
(defmethod is-valid-field ((acat astro-catalog) field)
  (gethash field (%map acat)))

(defgeneric object-mag (astro-catalog mag-name i &key error-if-not-exist)
  (:documentation "Returns (VALUES MAG MAG-ERR INVALID),
ie, the Ith magnitude and error as 2 values, 
and the IN-validity of the mag (T/NIL) as a third value.

If the variable ERROR-IF-NOT-EXIST is unset, then a non-existent
magnitude returns NIL instead of throwing an error."))


(defgeneric object-type (astro-catalog i)
  (:documentation "Return the type, :STAR, :GALAXY, :OTHER, :UNKNOWN of object I in astro-catalog"))


(defgeneric object-ra  (astro-catalog i &key mjd)
  (:documentation
   "Return the RA of object I, and uncertainty in arcsec as 2nd value (or
   DEFAULT-POSIITON-ERROR if unknown).  If MJD is set, and the catalog
   supports proper motions, adjust for proper motion to MJD.  The
   third value is T/NIL depending on whether an adjustment by MJD was
   made."))
(defgeneric object-dec  (astro-catalog i &key mjd)
  (:documentation
   "Return the Dec of object I, and uncertainty in arcsec as 2nd value (or
  DEFAULT-POSITION-ERROR if unknown). If MJD is set, and the catalog
  supports proper motions, adjust for proper motion to MJD.  The third
  value is T/NIL depending on whether an adjustment by MJD was
  made."))

(defgeneric object-ra-dec (astro-catalog i &key mjd)
  (:documentation
   "Return (VALUES RA DEC RA-ERROR DEC-ERROR MJD-ADJUSTED-P) for
   object I in ASTRO-CATALOG."))

(defgeneric object-ra-err  (astro-catalog i)
   (:documentation "Return the RA error of object I in ARCSEC, or possibly a default uncertainty"))
(defgeneric object-dec-err  (astro-catalog i)
  (:documentation "Return the Dec error of object I in ARCSEC, or possibly a default uncertainty"))

(defgeneric object-proper-motions (astro-catalog i)
  (:documentation "Return (values d[RA*cos(Dec)]/dt dDec/dt) in mas/yr, or NIL if not 
defined."))




(defgeneric object-id (astro-catalog i)
  (:documentation "Return the ID of object I, or NIL if :ID field is undefined."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; default methods
(defmethod object-ra ((acat astro-catalog) (i fixnum) &key mjd)
  (declare (ignorable mjd))
  (values (get-value acat :ra i)
	  (object-ra-err acat i)
	  nil)) ;; NIL means 'no adjustment by MJD made'a

(defmethod object-ra-err ((acat astro-catalog) (i fixnum))
  (declare (ignore i))
  (default-position-error acat))



;; bogus one arcsec error on ra
(defmethod object-dec ((acat astro-catalog) (i fixnum)  &key mjd)
  (declare (ignorable mjd))
  (values (get-value acat :dec i)
	  (object-ra-err acat i)
	  nil)) ;; NIL means 'no adjustment by MJD made'

(defmethod object-dec-err ((acat astro-catalog) (i fixnum))
  (declare (ignore i))
  (default-position-error acat))



;; both RA and DEC
(defmethod object-ra-dec ((acat astro-catalog) (i fixnum)  &key mjd)
  (declare (ignorable mjd))
  (values (get-value acat :ra i)
	  (get-value acat :dec i)
	  (object-ra-err acat i)
	  (object-dec-err acat i)
	  nil)) ;; NIL means 'no adjustment by MJD made'


(defmethod object-proper-motions ((acat astro-catalog) (i fixnum))
  (declare (ignore i))
  (values nil nil)) ;; none by default


(defmethod object-type ((acat astro-catalog) (i fixnum))
  (declare (ignore acat i))
  :unknown)

;; get the object ID, and NIL if undefined for this catalog
(defmethod object-id ((acat astro-catalog) (i fixnum))
  (get-value acat :id i  :error-if-not-exist nil :default-value nil))

