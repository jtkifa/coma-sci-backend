

#|

routines for caching catalog objects so they need to be retrieved only once

The main routine is

   (GET-CACHED-CATALOG-OBJECT RA DEC RADIUS RADIUS-TYPE)

   where CATALOG-TYPE is one of the allowed subclasses of
   ASTRO-CATALOG:CATALOG-OBJECT, in the variable
   *allowed-cache-catalog-types*

         '(2mass-point-source-catalog 
           usno-b1-catalog
           sdss7-catalog
           sdss8-catalog
           sdss9-catalog
           gaia-dr1-catalog
           psps-3pi-mean-psf-mag-catalog
           psps-3pi-stack-psf-mag-catalog
           psps-3pi-mean-kron-mag-catalog
           psps-3pi-stack-kron-mag-catalog)


   If a cached catalog that fully coveres the radius around ra,dec
   exists, it is returned instead of returning one from the net.


|#

(in-package astro-catalog)

;; allowed catalog types corresponding to catalog classes
(defparameter *allowed-cache-catalog-types* 
  '(2mass-point-source-catalog 
    usno-b1-catalog
    sdss7-catalog
    sdss8-catalog
    sdss9-catalog
    gaia-dr1-catalog gaia-dr2-catalog gaia-dr3-catalog
    refcat-catalog
    psps-3pi-mean-psf-mag-catalog
    psps-3pi-stack-psf-mag-catalog
    psps-3pi-mean-kron-mag-catalog
    psps-3pi-stack-kron-mag-catalog))

(defclass catalog-cache ()
    ;; the KEY is the CATALOG-TYPE and the VAL is a list
    ;; of catalogs
    ((hash  :accessor catalog-cache-hash
	    :initform (make-hash-table :test 'equalp)
	    :initarg :hash)
     (lock  :accessor catalog-cache-lock 
	    :initform (bordeaux-threads:make-lock "phot-calib:*catalog-cache* lock"))
     ;; number of catalogs stored
     (n-catalogs :accessor catalog-cache-n-catalogs 
		 :initform 0)
     ;; max number of catalogs allowed
     (n-catalogs-max :accessor catalog-cache-n-catalogs-max
		     :initform 50
		     :initarg :n-catalogs-max)))


(defvar *default-catalog-cache* (make-instance 'catalog-cache))

(defun clear-catalog-cache (&key (catalog-cache *default-catalog-cache*))
  (bordeaux-threads:with-lock-held ((catalog-cache-lock catalog-cache))
    (clrhash (catalog-cache-hash catalog-cache))
    (setf (catalog-cache-n-catalogs catalog-cache) 0)))


;; remotely get a catalog of a type
(defun get-catalog-object-of-type (ra dec radius catalog-type)
  (cond 
    ((eq catalog-type 'sdss7-catalog)
     (read-sdss7-catalog-object ra dec radius :method :vizquery))
    ;;
    ((eq catalog-type 'sdss8-catalog)
     (read-sdss8-catalog-object ra dec radius :method :vizquery))
    ;;
    ((eq catalog-type 'sdss9-catalog)
     (read-sdss9-catalog-object ra dec radius :method :vizquery))
    ;;
    ((eq catalog-type 'usno-b1-catalog)
     (read-usno-b1-catalog-object 
      ra dec radius :method :vizquery))
    ;;
    ((eq catalog-type '2mass-point-source-catalog)
     (read-2mass-point-source-catalog-object 
      ra dec radius :method :vizquery))
    ;;
    ;; different GAIA releases
    ((eq catalog-type 'gaia-dr1-catalog)
     (read-gaia-dr1-catalog-object
      ra dec radius :method :vizquery))
    
    ((eq catalog-type 'gaia-dr2-catalog)
     (read-gaia-dr2-catalog-object
      ra dec radius :method :vizquery))
    
    ((eq catalog-type 'gaia-dr3-catalog)
     (read-gaia-dr3-catalog-object
      ra dec radius :method :vizquery))
    ;;
    ((eq catalog-type 'refcat-catalog)
     (read-refcat-catalog-object
      ra dec radius))
    ;;
    ;; various psps catalog types
    ((eq catalog-type 'psps-3pi-mean-psf-mag-catalog)
     (read-psps-3pi-catalog-object
      ra dec radius :mag-source :mean :mag-type :psf ))

    ((eq catalog-type 'psps-3pi-stack-psf-mag-catalog)
     (read-psps-3pi-catalog-object
      ra dec radius :mag-source :stack :mag-type :psf ))

    ((eq catalog-type 'psps-3pi-mean-kron-mag-catalog)
     (read-psps-3pi-catalog-object
      ra dec radius :mag-source :mean :mag-type :kron ))

    ((eq catalog-type 'psps-3pi-stack-kron-mag-catalog)
     (read-psps-3pi-catalog-object
      ra dec radius :mag-source :stack :mag-type :kron ))
    ;;
    ;;
    (t
     (error "Unknown catalog class ~A" catalog-type))))




;; Return a cached catalog that fully covers RA,DEC,RADIUS
;; (all in degrees)
(defun get-cached-catalog (ra dec radius/deg catalog-type
			   &key (catalog-cache *default-catalog-cache*))
  (bordeaux-threads:with-lock-held ((catalog-cache-lock catalog-cache))
    (let ((catalog-list (gethash catalog-type (catalog-cache-hash catalog-cache))))
      (loop for catalog in catalog-list
	    for rac =     (ra-center catalog) 
	    for decc =    (dec-center catalog)
	    for radiusc = (radius-deg catalog)
	    for dist = (astro-coords:sky-angle 
			(float ra 1d0) (float dec 1d0) (float rac 1d0) (float decc 1d0)
			:units :degrees)
	    ;; the required circle on sky fits entirely into the catalog circle
	    when (<= (+ dist radius/deg) radiusc)
	      do (return catalog)))))

;; IMPROVE THIS - need better procedure to clear overflowing catalogs, maybe
;; keep track of most/least used newest/oldest catalogs?
;; MUST BE RUN WITHIN A LOCK
(defun %maybe-prune-catalogs (&key (catalog-cache *default-catalog-cache*))
  (when (>= (catalog-cache-n-catalogs catalog-cache)
	    (catalog-cache-n-catalogs-max catalog-cache))
    (setf (catalog-cache-n-catalogs catalog-cache) 0)
    (clrhash (catalog-cache-hash catalog-cache))))
    

;; put a catalog into the cache
(defun insert-catalog-into-cache (catalog &key (catalog-cache *default-catalog-cache*))
  (bordeaux-threads:with-lock-held ((catalog-cache-lock catalog-cache))
    (%maybe-prune-catalogs :catalog-cache catalog-cache) ;; run within lock
    (let ((catalog-type (type-of catalog)))
      (setf (gethash catalog-type (catalog-cache-hash catalog-cache))
	    (push catalog (gethash catalog-type (catalog-cache-hash catalog-cache))))
      (incf (catalog-cache-n-catalogs catalog-cache))
      catalog)))
  
;; get a catalog, with caching
(defun get-cached-catalog-object (ra dec radius/deg catalog-type
				     &key (catalog-cache *default-catalog-cache*)
				     (r-expand 1.1))
  "Get an astro-catalog using the caching mechanism.

CATALOG-TYPE is a symbol like 'ASTRO-CATALOG:SDSS8-CATALOG, naming a catalog class.

The RADIUS is expanded by R-EXPAND so that if this catalog is
retrieved remotely, slight dithers in RA,DEC will still allow it to 
be found in the cache in subsequent requests."
  (when (not (member catalog-type *allowed-cache-catalog-types*))
    (error "CATALOG-TYPE must be one of ~A" *allowed-cache-catalog-types*))
  (or (get-cached-catalog ra dec radius/deg catalog-type :catalog-cache catalog-cache)
      (insert-catalog-into-cache 
       (get-catalog-object-of-type ra dec (* r-expand radius/deg) catalog-type)
       :catalog-cache catalog-cache)))



(defun describe-catalog-cache (&key (catalog-cache *default-catalog-cache*))
  (loop with hash = (catalog-cache-hash catalog-cache)
	for key being the hash-key of hash
	for val being the hash-value of hash
	collect (list key val)))
