
;; routines to download astronomical catalogs



(in-package astro-catalog)


;; source name for vizquery
(defparameter *gaia-dr1-vizquery-source* "I/337/gaia")
(defparameter *gaia-dr2-vizquery-source* "I/345/gaia2")
(defparameter *gaia-dr3-vizquery-source* "I/355/gaiadr3") ;; DR3, not EDR3 (early release)

(defconstant +gaia-dr1-epoch+ 57023.0d0) ;; 2015.0 MJD
(defconstant +gaia-dr2-epoch+ 57023.0d0) ;; 2015.0 MJD
(defconstant +gaia-dr3-epoch+ 57388.0d0) ;; 2016.0 MJD
  

;; gaia is not at all sites
(defparameter *gaia-vizquery-sites*
  '("cds" ;; canonical, France
    "cn"  ;; fast
    "jp"  ;; has catalog, equal to 'adac'
    #+nil "cadc"))  ;; has been timing out

;; the different catalogs have slightly different keys
(defun read-gaia-dr1-catalog-vizquery (ra-deg dec-deg radius-deg
					      &key (vizquery-site-list *gaia-vizquery-sites*))
    (multiple-value-bind (data-vec keys-or-error)
	(ignore-errors
	 (run-vizquery-and-parse/multisites ra-deg dec-deg
					    (* radius-deg 60.0)
					    *gaia-dr1-vizquery-source*
					    `(("Source" :id string "")
					      ;; at epoch 2015
					      ("RA_ICRS" :ra double-float ,*invalid-dbl-value*)
					      ("DE_ICRS" :dec double-float ,*invalid-dbl-value*)
					      ;; mas
					      ("e_RA_ICRS" :ra-err double-float ,*invalid-dbl-value*)
					      ("e_DE_ICRS" :dec-err double-float ,*invalid-dbl-value*)
					      ;; proper motions
					      ("pmRA"     :pmra  double-float ,*invalid-dbl-value*)
					      ("pmDE"     :pmdec  double-float ,*invalid-dbl-value*)
					      ;;
					      ("<Gmag>" :g  double-float 0d0)
					      ("<FG>"   :g-flux  double-float 0d0)
					      ("e_<FG>"  :g-flux-err  double-float 0d0))
					    :vizquery-site-list vizquery-site-list))
      ;;
      (if (typep keys-or-error 'error)
	  (error keys-or-error)
	  (values data-vec keys-or-error))))

;; the different catalogs have slightly different keys
(defun read-gaia-dr2-catalog-vizquery (ra-deg dec-deg radius-deg
				       &key (vizquery-site-list *gaia-vizquery-sites*))
    (multiple-value-bind (data-vec keys-or-error)
	(ignore-errors
	 (run-vizquery-and-parse/multisites ra-deg dec-deg
					    (* radius-deg 60.0)
					    *gaia-dr2-vizquery-source*
					    `(("Source" :id string "")
					      ;; at epoch 2015
					      ("RA_ICRS" :ra double-float ,*invalid-dbl-value*)
					      ("DE_ICRS" :dec double-float ,*invalid-dbl-value*)
					      ;; mas
					      ("e_RA_ICRS" :ra-err double-float ,*invalid-dbl-value*)
					      ("e_DE_ICRS" :dec-err double-float ,*invalid-dbl-value*)
					      ;; proper motions
					      ("pmRA"     :pmra  double-float ,*invalid-dbl-value*)
					      ("pmDE"     :pmdec  double-float ,*invalid-dbl-value*)
					      ;;
					      ("Gmag" :g  double-float 0d0)
					      ("FG"   :g-flux  double-float 0d0)
					      ("e_FG"  :g-flux-err  double-float 0d0))
					    :vizquery-site-list vizquery-site-list))
      ;;
      (if (typep keys-or-error 'error)
	  (error keys-or-error)
	  (values data-vec keys-or-error))))


;; the different catalogs have slightly different keys
(defun read-gaia-dr3-catalog-vizquery (ra-deg dec-deg radius-deg
				       &key (vizquery-site-list *gaia-vizquery-sites*))
    (multiple-value-bind (data-vec keys-or-error)
	(ignore-errors
	 (run-vizquery-and-parse/multisites ra-deg dec-deg
					    (* radius-deg 60.0)
					    *gaia-dr3-vizquery-source*
					    `(("Source" :id string "")
					      ;; at epoch 2016 
					      ("RA_ICRS" :ra double-float ,*invalid-dbl-value*)
					      ("DE_ICRS" :dec double-float ,*invalid-dbl-value*)
					      ;; mas
					      ("e_RA_ICRS" :ra-err double-float ,*invalid-dbl-value*)
					      ("e_DE_ICRS" :dec-err double-float ,*invalid-dbl-value*)
					      ;; proper motions
					      ("pmRA"     :pmra  double-float ,*invalid-dbl-value*)
					      ("pmDE"     :pmdec  double-float ,*invalid-dbl-value*)
					      ;;
					      ("Gmag" :g  double-float 0d0)
					      ("FG"   :g-flux  double-float 0d0)
					      ("e_FG"  :g-flux-err  double-float 0d0))
					    :vizquery-site-list vizquery-site-list))
      
      (if (typep keys-or-error 'error)
	  (error keys-or-error)
	  (values data-vec keys-or-error))))







(defun read-gaia-dr1-catalog
    (ra-deg dec-deg radius-deg &key   (method :vizquery))
  "Query GAIA DR1 catalog at RA-DEG,DEC-DEG in circle of size RADIUS-DEG, using
METHOD of :WEB or :VIZQUERY."
  (cond ((eq method :vizquery)
	 (read-gaia-dr1-catalog-vizquery 
	  ra-deg dec-deg radius-deg))
	((eq method :web)
	 (error "Cannot get GAIA DR1 catalog over web"))
	(t
	 (error "Unknown METHOD ~A - must be :WEB or :VIZQUERY" method))))

(defun read-gaia-dr2-catalog
    (ra-deg dec-deg radius-deg &key   (method :vizquery))
  "Query GAIA DR2 catalog at RA-DEG,DEC-DEG in circle of size RADIUS-DEG, using
METHOD of :WEB or :VIZQUERY."
  (cond ((eq method :vizquery)
	 (read-gaia-dr2-catalog-vizquery 
	  ra-deg dec-deg radius-deg))
	((eq method :web)
	 (error "Cannot get GAIA DR2 catalog over web"))
	(t
	 (error "Unknown METHOD ~A - must be :WEB or :VIZQUERY" method))))

(defun read-gaia-dr3-catalog
    (ra-deg dec-deg radius-deg &key   (method :vizquery))
  "Query GAIA DR2 catalog at RA-DEG,DEC-DEG in circle of size RADIUS-DEG, using
METHOD of :WEB or :VIZQUERY."
  (cond ((eq method :vizquery)
	 (read-gaia-dr3-catalog-vizquery 
	  ra-deg dec-deg radius-deg))
	((eq method :web)
	 (error "Cannot get GAIA DR3 catalog over web"))
	(t
	 (error "Unknown METHOD ~A - must be :WEB or :VIZQUERY" method))))



(defclass %gaia-catalog (astro-catalog)
  ((gaia-epoch :accessor gaia-epoch :initform nil)))
(defclass gaia-dr1-catalog (%gaia-catalog) ((gaia-epoch :initform +gaia-dr1-epoch+)))
(defclass gaia-dr2-catalog (%gaia-catalog) ((gaia-epoch :initform +gaia-dr2-epoch+)))
(defclass gaia-dr3-catalog (%gaia-catalog) ((gaia-epoch :initform +gaia-dr3-epoch+)))



(defmethod object-mag ((acat %gaia-catalog) mag-name i &key (error-if-not-exist t))
  (cond ((eq mag-name :g)
	 (let* ((mag  (get-value acat :g i))
		(flux (get-value acat :g-flux i))
		(fluxerr (get-value acat :g-flux-err i))
		(mag-err
		  (if (and (< 0 flux 1d80)
			   (< 0 fluxerr 1d80))
		      (/ fluxerr flux)
		      99d0))
		(is-bad (not (<= 0 mag 25))))
	   (values mag mag-err is-bad)))
	;; no such mag
	(t
	 (if error-if-not-exist
	     (error "Magnitude type ~A unknown in ~A" mag-name acat)
	     nil))))
	
	

(defun read-gaia-dr1-catalog-object (ra-deg dec-deg radius-deg &key
					       (method :vizquery))
  (multiple-value-bind (data fields)
      (read-gaia-dr1-catalog ra-deg dec-deg radius-deg 
				       :method method)
    (mark-astro-catalog-ok ;; use method to mark objects that are OK
     (make-instance 'gaia-dr1-catalog
		    :n (if data 
			   (length (aref data 0))
			   0)
		    :ra-center (float ra-deg 1d0) :dec-center (float dec-deg 1d0) 
		    :radius-deg (float radius-deg 1d0)
		    :data data
		    :fields fields
		    :available-mags '(:g)
		    :%map (make-map fields)))))

(defun read-gaia-dr2-catalog-object (ra-deg dec-deg radius-deg &key
					       (method :vizquery))
  (multiple-value-bind (data fields)
      (read-gaia-dr2-catalog ra-deg dec-deg radius-deg 
				       :method method)
    (mark-astro-catalog-ok ;; use method to mark objects that are OK
     (make-instance 'gaia-dr2-catalog
		    :n (if data 
			   (length (aref data 0))
			   0)
		    :ra-center (float ra-deg 1d0) :dec-center (float dec-deg 1d0) 
		    :radius-deg (float radius-deg 1d0)
		    :data data
		    :fields fields
		    :available-mags '(:g)
		    :%map (make-map fields)))))


(defun read-gaia-dr3-catalog-object (ra-deg dec-deg radius-deg &key
					       (method :vizquery))
  (multiple-value-bind (data fields)
      (read-gaia-dr3-catalog ra-deg dec-deg radius-deg 
				       :method method)
    (mark-astro-catalog-ok ;; use method to mark objects that are OK
     (make-instance 'gaia-dr3-catalog
		    :n (if data 
			   (length (aref data 0))
			   0)
		    :ra-center (float ra-deg 1d0) :dec-center (float dec-deg 1d0) 
		    :radius-deg (float radius-deg 1d0)
		    :data data
		    :fields fields
		    :available-mags '(:g)
		    :%map (make-map fields)))))

;; Gaia delivers proper motion in mas/yr
(defmethod object-proper-motions ((acat %gaia-catalog) (i fixnum))
  (values (get-value acat :pmra i)
	  (get-value acat :pmdec i)))


(defmethod object-ra-dec ((acat %gaia-catalog) (i fixnum)  &key mjd)
  (declare (ignorable mjd))
  (let* ((ra  (get-value acat :ra i))
	 (dec (get-value acat :dec i))
	 (dra/dt (get-value acat :pmra i))    ;; mas/yr
	 (ddec/dt (get-value acat :pmdec i)) ;; mas/yr
	 (epoch (gaia-epoch acat))
	 ;; it's possible some GAIA objects do not have proper motion
	 (good-proper-motion
	   (and (not (invalid-value-p dra/dt))
		(not (invalid-value-p ddec/dt))))
	 (adjust-by-pm (and mjd epoch good-proper-motion
			    t)))

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

(defmethod object-ra  ((acat %gaia-catalog) (i fixnum)  &key mjd)
  (multiple-value-bind (ra dec dra ddec mjd-adj-p)
      (object-ra-dec acat i :mjd mjd)
    (declare (ignorable dec ddec))
    (values ra dra mjd-adj-p)))


(defmethod object-dec  ((acat %gaia-catalog) (i fixnum)  &key mjd)
  (multiple-value-bind (ra dec dra ddec mjd-adj-p)
      (object-ra-dec acat i :mjd mjd)
    (declare (ignorable ra dra))
    (values dec ddec mjd-adj-p)))

;; Gaia errors are in mas, not arcsec
(defmethod object-ra-err  ((acat %gaia-catalog) (i fixnum))
  (values (* 1e-3 (get-value acat :ra-err i))))
  
(defmethod object-dec-err  ((acat %gaia-catalog) (i fixnum))
  (values (* 1e-3 (get-value acat :dec-err i))))

