
;; ATLAS refcat catalog



(in-package astro-catalog)








(defclass refcat-catalog (astro-catalog) ())

(defmethod object-mag ((acat refcat-catalog) mag-name i  &key (error-if-not-exist t))
  (multiple-value-bind (mag mag-err)
      (cond ((eq mag-name :g-gaia) (values (get-value acat :gaia i) 
					   (get-value acat :dgaia i)))
	    ;; PS1 mags
	    ((eq mag-name :g) (values (get-value acat :g i)
				      (get-value acat :dg i)))
	    ((eq mag-name :r) (values (get-value acat :r i)
				      (get-value acat :dr i)))
	    ((eq mag-name :i) (values (get-value acat :i i)
				      (get-value acat :di i)))
	    ((eq mag-name :z) (values (get-value acat :z i)
				      (get-value acat :dz i)))
	    ;; 2MASS mags
	    ((eq mag-name :j) (values (get-value acat :j i)
				      (get-value acat :dj i)))
	    ((eq mag-name :h) (values (get-value acat :h i)
				      (get-value acat :dh i)))
	    ((eq mag-name :k) (values (get-value acat :k i)
				      (get-value acat :dk i)))
	    ;;
	    (t
	     (if error-if-not-exist
		 (error "Magnitude type ~A unknown in ~A" mag-name acat)
		 nil)))
    (if mag
	(values mag mag-err
		;; flag a bad mag as one out of range
		(not (< 1 mag 30))))))


;; these errors are probably roughly OK
;; actually, GAIA is 26 uas for bright stars, and 500 uas for 20 mag
(defmethod object-ra-err ((acat refcat-catalog) i)
  (/ 1d-3 3600)) ;; in degrees

(defmethod object-dec-err ((acat refcat-catalog) i)
  (/ 1d-3 3600)) ;; in degrees


;; all GAIA objects are stellar
(defmethod object-type ((acat refcat-catalog) i)
  (declare (ignorable acat i))
  :star)

(defun read-refcat-catalog-object (ra-deg dec-deg radius-deg)

  (let* ((refcat (refcat:read-refcat-entries-for-position ra-deg dec-deg radius-deg))
	 (refcat-fields ;; as keywords 
	   (mapcar (lambda (k) (intern (string k) :keyword)) refcat::*refcat-fields*))
	 (nf (length refcat-fields)) ;; add one field for ID
	 (nd (length refcat))
	 (data (make-array nf))
	 (example-datum (if (plusp (length refcat))
			    (first refcat)
			    (make-array nf :element-type t))))

    ;; make arrays of the correct type in data, and fill
    (loop for i below nf
	  for example-element across example-datum
	  for type = (cond ((typep example-element 'double-float) 'double-float)
			   ((typep example-element 'single-float) 'single-float)
			   ((typep example-element 'fixnum) 'fixnum)
			   (t t))			   
	  do (setf (aref data i) (make-array nd :element-type t))
	     (loop with vec = (aref data i) 
		   for j below nd
		   for entry in refcat
		   do (setf (aref vec j) (aref entry i))))
    ;; note that refcat seems to have valid proper motions everywhere
    (mark-astro-catalog-ok ;; use method to mark objects that are OK
     (make-instance 'refcat-catalog
		    :n (if data 
			   (length (aref data 0))
			   0)
		    :ra-center (float ra-deg 1d0) :dec-center (float dec-deg 1d0)
		    :radius-deg (float radius-deg 1d0)
		    :data data
		    :fields refcat-fields
		    :available-mags '(:g-gaia :g :r :i :z :j :h :k)
		    :%map (make-map refcat-fields)))))


;; a bit backwards to make catalog from the object ...
(defun read-refcat-catalog  (ra-deg dec-deg radius-deg)
  (let ((object (read-refcat-catalog-object ra-deg dec-deg radius-deg)))
    (values
     (astro-catalog-data object)
     (astro-catalog-fields object))))


(defmethod object-proper-motions ((acat refcat-catalog) (i fixnum))
  (values (get-value acat :pmra i)
	  (get-value acat :pmdec i)))


(defmethod object-ra-dec ((acat refcat-catalog) (i fixnum)  &key mjd)
  (declare (ignorable mjd))
  (let ((ra  (get-value acat :ra i))
	(dec (get-value acat :dec i))
	(dra/dt (get-value acat :pmra i))    ;; mas/yr
	(ddec/dt (get-value acat :pmdec i))) ;; mas/yr

    ;; refcat seems to have valid proper motion everywhere so we don't check for
    ;; invalid values like with other catalogs
    
    ;; shift to epoch mjd
    (when mjd
      (let* ((years-difference (/ (- mjd refcat:+refcat-epoch+) 365.25d0))
	     (delta-ra  (* years-difference 1d-3 dra/dt))    ;; arcsec
	     (delta-dec (* years-difference 1d-3 ddec/dt)))  ;; arcsec
	(multiple-value-setq (ra dec)
	  (astro-coords:sky-angles-slew ra dec delta-ra delta-dec :units :arcsec))))
    
    (values ra dec 
	    (object-ra-err  acat i)
	    (object-dec-err  acat i)
	    (if mjd t nil)))) ;; NIL means 'no adjustment by MJD made'

(defmethod object-ra  ((acat refcat-catalog) (i fixnum)  &key mjd)
  (multiple-value-bind (ra dec dra ddec mjd-adj-p)
      (object-ra-dec acat i :mjd mjd)
    (declare (ignorable dec ddec))
    (values ra dra mjd-adj-p)))


(defmethod object-dec  ((acat refcat-catalog) (i fixnum)  &key mjd)
  (multiple-value-bind (ra dec dra ddec mjd-adj-p)
      (object-ra-dec acat i :mjd mjd)
    (declare (ignorable ra dra))
    (values dec ddec mjd-adj-p)))
