
;; routines to download SDSS DR8 catalogs in simple manner



(in-package astro-catalog)



(defun read-sdss9-catalog-web
    (ra-deg dec-deg radius-deg &key (get-galaxies t) (get-stars t) (get-unknown t))
  (declare (ignore ra-deg dec-deg radius-deg get-galaxies get-stars get-unknown))
  (error "Getting SDSS9 through web forms is not supported."))



(defun read-sdss9-catalog-vizquery (ra-deg dec-deg radius-deg &key  (catalog "SDSS9"))
  (multiple-value-bind (data-vec keys-or-error)
      (ignore-errors
	(run-vizquery-and-parse/multisites ra-deg dec-deg (* radius-deg 60.0) catalog
				`(("SDSS9" :id string "")
				  ("RA_ICRS" :ra double-float ,*invalid-dbl-value*)
				  ("DE_ICRS" :dec double-float ,*invalid-dbl-value*)
				  ("umag"   :u double-float 0d0)
				  ("e_umag" :u-err double-float 0d0)
				  ("gmag"   :g double-float 0d0)
				  ("e_gmag" :g-err double-float 0d0)
				  ("rmag"   :r double-float 0d0)
				  ("e_rmag" :r-err double-float 0d0)
				  ("imag"   :i double-float 0d0)
				  ("e_imag" :i-err double-float 0d0)
				  ("zmag"   :z double-float 0d0)
				  ("e_zmag" :z-err double-float 0d0)
				  ("cl"     :type  integer))))
    (if (typep keys-or-error 'error)
	(error keys-or-error)
	(values data-vec keys-or-error))))



(defun read-sdss9-catalog (ra-deg dec-deg radius-deg &key  (method :vizquery))
  "Query USNO catalog at RA-DEG,DEC-DEG in circle of size RADIUS-DEG, using
METHOD of :WEB or :VIZQUERY"
  (cond ((eq method :vizquery)
	 (read-sdss9-catalog-vizquery ra-deg dec-deg radius-deg ))
	((eq method :web)
	 (read-sdss9-catalog-web ra-deg dec-deg radius-deg))
	(t
	 (error "Unknown METHOD ~A - must be :WEB or :VIZQUERY" method))))



(defclass sdss9-catalog (sdss-catalog)
  ())

(defmethod object-mag ((acat sdss9-catalog) mag-name i &key (error-if-not-exist t))
  (multiple-value-bind (mag mag-err)
      (cond ((eq mag-name :u) (values (get-value acat :u i) (get-value acat :u-err i)))
	    ((eq mag-name :g) (values (get-value acat :g i) (get-value acat :g-err i)))
	    ((eq mag-name :r) (values (get-value acat :r i) (get-value acat :r-err i)))
	    ((eq mag-name :i) (values (get-value acat :i i) (get-value acat :i-err i)))
	    ((eq mag-name :z) (values (get-value acat :z i) (get-value acat :z-err i)))
	    (t
	     (if error-if-not-exist
		 (error "Magnitude type ~A unknown in ~A" mag-name acat)
		 nil)))
    (if mag
	(values mag mag-err 
		;; sdss seems to mark bad mags as 0.0, so this test will catch them
		(not (< 1d0 mag 30d0))))))


(defmethod object-type ((acat sdss9-catalog) (i fixnum))
  (let ((k (get-value acat :type i)))
    (cond ((= k 0) :unknown)
	  ((= k 6) :star)
	  ((= k 3) :galaxy)
	  (t :other))))


(defun read-sdss9-catalog-object  (ra-deg dec-deg radius-deg
				   &key  
				     (method :vizquery))
    (multiple-value-bind (data fields)
	(read-sdss9-catalog ra-deg dec-deg radius-deg :method method)
      (mark-astro-catalog-ok ;; use method to mark objects that are OK
       (make-instance 'sdss9-catalog
		      :n (if data 
			    (length (aref data 0))
			    0)
		      :ra-center (float ra-deg 1d0) :dec-center (float dec-deg 1d0) 
		      :radius-deg (float radius-deg 1d0)
		      :data data
		      :fields fields
		      :available-mags '(:u :g :r :i :z) 
		      :%map (make-map fields)))))

	
