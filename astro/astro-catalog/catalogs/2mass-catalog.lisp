
;; routines to download astronomical catalogs



(in-package astro-catalog)

;; WARNING - THIS RETURNS JUST THE FIRST PART OF THE VIZQUERY RESPONSE, THE POINT SOURCES!!
(defun read-2mass-point-source-catalog-vizquery (ra-deg dec-deg radius-deg &key)
  (multiple-value-bind (data-vec keys-or-error)
      (ignore-errors
       (run-vizquery-and-parse/multisites ra-deg dec-deg
					  (* radius-deg 60.0) "2MASS-PSC"
				`(("2MASS" :id string "")
				  ("RAJ2000" :ra double-float ,*invalid-dbl-value*)
				  ("DEJ2000" :dec double-float ,*invalid-dbl-value*)
				  ("Jmag" :j  double-float 0d0) 
				  ("e_Jmag" :j-err  double-float 0d0)
				  ("Hmag" :h  double-float 0d0) 
				  ("e_Hmag" :h-err  double-float 0d0) 
				  ("Kmag" :k  double-float 0d0) 
				  ("e_Kmag" :k-err  double-float 0d0)
				  ("Qflg" :qflg string "")
				  ("Rflg" :rflg string "")
				  ("Bflg" :bflg string "")
				  ("Cflg" :cflg string "")
				  ("Xflg" :xflg integer "")
				  ("Aflg" :aflg integer ""))))
    (if (typep keys-or-error 'error)
	(error keys-or-error)
	(values data-vec keys-or-error))))
  




(defun read-2mass-point-source-catalog
    (ra-deg dec-deg radius-deg &key   (method :vizquery))
  "Query 2MASS catalog at RA-DEG,DEC-DEG in circle of size RADIUS-DEG, using
METHOD of :WEB or :VIZQUERY.  Does point sources only."
  (cond ((eq method :vizquery)
	 (read-2mass-point-source-catalog-vizquery 
	  ra-deg dec-deg radius-deg))
	((eq method :web)
	 (error "Cannot get 2MASS catalog over web"))
	(t
	 (error "Unknown METHOD ~A - must be :WEB or :VIZQUERY" method))))


(defclass 2mass-point-source-catalog (astro-catalog)
  ( ;; the default position error for objects in 2mass
   (defult-position-error :accessor default-position-error
     :initarg :default-position-error :initform #.(/ 0.10 3600d0))))



(defmethod object-mag ((acat 2mass-point-source-catalog) mag-name i &key (error-if-not-exist t))
  (multiple-value-bind (mag mag-err)
      (cond ((eq mag-name :j) (values (get-value acat :j i) (get-value acat :j-err i)))
	    ((eq mag-name :h) (values (get-value acat :h i) (get-value acat :h-err i)))
	    ((eq mag-name :k) (values (get-value acat :k i) (get-value acat :k-err i)))
	    (t
	     (if error-if-not-exist
		 (error "Magnitude type ~A unknown in ~A" mag-name acat)
		 nil)))
    (if mag
	(values mag mag-err
		;; flag a bad mag as one out of range
		(not (< -10 mag 30))))))
	
	

(defun read-2mass-point-source-catalog-object (ra-deg dec-deg radius-deg &key
					       (method :vizquery))
  (multiple-value-bind (data fields)
      (read-2mass-point-source-catalog ra-deg dec-deg radius-deg 
				       :method method)
    (mark-astro-catalog-ok ;; use method to mark objects that are OK
     (make-instance '2mass-point-source-catalog
		    :n (if data 
			   (length (aref data 0))
			   0)
		    :ra-center (float ra-deg 1d0) :dec-center (float dec-deg 1d0) 
		    :radius-deg (float radius-deg 1d0)
		    :data data
		    :fields fields
		    :available-mags '(:J :H :K)
		    :%map (make-map fields)))))



