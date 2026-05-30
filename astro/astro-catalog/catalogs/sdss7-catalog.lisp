
;; routines to download SDSS DR7 catalogs in simple manner



(in-package astro-catalog)





;; it might be easier to use the direct 
;; SDSS SQL page at http://cas.sdss.org/astrodr7/en/tools/search/sql.asp
(defun get-sdss7-catalog-page (ra-deg dec-deg radius-deg &key (get-galaxies t) (get-stars t) (get-unknown t))
  (multiple-value-bind (data return-code headers uri)
      (drakma:http-request  "http://cas.sdss.org/astrodr7/en/tools/search/x_iqs.asp"
			    :method :post
			    :content-type "multipart/form-data"
			    :form-data t
			    :parameters `(("format" . "csv")
					  ("limit" . "99999")
					  ("imgparams" . "objid")
					  ("imgparams" . "radec")
					  ("imgparams" . "model_mags")
					  ("imgparams" . "model_magerrs")
					  ("imgparams" . "type")
					  ("specparams" . "none")
					  ("dataset" . "bestdb")
					  ("positionType" . "cone")
					  ("raCenter" . ,(format nil "~F" ra-deg))
					  ("decCenter" . ,(format nil "~F" dec-deg))
					  ("radius" . ,(format nil "~F" (* 60 radius-deg)))
					  ("magType" . "model")
					  ("doGalaxy" . ,(if get-galaxies "on" ""))
					  ("doStar" . ,(if get-stars "on" ""))
					  ("doUnknown" . ,(if get-unknown "on" ""))
					  ("flagsOnList" . "ignore")
					  ))
     (when (not (= return-code 200))
       (error "HTTP error code ~A returned by catalog URI ~A~%" return-code uri))
    (map 'string 'code-char data))) ;; returns binary, so convert to string


(defun read-sdss7-catalog-web  (ra-deg dec-deg radius-deg &key (get-galaxies t) (get-stars t) (get-unknown t))
  "Query SDDS DR7 and return vectors corresponding to 
  id|ra|dec|u|g|r|i|z|modelMagErr_u|modelMagErr_g|modelMagErr_r|modelMagErr_i|modelMagErr_z|type
  0 |1 | 2 |3|4|5|6|7|8   |9            |10           |11           |12           |13      |14

where type is 0=uknown; 1=cosmic ray; 2=defect; 3=galaxy; 
              4=ghost; 5=non-SDSS knownobj; 6=star; 7=trail; 
              8=blank sky; 9=not-a-type

Return NIL for data if no objects found.

A list of column IDs is returned as the 2nd value."

  (let* ((text (get-sdss7-catalog-page ra-deg dec-deg radius-deg 
					 :get-galaxies get-galaxies :get-stars get-stars 
					 :get-unknown get-unknown)))
    (declare (type string text))
    ;; remove commas
    (loop for i below (length text) 
	  when (eql (aref text i) #\,) 
	    do (setf (aref text i) #\space))
    
    ;;
    (values
      (if (> (count #\lf text) 1) ;; sdss has return of one colum name line, and then data
	  (with-input-from-string (s text)
	    (numio:read-cols 
	     s
	     '(:integer 
	       :double-float :double-float ;; ra dec
	       :double-float :double-float :double-float :double-float :double-float ;; u g r i z
	       :double-float :double-float :double-float :double-float :double-float ;; u g r i z 
	       :integer) ;; type
	     :nskip-initial-lines 1))
	  nil)
      ;;
      '(:id :ra :dec :u :g :r :i :z 
	:u-err :g-err :r-err :i-err :z-err
	;;:modelMagErr_u :modelMagErr_g :modelMagErr_r :modelMagErr_i 	:modelMagErr_z
	:type))))

(defun read-sdss7-catalog-vizquery (ra-deg dec-deg radius-deg &key  (catalog "SDSS7"))
  (multiple-value-bind (data-vec keys-or-error)
      (ignore-errors
	(run-vizquery-and-parse/multisites ra-deg dec-deg (* radius-deg 60.0) catalog
				`(("SDSS" :id string "")
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
				  ;; bug in vizier's catalogs removes zmag key
				  ;("zmag"   :z double-float 0d0)
				  ;("e_zmag" :z-err double-float 0d0)
				  ("cl"     :type  integer))))
    (if (typep keys-or-error 'error)
	(error keys-or-error)
	(values data-vec keys-or-error))))
 


(defun read-sdss7-catalog (ra-deg dec-deg radius-deg &key  (method :vizquery))
  "Query USNO catalog at RA-DEG,DEC-DEG in circle of size RADIUS-DEG, using
METHOD of :WEB or :VIZQUERY"
  (cond ((eq method :vizquery)
	 (read-sdss7-catalog-vizquery ra-deg dec-deg radius-deg ))
	((eq method :web)
	 (read-sdss7-catalog-web ra-deg dec-deg radius-deg))
	(t
	 (error "Unknown METHOD ~A - must be :WEB or :VIZQUERY" method))))



(defclass sdss7-catalog (sdss-catalog)
  ())

(defmethod object-mag ((acat sdss7-catalog) mag-name i  &key (error-if-not-exist t))
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


(defmethod object-type ((acat sdss7-catalog) (i fixnum))
  (let ((k (get-value acat :type i)))
    (cond ((= k 0) :unknown)
	  ((= k 6) :star)
	  ((= k 3) :galaxy)
	  (t :other))))


(defun read-sdss7-catalog-object  (ra-deg dec-deg radius-deg
				   &key  
				     (method :vizquery))
    (multiple-value-bind (data fields)
	(read-sdss7-catalog ra-deg dec-deg radius-deg :method method)
      (mark-astro-catalog-ok ;; use method to mark objects that are OK
       (make-instance 'sdss7-catalog
		      :n (if data 
			     (length (aref data 0))
			     0)
		     :ra-center (float ra-deg 1d0) :dec-center (float dec-deg 1d0) 
		     :radius-deg (float radius-deg 1d0)
		      :data data
		      :fields fields
		      ;; kludge - 
		      :available-mags (if (eq method :web) '(:u :g :r :i :z)
					  '(:u :g :r :i))
		      :%map (make-map fields)))))

	
