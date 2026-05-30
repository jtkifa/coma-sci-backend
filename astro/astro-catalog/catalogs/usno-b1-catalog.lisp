
;; routines to download astronomical catalogs



(in-package astro-catalog)


;; generate a URL 
;; see http://www.nofs.navy.mil/data/fchpix/vo_nofs.html
(defun make-usno-b1-url (ra-deg dec-deg radius-deg &key (verb 1))
  (format 
   nil 
   "http://www.nofs.navy.mil/cgi-bin/vo_cone.cgi?CAT=~A&RA=~,6F&DEC=~,6F&SR=~,6F&VERB=~A&cftype=ASCII&slf=ddd.ddd,dd.ddd&skey=RA" "USNO-B1" ra-deg dec-deg radius-deg verb))



(defun get-usno-b1-catalog-page (ra-deg dec-deg radius-deg &key (verb 1))
  (multiple-value-bind (text return-code headers uri)
      (drakma:http-request
	  (make-usno-b1-url ra-deg dec-deg radius-deg :verb verb) 
	:keep-alive nil)
    (declare (ignorable headers))
    (when (not (= return-code 200))
      (error "HTTP error code ~A returned by catalog URI ~A~%" return-code uri))
    text))

(defun %usno-b-has-valid-line-p (string)
  (with-input-from-string (s string)
    (loop for line = (read-line s nil nil)
	  until (not line)
	  when (and (plusp (length line))
		    (not (char= (aref line 0) #\#)))
	    do (return t)
	  finally (return nil))))
	  

(defun read-usno-b1-catalog-web  (ra-deg dec-deg radius-deg)
  "Query USNO VO catalog in RADIUS-DEG around RA-DEC and DEC-DEG, returning vector
of vectors of following:

 id|      RA|   DEC|sra|sde| sRaEp | SdeEp |   MuRA|  MuDEC|  sMuRA|  sMuDE|   B1|   R1|   B2|  R2|  I2|
 id| dd.ddd |dd.ddd|mas|mas| mas   | mas   | mas/yr| mas/yr| mas/yr| mas/yr|  mag|  mag|  mag| mag| mag|
  0|       1|     2|  3|  4|      5|      6|      7|      8|      9|     10|   11|   12|   13|  13|  15|

A list of column IDs is returned as the 2nd value.
"
  (let* ((text (get-usno-b1-catalog-page ra-deg dec-deg radius-deg))
	 (v (if (%usno-b-has-valid-line-p text)
		 (with-input-from-string (s text)
		   (numio:read-cols 
		    s
		    '(:string                      ;; ID-string-0
		      :double-float :double-float  ;; ra-1 dec-2 
		      :double-float :double-float  ;; sra-3 sdec-4
		      :double-float :double-float  ;; sraep-5 sdeep-6
		      :double-float :double-float  ;; mura-7 mudec-8 
		      :double-float :double-float  ;; smura-9 smudec-10
		      :double-float :double-float  ;; b1mag-11 r1mag-12
		      :double-float :double-float  ;; b2mag-13  r2mag-14
		      :double-float )              ;; i2mag-15
		    :comment-char #\#)))))

    ;; rescale errors from mas to deg
    (loop with dravec = (aref v 3) and ddecvec = (aref v 4)
	  for i below (length dravec)
	  do (setf (aref dravec i) (* (aref dravec i) #.(/ 0.001d0 3600d0)))
	     (setf (aref ddecvec i) (* (aref ddecvec i) #.(/ 0.001d0 3600d0))))
    
	 (values v
		 '(:id :RA :DEC
		   :ra-err :dec-err ;; keep notation of sdss
		   :sRaEp :sDeEp :MuRA  :MuDEC 
		   :sMuRA  :sMuDE   :B1   :R1   :B2  :R2  :I2))))

(defun read-usno-b1-catalog-vizquery (ra-deg dec-deg radius-deg)
  (multiple-value-bind (result keys-or-error)
      (ignore-errors
	(run-vizquery-and-parse/multisites ra-deg dec-deg (* radius-deg 60.0) "USNO-B1"
				`(("USNO-B1.0" :id string "")
				  ("RAJ2000" :ra double-float ,*invalid-dbl-value*)
				  ("DEJ2000" :dec double-float ,*invalid-dbl-value*)
				  ("e_RAJ2000" :ra-err double-float 0d0) ;; arcsec
				  ("e_DEJ2000" :dec-err double-float 0d0);; arcsec
				  ;;(:sraep ) ;; some fields not available in vizquery version
				  ;;(:sdeep)
				  ("pmRA" :mura double-float 0d0) 
				  ("pmDE" :mudec double-float 0d0)
				  ;;(:smura)
				  ;;(:smude)
				  ("B1mag" :b1 double-float 0d0)
				  ("R1mag" :r1 double-float 0d0)
				  ("B2mag" :b2 double-float 0d0)
				  ("R2mag" :r2 double-float 0d0)
				  ("Imag" :i2 double-float 0d0))))
    ;;
    (if (typep keys-or-error 'error)
	(error keys-or-error)
	(values result keys-or-error))))


(defun read-usno-b1-catalog (ra-deg dec-deg radius-deg 
			     &key (method :vizquery))
  "Query USNO catalog at RA-DEG,DEC-DEG in circle of size RADIUS-DEG, using
METHOD of :WEB or :VIZQUERY"
  (cond ((eq method :vizquery)
	 (read-usno-b1-catalog-vizquery ra-deg dec-deg radius-deg ))
	((eq method :web)
	 (read-usno-b1-catalog-web ra-deg dec-deg radius-deg))
	(t
	 (error "Unknown METHOD ~A - must be :WEB or :VIZQUERY" method))))


(defclass usno-b1-catalog (astro-catalog) ())

(defmethod object-mag ((acat usno-b1-catalog) mag-name i  &key (error-if-not-exist t))
  (multiple-value-bind (mag mag-err)
      (cond ((eq mag-name :b1) (values (get-value acat :b1 i) 0d0))
	    ((eq mag-name :r1) (values (get-value acat :r1 i) 0d0))
	    ((eq mag-name :b2) (values (get-value acat :b2 i) 0d0))
	    ((eq mag-name :r2) (values (get-value acat :r2 i) 0d0))
	    ((eq mag-name :i2) (values (get-value acat :i2 i) 0d0))
	    (t
	     (if error-if-not-exist
		 (error "Magnitude type ~A unknown in ~A" mag-name acat)
		 nil)))
    (if mag
	(values mag mag-err
		;; flag a bad mag as one out of range
		(not (< 1 mag 30))))))

(defmethod object-ra-err ((acat usno-b1-catalog) i)
  (max
   (get-value acat :ra-err i) ;; arcsec
   0.1d0)) ;; 0.1" floor



(defmethod object-dec-err ((acat usno-b1-catalog) i)
  (max
   (get-value acat :dec-err i)
   0.1d0)) ;; 0.1" floor



(defun read-usno-b1-catalog-object (ra-deg dec-deg radius-deg &key
				    (method :vizquery))
  (multiple-value-bind (data fields)
      (read-usno-b1-catalog ra-deg dec-deg radius-deg :method method)
    (mark-astro-catalog-ok ;; use method to mark objects that are OK
     (make-instance 'usno-b1-catalog
		    :n (if data 
			   (length (aref data 0))
			   0)
		    :ra-center (float ra-deg 1d0) :dec-center (float dec-deg 1d0) 
		    :radius-deg (float radius-deg 1d0)
		    :data data
		    :fields fields
		    :available-mags '(:B1   :R1   :B2  :R2  :I2)
		    :%map (make-map fields)))))




;; this is an anachronism
(defun read-usno-b1-catalog-to-file  (outfile ra-deg dec-deg radius-deg 
				      &key (if-exists :supersede))
  
  (with-open-file (sout outfile :direction :output :if-exists if-exists)
    (let ((v (read-usno-b1-catalog ra-deg dec-deg radius-deg)))
      (write-string 
       "# id|      RA|   DEC|sra|sde|   MuRA|  MuDEC|  sMuRA|  sMuDE|   B1|   R1|   B2|  R2|  I2|
# id| dd.ddd |dd.ddd|mas|mas| mas/yr| mas/yr| mas/yr| mas/yr|  mag|  mag|  mag| mag| mag|
#  0|       1|     2|  3|  4|      5|      6|      7|      8|    9|   10|   11|  12|  13|
#
"
       sout)
      (loop for id across (aref v 0) 
	    for ra across (aref v 1) 
	    for dec across (aref v 2) 	    
	    for sra across (aref v 3) 
	    for sdec across (aref v 4) 
	    for mura across (aref v 5) 
	    for mudec across (aref v 6) 
	    for smura across (aref v 7) 
	    for smudec across (aref v 8) 
	    for b1 across (aref v 9)
	    for r1 across (aref v 10)
	    for b2 across (aref v 11)
	    for r2 across (aref v 12)
	    for i2 across (aref v 13)
	    do
	    (format sout "~12A" id)
	    (format sout " ~10,5F" ra)
	    (format sout " ~10,5F" dec)
	    (format sout " ~5,1F" sra)
	    (format sout " ~5,1F" sdec)
	    (format sout " ~6,1F" mura)
	    (format sout " ~6,1F" mudec)
	    (format sout " ~5,1F" smura)
	    (format sout " ~5,1F" smudec)
	    (format sout " ~6,2F" b1)
	    (format sout " ~6,2F" r1)
	    (format sout " ~6,2F" b2)
	    (format sout " ~6,2F" r2)
	    (format sout " ~6,2F" i2)
	    (terpri sout)))))


;; helper routine - also an anachronism
(defun get-catalog-column (column-name data-vec name-vec)
  "given a set of catalog data (eg from ASTRO-CATALOG:READ-USNO-B1-CATALOG) and the column names, return
the column from data."
  (aref data-vec (or (position column-name name-vec :test 'equalp)
		     (error "column ~A not found" column-name))))
