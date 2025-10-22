
(in-package coma-json-server)

;; get all JPL quanitities
(defparameter *jpl-quantities*
  (loop for i from 1 to 48 collect i))

;; make the EPH sub-structure (see ephem-fields.txt)
(defun %get-db-field-from-jpl-ephem (db-field jpl-hash object centerbody obs-code)
  (flet ((%non-gethash (key hash) ;; number-or-nil gethash
	   (when (gethash key hash) ;; only if the field is present; otherwise NULL
	     (map 'vector (lambda (x) (if (numberp x) (float x 1d0) nil))
		  (gethash key hash))))
	 (%onespace (string) (if (equalp string "") " " string)))
    (cond
      ;; the following are not vectors
      ((equalp db-field "targetbody")
       object)
      ((equalp db-field "centerbody")
       centerbody)
      ((equalp db-field "centersite")
       obs-code)
      ;;
      ((equalp db-field "ephstr") ;; FIXME!
       nil)
      ((equalp db-field "ctdatetime")
       (map 'vector 'astro-time:jd-to-ut-string
	    (gethash "Date_________JDUT" jpl-hash)))
      ((equalp db-field "jd")
       (gethash "Date_________JDUT" jpl-hash))
      ((equalp db-field "solarpres")
       (map 'vector #'%onespace
	    (gethash "solarpres" jpl-hash))) ;; CHECKME
      ((equalp db-field "lunarpres")
       (map 'vector #'%onespace
	    (gethash "lunarpres" jpl-hash))) ;; CHECKME
      ;;
      ((equalp db-field "j2kra")
       (map 'vector (lambda (x)  (ignore-errors (ra-dec:deg->hms-string x)))
	    (gethash "R.A._(ICRF)" jpl-hash)))
      ((equalp db-field "j2kdec")
       (map 'vector (lambda (x) (ignore-errors (ra-dec:deg->dms-string x)))
	    (gethash "DEC_(ICRF)" jpl-hash)))
      ((equalp db-field "j2kradeg") ;; NOTE - we added decimal deg, which is not in database
       (%non-gethash "R.A._(ICRF)" jpl-hash))
      ((equalp db-field "j2kdecdeg")
       (%non-gethash "DEC_(ICRF)" jpl-hash))
      ;;
      ((equalp db-field "appra")
       (map 'vector (lambda (x)  (ignore-errors (ra-dec:deg->hms-string x)))
	    (gethash "R.A._(a-app)" jpl-hash)))
      ((equalp db-field "appdec")
       (map 'vector (lambda (x) (ignore-errors (ra-dec:deg->dms-string x)))
	    (gethash "DEC_(a-app)" jpl-hash)))
      ((equalp db-field "appradeg") ;; NOTE - we added decimal deg, which is not in database
       (%non-gethash "R.A._(a-app)" jpl-hash))
      ((equalp db-field "appdecdeg")
       (%non-gethash "DEC_(a-app)" jpl-hash))
      ;;
      ((equalp db-field "dracosd")
       (%non-gethash "dRA*cosD" jpl-hash))
      ((equalp db-field "ddect")
       (%non-gethash "d(DEC)/dt" jpl-hash))
      ((equalp db-field "amass")
       (%non-gethash "a-mass" jpl-hash))
      ((equalp db-field "tmageph")
       (%non-gethash "T-mag" jpl-hash))
      ((equalp db-field "nmageph")
       (%non-gethash "N-mag" jpl-hash))
      ((equalp db-field "r")
       (%non-gethash "r" jpl-hash))
      ((equalp db-field "rdot")
       (%non-gethash "rdot" jpl-hash))
      ((equalp db-field "delta")
       (%non-gethash "delta" jpl-hash))
      ((equalp db-field "deldot")
	(%non-gethash "deldot" jpl-hash))
      ((equalp db-field "sto")
       (%non-gethash "S-T-O" jpl-hash))
      ((equalp db-field "tom")
       (%non-gethash "T-O-M" jpl-hash))
      ((equalp db-field "illupercent")
       (%non-gethash  "MN_Illu%" jpl-hash)) ;; not "Illu%", which is object fraction illum
      ((equalp db-field "glxlon")
       (%non-gethash "GlxLon" jpl-hash))
      ((equalp db-field "glxlat")
       (%non-gethash "GlxLat" jpl-hash))
      ;; two extra fields added at request of KJM 2021-11: True Anom and light travel time
      ((equalp db-field "trueanom")
       (%non-gethash "Tru_Anom" jpl-hash))
      ((equalp db-field "onewaylttime")
       (%non-gethash "1-way_down_LT" jpl-hash))
      )))




;; make a JSON eph structure from a JPL-HASH of observables
(defun %make-jpl-json-eph-struct (jpl-hash object centerbody obs-code)
  (loop with json-hash = (make-hash-table :test 'equalp)
	for db-field in *db-eph-fields*
	do (setf (gethash db-field json-hash) 
		 (%get-db-field-from-jpl-ephem db-field jpl-hash object centerbody obs-code))
	finally (return json-hash)))




(defun %get-jpl-json-vect-struct (object mjd-start mjd-end dt/min
				  &key obs-code (centerbody "@sun"))

  (let ((hash
	  (jpl-horizons:get-jpl-horizons-ephem-euclidean
	   object
	   :mjd-start mjd-start :mjd-end mjd-end
	   :dt  (format nil "~Am" dt/min)
	   :location  centerbody
	   :ref-plane :ecliptic ;;  not same as SLALIB, which is equatorial
	   :observatory obs-code
	   :ntries 2)))


    (flet ((convert-units (key vec)
	     (cond ((member key '("x" "y" "z") :test 'equalp)
		    (map 'vector (lambda (x) (/ x +au/km+))
			 vec))
		   ((member key '("vx" "vy" "vz") :test 'equalp)
		    (map 'vector (lambda (x) (* (/ x +au/km+)
						+day/sec+))
			 vec))
		   (t vec))))
    
    (let ((parameters (make-hash-table :test 'equalp)))
      (loop for key in '("x" "y" "z" "vx" "vy" "vz")
	    for val  = (gethash (string-upcase key) hash)
	    do (setf (gethash key parameters) 
		     (convert-units key val)))
      (setf (gethash "centerbody" parameters) centerbody)
      (setf (gethash "centersite" parameters) obs-code)
      (setf (gethash "vectorstr" parameters) nil)  ;; FIXME
      parameters))))
	  

  
(defun fill-jpl-ephem-response (json-resp object mjd-start mjd-end dt/min obs-code centerbody)
  (block ret
    (multiple-value-bind
	  (jpl-ephem-hash error)  ;; JPL-EPHEM is a HASH because we set RETURN-RAW-HASH-TABLE
	(ignore-errors
	 (jpl-horizons:get-jpl-horizons-ephem
	  object
	  :mjd-start mjd-start :mjd-end mjd-end
	  :dt (format nil "~Am" dt/min)
	  :location  centerbody
	  :observatory obs-code
	  :quantities *jpl-quantities*
	  :return-raw-hash-table t
	  :ntries 2))
      (when (not jpl-ephem-hash)
	(setf (json-object-error json-resp)
	      (make-error-object :error "JPL-EPHEM-DOWNLOAD-ERROR"
				 :desc (format nil "Error in JPL Ephem: ~A" error)))
	(return-from ret json-resp)) ;; ERROR
      ;; else pase the observables and insert into json EPH structure
      (multiple-value-bind (json-hash error)
	  (ignore-errors
	   (%make-jpl-json-eph-struct jpl-ephem-hash object centerbody obs-code))
	(when (not json-hash)
	  (setf (json-object-error json-resp)
		(make-error-object :error "JPL-EPH-PARSE-ERROR"
				   :desc (format nil "Error in JPL Ephem: ~A" error)))
	  (return-from ret json-resp)) ;; ERROR
	;;
	(setf (gethash "eph" (json-object-parameters json-resp)) json-hash))

      ;; get the solar vector
      (multiple-value-bind (json-hash error)
	  (ignore-errors
	   (%get-jpl-json-vect-struct object mjd-start mjd-end dt/min 
				      :centerbody "@sun" ;; center of sun
				      :obs-code "500"))
	(when (not json-hash)
	  (setf (json-object-error json-resp)
		(make-error-object :error "JPL-SUNVECT-ERROR"
				   :desc (format nil "Error in JPL sunvect Ephem: ~A" error)))
	  (return-from ret json-resp)) ;; ERROR
	 ;;
	 (setf (gethash "sunvect" (json-object-parameters json-resp)) json-hash))

      ;; get the earth vector
      (multiple-value-bind (json-hash error)
	  (ignore-errors
	   (%get-jpl-json-vect-struct object mjd-start mjd-end dt/min 
				      :centerbody "@399" ;; cent
				      :obs-code "500")) ;;
	(when (not json-hash)
	  (setf (json-object-error json-resp)
		(make-error-object :error "JPL-SUNVECT-ERROR"
				   :desc (format nil "Error in JPL sunvect Ephem: ~A" error)))
	  (return-from ret json-resp)) ;; ERROR
	;;
	(setf (gethash "earthvect" (json-object-parameters json-resp)) json-hash))

      

    json-resp
    )))
	  
  
  
