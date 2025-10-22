#|

Request:

{
    "TYPE":"REQUEST",
    "COMMAND":"ORBIT-PRECISION",
    "ID":"123ABC",
    "PARAMETERS": {
        "OBJECT":"2P",
        "UT":"2015-01-01T01:01:01-10" // or MJD
    }
}

Response:
{
    "TYPE":"RESPONSE",
    "COMMAND":"ORBIT-PRECISION",
    "ID":"123ABC",
    "PARAMETERS":{
       "TYPE": "ORBITAL-PRECISION",
       "ERR3SIGMA":0.10517466361161416,     // arcsec error in orbit
       "RA-ERR3SIGMA":0.07536299437830685,
       "DEC-ERR3SIGMA":0.07336299437830687,
       "MJD-GAP": 57.0d0   // the difference between MJD and the closest ephem point
       "OK": true          // OK, if we trust this point (MJD-GAP was small enough; no missing JPL data)
    }
}
|#


(in-package coma-json-server)

(defparameter *orbit-precision-directory*
  (concatenate 'string *coma-data-directory* "/orbit-precision-directory"))

(defparameter *orbit-precision-lock*  ;; prevents simultaneous read/write of orbit precision file
  (bordeaux-threads:make-lock "orbit-precision-lock"))

(when (not (probe-file *orbit-precision-directory*))
  (ensure-directories-exist (concatenate 'string *orbit-precision-directory* "/IGNORE"))
  (make-dir-world-usable  *orbit-precision-directory*))

(defun filename-ify-object (name)
  (with-output-to-string (s)
    (loop for char across name
	  if (alphanumericp char)
	    do (write-char char s)
	  else
	    do (write-char #\_ s))))

(defun write-jpl-precision-file (ephem file)
  (bordeaux-threads:with-lock-held (*orbit-precision-lock*)
    (with-open-file (sout file :direction :output :if-exists :supersede)
      (write-line "MJD,RA,DEC,RA3SIGMA,DEC3SIGMA" sout)
      (loop
	for mjd across (jpl-horizons:jpl-ephem-mjd ephem)
	for ra across (jpl-horizons:jpl-ephem-ra ephem)
	for dec across (jpl-horizons:jpl-ephem-dec ephem)
	for ra3sigma across (jpl-horizons:jpl-ephem-ra-3sigma-err ephem)
	for dec3sigma across (jpl-horizons:jpl-ephem-dec-3sigma-err ephem)
	;; it is possible that some quanities may be undefined, in which case the datum
	;; is useless.  For example, some JPL ephems in some eras omit the ra,dec errors
	when (and mjd ra dec ra3sigma dec3sigma)
	do
	   (format sout
		   "~,5F,~,6F,~,6F,~,6F,~,6F~%"
		   mjd ra dec ra3sigma dec3sigma)))))

(defun read-jpl-precision-file (csv-filename)
  (bordeaux-threads:with-lock-held (*orbit-precision-lock*)
    (csv-read:read-csv-headers/columns-from-file csv-filename)))
  

(defparameter *orbital-precision-server-day-interval* 14)

;; return a hash with fields MJD,RA,DEC,RA3SIGMA,DEC3SIGMA - the 3sigma are
;; arcseconds of error
(defun get-orbit-precision-ephem (object)
  (let ((filename (concatenate 'string *orbit-precision-directory* "/"
			       (filename-ify-object object) "_ephem.csv")))
    (cond ((probe-file filename)
	   (read-jpl-precision-file filename))
	  (t
	   (let ((ephem (jpl-horizons:get-jpl-horizons-ephem/closest-orbit
			 object
			 :mjd-start (astro-time:parse-ut-date-and-time-string-to-mjd "1978-01-01T00:00:00")
			 :mjd-end   (astro-time:parse-ut-date-and-time-string-to-mjd "2036-01-01T00:00:00")
			 :dt (format nil "~A d" *orbital-precision-server-day-interval*)))
		 (h (make-hash-table :test 'equalp)))
	     (when ephem
	       (write-jpl-precision-file ephem filename)
	       (setf (gethash "MJD" h) (jpl-horizons:jpl-ephem-mjd ephem))
	       (setf (gethash "RA" h) (jpl-horizons:jpl-ephem-ra ephem))
	       (setf (gethash "DEC" h) (jpl-horizons:jpl-ephem-dec ephem))
	       (setf (gethash "RA3SIGMA" h) (jpl-horizons:jpl-ephem-ra-3sigma-err ephem))
	       (setf (gethash "DEC3SIGMA" h) (jpl-horizons:jpl-ephem-dec-3sigma-err ephem))
	       h))))))


(defun get-orbital-precision-for-object (object mjd)
  "Return the orbital precision of an object in arcsec.
   (VALUES TOT-ERR/ARCSEC RA-ERR/ARCSEC DEC-ERR/ARCSEC MJD-GAP)
where MJD-GAP is the difference between MJD and the closet point in the ephemeris."
  (let ((hash (get-orbit-precision-ephem object)))
    (when hash
      (let* ((mjd-vec (gethash "MJD" hash))
	     (ra3sigma-vec (gethash "RA3SIGMA" hash))
	     (dec3sigma-vec (gethash "DEC3SIGMA" hash))
	     (n (length mjd-vec)))
	;; 2nd to last element is taken as limit because interpolation
	;; uses the i,1+i elements, and we don't want to bother with
	;; special case of exactly falling on last element with i+1
	;; undefined
	(when (not (<= (aref mjd-vec 0) mjd (aref mjd-vec (- n 2))))
	  (error
	   "Out of range MJD in GET-PRECISION-FOR-ORBIT: MJD=~A is not in [~A,~A]"
	   mjd  (aref mjd-vec 0) (aref mjd-vec (- n 2))))
	(loop for i from 0 to (- n 2)
	      for emjd = (aref mjd-vec (1+ i)) ;; when the point AFTER i crosses ..
	      until (>= emjd mjd)
	      ;; now we interpolate at i,i+1
	      finally
		 (let* ((mjd1 (aref mjd-vec i))
			(mjd2 (aref mjd-vec (1+ i)))
			(f (/ (- mjd mjd1) (- mjd2 mjd1))) ;; frac of way
			;; the average abs gap between MJD and the nearest ephem points
			(mjd-gap (* 0.5 (+ (abs (- mjd mjd1))
					   (abs (- mjd mjd2)))))
			(1-f (- 1.0 f))
			(ra3s
			  (+ (* 1-f (aref ra3sigma-vec i))
			     (*   f (aref ra3sigma-vec (1+ i)))))
			(dec3s
			  (+ (* 1-f (aref dec3sigma-vec i))
			     (*   f (aref dec3sigma-vec (1+ i))))))
		   (return (values
			    (sqrt (+ (* ra3s ra3s) (* dec3s dec3s)))
			    ra3s dec3s mjd-gap))))))))
		 
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(def-json-command orbit-precision (json-req)
  (with-json-command-setup (json-req)
  (let* ((object      (get-param "OBJECT" :required t))
	 (mjd-params  (get-param "MJD"))
	 (ut-string   (get-param "UT"))
	 (mjd-from-ut (if ut-string
			  (ignore-errors
			   (astro-time:parse-ut-date-and-time-string-to-mjd
			    ut-string))))
	 (mjd (or mjd-params mjd-from-ut)))

    (jcom-test-expr (or (not (or mjd-params ut-string))
			(and mjd-params ut-string))
                    "MJD-OR-UT-NOT-GIVEN"
                    "Neither MJD nor UT given, or both given")
    
    (jcom-test-expr (and ut-string (not mjd-from-ut))
                    "BAD-UT-STRING"
                    "Cannot parse given UT to an MJD")
    (jcom-test-expr (not (and (realp mjd)
			      (< 42778.0d0 mjd 64693.0d0))) ;; 1976 to 2036
                    "OUT-OF-RANGE-MJD"
                    "MJD out of range 1976-01-01 - 2036-01-01")

    (multiple-value-bind (err3sigma ra3sigma dec3sigma mjd-gap)
	(ignore-errors
	 (get-orbital-precision-for-object object mjd))
      
      (jcom-test-expr (not err3sigma)
		      "ERROR-GETTING-ORBIT-PRECISION"
		      (format nil "Error <~A>: error getting orbit from JPL?"
			      ra3sigma)) ;; err=ra3sigma after ignore-errors
      
      (let ((prec-params (make-hash-table :test 'equalp)))
	(set-param "ORBIT-PRECISION" prec-params)
	;;
	(setf (gethash "TYPE" prec-params) "ORBIT-PRECISION")
	(setf (gethash "ERR3SIGMA" prec-params) err3sigma)
	(setf (gethash "RA-ERR3SIGMA" prec-params) ra3sigma)
	(setf (gethash "DEC-ERR3SIGMA" prec-params) dec3sigma)
	(setf (gethash "MJD-GAP" prec-params) mjd-gap)
	;; it's OK, if the MJD-GAP is like the ephem spacing
	(setf (gethash "OK" prec-params)
	      (< mjd-gap
		 (* 0.5 (+ 1.0 *orbital-precision-server-day-interval*)))))
      ))))
	       
    
							     
	  
      



