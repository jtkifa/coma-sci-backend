(in-package observatories)

(defstruct observatory 
  (id "un-named") ;; short id
  (name "No name") ;; long name
  (obscode nil)  ;; a string - has to be inserted by hand
  (generic nil)  ;; this is the GENERIC identifier for this obs-code - trumps others
  wlongitude  ;; WEST longitude (the IRAF convention, unfortunately)
  latitude  ;; north lat (GEODETIC - ellipse based - not GEOCENTRIC - which is what
            ;; is generally used, eg by SLALIB)
  altitude   ;; above sea level in meters
  timezone)   ;; timezone 

;; define it early in file
(defparameter *obs-hash*  (make-hash-table :test 'equalp)) 

;; read an IRAF obs file, used just once to create the data structure
;; below
(defun read-iraf-obs-file (file)
  (with-open-file (sin file)
    (labels ((parse-obs (first-line)
	       (let (obs name wlon lat alt tz)
		 (loop for line = first-line then (read-line sin nil nil)
		    until (or (not line)
			      (not (find #\= line)))
		    do
		    (multiple-value-bind (key val)
			(parse-line line)
		      (cond ((equalp key "observatory")
			     (setf obs val))
			    ((equalp key "name")
			     (setf name val))
			    ((equalp key "longitude")
			     (setf wlon val)) 
			    ((equalp key "latitude")
			     (setf lat val))
			    ((equalp key "altitude")
			     (setf alt val))
			    ((equalp key "timezone")
			     (setf tz val)))))
		 (make-observatory
		  :id obs :name name :wlongitude wlon  :latitude lat 
		  :altitude alt :timezone tz)))
	     ;;
	     (parse-line (line)
	       (let* ((n (position #\= line))
		      (key (string-trim '(#\space #\tab) (subseq line 0 n)))
		      (val-raw (string-trim '(#\space #\tab) (subseq line (1+ n))))
		      (val
		       (cond ((eql (aref val-raw 0) #\")
			      (string-trim "\"" val-raw ))
			     ((member key '("latitude" "longitude") :test 'equalp)
			      (cond ((find #\: val-raw) ;; sexagesimal 
				     (ra-dec:dms-string->deg val-raw))
				    (t
				     (jk-parse-float:parse-float val-raw))))
			     ((equalp key "timezone")
			      ;; some tz are fractional
			      (float (jk-parse-float:parse-float val-raw) 1.0)) 
			     ((equalp key "altitude")
			      (jk-parse-float:parse-float val-raw)))))
		 (values key val))))
      
      (loop 
	 with obs-list = nil
	 for first-line = (read-line sin nil nil)
	 until (not first-line)
	 when (search "observatory" first-line)
	 do
	   (push (parse-obs first-line) obs-list)
	 finally (return obs-list)))))
       
	 
(defun get-observatory (obs &key (obs-hash *obs-hash*) (error-on-fail nil))
  "Get the observatory corresponding to OBS; if OBS is already an
observatory, return it."
  (if  (observatory-p obs)
       obs
       (or (gethash obs obs-hash)
	   (and error-on-fail (error "Observatory ~A not found" obs) nil))))
