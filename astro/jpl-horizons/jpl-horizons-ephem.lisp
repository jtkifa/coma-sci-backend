
;; FIXME - put some sort of retries or file locking to prevent JPL from
;; locking us out

(in-package jpl-horizons)

(defstruct jpl-ephem
  (location nil)
  (observatory nil)
  (mjd nil)
  (ra  nil)
  (dec nil)
  (ra-3sigma-err  nil)
  (dec-3sigma-err nil)
  (dra/dt nil)
  (ddec/dt nil)
  (rhelio nil)
  (drhelio/dt nil)
  (delta nil)
  (ddelta/dt nil)
  (phase-angle nil)
  (total-mag nil)     ;; comets
  (nucleus-mag nil)   ;; comets
  (apparent-mag nil)  ;; asteroids
  (airmass nil)
  (airmass-extinction nil) 
  )

(defun %mjd-to-jpl-ut (mjd)
  (format nil "'~A'"
	  (substitute #\space #\T 
		      (astro-time:mjd-to-ut-string mjd))))

(defun %parse-float-or-nil (x)
  (if (jk-parse-float:validate-float-string x)
      (jk-parse-float:parse-float x)
      nil))

;; the one-character fields representing the solar presence and lunar presence
;; are not labelled in CSV file, so we force labels
(defun %fix-jpl-header-list (header-list)
  (cond ((and (equalp (second header-list) "")
	      (equalp (third  header-list) ""))
	 (setf (second header-list) "solarpres")
	 (setf (third  header-list) "lunarpres")
	 header-list)
	;;
	(t
	 (error "Cannot fix JPL header list to insert SOLARPRES and LUNARPRES fields - see jpl-horizons-ephem.lisp"))))

(defun %parse-jpl-ephem-block (block type &key location observatory)
  (declare (type string block))
  (let* ((header-line (with-input-from-string (s block) (read-line s)))
	 (header-list (%fix-jpl-header-list
		       (mapcar (lambda (str) (string-trim " " str))
			       (string-utils:split-string header-line ","))))
	 ;; hash of keyword (eg :mjd) to field number
	 (header-hash (make-hash-table :test 'eql))
	 (data-block (subseq block (+ 6 (search "$$SOE" block))))
	 ;; clear blank lines from data-block
	 (clean-data-block
	   (with-output-to-string (sout)
	     (with-input-from-string (sin data-block)
	       (loop for line = (read-line sin nil nil)
		     until (not line)
		     when (and (> (length line) 100)
			       (not (search "Cut-off" line)))
		       do (write-line line sout)))))
	 (csv (with-input-from-string (s clean-data-block)
		(fare-csv:read-csv-stream s))))
    ;; fill the header-hash with a map from KEY to column number 
    (loop for header in header-list
	  for i from 0
	  ;; note - they seemed to change the RA,Dec header around
	  ;; start of 2020
	  for key =  (cond ((equalp header "Date_________JDUT")    :mjd)
			   ((or
			     (equalp header "R.A._(ICRF/J2000.0)")
			     (equalp header "R.A._(ICRF)"))
			    :ra)
			   ((or (equalp header "DEC_(ICRF/J2000.0)")
				(equalp header "DEC_(ICRF)"))
			    :dec)
			   ((equalp header "dRA*cosD")  :dra/dt)
			   ((equalp header "d(DEC)/dt")  :ddec/dt)
			   ((equalp header "RA_3sigma")  :ra-3sigma-err)
			   ((equalp header "DEC_3sigma")  :dec-3sigma-err)
			   ((equalp header "a-mass") :airmass)
			   ((equalp header "mag_ex") :airmass-extinction)
			   ((equalp header "T-mag")  :total-mag)
			   ((equalp header "N-mag")  :nucleus-mag)
			   ((equalp header "APmag")  :apparent-mag)
			   ((equalp header "r")  :rhelio)
			   ((equalp header "rdot")  :drhelio/dt)
			   ((equalp header "delta")  :delta)
			   ((equalp header "deldot")  :ddelta/dt)
			   ((equalp header "S-T-O")  :phase-angle))
	  when key
	    do (setf (gethash key header-hash) i))


    (labels ((getcol (key) (gethash key header-hash))
	     ;;
	     (mapitdbl (ncol func)
	       (map '(simple-array t (*))
		    (lambda (x) (let ((y (funcall func x))) (when y (float y 1d0))))
		    (mapcar (lambda (line) (nth ncol line))
			    csv)))
	     ;;
	     (mapitflt (ncol func)
	       (map '(simple-array t (*))
		    (lambda (x) (let ((y (funcall func x))) (when y (float y 1e0))))
		    (mapcar (lambda (line) (nth ncol line))
			    csv))))

      (make-jpl-ephem
       :location location
       :observatory observatory
       :mjd (mapitdbl (getcol :mjd)
		      (lambda (z) (astro-time:jd-to-mjd (%parse-float-or-nil z))))
       :ra  (mapitdbl (getcol :ra) '%parse-float-or-nil)
       :dec (mapitdbl (getcol :dec)  '%parse-float-or-nil)
       :ra-3sigma-err (mapitdbl (getcol :ra-3sigma-err) '%parse-float-or-nil)
       :dec-3sigma-err (mapitdbl (getcol :dec-3sigma-err) '%parse-float-or-nil)
       :dra/dt (mapitflt (getcol :dra/dt)  '%parse-float-or-nil)
       :ddec/dt (mapitflt (getcol :ddec/dt) '%parse-float-or-nil)
       :total-mag (if (eq type :comet)
		      (mapitflt (getcol :total-mag) '%parse-float-or-nil))
       ;; the next fields are applicable only to comets/asteroids
       :nucleus-mag (if (eq type :comet)
			(mapitflt (getcol :nucleus-mag)
				  '%parse-float-or-nil))
       :apparent-mag (if (eq type :asteroid)
			 (mapitflt (getcol :apparent-mag)
				   '%parse-float-or-nil))
       :rhelio (mapitflt (getcol :rhelio) '%parse-float-or-nil)
       :drhelio/dt (mapitflt (getcol :drhelio/dt)  '%parse-float-or-nil)
       :delta (mapitflt (getcol :delta) '%parse-float-or-nil)
       :ddelta/dt (mapitflt (getcol :ddelta/dt) '%parse-float-or-nil)
       :phase-angle (mapitflt (getcol :phase-angle) '%parse-float-or-nil)
       :airmass (mapitflt (getcol :airmass) '%parse-float-or-nil)
       :airmass-extinction (mapitflt (getcol :airmass-extinction) '%parse-float-or-nil)
       )

      )))




;; we fix headers for a OBSERVABLE type ephem, but not for a VECTORS type
(defun %parse-jpl-ephem-block-as-hash (block)
  (declare (type string block))
  (let* ((header-line (with-input-from-string (s block) (read-line s)))
	 (raw-header-list (mapcar (lambda (str) (string-trim " " str))
			       (string-utils:split-string header-line ",")))
	 (header-list (%fix-jpl-header-list raw-header-list))
	 ;; hash of key=field_name, val=data_vector
	 (data-hash (make-hash-table :test 'equalp))
	 (data-block (subseq block (+ 6 (search "$$SOE" block))))
	 (csv (with-input-from-string (s data-block)
		(fare-csv:read-csv-stream s)))
	 (nlines (length  csv)))

    (loop for header in header-list
	  for i from 0
	  for key in header-list
	  do (setf (gethash key data-hash) (make-array nlines :initial-element 0)))
    
    (loop
      for line in csv
      for iline from 0
      do (loop
	   for key in header-list
	   for value-string in line
	   when (not (equalp key "")) ;; should not happen when we fixed SOLARPRES and LUNARPRES
	   do (setf (aref (gethash key data-hash) iline)
		    (or (ignore-errors (parse-integer value-string))
			(ignore-errors (jk-parse-float:parse-float value-string))
			value-string))))

    (values data-hash block)))

	   
	       
		   
		 
;; return full location for JPL purposes
(defun %parse-jpl-observatory-and-location (location observatory)
  (cond ((not observatory)
	 (if (find #\@ location)
	     location ;; eg "123@500" or "@500"
	     (concatenate 'string "@" location)))
	;;
	;; if the observatory LOCATION is on the earth then 
	;; try to look it up in our obsevatory list
	((member location '("Earth" "@Earth" "399" "@399") :test 'equalp)
	 (let* ((obs-struct
		  (cond ((observatories:observatory-p observatory)
			 observatory)
			(observatory ;; try to resolve observatory
			 (or
			  (observatories:get-observatory observatory)
			  (error "Could not get Earth-located observatory ~A"
				 observatory)))))
		;; obs code can be "" for 'no observatory given'
		(obs-code (observatories:observatory-obscode obs-struct)))
	   ;;
	   (format nil "~A@~A" obs-code (string-left-trim "@" location))))
	;; not on earth!
	((stringp observatory)
	 (format nil "~A@~A" observatory (string-left-trim "@" location)))
	(t
	 (error "Can't understand observatory ~A" observatory))))
	 


(defparameter *default-location*    "@399")
(defparameter *default-observatory* "500") ;; geocenter

;; see https://ssd.jpl.nasa.gov/horizons/manual.html#output
(defparameter *default-jpl-ephem-quantities*
 '(1 3 8 9 19 20 23 24 36)) 


;; turn JPL quantites (1 2 3) into "'1,2,3'"
(defun %jpl-quantities-to-string (q-list)
  (with-output-to-string (s)
    (write-char #\' s)
    (loop for comma = "" then ","
	  for i in (sort (copy-seq q-list) '<)
	  do  (format s "~A~A" comma i))
    (write-char #\' s)))

(defun get-jpl-horizons-ephem 
    (object-name &key
		   (parse-ephem t)
		   (mjd-start 57031.333333333336d0)
		   (mjd-end   57031.334027777775d0)
		   (dt        "1 m")
		   (location *default-location*) 
		   (observatory *default-observatory*)
		   (resolve-ambiguous-object t)
		   (use-file-locking t)
		   (file-locking-timeout 60)
		   (ntries 1)
		   (airmass-limit nil)
		   (skip-daylight nil)
		   (return-raw-hash-table nil)
		   (quantities *default-jpl-ephem-quantities*))

	       
  "Get a JPL horizons EPHEMERIS object.  

Center requested is OBSERVATORY@LOCATION
 (@ is added if not present), or just @LOCATION if OBSERVATORY is nil.

For non-Earth locations, set OBSERVATORY must be a JPL obs code, because
we can't parse observatories elsewhere.

DT is the time span in an integer using JPL units (like '1 m' or h,d) but if it 
is unitless, then it is the number of subdivisions of the interval.

For low level use:

RETURN-RAW-HASH-TABLE means return a hash table of originally-named
fields with vectors as values, not the default JPL-EPHEM structure.
QUANTITIES is a list of JPL horizons fields '(1 2 3 ...) to return; if
not specified, use the ones to build default JPL-EPHEM.
"

  
  (let* ((eph-center
	   (%parse-jpl-observatory-and-location location observatory))
	 (lockfile *jpl-lockfile*)
	 (mjd-avg (* 0.5d0 (+ mjd-start mjd-end)))
	 (dont-file-lock (not use-file-locking)))

    (loop ;; loop over tries
      for itry from 0
      for ambiguous-resolver-result = nil
      for done-tries = (= itry ntries) 
      do
	 (assert (<= itry ntries)) ;; paranoid
	 ;; this is ugly because every block has a return
	 (multiple-value-bind (text/nil returncode/err)
	     (ignore-errors
	      ;; limit simultaneous access from this machine to prevent JPL
	      ;; lockout
	      (with-jpl-locking (:jpl-lockfile
				 lockfile
				 :lockfile-timeout file-locking-timeout
				 :hard-timeout 180
				 :dont-file-lock dont-file-lock)

		(sleep 1)
		(drakma:http-request  
		 *jpl-horizons-orbit-form-page*
		 :connection-timeout 60
		 :method :post
		 ;; see https://ssd-api.jpl.nasa.gov/doc/horizons.html
		 :parameters
		 `(;;("batch" . "1")
		   ("format" . "text") ;; json gets you nothing because it's just
		                       ;; a block response in a JSON struct LoL
		   ("COMMAND" . ,(format nil "'~A'" (fix-jpl-object-name object-name)))
		   ("CENTER" . ,eph-center)
		   ("MAKE_EPHEM" . "YES")
		   ("TABLE_TYPE" . "OBSERVER")
		   ("AIRMASS" . ,(if airmass-limit 
				     (format nil "~,2F" airmass-limit)
				     "38")) ;; 38='no limit'
		   ("SKIP_DAYLT" . ,(if skip-daylight "YES" "NO"))
		   ("START_TIME" . ,(%mjd-to-jpl-ut mjd-start))
		   ("STOP_TIME" .  ,(%mjd-to-jpl-ut mjd-end))
		   ("STEP_SIZE" . ,(format nil "'~A'" dt)) ;; like "1 d"
		   ("CAL_FORMAT" . "JD")
		   ("TIME_DIGITS" . "'SECONDS'")
		   ("ANG_FORMAT" . "DEG")
		   ("OUT_UNITS" . "KM-S")
		   ("RANGE_UNITS" . "AU")
		   ("APPARENT" . "AIRLESS") ;; NOT RELEV?
		   ("SOLAR_ELONG" . "'0,180'")
		   ("SUPPRESS_RANGE_RATE" . "NO")
		   ("EXTRA_PREC" . "NO")
		   ("R_T_S_ONLY" . "NO")
		   ("REF_SYSTEM" . "J2000")
		   ("CSV_FORMAT" . "YES")
		   ("OBJ_DATA"   . "NO")
		   ("QUANTITIES" . ,(%jpl-quantities-to-string quantities))
		   ))))

	   (cond
	     ;; failed in drakma
	     ((not text/nil)
	      (when done-tries
		(return (values nil text/nil returncode/err))))
	     ;; ambiguous object
	     ((and resolve-ambiguous-object
		   (setf ambiguous-resolver-result ;; kludgy setf
			 (resolve-ambiguous-jpl-horizons-id text/nil
							    :desig object-name
							    :mjd mjd-avg)))
	      (sleep 2)
	      (return
		(get-jpl-horizons-ephem
		 ambiguous-resolver-result
		 :mjd-start mjd-start :mjd-end mjd-end
		 :dt dt
		 :location location
		 :resolve-ambiguous-object nil ;; don't recurse deeper
		 :parse-ephem parse-ephem
		 :return-raw-hash-table return-raw-hash-table
		 :quantities quantities
		 :use-file-locking nil))) ;; if recursing, don't use lockfile again
	     ;; not parsing
	     ((not parse-ephem) ;; text/NIL is non-NIL here
	      (return (values text/nil returncode/err)))
	     ;; invalid result
	     ((not (and (search "$$SOE" text/nil)
			(search "$$EOE" text/nil)))
	      ;; abort if too many tries, or if we have ambiguous object (which we're not
	      ;; resolving, at this point in code)
	      (when (or done-tries 
			(jpl-response-has-multiple-bodies text/nil))
		(return (values nil text/nil nil))))
	     ;; parse the answer
	     (t ;; we have an answer
	      (let ((type (if (and (search "T-mag" text/nil) ;; comet type headers
				   (search "N-mag" text/nil)
				   (search "Comet physical" text/nil))
			      :comet
			      :asteroid)))
		;;(print text/nil)
		(return
		  (if (not return-raw-hash-table)
		      ;;
		      (%parse-jpl-ephem-block ;; parse into JPL-EPHEM
		       (subseq text/nil
			       (search "Date_________JDUT" text/nil) ;; grab header line
			       (search "$$EOE" text/nil))
		       type
		       :location location
		       :observatory observatory)
		      ;;
		      (%parse-jpl-ephem-block-as-hash ;; parse into a hash table of vectors
		       (subseq text/nil
			       (search "Date_________JDUT" text/nil) ;; grab header line
			       (search "$$EOE" text/nil)))))))))
	   (sleep 5))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-jpl-radecr-and-rates-for-observatory (object-name mjd observatory
						 &key (ntries 1)
						 (location *default-location*))
  "Use JPL Horizons to compute 
   (VALUES RA DEC DELTA DRA/DT DDEC/DT  RA-3SIGMA-ERROR DEC-3SIGMA-ERROR)
for OBJECT-NAME at MJD, OBSERVATORY.

NIL values may be returned, eg for undefined RA,DEC errors."
  (let ((ephem (get-jpl-horizons-ephem object-name
				       :mjd-start mjd
				       :mjd-end (+ mjd 0.01d0)
				       :dt "1h"
				       :observatory observatory
				       :location location
				       :ntries ntries)))
    (when ephem
      (values
       (* 1d0 (aref (jpl-ephem-ra ephem) 0))
       (* 1d0 (aref (jpl-ephem-dec ephem) 0))
       (* 1d0 (aref (jpl-ephem-delta ephem) 0))
       (* 1d0 (aref (jpl-ephem-dra/dt ephem) 0))
       (* 1d0 (aref (jpl-ephem-ddec/dt ephem) 0))
       (when (aref (jpl-ephem-ra-3sigma-err ephem) 0)
	 (* 1d0 (aref (jpl-ephem-ra-3sigma-err ephem) 0)))
       (when  (aref (jpl-ephem-dec-3sigma-err ephem) 0)
	 (* 1d0 (aref (jpl-ephem-dec-3sigma-err ephem) 0)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; allowed intervals:   3 "4 h" "5 m" "6 d"  (space not mandatory)
;; an integer without units means that the interval mjd-start to mjd-end is
;; divided into DT segments
(defun %parse-jpl-time-interval (dt mjd-start mjd-end &key (units :seconds))
  (declare (type (or string integer) dt)
	   (type double-float mjd-start mjd-end)
	   (type (member :mjd :seconds) units))

  ;; convert the answer in seconds to desired units (MJD or seconds)
  (flet ((to-units (seconds)
	   (nth-value 0 
		      (cond ((eq units :mjd) (/ seconds (* 24 3600d0)))
			    (t               (round seconds))))))
  
    (block nil
      
      (when (integerp dt) ;; it's an integer count between mjd-start,end
	(return (to-units (* 24 3600 (/ (- mjd-end mjd-start) dt)))))

      ;; a string representing an integer count, like above
      (let ((ndt (ignore-errors (parse-integer dt :junk-allowed nil))))
	(when ndt
	  (return (to-units (* 24 3600 (/ (- mjd-end mjd-start) ndt))))))


      (let* ((n-nondigit (position-if-not 'digit-char-p dt))
	     (digit-string (string-trim " " (subseq dt 0 n-nondigit)))
	     (unit-string  (string-trim " " (subseq dt n-nondigit)))
	     (i-dt (or (ignore-errors (parse-integer digit-string))
		       (error "Could not parse integer in ~A" dt)))
	     (dt-unit-in-sec
	       (cond ((member unit-string '("d" "day" "days") :test 'equalp)
		      (* 24 3600))
		     ((member unit-string '("h" "hr" "hour" "hours") :test 'equalp)
		      3600)
		     ((member unit-string '("m" "min" "minute" "minutes") :test 'equalp)
		      60)
		     (t
		      (error "Unrecognized units <~A> in DT=<~A>" unit-string dt)))))

	(to-units (* i-dt dt-unit-in-sec))))))

	   
	 

(defun concatenate-jpl-ephems (ephem-list)
  (setf ephem-list (remove nil ephem-list)) ;; warning - should check this to be safe
  (when ephem-list
    (let ((ntot (loop for ephem in ephem-list
		      sum (length (jpl-ephem-mjd ephem))))
	  (output-ephem (copy-jpl-ephem (first ephem-list))))
      (macrolet ((copy-one-jpl-vector (accessor)
		   `(progn
		      (setf (,accessor output-ephem) nil) ;; first nullify
		      (when (every (quote ,accessor) ephem-list) ;; must be defined in all ephems
			(setf (,accessor output-ephem) (make-array ntot))
			(loop with k = 0
			      with v-out = (,accessor output-ephem)
			      for ephem in ephem-list
			      for v-in = (,accessor ephem)
			      do (loop for i below (length v-in)
				       do (setf (aref v-out k) (aref v-in i))
					  (incf k)))))))
	(copy-one-jpl-vector jpl-ephem-mjd)
	(copy-one-jpl-vector jpl-ephem-ra)
	(copy-one-jpl-vector jpl-ephem-dec)
	(copy-one-jpl-vector jpl-ephem-ra-3sigma-err)
	(copy-one-jpl-vector jpl-ephem-dec-3sigma-err)
	(copy-one-jpl-vector jpl-ephem-dra/dt)	
	(copy-one-jpl-vector jpl-ephem-ddec/dt)
	(copy-one-jpl-vector jpl-ephem-rhelio)
	(copy-one-jpl-vector jpl-ephem-drhelio/dt)
	(copy-one-jpl-vector jpl-ephem-delta)
	(copy-one-jpl-vector jpl-ephem-ddelta/dt)
	(copy-one-jpl-vector jpl-ephem-phase-angle)
	(copy-one-jpl-vector jpl-ephem-total-mag)
	(copy-one-jpl-vector jpl-ephem-nucleus-mag)
	output-ephem))))
	       
      

(defun get-jpl-horizons-ephem/closest-orbit
    (object-name &key
		   ;;(parse-ephem t)
		   (mjd-start 57031.333333333336d0)
		   (mjd-end   57031.334027777775d0)
		   (dt        "1 m")
		   (location *default-location*) 
		   (observatory *default-observatory*)
		   (use-file-locking t)
		   (ntries 1)
		   ;;(return-raw-hash-table nil)
		   (quantities nil))
  "Like GET-JPL-HORIZONS-EPHEM but, for a long timespan, always pick the closest
orbit from the resolver, and concatenate the piecewise ephems together.

Unlike GET-JPL-HORIZONS-EPHEM, there is no PARSE-EPHEM or RETURN-RAW-HASH-TABLE 
because these are always taken to be T and NIL, respectively.

There is no RESOLVE-AMBIGUOUS-OBJECT because this always happens (it
is the purpose of this routine to split requests among objects
representing different epochs)

WARNING: there can still be eras with missing fields, like RA-3-SIMGA-ERR,
DEC-3SIGMA-ERR.
"

  (when (>= mjd-start mjd-end)
    (error "MJD-START>MJD-END"))

  (multiple-value-bind (ephem0 raw-text)
      (get-jpl-horizons-ephem
       object-name
       :parse-ephem t
       :mjd-start mjd-start :mjd-end mjd-end
       :dt dt  :location location :observatory observatory
       ;; don't resolve on first round - use RAW-TEXT to resolve below
       :resolve-ambiguous-object NIL 
       :ntries ntries
       :use-file-locking use-file-locking
       :quantities quantities)

    
    ;; if we got an ephem, we're done, because there were no ambiguous
    ;; orbits to resolve
    (when (typep ephem0 'jpl-ephem)
      (return-from get-jpl-horizons-ephem/closest-orbit ephem0))

    ;; if we got a NIL for the text too, there's a nerror
    (when (and (not ephem0) (not (stringp raw-text)))
      (return-from get-jpl-horizons-ephem/closest-orbit nil))

    (let* ((resolver-list  ;; already sorted oldest first
	     (parse-resolver-jpl-response 
	      raw-text :desig object-name))
	   (nr (length resolver-list))
	   (dt/sec (%parse-jpl-time-interval
		    dt mjd-start mjd-end :units :seconds))
	   (resolver-mjd-orb-vec
	     (make-array nr :initial-element 0d0
			    :element-type 'double-float))
	   (resolver-mjd-start-vec
	     (make-array nr :initial-element most-positive-double-float
			    :element-type 'double-float))
	   (resolver-mjd-end-vec
	     (make-array nr :initial-element most-negative-double-float
			    :element-type 'double-float))
	   (resolver-n-ephem-vec
	     (make-array nr :initial-element 0 :element-type 'fixnum))
	   ;; the output ephem list
	   (ephem-list nil))

     (when (not (zerop (rem dt/sec 60)))
	(error "The ephemeris time interval in seconds is not a multiple of 60.  This will prevent an evenly sampled ephemeris spanning multiple JPL orbits.  The interval should be specified using the notation '10 m'  or '2 h' or '5 d', not as a simple integer."))
      
      ;; note the MJD of each orbit in resolver-mjd-orb-vec 
      (loop for i below nr
	    for rlist in resolver-list
	    for year = (second rlist) ;; year of JPL's ephem
	    for mjd = (astro-time:decimal-year-to-mjd (float year 1d0))
	    do (setf (aref resolver-mjd-orb-vec i) mjd)) 
	   

      ;; for some reason, no orbits - should not happen
      (when (not resolver-list)
	(return-from get-jpl-horizons-ephem/closest-orbit nil))

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; return the index of the orbit with the closest MJD to this MJD
      (flet ((search-for-closest-mjd (mjd)
	       (declare (type double-float mjd))
	       (loop with i-best-orbit = 0
		     with min-dev of-type double-float
		       = most-positive-double-float
		     for i from 0
		     for mjd-orbit across resolver-mjd-orb-vec
		     for dev = (abs (- mjd mjd-orbit))
		     when (< dev min-dev)
		       do (setf min-dev dev)
			  (setf i-best-orbit i)
		     finally (return i-best-orbit))))
	
	;; find the orbit spans for each mjd of the ephemeris; we loop
	;; by UT in integer seconds to prevent roundoff error accumulation
	(loop with ut-start = (astro-time:mjd-to-ut mjd-start) ;; lisp seconds
	      with ut-end   = (astro-time:mjd-to-ut mjd-end)
	      for ut from ut-start to ut-end by dt/sec
	      for mjd = (astro-time:ut-to-mjd ut)
	      for i = (search-for-closest-mjd mjd)
	      do (setf (aref resolver-mjd-start-vec i)
		       (min (aref resolver-mjd-start-vec i) mjd))
		 (setf (aref resolver-mjd-end-vec i)
		       (max (aref resolver-mjd-end-vec i) mjd))
		 (incf (aref resolver-n-ephem-vec i))))
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
      (setf ephem-list
	   (loop with dt/min = (/ dt/sec 60)
		 with dt-string = (format nil "~A min" dt/min)
		 for i below nr
		 for rlist in resolver-list
		 for this-object-name = (first rlist)
		 for this-mjd-start across resolver-mjd-start-vec
		 for this-mjd-end   across resolver-mjd-end-vec
		 for n-ephem   across resolver-n-ephem-vec
		 when (plusp n-ephem)
		   collect (get-jpl-horizons-ephem
			    this-object-name
			    :parse-ephem t
			    :mjd-start this-mjd-start :mjd-end this-mjd-end
			    :dt dt-string
			    :location location :observatory observatory
			    :resolve-ambiguous-object NIL  ;; already resolved
			    :ntries ntries
			    :use-file-locking use-file-locking
			    :quantities quantities)))

      (when ephem-list (concatenate-jpl-ephems ephem-list)))))




      
	

	

	
	   
      

