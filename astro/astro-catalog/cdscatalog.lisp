
#| use the cds catalog retrieval external programs


For vizquery see
   http://cdsarc.u-strasbg.fr/doc/vizquery.htx
For catalog list see
   http://cdsarc.u-strasbg.fr/viz-bin/vizHelp?cats/U.htx

Place the following into your Lisp configuration:
 ;; configure astro-catalog
 (pconfig:set-config "ASTRO-CATALOG:VIZQUERY-PROGRAM" "/opt/local/bin/vizquery")
 (pconfig:set-config "ASTRO-CATALOG:VIZQUERY-SITE" "cfa")


the main function is:

(astro-catalog::run-vizquery-and-parse  
	  123.23 45.54 1.0 "usnob"
	  '(("USNO-B1.0" :ID string "") ;; vizquery-name our-name type default-value
	    ("RAJ2000" :ra double-float 0d0)
	    ("DEJ2000" :dec double-float 0d0)
	    ("R2mag" :r2 single-float 0.0)))


WARNING! - THIS WILL READ ONLY THE FIRST DATA BLOCK IN A VIZQUERY - It
is possible for vizquery to return serveral results from several
catalogs.  Eg, 2MASS returns the point source and extended source
catalogs together sequentially.


This thing is really a mess.  It should use vizquery to return fits
tables to read in, instead of ascii format and complicated parsing.

It could probably also give a list of pointings at once.

|#

(in-package astro-catalog)

;; output which site we're querying, and now, for diagnostics
(defvar *vizquery-verbose* nil) ;; can be NIL, T, stream

(defun vizquery-mesg (format-string &rest format-args)
  (when *vizquery-verbose*
    (let ((stream (if (eq *vizquery-verbose* t)
		      *standard-output*
		      *vizquery-verbose*)))
    (apply 'format stream format-string format-args)
    (terpri stream))))

(defparameter *vizquery-program* 
  (or (pconfig:get-config "ASTRO-CATALOG:VIZQUERY-PROGRAM")
      ;; try to find vizquery in some likely places
      (namestring
       (or (probe-file "/usr/local/bin/vizquery") ;; standard location
	   (probe-file "/usr/bin/vizquery")       
	   (probe-file "/opt/local/bin/vizquery") ;; macports
	   (probe-file "/sw/bin/vizquery")        ;; fink
	   (probe-file (concatenate
			'string
			(#-abcl uiop/os:getenv ;; in asdf
			 #+abcl ext:getenv "HOME")  "/bin/vizquery"))
	   ;; try running just vizquery to see if we get a return of '1'
	   (if (eql 1 (ignore-errors (uiop:run-program "vizquery"))) "vizquery" NIL)
	   (error "Cannot find vizquery program - you need to set CL-USER::*VIZQUERY-PROGRAM*")))))

(defparameter *vizquery-site* 
  (or (pconfig:get-config "ASTRO-CATALOG:VIZQUERY-SITE")
      "adac")) ;; seems to be working
  
;; series of several top sites - put adac and cds first because they seem to be
;; working
(defparameter *vizquery-site-list* '("adac"  "cds" "cadc" "cfa" "ukirt"
				     "cambridge" "beijing" "moscow"))
  

(defparameter *vizquery-timeout* 60)

(defun %run-program (program argument-list)
  (run-program:run-program-to-string  
   program argument-list
   :timeout *vizquery-timeout*)) 
   


;; run a program in the shell.  using asdf's run-program is deprecated but
;; it is the only one that supports ABCL. Others, like INFERIOR-SHELL,
;; are probably better, but don't support ABCL.
#+nil
(defun %run-program (program argument-string)
  (let* ((return-value nil)
	 (output-string
	  (with-output-to-string (s)
	    #-abcl
	    (let ((asdf:*verbose-out* s))
	      (setf return-value
		    ;; abcl's version of asdf:run-shell-command fails
		    ;; to return value when output stream bound, so we
		    ;; need the following kludge
		    (asdf:run-shell-command  "~A ~A" program argument-string)))
	    #+abcl (setf return-value
			 (extensions:run-shell-command
			  (format nil "~A ~A" program argument-string) :output s))))
	 ;; strip out the initial line which contains the command - UGLY
	 (final-output-string
	  (with-output-to-string (s)
	    (with-input-from-string (sin output-string)
	      (loop for i from 0
		    for line = (read-line sin nil nil)
		    until (not line)
		    when #-abcl (plusp i) #+abcl t ;; abcl's extensions::run-shell-command doesn't add extra line
		      do (write-line line s))))))
    (values return-value final-output-string)))


(defstruct vz-resource
  resource
  vizquery-args
  error
  column-list
  units-list
  lines-list)

(defun %parse-vizquery-output (string vizquery-args)
  (with-input-from-string (s string)
    (let ((column-list nil)
	  (units-list nil)
	  (output-lists nil)
	  (resource nil)
	  (error nil))
      (block parse
	(loop 
	  with in-data = nil
	  for line = (read-line s nil nil)
	  if (not line)
	    do  (when (not units-list)
		  (setf error "truncated input, or no data")
		  (return-from parse t))
		(return-from parse t)
	  else
	    do
	 ;; structure of file is  
	 ;; 1) lines that start with hash or are blank 2) a fields line 3) units line 
	 ;; 4) line with ----- 5) data lines
	 (cond ((zerop (length line))
		(when in-data (return-from parse t)) ;; this data block is over
		t)
	       ((char= (aref line 0) #\#)
		(when (and (> (length line) 10)
			   (string-equal line "#RESOURCE=" :end1 10 :end2 10))
		  (setf resource (subseq line 11)))
		(when in-data
		  (setf error "zero length line in data block")
		  (return-from parse t))
		t)
	       ((and units-list (not in-data)) ;; eat blank line after units line
		(when (not (every (lambda (c) (or (char= c #\-) (char= c #\;))) line))
		  (setf error "Strange vizquery output: line after units not '---;---;----'")
		  (return-from parse t))
		(setf in-data t)
		t)
	       (in-data
		(push (string-utils:split-string line ";" :strict t) output-lists))
	       ((not column-list)
		(setf column-list  (string-utils:split-string line ";" :strict t)))
	       ((not units-list)
		(setf units-list  (string-utils:split-string line ";" :strict t))))))

      (setf output-lists (nreverse output-lists))
      
      (when (not error)
	(let ((n (length column-list)))
	  (when (not (and (= (length units-list) n)
			  (every (lambda (list) (= (length list) n)) output-lists)))
	    (setf error "Every line in vizquery output does not have same length"))))
      ;;
      (make-vz-resource
       :vizquery-args vizquery-args
       :resource resource
       :error error 
       :column-list column-list
       :units-list units-list
       :lines-list output-lists)))) 
			  
		  
	 


(defun run-vizquery (ra dec rad/arcmin source 
		     &key (vizquery-site *vizquery-site*)
		       (vizquery-program *vizquery-program*))
  (let* ((vizquery-args
	   (list
	    (format nil "-c=~,4F~A~,4F"
		    ra (if (minusp dec) "-" "+") (abs dec))
	    (format nil "-c.rm=~,8F" rad/arcmin)
	    "-mime=csv"
	    (format nil "-source=~A" source)
	    (format nil "-site=~A" vizquery-site))))
    (vizquery-mesg  "Starting vizquery call to site ~A with args ~A"
		    vizquery-site vizquery-args)
    (multiple-value-bind (retval string)
	(%run-program *vizquery-program* vizquery-args)
      (vizquery-mesg "Done call to site ~A" vizquery-site)
      (cond ((not (zerop retval))
	     (error "Error calling ~A ~A - shell returned ~A" vizquery-program vizquery-args retval))
	    ((not (char= (aref string 0) #\#))
	     (error "Error  calling ~A ~A (returned ~A) ~A ...."
		    vizquery-program vizquery-args retval
		    (subseq string 0 (min 100 (length string)))))
	    (t
	     (%parse-vizquery-output string vizquery-args))))))

	    

;; field-descs is a list like (("RAJ2000" :ra double-float dummy-value)
;; the return is (values #(vectors-of-vectors) keyword-list 
;; the type may be 'string 'single-float 'double-float 'fixnum
(defun parse-vizquery-fields (vz-resource field-descs)
  (let ((keys (vz-resource-column-list vz-resource))
	(lines (vz-resource-lines-list vz-resource)))
  (loop for field in field-descs 
	for fieldname = (first field)
	when (not (find fieldname keys :test 'equalp))
	  do (error "Could not find field ~A in vizquery output, which has fields ~A" fieldname keys))
  (let* ((n (length field-descs))
	 (outkeys (mapcar 'second field-descs))
	 (nlines (length lines))
	 (v (make-array n))
	 ;; hash table that maps "RAJ2000" to the index in a line
	 (h (make-hash-table :test 'equalp)))
    ;;
    (loop for key in keys
	  for i from 0
	  do (setf (gethash key h) i))
    ;;
    (loop for field in field-descs 
	  for type = (third field)
	  for i from 0
	  do (setf (aref v i) (make-array nlines :element-type type)))
    ;;
    (loop for iline below nlines
	  for line in lines
	  do (loop for field in field-descs
		   for type = (third field)
		   for dummy-value = (fourth field)
		   for ifield = (gethash (first field) h)
		   for item-string = (string-trim " " (nth ifield line))
		   for vec across v
		   do (let ((value (cond ((eq type 'single-float)
					  (float (if (equal item-string "") 
						     dummy-value
						     (jk-parse-float:parse-float item-string)) 
						 1.0))
					 ((eq type 'double-float)
					  (float  (if (equal item-string "") 
						     dummy-value
						      (jk-parse-float:parse-float item-string))
						  1d0))
					 ((eq type 'integer)
					  (if (equal item-string "") 
					      dummy-value
					      (parse-integer item-string)))
					 ((eq type 'string)
					  (if (equal item-string "") 
					      dummy-value
					      item-string))
					 (t (error "Unknown type ~A" type)))))
			(setf (aref vec iline) value))))
    ;;
    (values v outkeys))))
    
		
	  
;; run vizquery and produced a parsed output 
(defun run-vizquery-and-parse (ra dec rad/arcmin source field-descs 
			       &key (vizquery-site *vizquery-site*) 
				 (vizquery-program *vizquery-program*))
  (let ((vz-resource  (run-vizquery ra dec rad/arcmin source
				    :vizquery-site vizquery-site 
				    :vizquery-program vizquery-program)))
    (if (not (vz-resource-error vz-resource))
	(parse-vizquery-fields vz-resource field-descs)
	(values nil (vz-resource-error vz-resource)))))


;; run for multiple sites until one works, returning (VALUES DATA-VEC KEYS) 
;; or (VALUES NIL ERROR) on error

(defparameter *sitelock* (bordeaux-threads:make-lock "cds-sitelock"))
(defparameter *last-good-site* nil)



;; diagnostic routine to print which sites are responding
;; returns list of ("site" good/bad  response-time/sec)
(defun get-cdsclient-site-timings (&key (vizquery-site-list *vizquery-site-list*) 
					  (vizquery-program *vizquery-program*))
  (loop with timings = nil
	for site in vizquery-site-list
	do
	   (let* ((t0 (get-internal-real-time))
		  (result (ignore-errors
			   (run-vizquery-and-parse 0.0 0.0 4.0  "2MASS-PSC"
						   '(("2MASS" :id string "")
						     ("RAJ2000" :ra double-float 1d99)
						     ("DEJ2000" :dec double-float 1d99))
						   :vizquery-program vizquery-program
						   :vizquery-site site)))
		  (ok-result (and (vectorp result)
				  (vectorp (aref result 0))
				  (>= (length (aref result 0)) 5)))
		  (t1 (get-internal-real-time))
		  (interval/sec (/ (- t1 t0) (* 1d0 internal-time-units-per-second))))
	     (push (list site ok-result interval/sec)
		   timings))
	finally
	   (return timings)))
	     


(defun run-vizquery-and-parse/multisites (ra dec rad/arcmin source field-descs 
					  &key (vizquery-site-list *vizquery-site-list*) 
					  (vizquery-program *vizquery-program*))
  
  (loop 
    with data-vec = nil and error-or-keys = nil ;;
    ;; loop across sites favoring the last site that worked
    for site in (bordeaux-threads:with-lock-held (*sitelock*)
		  (remove nil
			  (cons  *last-good-site*
				 (remove *last-good-site* vizquery-site-list  :test 'equalp))))
    do (multiple-value-setq (data-vec error-or-keys)
	 (ignore-errors
	   (run-vizquery-and-parse ra dec rad/arcmin source field-descs 
				   :vizquery-program vizquery-program
				   :vizquery-site site)))
       (when data-vec
	 (bordeaux-threads:with-lock-held (*sitelock*)
	   (setf  *last-good-site* site))
	 (return (values data-vec error-or-keys)))
    finally (error error-or-keys)))
     
