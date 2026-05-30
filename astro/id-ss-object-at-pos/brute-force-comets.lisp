;; search all comets by brute force

(defpackage brute-force-comets
  (:use #:cl)
  (:export
   #:brute-force-search-comets ;; at MJD, RA, DEC
   #:build-comet-database
   ))

(in-package brute-force-comets)

(defparameter *comet-data-dir*
  (concatenate 'string
	       (namestring
		(asdf:system-source-directory
		 (asdf:find-system "brute-force-comets")))
	       "data/"))

;; has fields of form of lists (name year orbit)
(defparameter *comet-data-file*
  (concatenate 'string *comet-data-dir* "comet-orbits-by-year.dat"))

;; KEY=NAME,  VALUE=((YEAR ORBIT) (YEAR ORBIT) ..)
(defvar *comet-hash* (make-hash-table :test 'equalp))
(defvar *comet-data-read* nil)


;; in file comet-data are (NAME YEAR ORBIT)
(defun read-comet-data (&key (infile *comet-data-file*)
			  (hash *comet-hash*))
  "Read the comet data from *COMET-DATA-FILE* into *COMET-HASH*"
  (clrhash hash)
  (with-open-file (sin infile :direction :input)
    (loop for lst = (read sin  nil :eof)
	  until (eq lst :eof)
	  for name  = (first lst)
	  for year = (second lst)
	  for orbit = (third lst)
	  do (push (list year orbit)
		   (gethash name hash)))))

(eval-when (:load-toplevel)
  (read-comet-data))


(defun backup-file (file)
  (when (probe-file file)
    (let ((bfile (format nil "~A.BAK" file)))
      (file-io:copy-file file bfile :overwrite t))))

(defun write-comet-data (&key  (outfile *comet-data-file*)
			   (hash *comet-hash*))
  "Write comet data *COMET-HASH* to *COMET-DATA-FILE*"
  (backup-file outfile)
  (with-open-file (sout outfile :direction :output :if-exists :supersede)
    (loop for comet-name being the hash-key of hash
	  for year-orb-list being the hash-value of hash
	  do (loop for (year orbit) in year-orb-list
		   for lst = (list comet-name year orbit)
		   do (write lst :stream sout)
		      (terpri sout)))))
	       

(defun get-comet-list ()
   (mapcar 'first
	   (alexandria:hash-table-alist
	    small-body-name::*all-comet-els-name-hash*)))

(defun build-comet-database (&key (start-year 1980) (end-year 2030)
				  (dyear 5) (hash *comet-hash*)
			       (write-comet-file t)
			       (verbose nil))
  "Retrieve comet orbits, taken from SMALL-BODY-NAME, by year,
and write to the storage file *COMET-DATA-FILE*."
  (loop for year from start-year to end-year by dyear
	do (loop for comet-name in (get-comet-list)
		 for mjd = (astro-time:decimal-year-to-mjd
			    (float (round year) 1d0))
		 for key = (list comet-name year)
		 ;; do only those that aren't done
		 when (not (gethash (list comet-name year) hash))
		   do (let ((orbit
			      (or (ignore-errors
				   (jpl-horizons:get-jpl-horizons-elements
				    comet-name :mjd mjd))
				  (ignore-errors
				   (mpc:get-mpc-elements comet-name
							 :mjd mjd)))))
			(when verbose (format t "~A ~A~%"
					      key
					      (if orbit "have-orbit"
						  "no-orbit")))
			(when orbit
			  (setf (gethash key hash) orbit)))))
  (when write-comet-file
    (write-comet-data :hash hash)))


;; teset if orbit falls within tol/arcmin of RA,DEC,MJD
(defun %test-orbit (orbit ra dec mjd tol/arcmin)
  (multiple-value-bind (rao deco)
      (slalib-ephem:compute-radecr-from-comet-elem-for-observatory
       orbit mjd "geocenter")
    (let ((dist/arcmin
	    (astro-coords:sky-angle ra dec rao deco :units :arcmin)))
      (when (<= dist/arcmin tol/arcmin)
	dist/arcmin))))
  

(defun brute-force-search-comets (ra dec mjd &key (tol/arcmin 20)
					       (hash *comet-hash*))
  "Given an RA,DEC,MJD search internal database of all comets,
usig orbit closest to MJD, for matches within TOL/ARCMIN."
  (loop with ra = (float ra 1d0)
	with dec = (float dec 1d0)
	with outlist = nil
	with year-mjd = (astro-time:mjd-to-decimal-year mjd)
	for comet-name being the hash-key of hash
	for year-orbit-list being the hash-value of hash
	;; loop in year-orbit-list to find the closest (year orbit)
	for orbit-year = (loop with best-dtime = 1d10
			       with best-orbit-year = nil
			       for (year orbit) in year-orbit-list
			       for dtime = (abs (- year year-mjd))
			       when (< dtime best-dtime)
				 do (setf best-dtime dtime)
				    (setf best-orbit-year
					  (list orbit year))
			       finally (return best-orbit-year))
	for orbit = (first orbit-year)
	for year = (second orbit-year)
	when orbit-year
	  do
	     (let ((dist/arcmin
		     (ignore-errors ;; ignore numerical failures
		      (%test-orbit orbit ra dec mjd tol/arcmin))))
	       (when dist/arcmin ;; if T, it's within TOL/ARMIN
		 ;; just in case this comet is eactly between two orbit
		 ;; years so it shows up twice
		 (when (not (find comet-name outlist :key 'first))
		   (push (list comet-name dist/arcmin year) outlist))))
	finally
	   (return outlist)))
  
  
			  
			
				 
				 
				  
  
  
  
