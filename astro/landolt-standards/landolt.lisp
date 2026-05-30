
#|

Read in a list of landolt standards, setting *LANDOLT-STARS* to a list of LANDOLT-STAR
structures.

Function (FIND-NEAREST-LANDOLT-STARS RA DEC DIST/ARCSEC) returns a list of LANDOLT-STAR within
DIST/ARCSEC of RA,DEC as
  ((STAR1  DIST1) (STAR2 DIST2) ...)

|#


(defpackage landolt
  (:use #:cl)
  (:export
   #:*landolt-stars*
   #:find-nearest-landolt-stars
   #:find-landolt-by-name

   #:landolt-star #:landolt-star-p
   #:landolt-star-name
   #:landolt-star-vmag
   #:landolt-star-b-v #:landolt-star-u-b #:landolt-star-v-r #:landolt-star-r-i
   #:landolt-star-nobs #:landolt-star-ra #:landolt-star-dec
   #:landolt-star-pmra #:landolt-star-pmdec
   #:landolt-star-simbadname
   ))

(in-package landolt)

(defparameter *landolt-dir*
  (asdf:system-source-directory (asdf:find-system "landolt")))


(defparameter *landolt-data-file*
  (concatenate 'string
	      (namestring *landolt-dir*)
	      "data/landolt2009table.txt"))

(defstruct landolt-star
  name vmag b-v u-b v-r r-i nobs
  ra dec
  (mjd 51544.0d0)  ;; WHEN this ra,dec is true, by default 2000
  pmra pmdec  ;; mas/yr
  simbadname)

(defvar *landolt-stars* nil)
(defvar *landolt-star-hash*  (make-hash-table :test 'equalp))

#|
            Vmag   B-V    U-B    V-R    R-I    N   RAJ20        DEJ20        pmRA     pmDE                         
Name        (mag)  (mag)  (mag)  (mag)  (mag)  obs 00 ("h:m:s") 00 ("d:m:s") (mas/yr) (mas/yr) SimbadName        
PG0918+029D 12.272  1.044  0.821  0.575  0.535  19 09 21 21.936 +02 47 28.28   -15.60   -10.10 [L92b] PG 0918+029 D
|#

(defun parse-landolt-line (line)
  (flet ((get-field (n1 n2 type)
	   (let ((ss (string-trim " " (subseq line n1 n2))))
	     (cond ((eq type :string)
		    ss)
		   ((eq type :integer)
		    (parse-integer ss))
		   ((eq type :float)
		    (jk-parse-float:parse-float ss :output-type 'single-float))
		   ((eq type :ra)
		    (ra-dec:hms-string->deg ss))
		   ((eq type :dec)
		    (ra-dec:dms-string->deg ss))))))
    (make-landolt-star
     :name  (get-field 0 11   :string)
     :vmag  (get-field 12 18  :float)
     :b-v   (get-field 19 25  :float) ;; each color can have a minus in front of it
     :u-b   (get-field 26 32  :float)
     :v-r   (get-field 33 39  :float)
     :r-i   (get-field 40 47  :float)
     :nobs  (get-field 47 50  :integer)  ;; can have 100+ obs
     :ra    (get-field 51 63  :ra)
     :dec   (get-field 64 76  :dec)
     :pmra  (get-field 77 85  :float)  ;; can be as wide as -xxxx.xx
     :pmdec (get-field 86 94  :float) 
     :simbadname (get-field 95  nil :string)))) 

(defun read-landolt-file (&key (infile *landolt-data-file*))
  ;; clear header
  (with-open-file (s infile)
    (loop for line = (read-line s nil nil)
	  until (not line)
	  when (eql 0 (search "Name" line))
	    do (read-line s nil nil) ;; clear line
	       (return)
	  finally (error "Premature end of Landolt file"))
    ;;
    (setf *landolt-stars*
	  (loop for line = (read-line s nil nil)
		until (< (length line) 2)
		collect (parse-landolt-line line)))
    (clrhash *landolt-star-hash*)
    (loop for lstar in *landolt-stars*
	  do (setf (gethash (landolt-star-name lstar) *landolt-star-hash*)
		   lstar))
    *landolt-stars*))
	  
    

(eval-when (:load-toplevel)
  (when (not *landolt-stars*)
    (read-landolt-file)))
  

(defun find-nearest-landolt-stars (ra dec tol/arcsec &key (mjd 51544.0d0))
  "Return a list of Landolt stars within tol/arcsec of RA,DEC
as ((landolt-star dist/arcsec) ...) sorted by distance.

If MJD (for proper motions) not given, it is assumed to be 51544.0, or 2000-01-01."
  (loop with ra = (* 1d0 ra)
	with dec = (* 1d0 dec)
	;; clunky - the shifted coords of star, set when computing dist
	with rastar = 0d0 and decstar = 0d0 
	;; years from 2000
	with dyears = (/ (- mjd 51544.0d0) 365.25)
	for star in *landolt-stars*
	for dist = (progn
		     (multiple-value-setq (rastar decstar)
		       (astro-coords:sky-angles-slew ;; move the star by propermotion
			(landolt-star-ra star)
			(landolt-star-dec star)
			(* dyears 0.001d0 (landolt-star-pmra star))
			(* dyears 0.001d0 (landolt-star-pmdec star))))
		     (astro-coords:sky-angle ra dec rastar decstar
					     :units :arcsec))
	when (<= dist tol/arcsec)
	  collect (let ((outstar (copy-landolt-star star)))
		    (setf (landolt-star-mjd outstar) mjd)
		    (setf (landolt-star-ra  outstar) rastar)
		    (setf (landolt-star-dec outstar) decstar)
		    (list outstar dist))
	    into outlist
	finally (return (sort outlist '< :key 'second))))

(defun find-landolt-by-name (name)
  (gethash (string-trim " " name) *landolt-star-hash*))
	
