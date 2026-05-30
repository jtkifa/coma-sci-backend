#|

Routines for addressing astronomical time.

Notes on JD/MJD <---> UT conversion: It is correct (agrees with JPL)
if the JD is assumed to be JDUT.  Both UTC and JDUTC have leap seconds
inserted.

This means that JD and MJD are not a smoothly varying quantity, if
generated from UTC, and that differences in JD cannot be used to
compute true time intervals.

In general, the MJD reported in telescope headers is be derived from
the correspnding UTC.

The true (atomic time) is given by TAI, and a file of offsets
is here: ftp://maia.usno.navy.mil/ser7/tai-utc.dat

The time scales relevant to astronomy are:

TAI    -  atomic time

UTC    - atomic time leap-second adjusted to the earth's rotation, really
         a measure of the angular position of the earth.  Technically UTC
         should not be converted to MJDUT because of overlaps at the leap
         seconds.

MJDUTC - MJD based on the current UTC, using standard conversion function.
         This has leap seconds so that subtracting two MJDUTCs does not
         give a correct time differnce if leap seconds have been inserted.

TT     - Terrestrial Time, TT=TAI+32.184s, the time for computing
         ephemerides.

TBD    - Barycentric Dynamical Time, TT with quasi-periodic adjustments for
         General Relativistic effects.

MJDTAI - MJD that varies smoothly with TAI, derived from MJDUT
         using a function like MJDUTC-TO-MJDTAI

MJDTT  - MJD that varies smoothly with TT, derived from MJDUT
         using a function like MJDUTC-TO-MJDTT

The routine SLALIB:CORRECT-MJDUT-TO-MJDTT in the SLALIB package
performs the corrections from UT to MJDTT. F2C can be used to convert it
to Fortran using (f2cl:f2cl "/slalib/dir/dat.f") which is how we obtained
TAI-MINUS-UTC, MJDUTC-TO-MJDTAI, and MJDUTC-TO-MJDTT


|#



(in-package astro-time)

(defun calendar-date-to-jd (year month day hour min sec &key (mjd nil))
 "Convert universal time (year month day hr min sec) to decimal Julian day -
if optional MJD keyword is true, then do an MJD conversion instead.  Doing
MJD conversion this way should preserve full double float precision."
 (declare (fixnum month day year hour min)
	  (real sec))

  (when (or (< month 1) (> month 12))
    (error "calendar-date-to-jd: Month = ~A out of range [1-12]" month))
  (when (or (< day 1) (> day 31))
    (error "calendar-date-to-jd: Day = ~A out of range [1-31]" day))
  (when (or (< hour 0) (> hour 24))
    (error "calendar-date-to-jd: Hour = ~A out of range [0 24]" hour))
  (when (or (< min 0) (> min 60))
    (error "calendar-date-to-jd: Minute = ~A out of range [0 60]" min))
  (when (or (< sec 0) (> sec 60))
    (error "calendar-date-to-jd: Second = ~A out of range [0 60]" sec))
  

 (prog ((igreg 0) (jy 0) (jm 0) (julday 0) (ja 0) (frac 0d0))
  (declare (fixnum igreg jy jm julday ja))

  (setq igreg (+ 15 (* 31 (+ 10  (* 12 1582)))))
    (if (= year 0)
	(error "calendar-date-to-jd: There is no year zero in Julian calendar.")) 
  (if (< year 0) (setf year (1+ year))) 

  (cond 
   ((> month 2) 
    (setf jy year) 
    (setf jm (1+ month)))
   (t 
    (setf jy (1- year)) 
    (setf jm (+ month 13)))) 

  (setf julday (+ (+ (+ (floor (* 365.25d0 jy)) 
			(floor (* 30.6001d0 jm))) 
		     day)
                  1720995))

  (when mjd (decf julday  2400000)) ;; remember another -0.5 at end!
 
  (when (>= (+ day (* 31 (+ month (* 12 year)))) igreg)
   (setf ja (floor (* 0.01d0 jy)))
   (setf julday (+ (- (+ julday 2) ja) (floor (* 0.25d0 ja)))))

  (setf frac (/ (+ (* 3600d0 hour)   (* 60d0 min)  sec)
		#.(* 24d0 3600)))

  (return (+ julday 
	     frac -0.5d0 ;; -0.5 is because JD is from NOON, not midnight
	     (if mjd -0.5d0 0d0))))) ;; the 0.5 we omitted above


  
  
(defun calendar-date-to-mjd (year month day hour min sec)
  "Convert universal time (year month day hr min sec) to decimal MJD"
  (calendar-date-to-jd year month day hour min sec :mjd t))





(defun jd-to-calendar-date (jday)
  "convert a Julian day back into a calendar day, returning YEAR MONTH DAY HOUR MIN SEC -
the SEC, although double precision will be accurate only to 5 decimal places or so."
  (declare (type real jday))
  
  (let ((igreg 0) (julian 0) (fracday 0d0) (id 0) (iyyy 0) (jb 0) (jc 0) 
	(ja 0) (jd 0) (je 0) (jalpha 0) (mm 0)
	(hr 0) (min 0) (sec 0d0))
    (declare (type fixnum igreg mm id iyyy jb jc ja jalpha))

    (multiple-value-setq (julian fracday)
      (floor (+ 0.5d0 (float jday 1d0)))) ;; 0.5 pushes back to midnight
     
     (setq igreg 2299161) 
     (cond 
   ((>= julian igreg) 
    (setf jalpha (floor
		  (/ (- (- julian 1867216) 0.25d0) 36524.25d0)))
    (setf ja (- (+ (+ julian 1) jalpha) (floor (* 0.25d0 jalpha)))))
   (t
    (setf ja julian)))
     (setf jb (+ ja 1524)) 
     (setf jc (floor
	       (+ 6680
		  (/ (- (- jb 2439870) 122.0999d0) 365.25d0)))) 
     (setf jd (+ (* 365 jc) (floor (* 0.25d0 jc)))) 
     (setf je (floor (/ (- jb jd) 30.6001d0))) 
     (setf id (- (- jb jd) (floor (* 30.6001d0 je)))) 
     (setf mm (1- je)) 
     (if (> mm 12) (setf mm (- mm 12))) 
     (setf iyyy (- jc 4715)) 
     (if (> mm 2) (setf iyyy (1- iyyy))) 
     (if (<= iyyy 0) (setf iyyy (1- iyyy))) 
     
     (let* ((daysec #.(*  3600 24)) ;; total secs in a day
	    (secs-left 0))          ;; how many secs left to account for
       (multiple-value-bind (fracsecs frac-of-sec)
	   (floor (* daysec fracday))
	 (setf secs-left fracsecs)
	 (setf hr (truncate secs-left 3600))
	 (decf secs-left (* 3600 hr))
	 (setf min (truncate secs-left 60))
	 (decf secs-left (* 60 min))
	 (setf sec (+ secs-left frac-of-sec))))

     (values  iyyy mm id hr min sec)))



(defun mjd-to-calendar-date (mjd)
  "convert a MJD back into a calendar day, returning YEAR MONTH DAY
HOUR MIN SEC - the SEC, although double precision will be accurate
only to 5 decimal places or so."
  (jd-to-calendar-date (+ mjd 2400000.5d0)))


;; if hr,min,sec are over 59, then normalize the date by rolling the calendar
;; over.  Works only for 1899 onward
(defun %normalize-date (year month day hr min sec)
  (let ((extra-sec 0))
    (when (> sec 59)
      (incf extra-sec (- sec 59))
      (setf sec 59))
    (when (> min 59)
      (incf extra-sec (* 60 (- min 59)))
      (setf min 59))
    (when (> hr 23)
      (incf extra-sec (* 3600 (- hr 23)))
      (setf hr 23))
    (cond ((zerop extra-sec)
	   (values year month day hr min sec))
	  (t
	   (let ((ut (encode-universal-time sec min hr day month year)))
	     (incf ut extra-sec)
	     (multiple-value-bind (sec1 min1 hr1 day1 month1 year1)
		 (decode-universal-time ut)
	       (values year1 month1 day1 hr1 min1 sec1)))))))
	  
      
	  
      

	


(defun calendar-date-to-date-string (year month day hr min sec 
				     &key (stream nil) (frac-seconds t))
  "Format UT year month day hr min sec as string YYYY-MM-DDTHH:MM:SS.SS.
May be off by 0.01 sec if you give it xx.999 seconds, to avoid having to
roll everything over calendarwise."
  (declare (type unsigned-byte year month day hr min)
	   (type real sec))
  (let* ((sec-int (floor sec))
	 (sec-frac (round (* 100 (- sec sec-int)))))
    ;;
    ;; handle freak case of seconds = 59.9999999
    (when (>= sec-frac 100) 
      (setf sec-frac 0)
      (incf sec-int)
      (multiple-value-setq (year month day hr min sec-int )
	(%normalize-date year month day hr min sec-int)))
    ;;
    (cond (frac-seconds
	   (format stream "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0D.~2,'0D"
		   year month day hr min sec-int sec-frac))
	  (t
	   (format stream "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0D"
		   year month day hr min sec-int))))) 

  
(defun ut-to-date-string (ut)
  "Convert Lisp integer UT to a date string, assuming Timezone=0"
   (multiple-value-bind (sec min hour day month year)
       (decode-universal-time ut 0)
     (format nil "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0D"
	     year month day hour min sec)))
   


(defun JD-to-ut-string (jd &key (frac-seconds t))
"Convert JD to YYYY-MM-DDTHH:MM:SS.SS or YYYY-MM-DDTHH:MM:SS
depending on FRAC-SECONDS"
  (multiple-value-bind (iyyy mm id hr min sec)
      (jd-to-calendar-date jd)
    ;; avoid seconds being 60.00 - if >59.99 recompute and add 
    (when (> sec 59.99d0)
      (multiple-value-setq (iyyy mm id hr min sec)
	(jd-to-calendar-date (+ jd (* 0.01 (/ 1d0 24d0 3600d0)))))
      (setf sec 0d0))
    ;;
    (calendar-date-to-date-string iyyy mm id hr min sec :stream
				  nil :frac-seconds frac-seconds)))


(defun MJD-to-ut-string (mjd  &key (frac-seconds t))
  "Convert JD to YYYY-MM-DDTHH:MM:SS.SS or YYYY-MM-DDTHH:MM:SS
depending on FRAC-SECONDS"
  (JD-to-ut-string (+ mjd 2400000.5d0) :frac-seconds frac-seconds))





(defun parse-date-string (date-string)
  (declare (type string date-string))
  (loop for c across date-string
	when (not (or (digit-char-p c) (char= c #\-)))
	do (error  "Invalid date string ~A" date-string))
  (let* ((n1 (or (position #\- date-string :start 0)
		 (error "Invalid date string ~A" date-string)))
	 (n2 (or (position #\- date-string :start (1+ n1))
		 (error "Invalid date string ~A" date-string)))
	 (yyyy (parse-integer date-string :start 0 :end  n1 :junk-allowed nil))
	 (mm (parse-integer date-string :start (1+ n1) :end n2 :junk-allowed nil))
	 (dd (parse-integer date-string :start (1+ n2) :junk-allowed nil)))
    (values yyyy mm dd)))
    
(defun parse-time-string (time-string)
  (declare (type string time-string))
  (loop 
   for c across time-string
   for i from 0
   for valid = (cond ((or (= i 2) (= i 5))
		      (char= c #\:))
		     ((= i 8)
		      (char= c #\.))
		     (t
		      (digit-char-p c)))
   when (not valid)
     do
     (error "Time string '~A' is not of form nn:nn:nn.xx" time-string))
  ;;
  (let* ((n1 (or (position #\: time-string :start 0)
		 (error "Invalid time string ~A" time-string)))
	 (n2 (position #\: time-string :start (1+ n1))) ;; no 2nd ":" means only minutes
	 (h (parse-integer time-string :start 0 :end  n1 :junk-allowed nil))
	 (m (parse-integer time-string :start (1+ n1) :end n2 :junk-allowed nil))
	 (s (if n2 (numio:parse-float time-string :start (1+ n2) :junk-allowed nil) 0d0)))
    ;;
    (values h m s)))

(defun parse-tz-string (tz-string)
  (declare (type string tz-string))
  (loop for j from 1 below (length tz-string)
	for c = (aref tz-string j)
	when (not (or (digit-char-p c) (char= c #\:)))
	do (error  "Invalid TZ string ~A" tz-string))
  ;;
  (let* ((sign (if (char= (aref tz-string 0) #\+) +1 -1))
	 (n1 (position #\: tz-string :start 1))
	 (tzh (parse-integer tz-string :start 1 :end  n1 :junk-allowed nil))
	 (tzm (if n1 ;; there is a colon
		  (parse-integer tz-string :start (1+ n1)  :junk-allowed nil)
		  0)))
    (values sign tzh tzm)))


;; a pattern matcher would make more sense here
(defun parse-ut-date-and-time-string (ut-date-time-string &key
							    (allow-space-separator nil)
							    (allow-date-only t) 
							    (space-trim t))
  "parse a date of ISO 8601 form 2007-09-19T00:46:10+xx:yy returning 
(VALUES YYYY MM DD HH MM SS.SS TZ-SIGN TZ-HR TX-MIN)
where any term and its subsequent terms can be NIL.

If ALLOW-SPACE-SEPARATOR is true, then permit a variant in which the T
is replaced by a space."
  (when space-trim
    (setf ut-date-time-string (string-trim '(#\space #\tab) ut-date-time-string)))


  
  (if allow-date-only
      (when (< (length ut-date-time-string) 10)
	(error "UT-DATE-TIME-STRING ~A too short; must be at least 10 chars if ALLOW-DATE-ONLY is true."
	       ut-date-time-string))
      (when (< (length ut-date-time-string) 19)
	(error "UT-DATE-TIME-STRING ~A too short; must be at least 19 chars if ALLOW-DATE-ONLY is false."
	       ut-date-time-string )))

  (when (not (digit-char-p (aref ut-date-time-string 0)))
    (error "UT-DATE-TIME-STRING '~A' does not begin with a digit"
	   ut-date-time-string))
      

  (when (and (not allow-date-only)
	     (< (length ut-date-time-string) 19))
    (error "UT-DATE-TIME-STRING '~A' is too short to contain a date and time"
	   ut-date-time-string))
		  
  
  (let* ((sep-pos (and
		   (>= (length ut-date-time-string) 11) ;; need full YYYY-MM-DD HH:MM:SS..
		   (or (and allow-space-separator
			    (char= #\space (aref ut-date-time-string 10)))
		       (char= #\T (aref ut-date-time-string 10)))
		   10)) ;; 10th char is location of space
	 (date-string (if sep-pos
			  (subseq ut-date-time-string 0 sep-pos)
			  ut-date-time-string))
	 (time-and-tz-string (if sep-pos
				 (subseq ut-date-time-string (1+ sep-pos))
				 nil))
	 (tz-pos (or (position #\+ time-and-tz-string)
		     (position #\- time-and-tz-string)))
	 (time-string (if tz-pos
			  (subseq time-and-tz-string 0 tz-pos)
			  time-and-tz-string))
	 (tz-string  (if tz-pos
			 (subseq time-and-tz-string tz-pos)
			 nil))
	 ;;
	 yyyy mm dd (h 0) (m 0) (s 0d0) (tz-sign +1) (tzh 0) (tzm 0))

    (when (> (length date-string) 10)
      (error "Inferred date component '~A' is too long~A"
	     date-string
	     (if (and (not allow-space-separator)
		      (char= (aref date-string 10) #\space))
		 "; ALLOW-SPACE-SEPARATOR is false so date and time must be separated by letter T"
		 "")))
	     
    
    (when (and (not allow-date-only)
	       (not time-and-tz-string))
      (error "ALLOW-DATE-ONLY is false but there is no time information in '~A'~A"
	     ut-date-time-string
	     (if allow-space-separator
		 ""
		 "; ALLOW-SPACE-SEPARATOR is false so date and time must be separated by letter T")))

    ;;
    (multiple-value-setq (yyyy mm dd)
      (parse-date-string date-string))
    (when time-string
      (multiple-value-setq (h m s)
	(parse-time-string time-string)))
    (when tz-string
      (multiple-value-setq (tz-sign tzh tzm)
	(parse-tz-string tz-string)))
    
    (values yyyy mm dd h m s tz-sign tzh tzm)))
	 

(defun to-mjd (date &key (allow-general t))
  "Given a DATE, do a best guess conversion to MJD.
If a string, try parsing it using our routine, then package
CL-DATE-TIME-PARSER package.  If a number between 10000 and 90000, assume it's
an MJD.  If it's a number 2.42e6 < 2.49e6, assume it's JD.  Return a
keyword representing what it was
(:MJD, :JD, :UT-STRING :DATE-STRING) as 2nd value.

Warning: general parsing can glitch out if there is junk.
ALLOW-GENERAL=NIL turns off general string parsing using
CL-DATE-TIME-PARSE package."
  (let ((temp nil))  ;; so we parse only once
    (cond ((and (realp date)
		(<= 10000 date 90000))
	   (values (* 1d0 date)
		   :mjd))
	  ;;
	  ((and (realp date)
		(<= 2420000.0d0 date 2490000.0d0))
	   (values (jd-to-mjd (* 1d0 date))
		   :jd))
	  ((and (stringp date)
 		(setf temp
		      (ignore-errors (parse-ut-date-and-time-string-to-mjd
				      date
				      :allow-space-separator t))))
	   (values
	    temp
	    :ut-string))
	  ((and allow-general
		(stringp date)
		(setf temp
		      (ignore-errors
		       ;; this routine is buggy if junk at end of string
		       (cl-date-time-parser:parse-date-time date))))
	   (ut-to-mjd temp))
	  (t
	   (error "Can't convert ~S into MJD" date)))))
	

	 
(defun parse-ut-date-and-time-string-to-jd (ut-date-time-string &key
								  (allow-space-separator nil)
								  (allow-date-only t)
								  (space-trim t))
  (multiple-value-bind (yyyy mm dd h m s tz-sign tzh tzm)
      (parse-ut-date-and-time-string ut-date-time-string
				     :space-trim space-trim
				     :allow-date-only allow-date-only
				     :allow-space-separator allow-space-separator)
    ;; return JD adjusted by timezone: +XX:YY means the time is ahead of UT
    ;; so we subtract XX YY from the JD
    (+ (calendar-date-to-jd yyyy mm dd h m s)
       (* -1d0 tz-sign (+ (/ tzh 24d0) (/ tzm #.(* 24d0 60d0))))))) 
       

(defun parse-ut-date-and-time-string-to-mjd (ut-date-time-string  &key
								    (allow-space-separator nil)
								    (allow-date-only t)
								    (space-trim t))
  (multiple-value-bind (yyyy mm dd h m s tz-sign tzh tzm)
      (parse-ut-date-and-time-string ut-date-time-string
				     :space-trim space-trim
				     :allow-date-only allow-date-only
				     :allow-space-separator allow-space-separator)
    ;; return JD adjusted by timezone: +XX:YY means the time is ahead of UT
    ;; so we subtract XX YY from the JD
    (+ (calendar-date-to-mjd yyyy mm dd h m s)
       (* -1d0 tz-sign (+ (/ tzh 24d0) (/ tzm #.(* 24d0 60d0)))))))



(defun mjd-to-jd (mjd)
  "Convert MJD to JD by adding 2400000.5d0"
  (+ mjd 2400000.5d0))

(defun jd-to-mjd (jd)
   "Convert JD to MJD by adding -2400000.5d0"
  (+ jd -2400000.5d0))


(defun ut-to-jd (ut)
  "Convert UT (lisp seconds) to JD"
  (multiple-value-bind (sec min hour day month year)
      (decode-universal-time ut 0)
    (calendar-date-to-jd year month day hour min sec)))

(defun ut-to-mjd (ut)
  "Convert UT (lisp seconds) to MJD; should not suffer precision loss."
   (multiple-value-bind (sec min hour day month year)
      (decode-universal-time ut 0)
    (calendar-date-to-jd year month day hour min sec :mjd t)))


(defun jd-to-ut (jd)
  "Convert Julian Day to Lisp universal time.  Seconds are rounded to nearest
integer."
  (multiple-value-bind (year month day hour min sec)
      (jd-to-calendar-date jd)
    (encode-universal-time (round sec) min hour day month year 0)))


(defun mjd-to-ut (mjd)
  "Convert MJD to Lisp universal time - Seconds are rounded to nearest
integer."
  (multiple-value-bind (year month day hour min sec)
      (mjd-to-calendar-date mjd)
    (setf sec (round sec))
    (when (= sec 60) 
      (multiple-value-setq ( year month day hour min sec)
	(%normalize-date year month day hour min sec)))
    (encode-universal-time (round sec) min hour day month year 0)))


(defun number-of-days-in-year (year)
  "Return number of days in a year (integer), assuming years divisible
by 400 are leap years, 100 are not, divisible by 4 are, and else are not."
  (declare (type integer year))
  (when (minusp year) (incf year)) ;; no year zero - hope this is right
  (cond
    ;; years divided by 400 are leap years
    ((zerop (nth-value 1 (floor year 400))) 366)
    ;; by 100 are not
    ((zerop (nth-value 1 (floor year 100))) 365)
    ;; by 4 are
    ((zerop (nth-value 1 (floor year 4))) 366)
    ;; else are not
    (t 365)))

(defun mjd-to-decimal-date-string (mjd &key (ndecimal 1) (separator "-"))
  "Convert MJD to a date like 2008-07-09.23"
  (multiple-value-bind (iyyy mm id hr min sec)
      (astro-time:mjd-to-calendar-date mjd)
    (let ((frac-day (/ (+ hr (/ min 60.0) (/ sec 3600.0))
		       24.0)))
      (format nil  "~4,'0D~A~2,'0D~A~A~2,VF"
	      iyyy separator mm  separator
	      (if (< id 10) "0" "") ;; zero pad days less than 10
	      ndecimal (+ id frac-day)))))

(defun decimal-year-to-mjd (decimal-year)
  "Convert a decimal year like YYYY.xx to MJD, multiplying the
fractional part by the number of days in this year, 365 or 366.

WARNING: Does not account for leap seconds, so that subtracting two 
decimal years is not quite the interval spanned."
  (multiple-value-bind (iyr fyr)
      (round decimal-year)
    (+ (calendar-date-to-mjd iyr 1 1  0 0 0)
       (* (number-of-days-in-year iyr) fyr))))
    

(defun mjd-to-decimal-year (mjd)
  "Convert an MJD to a fractional year as 
YEAR(MJD) + MJD(YEAR,1,1)/DAYS_IN_THIS_YEAR.

WARNING: Does not account for leap seconds, so that subtracting two 
decimal years is not quite the interval spanned."
  (let* ((iyr (mjd-to-calendar-date mjd))
	 (mjdyr (calendar-date-to-mjd iyr 1 1 0 0 0)))
    (+ iyr (/  (- mjd mjdyr) (number-of-days-in-year iyr)))))



(defun lisp-ut-to-unix-time (ut-lisp)
  "Convert a lisp UT (as from get-universal-time) to a UNIX time
starting at 1970-1-1T00:00:00"
  (- ut-lisp #.(encode-universal-time 0 0 0 1 1 1970 0)))

(defun unix-time-to-lisp-ut (ut-unix)
  "Convert a unix integer UT starting at  1970-1-1T00:00:00 to a Lisp
UT at 1900"
  (+ ut-unix #.(encode-universal-time 0 0 0 1 1 1970 0)))

(defun unix-time-to-date-string (ut-unix)
  "Convert a unix time to a UT date string"
  (ut-to-date-string (unix-time-to-lisp-ut ut-unix)))


(eval-when (:load-toplevel)
  (let ((next-ut #.(encode-universal-time 0 0 0 1
					  7 2026  ;; 1 or 7
					  0)))
  (when (>= (get-universal-time) next-ut)
    (format t "~%WARNING: - ASTRO-TIME PACKAGE - you may need to
 update TAI-MINUS-UTC for the latest leap second using data from
 ftp://maia.usno.navy.mil/ser7/tai-utc.dat The latest possible leap
 second in code is from six month prior to ~A~%"
	    (ut-to-date-string next-ut)))))

(defun tai-minus-utc (mjdutc &key (allow-utc-before-1960 nil))
  "Returns the value of TAI-UTC (atomic time minus UTC) in seconds,
given a MJDUTC.  

This code was obtained by running F2CL on the SLALIB routine sla_DAT
in file dat.f and making small modifications to remove f2cl dependence.

See ftp://maia.usno.navy.mil/ser7/tai-utc.dat for new leap seconds."
  (declare (type (double-float) mjdutc))
  (prog ((dt 0.0d0) (sla_dat 0.0d0))
    (declare (type (double-float) sla_dat dt))
    (cond ;; unlike SLALIB, don't allow MJDUT before 1960
          ;; unless requested
          ((and (not allow-utc-before-1960) (< mjdutc  36934.0d0))
	     (error "MJDUT before 1960 and ALLOW-UTC-BEFORE-1960 keyword not set."))
	  ;;
	  ;; https://en.wikipedia.org/wiki/Leap_second
	  ;; nothing on 2017 Jul to 2026 Jan
	  ((>= mjdutc 57754.0d0) (setf dt 37.0d0)) ; 2017 Jan  1 
	  ((>= mjdutc 57204.0d0) (setf dt 36.0d0)) ; 2015 July 1
	  ((>= mjdutc 56109.0d0) (setf dt 35.0d0)) ; 2012 July 1
          ((>= mjdutc 54832.0d0) (setf dt 34.0d0)) ; 2009 Jan  1
          ((>= mjdutc 53736.0d0) (setf dt 33.0d0)) ; 2006 Jan  1
          ((>= mjdutc 51179.0d0) (setf dt 32.0d0)) ; 1999 Jan  1
          ((>= mjdutc 50630.0d0) (setf dt 31.0d0)) ; 1997 July 1
          ((>= mjdutc 50083.0d0) (setf dt 30.0d0)) ; 1996 Jan  1
          ((>= mjdutc 49534.0d0) (setf dt 29.0d0))
          ((>= mjdutc 49169.0d0) (setf dt 28.0d0))
          ((>= mjdutc 48804.0d0) (setf dt 27.0d0))
          ((>= mjdutc 48257.0d0) (setf dt 26.0d0))
          ((>= mjdutc 47892.0d0) (setf dt 25.0d0))
          ((>= mjdutc 47161.0d0) (setf dt 24.0d0))
          ((>= mjdutc 46247.0d0) (setf dt 23.0d0))
          ((>= mjdutc 45516.0d0) (setf dt 22.0d0))
          ((>= mjdutc 45151.0d0) (setf dt 21.0d0))
          ((>= mjdutc 44786.0d0) (setf dt 20.0d0))
          ((>= mjdutc 44239.0d0) (setf dt 19.0d0))
          ((>= mjdutc 43874.0d0) (setf dt 18.0d0))
          ((>= mjdutc 43509.0d0) (setf dt 17.0d0))
          ((>= mjdutc 43144.0d0) (setf dt 16.0d0))
          ((>= mjdutc 42778.0d0) (setf dt 15.0d0))
          ((>= mjdutc 42413.0d0) (setf dt 14.0d0))
          ((>= mjdutc 42048.0d0) (setf dt 13.0d0))
          ((>= mjdutc 41683.0d0) (setf dt 12.0d0))
          ((>= mjdutc 41499.0d0) (setf dt 11.0d0))
          ((>= mjdutc 41317.0d0) (setf dt 10.0d0))
          ((>= mjdutc 39887.0d0)
           (setf dt (+ 4.21317d0 (* (- mjdutc 39126.0d0) 0.002592d0))))
          ((>= mjdutc 39126.0d0)
           (setf dt (+ 4.31317d0 (* (- mjdutc 39126.0d0) 0.002592d0))))
          ((>= mjdutc 39004.0d0)
           (setf dt (+ 3.84013d0 (* (- mjdutc 38761.0d0) 0.001296d0))))
          ((>= mjdutc 38942.0d0)
           (setf dt (+ 3.74013d0 (* (- mjdutc 38761.0d0) 0.001296d0))))
          ((>= mjdutc 38820.0d0)
           (setf dt (+ 3.64013d0 (* (- mjdutc 38761.0d0) 0.001296d0))))
          ((>= mjdutc 38761.0d0)
           (setf dt (+ 3.54013d0 (* (- mjdutc 38761.0d0) 0.001296d0))))
          ((>= mjdutc 38639.0d0)
           (setf dt (+ 3.44013d0 (* (- mjdutc 38761.0d0) 0.001296d0))))
          ((>= mjdutc 38486.0d0)
           (setf dt (+ 3.34013d0 (* (- mjdutc 38761.0d0) 0.001296d0))))
          ((>= mjdutc 38395.0d0)
           (setf dt (+ 3.24013d0 (* (- mjdutc 38761.0d0) 0.001296d0))))
          ((>= mjdutc 38334.0d0)
           (setf dt (+ 1.945858d0 (* (- mjdutc 37665.0d0) 0.0011232d0))))
          ((>= mjdutc 37665.0d0)
           (setf dt (+ 1.845858d0 (* (- mjdutc 37665.0d0) 0.0011232d0))))
          ((>= mjdutc 37512.0d0)
           (setf dt (+ 1.372818d0 (* (- mjdutc 37300.0d0) 0.001296d0))))
          ((>= mjdutc 37300.0d0)
           (setf dt (+ 1.422818d0 (* (- mjdutc 37300.0d0) 0.001296d0))))
          (t (setf dt (+ 1.417818d0 (* (- mjdutc  37300.0d0) 0.001296d0)))))
    (setf sla_dat dt)
   end_label
    (return sla_dat)))



(defun mjdutc-to-mjdtai (mjdutc)
  "Convert MJDUTC to MJDTAI, giving smoothly varying atomic time instead
of MJDUTC, which has leap seconds."
  (declare (type double-float mjdutc))
  (+ mjdutc (/ (tai-minus-utc mjdutc)
	       (* 24d0 3600d0))))

(defun mjdutc-to-mjdtt (mjdutc)
  "Convert MJDUTC to MJDTT, giving the smooth time scale used in
ephemerides.  It is improper to call this before 1960"
  (declare (type double-float mjdutc))
  (+ mjdutc (/ (+ (tai-minus-utc mjdutc) 32.184d0)
	       (* 24d0 3600d0))))

(defun mjd-true-time-span (mjd1 mjd2)
  "Returns the true MJD2 - MJD1, representing the true clock-years
between MJD1 and MJD2, by converting to MJDTAI."
  (- (mjdutc-to-mjdtai mjd2) (mjdutc-to-mjdtai mjd1)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; routines for parsing MM/DD/YY DD/MM/YY, etc
;;   first some helpers in LABELS
(labels ((make-output (output fullyr month day)
	   (cond ((eq output :list)
		  (list fullyr month day))
		 ((eq output :string)
		  ;; generate properly formatted date
		  (format nil "~D-~2,'0D-~2,'0D" fullyr month day))
		 ((eq output :integer)
		  (+ day (* 100 month) (* 10000 fullyr)))
		 ((eq output :mjd)
		  (astro-time:calendar-date-to-mjd fullyr month day 0 0 0))))
	 ;;
	 (valid-date-p (day month fullyr)
	   (and day month fullyr
		  (<= 1900 fullyr 2100)
		  (<= 1 month 12)
		  (<= 1 day 31)))
	 ;;
	 (fix-year (yr)
	   (cond ((> yr 1900) yr) ;; turn YY into YYYY
		 ((> yr 50) (+ yr 1900))
		 ((<= yr 50) (+ yr 2000)))))
	   


  (defun parse-dd/mm/yy-string (dstring &key (separator-char #\/) (output :list))
  "Parse a date string DSTRING of form DD/MM/YY or DD/MM/YYYY 

SEPARATOR-CHAR can be used to change the separation character from '/'

If keyword OUTPUT is
  - :LIST then return (YYYY MM DD) 
  - :STRING output 'YYYY-MM-DD'
  - :INTEGER output integer YYYYMMDD
  - :MJD output mjd double float"
  (declare (type string dstring)
	   (type character separator-char)
	   (type (member :string :list :integer :mjd) output))
  (flet ((failparse ()
	   (error
	    "Failed to parse DATE-OBS=~A for DD/MM/YY style date"
	    dstring)))
    (when (not (= (count separator-char dstring) 2))
      (error "Separator char <~A> does not appear twice in ~A"
	     separator-char dstring))
    (let*
	((nsl1 (position separator-char dstring))
	 (nsl2  (or (position separator-char dstring :start (1+ nsl1))
		    (failparse)))
	 (day
	   (ignore-errors (parse-integer dstring :start 0 :end nsl1)))
	 (month
	   (ignore-errors (parse-integer dstring
					 :start (1+ nsl1) :end nsl2)))
	 (yr
	   (ignore-errors (parse-integer dstring :start (1+ nsl2) )))
	 ;;
	 (fullyr (fix-year yr)))
      ;;
      (when (not (valid-date-p day month fullyr))
	(failparse))
      ;;
      (make-output output fullyr month day))))
  
  (defun parse-mm/dd/yy-string (dstring &key (separator-char #\/) (output :list))
    "Parse a date string DSTRING of form MM/DD/YY or MM/DD/YYYY 
 ('American' format, like July 4 1976)

SEPARATOR-CHAR can be used to change the separation character from '/'

If keyword OUTPUT is
  - :LIST then return (YYYY MM DD) 
  - :STRING output 'YYYY-MM-DD'
  - :INTEGER output integer YYYYMMDD
  - :MJD output mjd double float"
    (declare (type string dstring)
	     (type character separator-char)
	     (type (member :string :list :integer :mjd) output))
    (flet ((failparse ()
	     (error
	      "Failed to parse DATE-OBS=~A for DD/MM/YY style date"
	      dstring)))
      (when (not (= (count separator-char dstring) 2))
	(error "Separator char <~A> does not appear twice in ~A"
	       separator-char dstring))
      (let*
	  ((nsl1 (position separator-char dstring))
	   (nsl2  (or (position separator-char dstring :start (1+ nsl1))
		      (failparse)))
	   (month
	     (ignore-errors (parse-integer dstring :start 0 :end nsl1)))
	   (day
	     (ignore-errors (parse-integer dstring
					   :start (1+ nsl1) :end nsl2)))
	   (yr
	     (ignore-errors (parse-integer dstring :start (1+ nsl2) )))
	   ;;
	   (fullyr (fix-year yr)))
	;;
	(when (not (valid-date-p day month fullyr))
	   (failparse))
	;;
	(make-output output fullyr month day))))
  


  
    (defun parse-yy/mm/dd-string (dstring &key (separator-char #\/) (output :list))
      "Parse a date string DSTRING of form YY/MM/DD YYYY/MM/DD

SEPARATOR-CHAR can be used to change the separation character from '/'

If keyword OUTPUT is
  - :LIST then return (YYYY MM DD) 
  - :STRING output 'YYYY-MM-DD'
  - :INTEGER output integer YYYYMMDD
  - :MJD output mjd double float"
      (declare (type string dstring)
	       (type character separator-char)
	       (type (member :string :list :integer :mjd) output))
      (flet ((failparse ()
	       (error
		"Failed to parse DATE-OBS=~A for DD/MM/YY style date"
		dstring)))
	(when (not (= (count separator-char dstring) 2))
	  (error "Separator char <~A> does not appear twice in ~A"
		 separator-char dstring))
	(let*
	    ((nsl1 (position separator-char dstring))
	     (nsl2  (or (position separator-char dstring :start (1+ nsl1))
			(failparse)))
	     (yr
	       (ignore-errors (parse-integer dstring :start 0 :end nsl1)))
	     (month
	       (ignore-errors (parse-integer dstring
					     :start (1+ nsl1) :end nsl2)))
	     (day
	       (ignore-errors (parse-integer dstring :start (1+ nsl2) )))
	     ;;
	     (fullyr (fix-year yr)))
	  ;;
	  (when (not (valid-date-p day month fullyr))
	    (failparse))
	  ;;
	(make-output output fullyr month day))))
  ;;
  ) ;; end of labels containing helpers

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
