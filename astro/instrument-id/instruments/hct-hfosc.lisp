

(in-package instrument-id)

;; parent class of HFOSC camera
(defclass/inst %hct-hfosc (imaging-instrument onechip)
  ((name :initform "HCT-HFOSC parent class")
   (aperture :initform 2.01)
   (observatory :initform "HCT")))

(defclass/inst hct-hfosc (%hct-hfosc)
  ((name :initform "HCT-HFOSC")))

;; new chip added 2019
(defclass/inst hct-hfosc2 (%hct-hfosc)
  ((name :initform "HCT-HFOSC2")
   ;; this chip has 32 bit ints and stars seem to go very high flux without saturating - hence flats also go high
   (saturation-level :initarg :saturation-level ;; in ADU
		     :accessor saturation-level
                     :initform 200000)  
   (non-linear-level :initarg :non-linear-level ;; in ADU                                                                                                                                                                         
                     :accessor non-linear-level
                     :initform 200000)))


		   

    
				   

(defun %hct-hfosc-identify-instrument (fits-file)
  (let ((instrume (cf:read-fits-header fits-file "INSTRUME")))
    (cond ((equalp instrume "HFOSC")
	   (make-instance 'hct-hfosc))
	  ((equalp instrume "HFOSC2")
	   (make-instance 'hct-hfosc2))
	  (t
	   nil))))

(%add-instrument-id-function '%hct-hfosc-identify-instrument)

(defmethod get-critical-headers-for-instrument ((inst %hct-hfosc)
						fits-file)
  (declare (ignore inst fits-file))
  (remove-duplicates
   (append
    '("IMGTYP" "SHSTAT" "DATE-OBS" "DATE-AVG" "GAINM" "RA" "DEC")
    (call-next-method))
   :test 'equalp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; the new instrument has 2 extensions
(defmethod get-image-extension-for-onechip-instrument ((inst hct-hfosc2) fits-file)
  (declare (ignore fits-file))
  2) 

(defmethod get-standard-filter-for-instrument ((inst %hct-hfosc) fits-file) 
  (flet ((parse-filter (filter)
	   (if (not filter) ;; some files just lack a filter - tough cookies for the user
	       nil
	       (progn
		 (if (or (not (stringp filter))  (< (length filter) 1))
		     nil ;; no filter
		     ;; hct-hfosc instrument sometimes has "U" and sometimes "4 Bes U"
		     ;; so we just test for last char.  hct-hfosc2 seems to have single
		     ;; char filters, so this should be OK too
		     (flet ((is-filt (char)
			      (char= (aref filter (1- (length filter))) char))
			    (is-sdss (char)
			      (search (format nil "SDSS ~A" char) filter)))
		       (cond ((is-filt #\U) :uj)
			     ((is-filt #\B) :bj)
			     ((is-filt #\V) :vj)
			     ((is-filt #\R) :rc)
			     ((is-filt #\I) :ic)
			     ;; not sure all these SDSS exist, but r does
			     ((is-sdss #\u) :usdss)
			     ((is-sdss #\g) :gsdss)
			     ((is-sdss #\r) :rsdss)
			     ((is-sdss #\i) :isdss)
			     ((is-sdss #\z) :zsdss)
			     ((search "Free" (string-trim " " filter))
			      :open)
			     (t NIL))))))))
      (let* ((filter1 (cf:read-fits-header fits-file "FILTER" :extension 1))
	     ;; hsofsc2 is a mess - Sometimes the filter in ext 1 is invalid, and we have
	     ;; to use filter 2.  Sometimes the other way.  
	     (filter2 (if (typep inst 'hct-hfosc2)
			  (cf:read-fits-header fits-file "FILTER" :extension 2)))
	     (f1 (parse-filter filter1))
	     (f2 (parse-filter filter2))
	     (good-filters '(:uj :bj :rc :vj :ic)))
	(cond ((member f1 good-filters)
	       f1)
	      ((member f2 good-filters)
	       f2)
	      (t ;; now it might be Free=:open, or nil
	       (or f1 f2))))))
	      
	
	
;; generic version OK
;(defmethod get-exptime-for-instrument ((inst %hct-hfosc) fits-file) ... )

;; this can be screwed up for HCT - we see things like object=2009 (number) and comment="MS9 /"
(defmethod get-object-for-instrument ((inst %hct-hfosc) fits-file)
  (multiple-value-bind (object comment)
      (cf:read-fits-header fits-file "OBJECT" :extension 1)
    (when (not object)
      (error "OBJECT keyword not found in HCT file ~A" fits-file))
    (substitute 
     #\/ #\| ;; "|" is used as a substitute for / in  comet names
     ;; but it depends on observer.  UGH.
     
     (cond ((stringp object)
	    object)
	   (t
	    (format nil "~A ~A"
		    object
		    (string-trim " " (string-right-trim "/" comment))))))))

(defmethod get-object-type-for-instrument ((inst hct-hfosc) fits-file)
  (let ((obj-type (or (cf:read-fits-header fits-file "IMGTYP") ;; old vers - seems more reliable - imred rewrites this
		      (cf:read-fits-header fits-file "IMAGETYP")
		      "")) ;; "" means 'not present'
	;; @&$%! this - they tend to mislabel flats and biases
	(object (cf:read-fits-header fits-file "OBJECT"))
	(exptime (cf:read-fits-header fits-file "EXPTIME")))
    (if (stringp obj-type)
	(setf obj-type (string-trim " " obj-type))
	(if obj-type
	    (error "Header IMAGETYP or IMGTYP is not a string.")))
    (flet ((matches (thing type)
	      (search (string-upcase type) (string-upcase thing))))
      (if (not (realp exptime))
	  (error "Header EXPTIME is not a number"))
  
      (cond
	;; if they call it a bias, and it has zero exptime, it's a bias
	((and (matches obj-type "bias") (zerop exptime))
	 :BIAS)
	;; if they call it a flat, and it has positive exptime, it's a flat
	((and (matches obj-type "flat") exptime (plusp exptime)) ;; or FLATFIELD
	 :FLAT)
	;; if they call it a bias, and it has positive exptime, and the object name
	;; is flat, then it is a mis-named flat - they probably didn't change the name
	;; during the observing run
	((and (matches obj-type "bias") exptime (plusp exptime)
	      (matches "flat" (string-upcase object)))
	 :FLAT)
	;; if they call it an object, and it has positive exptime, it's an object
	((and (matches obj-type "object")
	      exptime (plusp exptime))
	 :OBJECT)
	;;
	;; LAST DITCH EFFORT
	;; if they made it a dark, then for SMALL exptime, it's a bias
	((and (matches obj-type "dark") (< exptime 0.003))
	 :BIAS)
	;; if they made it a dark, then for LARGE exptime, it's a flat
	((and (matches obj-type "dark") (> exptime 0.5))
	 :FLAT)
	;;
	;; REALLY, REALLY LAST DITCH EFFORT
	;; if they didn't define a type, then go just by the name and exposure time
	((and (equalp obj-type "") (matches object "bias") (< exptime 0.003))
	 :BIAS)
	((and (equalp obj-type "") (matches object "flat") (> exptime 0.5))
	 :FLAT)
	;;; ugh - a long bias can be a flat
	((and (equalp obj-type "") (matches object "bias") (> exptime 0.5))
	 :FLAT)
	((and (equalp obj-type "")
	      (not (matches object "dark"))
	      (not (matches object "flat"))
	      (not (matches object "bias"))
	      (>= exptime 0.003))
	 :OBJECT)
	;;
	(T :OTHER)))))


;; Fixme - should this be in extension 1 (main) or 2 (image)?  wcstools sethead
;; might write in extension 2 by default?
(defmethod get-object-type-for-instrument ((inst hct-hfosc2) fits-file)
  (let ((obj-type (%gethead-or-error fits-file "IMAGETYP")))
    (cond ((equalp obj-type "bias") :BIAS)
	  ((equalp obj-type "flat") :FLAT)
	  ((equalp obj-type "dark") :DARK)
	  ((equalp obj-type "object") :OBJECT)
	  ;;
	  (T ;; screwed up hfosc2 headers with no IMAGETYP
	   (flet ((matches (thing type)
		    (search (string-upcase type) (string-upcase thing))))
	     (let ((object   (%gethead-or-error fits-file "OBJECT"))
		   (exptime   (%gethead-or-error fits-file "EXPTIME"))
		   (shutstat  (%gethead-or-error fits-file "SHSTAT")))
	       (cond
		 ;; a short closed shutter is a bias, if named DARK or BIAS
		 ((and (or (matches object "DARK")
			   (matches object "BIAS"))
		       (equalp shutstat "CLOSED")
		       (< exptime 0.01))
		  :bias)
		 ;; anything named DARK,BIAS,FLAT is a FLAT if the shutter is open
		 ;; they seem to use short flats - sure hope their shutter
		 ;; is fast!
		 ((and (or (matches object "DARK")  ;; rampant mis-naming
			   (matches object "BIAS")
			   (matches object "FLAT"))
		       (equalp shutstat "OPEN")
		       (> exptime 0.1))
		  :flat)
		 ;; anything named DARK or BIAS is a DARK if shutter is closed
		 ((and (or (matches object "DARK")
			   (matches object "BIAS"))
		       (> exptime 0.1)
		       (equalp shutstat "CLOSED"))
		  :dark)
		 ;; anything else with an open shutter is an object
		 ((and (not (matches object "DARK"))
		       (not (matches object "BIAS"))
		       (not (matches object "FLAT"))
		       (equalp shutstat "OPEN"))
		  :object)
		 ;; otherwise give up
		 (t :unknown))))))))


;; version 1 of instrument
(defmethod get-mjd-start-for-instrument ((inst hct-hfosc) fits-file)
   (let* ((date-string   (%gethead-or-error fits-file "DATE-OBS"))
	  (ut (%gethead-or-error fits-file "UT")) ;; seconds into day
	  (sec-into-day
	    (cond ((realp ut) ;; just seconds into day
		   ut)
		   ;; rarely it is HH:MM:SS
		   ((and (stringp ut)
			 (= (count #\: ut) 2))
		    (let* ((hms-list 
			     (mapcar 'parse-integer 
				     (string-utils:split-string
				      "22:10:12" ":"))))
		      (if (= (length hms-list) 3)
			  (+ (* 3600 (first hms-list))
			     (* 60   (second hms-list))
			     (* 1    (third  hms-list)))
			  (error "Invalid UT of form HH:MM:SS <~A>" ut))))
		   (t
		    (error "UT is neither seconds in day, or HH:MM:SS: <~A>"
			   ut))))
	  ;;
	  (mjd-day-start ;; mjd of date-string
	     (multiple-value-bind (year month day hour min sec)
		 (astro-time:parse-ut-date-and-time-string date-string)
	       (astro-time:calendar-date-to-mjd year month day hour min sec)))
  	   (mjd-start (+ mjd-day-start ;; start of obs
			     (/ sec-into-day #.(* 3600 24d0)))))
     mjd-start))


;; generic version OK
;(defmethod get-mjd-mid-for-instrument ((inst %hct-hfosc) fits-file) ...  )

(defmethod get-mjd-mid-for-instrument ((inst hct-hfosc2) fits-file)
  (let* ((date-string   (%gethead-or-error fits-file "DATE-AVG")))
    (astro-time:parse-ut-date-and-time-string-to-mjd date-string)))

(defmethod get-mjd-start-for-instrument ((inst hct-hfosc2) fits-file)
  (let* ((exptime  (%gethead-or-error fits-file "EXPTIME"))
	 (mjd-mid  (get-mjd-mid-for-instrument inst fits-file)))
    (- mjd-mid (* 0.5d0 (/ exptime (* 24 3600))))))

 


(defmethod get-gain-for-instrument ((inst hct-hfosc) fits-file &key extension)
  (declare (ignore extension))
  (let* ((gainm (%gethead-or-error fits-file "GAINM"))
	 (gain (cond ((equalp gainm "HIGH")
		      1.22) ;; actually 1.21 for Amp B, but close enough
		     ((equalp gainm "LOW")
		      5.6)
		     (t
		      (error "Unknown gain keyword ~A for ~A"
			     gainm fits-file)))))
    gain))

;; don't really trust this
(defmethod get-gain-for-instrument ((inst hct-hfosc2) fits-file &key extension)
  (declare (ignore extension))
  (%gethead-or-error fits-file "GAIN0-0" :extension 2))

;(defmethod get-chip-id-for-instrument ((inst %hct-hfosc) fits-file) ..)

;; sometimes images are so broken that there's no RA estimate, hence
;; no initial wcs estimate, so do it more carefully
(defmethod get-pixel-scale-for-instrument ((inst hct-hfosc) fits-file &key extension)
  (declare (ignore extension))
  (or
   ;; try the usual method depending on wcs
   (let ((wcs (cf:read-wcs fits-file)))
     (when wcs (wcs:get-pixel-scale-for-wcs wcs)))
   ;; otherwise, fall back on known pixel scale and binning
   (let*  ((naxis1  (%gethead-or-error fits-file "NAXIS1")) ;; = naxis2
	   ;; hope no asymmetrical binning (1x2, 2x1) exists
	   ;; binning might be 2148 too, so we do round
	   (binning (round (/ 2048d0 naxis1)))
	   ;; not in the headers, so we use manual
	   (pixscale/arcsec (* 0.296d0 binning)))
     pixscale/arcsec)))

(defmethod get-initial-wcs-for-instrument
    ((inst hct-hfosc) fits-file &key extension)
  (declare (ignore extension))
  (let*  
      ((ra-string  (or (cf:read-fits-header fits-file "RA")
		       (cf:read-fits-header fits-file "OBJRA") ;; rare
		       (error "RA or OBJRA header not found.")))
       (dec-string (or (cf:read-fits-header fits-file "DEC")
		       (cf:read-fits-header fits-file "OBJDEC") ;; rare
		       (error "DEC or OBJDEC header not found.")))
       (ra (ra-dec:hms-string->deg ra-string))
       (dec (ra-dec:dms-string->deg dec-string))
       (naxis1  (%gethead-or-error fits-file "NAXIS1")) ;; = naxis2
       (pcenter  (* 0.5d0 naxis1)) ;; for binning
       ;; hope no asymmetrical binning (1x2, 2x1) exists
       (binning (/ 2048d0 naxis1))
       ;; not in the headers, so we use manual
       (pixscale/arcsec (* 0.296d0 binning))
       (pixscale (/ pixscale/arcsec 3600d0))
       (wcs
	 (wcs:make-wcs-radec-tan 
	  :crval1 ra :crval2 dec
	  :crpix1 pcenter
	  :crpix2 pcenter
	  :cd1_1 (+ pixscale)
	  :cd2_2 (- pixscale)
	  :equinox 2000d0
	  )))
    wcs))


(defmethod get-initial-wcs-for-instrument
    ((inst hct-hfosc2) fits-file &key extension)
  (declare (ignore extension))
  (let* 
      ((ra-string  (or (cf:read-fits-header fits-file "RA")
		       (cf:read-fits-header fits-file "OBJRA") ;; rare
		       (error "RA or OBJRA header not found.")))
       (dec-string (or (cf:read-fits-header fits-file "DEC")
		       (cf:read-fits-header fits-file "OBJDEC") ;; rare
		       (error "DEC or OBJDEC header not found.")))
       (ra (ra-dec:hms-string->deg ra-string))
       (dec (ra-dec:dms-string->deg dec-string))
       (naxis1  (%gethead-or-error fits-file "NAXIS1" :extension 2)) ;; = naxis2
       (pcenter  (* 0.5d0 naxis1)) ;; for binning
       ;; hope no asymmetrical binning (1x2, 2x1) exists
       (binning (round (/ 2048d0 naxis1)))
       ;; not in the headers, so we use manual
       (pixscale/arcsec (* 0.296d0 binning))
       (pixscale (/ pixscale/arcsec 3600d0))
       (wcs
	 (wcs:make-wcs-radec-tan 
	  :crval1 ra :crval2 dec
	  :crpix1 pcenter
	  :crpix2 pcenter
	  :cd1_1 (+ pixscale)
	  :cd2_2 (- pixscale)
	  :equinox 2000d0
	  )))
    wcs))

;; new chip has WCS
#+nil
(defmethod get-initial-wcs-for-instrument
    ((inst hct-hfosc2) fits-file &key extension)
  (declare (ignore extension))
  (let* ((wcs
	   (or (cf:read-wcs fits-file :extension 2)
	       (error "No WCS found")))
	 (ra-string (%gethead-or-error fits-file "RA"))
	 (dec-string (%gethead-or-error fits-file "DEC"))
	 (ra (ra-dec:hms-string->deg ra-string))
	 (dec (ra-dec:dms-string->deg dec-string)))
    (setf (wcs:wcs-radec-tan-crval1 wcs) ra)
    (setf (wcs:wcs-radec-tan-crval2 wcs) dec)
    wcs))
 

(defmethod insert-initial-wcs-for-instrument ((inst hct-hfosc)  fits-file
					      &key extension)
  (declare (ignore extension))
  (cf:write-wcs (get-initial-wcs-for-instrument inst fits-file) fits-file))


;; the initial wcs in HSOFC2 is wrong
(defmethod insert-initial-wcs-for-instrument ((inst hct-hfosc2)  fits-file
					      &key extension)
  (declare (ignore extension))
  (cf:write-wcs (get-initial-wcs-for-instrument inst fits-file) fits-file
		:extension 2))

