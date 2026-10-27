
;; some higher level ephemeris functions using slalib routines

;; WARNING - NOT REALLY TESTED, PROBABLY LOTS OF BUGS AND ISSUES
;; in particular, anything using PERTURM-COMET-ELEM or any orbit
;; perturbations is thoroughly hosed

#+nil
(format t 
"~%~%WARNING - slalib-ephem is still a bit risky to use.
 Planetary positions are intrinsically imprecise.  Asteroids 
 and orbit perturbation seems OK now.~%")

 


(in-package slalib-ephem)


(defconstant +pi/180+ (/ pi 180))
(defconstant +180/pi+ (/ 180 pi))

;; some ephem routines to convert PS1 comet-style ephems
;; to positions

#|

 THIS       JPL Notation:

 EPOCH      EPOCH - 2400000.5d0
 TIME-PERI  TP - 2400000.5d0
 ORBINC     INC
 ANODE      OM
 PERIH      W
 Q          QR
 E          EC

|#

(defun perturb-universal-elem (epoch-final univ-elem &key (univ-elem-out nil))
    "Compute perturbed orbital elements from UNIV-ELEM, perturbing to
EPOCH-FINAL. Note that EPOCH here means the time (mjd) of the orbit 
but in SLALIB it refers to TIME-PERI.  If the value JSTAT is not zero, then
a warning-condition occurred.

By default new elements are created, but if UNIV-ELEM-OUT is provided
it will be filled with new values.

Note that EPOCH-FINAL should be MJDTT, not MJDUT, but this generally
doesn't matter, because one simply wants a good orbit for an approximate
time."
  (declare (type univ-elem univ-elem)
	   (type double-float epoch-final)
	   (type (or null univ-elem) univ-elem-out))
  (when (eq univ-elem univ-elem-out)
    (error "univ-elem cannot be same as univ-elem-out"))
  (let ((elem-out (or univ-elem-out
		      (make-univ-elem :id (univ-elem-id univ-elem)
				      :data (univ-elem-data univ-elem)))))
    (loop for i below (length (univ-elem-velem elem-out))
	  do (setf (aref (univ-elem-velem elem-out)  i)
		   (aref (univ-elem-velem univ-elem) i)))
		   
    (slalib::sla-pertue epoch-final (univ-elem-velem elem-out))
    elem-out))


  
(defun perturb-comet-elem (epoch-final comet-elem &key (comet-elem-out nil))
  "Compute perturbed orbital elements from COMET-ELEM, perturbing to
EPOCH-FINAL. Note that EPOCH here means the time (mjd) of the orbit 
but in SLALIB it refers to TIME-PERI.  If the value JSTAT is not zero, then
a warning-condition occurred.

By default new elements are created, but if COMET-ELEM-OUT is provided
it will be filled with new values.

Note that EPOCH-FINAL should be MJDTT, not MJDUT, but this generally
doesn't matter, because one simply wants a good orbit for an approximate
time."
  (declare (type comet-elem comet-elem)
	   (type (or null comet-elem) comet-elem-out))
  (when (eq comet-elem comet-elem-out)
    (error "comet-elem cannot be same as comet-elem-out"))
  (let ((elem-out (or comet-elem-out (make-comet-elem))))
    (multiple-value-bind (epoch1 orbinc1 anode1 perih1 aorq1 e1 am1 jstat)
	(slalib:sla-pertel 
	 3
	 ;; DATE1,2, are old,new dates of osculation
	 (comet-elem-epoch comet-elem) ;; DATE1
	 epoch-final  ;; DATE2
	 ;;
	 (comet-elem-time-peri comet-elem) 
	 (* (comet-elem-orbinc comet-elem) +pi/180+) 
	 (* (comet-elem-anode comet-elem)  +pi/180+)
	 (* (comet-elem-perih comet-elem)  +pi/180+)
	 (comet-elem-q comet-elem) 	  
	 (comet-elem-e comet-elem)   
	 0d0) ;; dummy value for non-comet ephems
      (declare (ignorable am1)) ;; only defined for type 2 orbits
      (setf 
       (comet-elem-id elem-out) (comet-elem-id comet-elem)	
       (comet-elem-epoch elem-out) epoch-final
       (comet-elem-time-peri elem-out) epoch1
       (comet-elem-orbinc elem-out) (* orbinc1 +180/pi+)
       (comet-elem-anode elem-out) (* anode1 +180/pi+)
       (comet-elem-perih elem-out) (* perih1 +180/pi+)
       (comet-elem-q elem-out) aorq1
       (comet-elem-e elem-out) e1)
      (values elem-out jstat))))
 
(defun compute-pv-from-comet-elem (comet-elem mjd &key pv (units :au)
				   (correct-mjd-to-tt t))
  "Compute PV=[x,y,z,xdot,ydot,zdot] from COMET-ELEM at time
MJD. Keyword UNITS is either :AU (default) or :KM.  Velocities are
always per second.

WARNING: ELEMENTS ARE NOT PERTURBED BEFORE USE.

In this system for J2000
   x=cos(ra)cos(dec);   y=sin(ra)cos(dec); z=sin(dec)  
"
  (declare (type comet-elem comet-elem)
	   (type double-float mjd))
  (let ((pv  (slalib:sla-planel 
	      (if correct-mjd-to-tt (slalib:correct-mjdut-to-mjdtt mjd)   mjd)
	      3 ;; 3 = comet type ephem as used by MOPS  
	      (comet-elem-time-peri comet-elem)
	      (* (comet-elem-orbinc comet-elem) +pi/180+) 
	      (* (comet-elem-anode comet-elem)  +pi/180+)
	      (* (comet-elem-perih comet-elem)  +pi/180+)
	      (comet-elem-q comet-elem) 	  
	      (comet-elem-e comet-elem)   
	      0d0 0d0 ;; dummy values for non-comet ephems
	      :pv pv)))
    (when (eq units :km)  (slalib:convert-pv-from-au-to-km pv))
    pv))


(defun compute-pv-from-universal-elem (univ-elem mjd &key pv (units :au)
						       (correct-mjd-to-tt t))
  "Compute PV=[x,y,z,xdot,ydot,zdot] from UNIV-ELEM at time
MJD. Keyword UNITS is either :AU (default) or :KM.  Velocities are
always per second.

In this system for J2000
   x=cos(ra)cos(dec);   y=sin(ra)cos(dec); z=sin(dec)  
"
  (declare (type univ-elem univ-elem)
	   (type (or null (simple-array double-float (6))) pv)
	   (type double-float mjd))
  (let ((pv  (slalib:sla-ue2pv
	      (univ-elem-velem univ-elem) 
	      (or pv
		  (make-array 6 :element-type 'double-float))
	      (if correct-mjd-to-tt (slalib:correct-mjdut-to-mjdtt mjd)   mjd))))
    (when (eq units :km)  (slalib:convert-pv-from-au-to-km pv))
    pv))

(declaim (inline %convert-to-mean-epoch))

(defun %convert-to-mean-epoch (ra dec mjd epoch oelon olat altitude)
  "Convert topocentric apparent RA DEC in degrees to MEAN (eg J2000
for epoch=2000d0).  All angles are in degrees."
  (multiple-value-bind (ra1 dec1) ;; ra1 dec1 in radians
      (slalib:sla-oap "R"  ;; airless (no refraction) ra dec correction
		      (* #.(/ pi 180d0) ra) 
		      (* #.(/ pi 180d0) dec)
		      mjd 0d0 
		      (* #.(/ pi 180d0) oelon) (* #.(/ pi 180d0) olat)
		      altitude
		      0d0 0d0 0d0 
		      0d0 ;; pressure is zero (no refraction?)
		      0d0 1d0 0d0)
    (multiple-value-bind (ra2 dec2) ;; ra2 dec2 in radians
	(slalib:sla-amp ra1 dec1
			mjd epoch)
      (values
       (* #.(/ 180d0 pi) ra2)
       (* #.(/ 180d0 pi) dec2)))))



(defun compute-radecr-from-universal-elem (univ-elem mjd oelon olat
				       &key 
					 (altitude 0d0)
					 (units :au)
					 (perturb t)
					 (correct-mjd-to-tt t))
  "Compute (VALUES RA DEC R) from UNIV-ELEM at time MJD.  OELON
and OLAT are observer's latitude and longitude (+EAST) in
degrees. Units are in AU and AU/day. Keyword UNITS is
either :AU (default) or :KM for R.

If RETURN-APPARENT-ALSO is T, then return (VALUES RA DEC R RA-APPARENT DEC-APPARENT)

Unlike COMPUTE-RADECR-FROM-COMET-ELEM/PLANTE, this routine works at
the geocenter, but it does not also return apparent coordinates, and
coordinates are always at mean epoch 2000."
    (let* ((elem-use
	     (if perturb
		 (perturb-universal-elem mjd univ-elem)
		 univ-elem))
	   (pv (make-array 6 :element-type 'double-float)))
      
      (declare (dynamic-extent pv))
      (slalib-ephem:compute-pv-from-universal-elem 
       elem-use mjd
       :pv pv
       :units units 
       :correct-mjd-to-tt correct-mjd-to-tt)
      ;;
      (slalib-ephem:compute-radecr-from-pv
       mjd oelon olat pv
       :units units :altitude altitude)))


(defun compute-radecr-from-comet-elem (comet-elem mjd oelon olat
				       &key 
					 (altitude 0d0)
					 (units :au)
					 (perturb t)
					 (correct-mjd-to-tt t))
  "Compute (VALUES RA DEC R) from COMET-ELEM at time MJD.  OELON
and OLAT are observer's latitude and longitude (+EAST) in
degrees. Units are in AU and AU/day. Keyword UNITS is
either :AU (default) or :KM for R.

If RETURN-APPARENT-ALSO is T, then return (VALUES RA DEC R RA-APPARENT DEC-APPARENT)

Unlike COMPUTE-RADECR-FROM-COMET-ELEM/PLANTE, this routine works at
the geocenter, but it does not also return apparent coordinates, and
coordinates are always at mean epoch 2000."
    (let* ((elem-use
	     (if perturb
		 (slalib-ephem:perturb-comet-elem mjd comet-elem)
		 comet-elem))
	   (pv (make-array 6 :element-type 'double-float)))
      
      (declare (dynamic-extent pv))
      (slalib-ephem:compute-pv-from-comet-elem  
       elem-use mjd
       :pv pv
       :units units 
       :correct-mjd-to-tt correct-mjd-to-tt)
      ;;
      (slalib-ephem:compute-radecr-from-pv
       mjd oelon olat pv
       :units units :altitude altitude)))
      


(defun compute-radecr-from-comet-elem/plante
    (comet-elem mjd oelon olat
     &key 
       (altitude 0d0)
       (units :au)
       (perturb t)
       (correct-mjd-to-tt t)
       (convert-to-mean-epoch 2000d0)
       (return-apparent-also nil))
  "Compute (VALUES RA DEC R) from COMET-ELEM at time MJD.  OELON
and OLAT are observer's latitude and longitude (+EAST) in
degrees. Units are in AU and AU/day. Keyword UNITS is
either :AU (default) or :KM for R.

If RETURN-APPARENT-ALSO is T, then return 
(VALUES RA DEC R RA-APPARENT DEC-APPARENT)

This version uses SLA-PLANTE and it does not work for geocenter. It supports
also returning other than mean-epoch 2000, and supports apparent coords."
  (declare (type comet-elem comet-elem)
	   (type double-float mjd))

  (when (> (abs altitude) 10000)
    (error "SLALIB:COMPUTE-RADECR-FROM-COMET-ELEM/PLANTE cannot compute RA,DEC for observatory ALTITUDE=~A far from earth's surface because SLA_PLANTE does not support an ALTITUDE argument.  Use COMPUTE-RADECR-FROM-COMET-ELEM2 instead."
	   altitude))
  
  (setf mjd
	(if correct-mjd-to-tt (slalib:correct-mjdut-to-mjdtt mjd)   mjd))
  ;;
  (let ((comet-elem (if perturb 
			 (perturb-comet-elem mjd comet-elem)
		       comet-elem)))
    (multiple-value-bind (ra/rad dec/rad r/au)
	(slalib:sla-plante
	 mjd
	 (* oelon +pi/180+)
	 (* olat  +pi/180+)
	 3 ;; 3 = comet type ephem as used by MOPS  
	 (comet-elem-time-peri comet-elem)
	 (* (comet-elem-orbinc comet-elem) +pi/180+) 
	 (* (comet-elem-anode comet-elem)  +pi/180+)
	 (* (comet-elem-perih comet-elem)  +pi/180+)
	 (comet-elem-q comet-elem) 	  
	 (comet-elem-e comet-elem)   
	 0d0 0d0) ;; dummy values for non-comet ephems
      ;;
      (let* ((ra-app (* +180/pi+ ra/rad))   ;; Topocentric apparent
	     (dec-app (* +180/pi+ dec/rad))
	     (ra ra-app)
	     (dec dec-app)
	     (r (if (eq units :km)  (* r/au 149597870.69d0) r/au)))
	;;
	(when convert-to-mean-epoch
	  (multiple-value-setq (ra dec)
	      (%convert-to-mean-epoch ra dec mjd convert-to-mean-epoch
				      oelon olat altitude)))
	;;
	(if return-apparent-also
	    (values ra dec r ra-app dec-app)
	    (values ra dec r))))))


;; Given two values of RA,DEC compute the rate, controlling for wrap-around in RA
;; rates in arcsec/hr
(defun %compute-rates-for-ra-dec-pair (ra1 dec1 ra2 dec2 dt/mjd)
  (let* ((dra0 (- ra2 ra1))
	 (dra+ (- (+ ra2 360d0) ra1))
	 (dra- (- (- ra2 360d0) ra1))
	 (dra (cond ((and (< (abs dra0) (abs dra+)) (< (abs dra0) (abs dra-)))
		     dra0)
		    ((< (abs dra+) (abs dra-))
		     dra+)
		    (t
		     dra-)))
	 (ddec (- dec2 dec1))
	 (dec-mean (* 0.5d0 (+ dec1 dec2)))
	 (cosdec (cos (* (/ pi 180) dec-mean)))
	 (dracosdec/dt
	    (* (/ 3600 24 dt/mjd) 
	       (* cosdec dra)))
	 (ddec/dt
	    (* (/ 3600 24 dt/mjd) 
	       ddec)))
    (when (> dra 10d0) (error "DRA>10 deg. Probably out of range. This is risky because RA wraps around."))
    (values dracosdec/dt ddec/dt)))
    


(defun compute-radecr-from-comet-elem-with-rates
    (comet-elem mjd oelon olat
     &key 
       (altitude 0d0)
       (units :au)
       (perturb t)
       (correct-mjd-to-tt t)
       (dt/mjd 0.001d0)) ;; the time span in days over which to compute rate
  "Compute apparent RA,DEC,R from COMET-ELEM at time MJD.  OELON
and OLAT are observer's latitude and longitude (+east) in
degrees. Units are in AU and AU/day. Keyword UNITS is
either :AU (default) or :KM for R.
Also compute rates in arcsec/hr.
Return
 (VALUES RA DEC R d(RA*cos(DEC))/dt  dDEC/dt)
The rates were verified using JPL HORIZONS, but not extensively.
"
  (let (ra1 dec1 ra2 dec2 r1 r2 dracosdec/dt ddec/dt)

    (let ((comet-elem-use
	    (if perturb
		(perturb-comet-elem mjd comet-elem)
		comet-elem)))
      
      (multiple-value-setq (ra1 dec1 r1)
	(compute-radecr-from-comet-elem
	 comet-elem-use mjd oelon olat
	 :altitude altitude :units units
	 :perturb perturb :correct-mjd-to-tt correct-mjd-to-tt))
      (multiple-value-setq (ra2 dec2 r2)
	(compute-radecr-from-comet-elem
	 comet-elem-use (+ mjd dt/mjd) oelon olat
	 :altitude altitude :units units
	 :perturb perturb :correct-mjd-to-tt correct-mjd-to-tt))
      (multiple-value-setq (dracosdec/dt ddec/dt)
	(%compute-rates-for-ra-dec-pair ra1 dec1 ra2 dec2 dt/mjd))
      (values ra1 dec1 r1 dracosdec/dt  ddec/dt))))

(defun compute-radecr-from-comet-elem-with-rates/plante
    (comet-elem mjd oelon olat
     &key 
       (altitude 0d0)
       (units :au)
       (perturb t)
       (correct-mjd-to-tt t)
       (convert-to-mean-epoch 2000d0)
       (return-apparent-also nil)
       (dt/mjd 0.001d0)) ;; the time span in days over which to compute rate
  "Compute apparent RA,DEC,R from COMET-ELEM at time MJD.  OELON
and OLAT are observer's latitude and longitude (+east) in
degrees. Units are in AU and AU/day. Keyword UNITS is
either :AU (default) or :KM for R.
Also compute rates in arcsec/hr.
Return
 (VALUES RA DEC R d(RA*cos(DEC))/dt  dDEC/dt)
The rates were verified using JPL HORIZONS, but not extensively.

This version uses SLA-PLANTE and it does not work for geocenter. It supports
also returning other than mean-epoch 2000, and supports apparent coords."
  (let (ra1 dec1 ra2 dec2 r1 r2 dracosdec/dt ddec/dt
	ra1-app dec1-app)
    (let ((comet-elem-use
	    (if perturb
		(perturb-comet-elem mjd comet-elem)
		comet-elem)))
      (multiple-value-setq (ra1 dec1 r1 ra1-app dec1-app)
	(compute-radecr-from-comet-elem/plante
	 comet-elem-use mjd oelon olat
	 :altitude altitude :units units
	 :convert-to-mean-epoch convert-to-mean-epoch
	 :perturb perturb :correct-mjd-to-tt correct-mjd-to-tt))
      (multiple-value-setq (ra2 dec2 r2)
	(compute-radecr-from-comet-elem/plante
	 comet-elem-use (+ mjd dt/mjd) oelon olat
	 :altitude altitude :units units
	 :convert-to-mean-epoch convert-to-mean-epoch
	 :perturb perturb :correct-mjd-to-tt correct-mjd-to-tt))
      (multiple-value-setq (dracosdec/dt ddec/dt)
	(%compute-rates-for-ra-dec-pair ra1 dec1 ra2 dec2 dt/mjd))
      (if return-apparent-also
	  (values ra1 dec1 r1 dracosdec/dt  ddec/dt ra1-app dec1-app)
	  (values ra1 dec1 r1 dracosdec/dt  ddec/dt)))))





 
(defun compute-radecr-from-comet-elem-for-observatory
    (comet-elem mjd observatory
     &key 
       (units :au)
       (perturb t)
       (correct-mjd-to-tt t))
  "Compute apparent RA,DEC,R from COMET-ELEM at time MJD.  Units are
in AU and AU/day. Keyword UNITS is either :AU (default) or :KM for R.
Uses compute-radecr-from-comet-elem.  Note switched convention of
observatory latitude."
  (declare (type comet-elem comet-elem))
  (setf observatory (get-observatory observatory :error-on-fail t))
  ;;
  (compute-radecr-from-comet-elem 
   comet-elem mjd 
   (- (observatory-wlongitude observatory)) ;; EAST
   (observatory-latitude observatory)
   :altitude (observatory-altitude observatory)
   :perturb perturb
   :units units
   :correct-mjd-to-tt correct-mjd-to-tt))



(defun compute-radecr-from-universal-elem-for-observatory
    (univ-elem mjd observatory
     &key 
       (units :au)
       (perturb t)
       (correct-mjd-to-tt t))
  "Compute apparent RA,DEC,R from UNIV-ELEM at time MJD.  Units are
in AU and AU/day. Keyword UNITS is either :AU (default) or :KM for R.
Uses compute-radecr-from-comet-elem.  Note switched convention of
observatory latitude."
  (declare (type univ-elem univ-elem))
  (setf observatory (get-observatory observatory :error-on-fail t))
  ;;
  (compute-radecr-from-universal-elem
   univ-elem mjd 
   (- (observatory-wlongitude observatory)) ;; EAST
   (observatory-latitude observatory)
   :altitude (observatory-altitude observatory)
   :perturb perturb
   :units units
   :correct-mjd-to-tt correct-mjd-to-tt))


(defun compute-radecr-from-universal-elem-with-rates
    (univ-elem mjd oelon olat
     &key
       (altitude 0d0)
       (units :au)
       (perturb t)
       (correct-mjd-to-tt t)
       (dt/mjd 0.001d0)) ;; the time span in days over which to compute rate
  "Compute apparent RA,DEC,R from UNIV-ELEM at time MJD.  OELON
and OLAT are observer's latitude and longitude (+east) in
degrees. Units are in AU and AU/day. Keyword UNITS is
either :AU (default) or :KM for R.
Also compute rates in arcsec/hr.
Return
 (VALUES RA DEC R d(RA*cos(DEC))/dt  dDEC/dt)
The rates were verified using JPL HORIZONS, but not extensively.
"
  (let (ra1 dec1 ra2 dec2 r1 r2 dracosdec/dt ddec/dt)

    (let ((univ-elem-use
	    (if perturb
		(perturb-universal-elem mjd univ-elem)
		univ-elem)))

      (multiple-value-setq (ra1 dec1 r1)
	(compute-radecr-from-universal-elem
	 univ-elem-use mjd oelon olat
	 :altitude altitude :units units
	 :perturb nil :correct-mjd-to-tt correct-mjd-to-tt))
      (multiple-value-setq (ra2 dec2 r2)
	(compute-radecr-from-universal-elem
	 univ-elem-use (+ mjd dt/mjd) oelon olat
	 :altitude altitude :units units
	 :perturb nil :correct-mjd-to-tt correct-mjd-to-tt))
      (multiple-value-setq (dracosdec/dt ddec/dt)
	(%compute-rates-for-ra-dec-pair ra1 dec1 ra2 dec2 dt/mjd))
      (values ra1 dec1 r1 dracosdec/dt  ddec/dt))))


(defun compute-radecr-from-universal-elem-with-rates-for-observatory
    (univ-elem mjd observatory
     &key
       (units :au)
       (perturb t)
       (correct-mjd-to-tt t)
       (dt/mjd 0.001d0)) ;; the time span in days over which to compute rate
  "Compute apparent RA,DEC,R from UNIV-ELEM at time MJD.  Keyword
UNITS is either :AU (default) or :KM for R.  Also compute rates in
arcsec/hr.  Return
 (VALUES RA DEC R d(RA*cos(DEC))/dt  dDEC/dt)
The rates were verified using JPL HORIZONS, but not extensively.
"
  (setf observatory (get-observatory observatory :error-on-fail t))
  ;;
  (compute-radecr-from-universal-elem-with-rates
   univ-elem mjd
   (- (observatory-wlongitude observatory)) ;; convert WEST to EAST longitude
   (observatory-latitude observatory)
   :altitude (observatory-altitude observatory)
   :units units
   :perturb perturb
   :correct-mjd-to-tt correct-mjd-to-tt
   :dt/mjd dt/mjd))


(defun compute-radecr-from-comet-elem-for-observatory/plante
    (comet-elem mjd observatory
     &key 
       (units :au)
       (perturb t)
       (correct-mjd-to-tt t)
       (convert-to-mean-epoch 2000d0)
       (return-apparent-also nil))
  "Compute apparent RA,DEC,R from COMET-ELEM at time MJD.  Units are
in AU and AU/day. Keyword UNITS is either :AU (default) or :KM for R.
Uses compute-radecr-from-comet-elem.  Note switched convention of
observatory latitude.

This version uses COMPUTE-RADECR-FROM-COMET-ELEM2, which supports
observatories not at earth's surface, like geocenter."
  (setf observatory (get-observatory observatory :error-on-fail t))
  ;;
  (compute-radecr-from-comet-elem/plante
   comet-elem mjd 
   (- (observatory-wlongitude observatory)) ;; EAST
   (observatory-latitude observatory)
   :altitude (observatory-altitude observatory)
   :perturb perturb
   :units units
   :correct-mjd-to-tt correct-mjd-to-tt
   :convert-to-mean-epoch convert-to-mean-epoch
   :return-apparent-also return-apparent-also))

(defun compute-radecr-from-comet-elem-with-rates-for-observatory
    (comet-elem mjd observatory
     &key 
       (units :au)
       (perturb t)
       (correct-mjd-to-tt t)
       (dt/mjd 0.001d0)) ;; the time span in days over which to compute rate
  "Compute apparent RA,DEC,R from COMET-ELEM at time MJD.  Keyword
UNITS is either :AU (default) or :KM for R.  Also compute rates in
arcsec/hr.  Return
 (VALUES RA DEC R d(RA*cos(DEC))/dt  dDEC/dt)
The rates were verified using JPL HORIZONS, but not extensively.
"
  (setf observatory (get-observatory observatory :error-on-fail t))
  ;;
  (compute-radecr-from-comet-elem-with-rates       
   comet-elem mjd
   (- (observatory-wlongitude observatory)) ;; convert WEST to EAST longitude
   (observatory-latitude observatory)
   :altitude (observatory-altitude observatory)
   :units units
   :perturb perturb
   :correct-mjd-to-tt correct-mjd-to-tt
   :dt/mjd dt/mjd))


(defun compute-radecr-from-comet-elem-with-rates-for-observatory/plante
    (comet-elem mjd observatory
     &key 
       (units :au)
       (perturb t)
       (correct-mjd-to-tt t)
       (convert-to-mean-epoch 2000d0)
       (dt/mjd 0.001d0) ;; the time span in days over which to compute rate
       (return-apparent-also nil))
  "Compute apparent RA,DEC,R from COMET-ELEM at time MJD.  Keyword
UNITS is either :AU (default) or :KM for R.  Also compute rates in
arcsec/hr.  Return
 (VALUES RA DEC R d(RA*cos(DEC))/dt  dDEC/dt)
The rates were verified using JPL HORIZONS, but not extensively.

This version uses SLA-PLANTE and it does not work for geocenter. It supports
also returning other than mean-epoch 2000, and supports apparent coords."
  (setf observatory (get-observatory observatory :error-on-fail t))
  ;;
  (compute-radecr-from-comet-elem-with-rates/plante       
   comet-elem mjd
   (- (observatory-wlongitude observatory)) ;; convert WEST to EAST longitude
   (observatory-latitude observatory)
   :altitude (observatory-altitude observatory)
   :units units
   :perturb perturb
   :correct-mjd-to-tt correct-mjd-to-tt
   :convert-to-mean-epoch convert-to-mean-epoch
   :return-apparent-also return-apparent-also
   :dt/mjd dt/mjd))


(defun compute-comet-elem-from-pv (pv mjd &key (pmass 0d0) 
					    (comet-elem-out nil)
					    (correct-mjd-to-tt t)
				       (units :au))
  "Convert  PV=[x,y,z,xdot,ydot,zdot] into a COMET-ELEM. PMASS is planetary
mass in solar units; for small bodies use 0d0. COMET-ELEM-OUT is an optional
output structure to be filled. If UNITS is :KM, first convert PV to :AU"
  (declare (type (simple-array double-float (*)) pv)
	   (type double-float mjd pmass)
	   (type (or null comet-elem) comet-elem-out))
  (let ((pv (if (eq units :km)
		(slalib:convert-pv-from-km-to-au (copy-seq pv))
	      pv))
	(elem-out (or comet-elem-out (make-comet-elem)))
	(mjdcor (if correct-mjd-to-tt  (slalib:correct-mjdut-to-mjdtt mjd) mjd)))
    (multiple-value-bind (jform epoch-perih orbinc anode perih aorq e aorl dm)
	(slalib:sla-pv2el pv mjdcor pmass 3)
      (declare (ignore aorl dm))
      (when (not (= jform 3))
	(error "jform=~A - cometary orbit not returned - should not happen!"
	       jform))
      (setf (comet-elem-time-peri elem-out) epoch-perih
	    ;; guess that the epoch is the mjd at which computed?
	    (comet-elem-epoch elem-out) mjdcor
	    (comet-elem-orbinc elem-out) (* orbinc +180/pi+)
	    (comet-elem-anode elem-out) (* anode +180/pi+)
	    (comet-elem-perih elem-out) (* perih +180/pi+)
	    (comet-elem-q elem-out) aorq
	    (comet-elem-e elem-out) e))
    elem-out))
	       
	   


(defun convert-comet-elem-to-universal-elem 
    (comet-elem  &key (univ-elem nil))
  "Convert COMET-ELEM structure to UNIV-ELEM structure, where
UNIV-ELEM is valid at the COMET-ELEM-EPOCH of osculaton."
  (declare (type comet-elem comet-elem)
	   (type (or null univ-elem) univ-elem))
  (let ((univ-elem (or univ-elem (make-univ-elem))))
    (slalib:sla-el2ue (comet-elem-epoch comet-elem) ;; epoch of osculation
		      3 
		      (comet-elem-time-peri comet-elem) 
		      (* (comet-elem-orbinc comet-elem) +pi/180+)
		      (* (comet-elem-anode comet-elem) +pi/180+)
		      (* (comet-elem-perih comet-elem) +pi/180+)
		      (comet-elem-q comet-elem)  
		      (comet-elem-e comet-elem)   
		      0d0 0d0 ;; dummy 
		      (univ-elem-velem univ-elem))
    (setf (univ-elem-id univ-elem) (comet-elem-id comet-elem))
    (setf (univ-elem-data univ-elem) (comet-elem-data comet-elem))
    univ-elem))



(defun convert-asteroid-elem-to-universal-elem 
    (asteroid-elem  &key (univ-elem nil))
  "Convert ASTEROID-ELEM structure to UNIV-ELEM structure, where
UNIV-ELEM is valid at the ASTEROID-ELEM-EPOCH of osculaton"
  (declare (type asteroid-elem asteroid-elem)
	   (type (or null univ-elem) univ-elem))
  (let ((univ-elem (or univ-elem (make-univ-elem))))
    (setf (univ-elem-data univ-elem) (asteroid-elem-data asteroid-elem))
    (slalib:sla-el2ue (asteroid-elem-epoch asteroid-elem) ;; epoch of osculation
		      2 ;; JFORM = 2 = asteroid
		      (asteroid-elem-epoch asteroid-elem) 
		      (* (asteroid-elem-orbinc asteroid-elem) +pi/180+)
		      (* (asteroid-elem-anode asteroid-elem) +pi/180+)
		      (* (asteroid-elem-perih asteroid-elem) +pi/180+)
		      (asteroid-elem-a asteroid-elem)  
		      (asteroid-elem-e asteroid-elem)   
		      (* (asteroid-elem-m asteroid-elem) +pi/180+)
		      0d0 ;; dummy daily motion; not used for JFORM=2
		      (univ-elem-velem univ-elem))
    (setf (univ-elem-id univ-elem) (asteroid-elem-id asteroid-elem))
    (setf (univ-elem-data univ-elem) (asteroid-elem-data asteroid-elem))
    univ-elem))


(defun convert-universal-elem-to-comet-elem (univ-elem)
  (multiple-value-bind (epoch orbinc anode perih aorq e aorl dm)
      (slalib:sla-ue2el (univ-elem-velem univ-elem) 3)
    (declare (ignore dm aorl))
    (make-comet-elem
     :id (univ-elem-id univ-elem)
     :epoch (aref (univ-elem-velem univ-elem) 2) ;; osculating epoch
     :time-peri epoch 
     :anode (* anode +180/pi+)
     :perih (* perih +180/pi+)
     :orbinc (* orbinc +180/pi+)
     :q aorq
     :e e
     :data (univ-elem-data univ-elem))))
  

(defun convert-universal-elem-to-asteroid-elem (univ-elem)
  (multiple-value-bind (epoch orbinc anode perih aorq e aorl dm)
      (slalib:sla-ue2el (univ-elem-velem univ-elem) 2)
    (declare (ignore dm epoch))
    (make-asteroid-elem 
     :id (univ-elem-id univ-elem)
     :epoch (aref (univ-elem-velem univ-elem) 2) ;; osculating epoch
     :anode (* anode +180/pi+)
     :perih (* perih +180/pi+)
     :orbinc (* orbinc +180/pi+)
     :a aorq
     :m (* aorl +180/pi+)
     :e e
     :data (univ-elem-data univ-elem))))
  

(defun convert-asteroid-elem-to-comet-elem (asteroid-elem)
  (convert-universal-elem-to-comet-elem
   (convert-asteroid-elem-to-universal-elem asteroid-elem)))


(defun convert-comet-elem-to-asteroid-elem (comet-elem)
  (convert-universal-elem-to-asteroid-elem
   (convert-comet-elem-to-universal-elem comet-elem)))


(defun compute-radecr-from-pv (mjd elong phi pv
			       &key
				 (units :au)
				 (altitude 0d0)
				 (correct-mjd-to-tt t)
				 (convert-to-mean-epoch 2000d0)
				 (compute-apparent-also nil))
  "Convert from a PV vector (in UNITS) to ra,dec - outputs
are (VALUES RA DEC R) and the units of R is UNITS - if
CONVERT-TO-J2000 is T (default), then convert from geocentric to
mean J2000 - may not be completely precise because planets are not.

Employs SLALIB:PV-TO-RADECR, which performs a linear light travel time
correction to find where the object was when the light left the
surface.

Observatory locations ELONG,PHI are in degrees."
  (declare (type (simple-array double-float (6)) pv)
	   (type double-float mjd elong phi altitude)
	   (type (or null double-float) convert-to-mean-epoch)
	   (type (member :au :km) units))
  (let ((pvcopy (make-array 6 :element-type 'double-float)))
    (declare (type (simple-array double-float (6)) pvcopy)
	     ;; dynamic extent is buggy!
	     #+nil(dynamic-extent pvcopy)) ;; does not help with consing
    (loop for i below 6 do (setf (aref pvcopy i) (aref pv i)))
  (when (eq units :km) 
    (slalib:convert-pv-from-km-to-au pvcopy pvcopy))
  (multiple-value-bind (ra/rad dec/rad r/au)
      (slalib:pv-to-radecr
       (if correct-mjd-to-tt (slalib:correct-mjdut-to-mjdtt mjd) mjd)       
       (* +pi/180+ elong) (* +pi/180+ phi) altitude pvcopy)
    (declare (type double-float ra/rad dec/rad r/au))

    (let* ((ra-app  (* +180/pi+ ra/rad)) ;; apparent
	   (dec-app (* +180/pi+ dec/rad))
	   (ra ra-app)
	   (dec dec-app)
	   (r (if (eq units :au)
		  r/au
		  (* r/au slalib:+km/au+))))
      ;;
    (when convert-to-mean-epoch
      (multiple-value-setq (ra dec)
	(%convert-to-mean-epoch ra-app dec-app mjd convert-to-mean-epoch 
				elong phi altitude)))
      
      (if compute-apparent-also
	(values ra dec r ra-app dec-app)
	(values ra dec r))))))
 

(defun compute-radecr-from-pv-for-observatory (mjd observatory pv
					       &key
						 (units :au)
						 (convert-to-mean-epoch 2000d0)
						 (compute-apparent-also nil))
								    
    "Convert from a PV vector (in UNITS) to ra,dec - outputs
are (VALUES RA DEC R) and the units of R is UNITS - if
CONVERT-TO-J2000 is T (default), then convert from geocentric to
mean J2000 - may not be completely precise because planets are not."
  (let ((observatory (get-observatory observatory :error-on-fail t)))
    (compute-radecr-from-pv 
     mjd 
     (- (observatory-wlongitude observatory)) 
     (observatory-latitude observatory)
     pv
     :altitude (observatory-altitude observatory)
     :units units
     :convert-to-mean-epoch convert-to-mean-epoch
     :compute-apparent-also compute-apparent-also)))


(defun compute-true-anomaly-from-pv (pv &key (units :au) (pmass 0.0d0))
  "Given PV vector compute the true anomaly in degrees - pmass is mass
of planet, where mass of sun is 1. - WARNING - unlike JPL, not light
travel time adjusted."
  ;; code taken from slalib pv2el.f
  (declare (type (simple-array double-float (6)) pv)
	   (type double-float pmass))
  (let* ((conv (if (eq units :au) 1d0 (/ 1d0 slalib:+km/au+)))
	 (day 86400D0)
	 (gcon 0.01720209895D0)
	 (x (* conv (aref pv 0)))
	 (y (* conv (aref pv 1)))
	 (z (* conv (aref pv 2)))
	 (xd (* conv day (aref pv 3)))
	 (yd (* conv day (aref pv 4)))
	 (zd (* conv day (aref pv 5)))
	 (r2 (+ (* x x) (* y y) (* z z)))
	 (r (sqrt r2))
	 (rdv (+ (* x xd) (* y yd) (* z zd)))
	 (gmu (* (+ 1d0 pmass) gcon gcon))
	 (hx (- (* y zd) (* z yd)))
	 (hy (- (* z xd) (* x zd)))
	 (hz (- (* x yd) (* y xd)))
	 (h2 (+ (* hx hx) (* hy hy) (* hz hz)))
	 (h (sqrt h2))
	 (s (* h rdv))
	 (c (- h2 (* r gmu)))
	 (at (if (or (not (zerop s)) (not (zerop c)))
		 (* (/ 180 pi) (atan s c))
		 0d0)))
    (declare (type double-float conv day gcon x y z xd yd zd r rdv gmu hx hy hz h2 h s c at)
	     (type (double-float 0d0) r2 h2))
    ;; return true anomaly in degrees but convert to customary [0-360]
    (when (minusp at) (incf at 360d0))
    at))
	
  


(defun compute-planet-radecr (nplanet mjd oelon olat &key 
			      (pv nil)
			      (correct-mjd-to-tt t)
			      (altitude 0d0)
			      (convert-to-mean-epoch 2000d0))
  (declare (type double-float mjd oelon olat altitude)
	   (type (integer 1 9) nplanet)
	   (type (or null double-float) convert-to-mean-epoch)
	   (type (or null (simple-array double-float (6))) pv))
  "Return the RA DEC of planet number NPLANET at MJD 
WARNING - slalib planet orbits are not precise!

Planets are NPLANET = 
  1=Mercury  2=Venus    3=Earth-Moon-Barycenter
  4=Mars     5=Jupiter  6=Saturn   
  7=Uranus   8=Neptune  9=Pluto  
(No Sun)"
  (setf mjd
	 (if correct-mjd-to-tt (slalib:correct-mjdut-to-mjdtt mjd)   mjd))
  
  (compute-radecr-from-pv 
   mjd
   (* +pi/180+ oelon)
   (* +pi/180+ olat)
   (slalib:sla-planet   
    mjd nplanet 
    (or pv (make-array 6 :element-type 'double-float)))
   :units :au 
   :altitude altitude
   :convert-to-mean-epoch convert-to-mean-epoch))


(defun compute-phase-angle-for-pv (pv mjd &key 
				   (epoch 2000d0))

  "Compute the phase angle in degrees for PV vector at mjd, using low precision
SLA_EVP to compute position of Earth. The Epoch of Earth's position should match epoch
of PV.  Returns (VALUES PHASE-ANGLE/DEG PV-EARTH/AU)"
  (multiple-value-bind (vxyze-bary xyze-bary vxyze-helio xyze-helio) ;; heliocentric earth vec, because SLA_PLANTE does heliocentric
      (slalib:sla-evp mjd epoch) 
    (declare (ignore xyze-bary vxyze-bary)) ;; don't use barycentric values
    ;; the vector to add to earth's position to get to object in PV
    (let* ((dvec (let ((v (make-array 3 :element-type 'double-float)))
		   (setf (aref v 0) (- (aref pv 0) (aref xyze-helio 0))
			 (aref v 1) (- (aref pv 1) (aref xyze-helio 1))
			 (aref v 2) (- (aref pv 2) (aref xyze-helio 2)))
		   v))
	   (pv-earth (make-array 6 :element-type 'double-float))
	   (dotprod (+ (* (aref pv 0) (aref dvec 0))
		       (* (aref pv 1) (aref dvec 1))
		       (* (aref pv 2) (aref dvec 2))))
	   (lenpv2 (+  (* (aref pv 0) (aref pv 0))
		       (* (aref pv 1) (aref pv 1))
		       (* (aref pv 2) (aref pv 2))))
	   (lendvec2 (+  (* (aref dvec 0) (aref dvec 0))
			 (* (aref dvec 1) (aref dvec 1))
			 (* (aref dvec 2) (aref dvec 2))))
	   (cos-phase-angle
	    (/ dotprod (sqrt (* lenpv2 lendvec2))))
	   (phase-angle/deg
	    (* (/ 180 pi) (acos cos-phase-angle))))
      ;;
       (loop for i of-type fixnum below 3 
		 do (setf (aref pv-earth i) (aref xyze-helio i)
			  (aref pv-earth (+ i 3)) (aref vxyze-helio i)))
      ;;
      (values phase-angle/deg pv-earth))))





(defun compute-planet-radecr-for-observatory
    (nplanet mjd observatory &key 
     (pv nil)
     (correct-mjd-to-tt t)
     (convert-to-mean-epoch 2000d0))
    "Return the RA DEC of planet number NPLANET at MJD 
WARNING - slalib planet orbits are not precise!
Planets are NPLANET = 
  1=Mercury  2=Venus    3=Earth-Moon-Barycenter
  4=Mars     5=Jupiter  6=Saturn   
  7=Uranus   8=Neptune  9=Pluto  
(No Sun)"
    (let ((obs (get-observatory observatory :error-on-fail t)))
      ;;
      (compute-planet-radecr
       nplanet mjd 
       (- (observatory-wlongitude obs))
       (observatory-latitude obs)
       :pv pv
       :altitude (observatory-altitude obs)
       :correct-mjd-to-tt correct-mjd-to-tt
       :convert-to-mean-epoch convert-to-mean-epoch)))



(defun compute-airmass-for-ra-dec-for-observatory
    (ra dec mjd observatory &key (ut1-utc 0d0))
  "Compute the airmass for observatory, given an apparent RA, DEC, and
MJD, performing the correct conversion from east (observatory) to
west (slalib) observer longitude.  Returns (VALUES AIRMASS
HOUR-ANGLE)"
  (let ((obs (get-observatory observatory :error-on-fail t)))
    ;;
    (slalib:compute-airmass-for-ra-dec 
     ra dec mjd 
     (- (observatory-wlongitude obs))
     (observatory-latitude obs) 
     :ut1-utc ut1-utc)))

(defun compute-hour-angle-for-ra-for-observatory
    (ra mjd observatory &key (ut1-utc 0d0))
  "Compute hour angle for an apparent RA at MJD for OBSERVATORY."
  (let ((obs (get-observatory observatory :error-on-fail t)))
    (slalib:compute-hour-angle-for-ra
     ra
     mjd
     (- (observatory-wlongitude obs))
     :ut1-utc ut1-utc)))




(defun moon-position-for-observatory (mjd observatory &key
				      (correct-mjd-to-tt t)
				      (convert-to-mean-epoch 2000d0))
  "Computes the position and size of the Earth's moon for a given observatory,
returning (VALUES RA DEC SIZE) in degrees.  Agrees with JPL Horizons to
about 0.001 deg."
  (rdplan-planet-position-for-observatory 
   3 mjd observatory
   :correct-mjd-to-tt correct-mjd-to-tt
   :convert-to-mean-epoch convert-to-mean-epoch))


(defun compute-elongation-for-ra-dec (mjd ra dec &key (observatory "geocenter"))
  "Compute the solar elongation (angle between sun and object) for a
RA,DEC at MJD.  It uses the geocenter as the observatory, but other
observatories may be substituted.  No sign convention is observed, so the
elongation should always be 0 to 180."
   (multiple-value-bind (ra-sun dec-sun)
       (slalib-ephem:rdplan-planet-position-for-observatory 10 mjd observatory)
     (astro-coords:sky-angle ra dec ra-sun dec-sun :units :degrees)))
    
(defun rdplan-planet-position-for-observatory (nplanet mjd observatory  &key
					       (correct-mjd-to-tt t)
					       (convert-to-mean-epoch 2000d0))
  "Computes the position and size of a planet (or moon) for a given
observatory using SLA_RDPLAN, returning (VALUES RA DEC SIZE) in
degrees.  This seems to agree with JPL Horizons to about 0.001 deg for
the moon.

Planets are NPLANET = 
  1=Mercury  2=Venus    3=Moon     4=Mars   5=Jupiter
  6=Saturn   7=Uranus   8=Neptune  9=Pluto  Else=Sun"
  (declare (type double-float mjd)
	   (type (signed-byte 32) nplanet)
	   (type (or null double-float) convert-to-mean-epoch))
  (let* ((obs (get-observatory observatory :error-on-fail t))
	 (oelon/deg (- (observatory-wlongitude obs)))  ;; west
	 (oelon/rad (* +pi/180+ oelon/deg))
	 (olat/deg (observatory-latitude obs))
	 (olat/rad (* +pi/180+ olat/deg))
	 (altitude (observatory-altitude obs))
	 (mjd-true (if correct-mjd-to-tt (slalib:correct-mjdut-to-mjdtt mjd) mjd)))
    (multiple-value-bind (ra/rad dec/rad diam/rad)
	(slalib:sla-rdplan   
	 mjd-true
	 nplanet
	 oelon/rad olat/rad)
      (let ((ra/deg (* +180/pi+ ra/rad))
	    (dec/deg (* +180/pi+ dec/rad))
	    (diam/deg (* +180/pi+ diam/rad)))
	;;
	(when convert-to-mean-epoch
	  (multiple-value-setq (ra/deg dec/deg)
	    (%convert-to-mean-epoch ra/deg dec/deg mjd-true convert-to-mean-epoch 
				    oelon/deg olat/deg altitude)))
      (values
       ra/deg dec/deg diam/deg)))))    




(defun moon-illuminated-fraction-for-observatory (mjd observatory  &key
						  (correct-mjd-to-tt t))
  "Compute the fraction of the moon illuminated; this ignores the non-circularity
of the moon's orbit and the altitude of the observatory, but it still agrees with JPL Horizons  
to about 1%.  Return (VALUES FRAC-ILLUMINATED MOON-PHASE-ANGLE-IN-DEG)"
  (declare (type double-float mjd))
  
  (let* ((obs (get-observatory observatory :error-on-fail t))
	 (oelon/deg (- (observatory-wlongitude obs)))  ;; west
	 (oelon/rad (* +pi/180+ oelon/deg))
	 (olat/deg (observatory-latitude obs))
	 (olat/rad (* +pi/180+ olat/deg))
	 ;;(altitude (observatory-altitude obs))
	 (mjd-true (if correct-mjd-to-tt (slalib:correct-mjdut-to-mjdtt mjd) mjd))
	 ;;
	 (ram 0d0)  (decm 0d0) (ras 0d0) (decs 0d0))

    ;; moon position, radians
    (multiple-value-setq (ram decm)
      (slalib:sla-rdplan mjd-true 3 oelon/rad olat/rad))
    ;; sun position, radians
    (multiple-value-setq (ras decs)
      (slalib:sla-rdplan mjd-true 99 oelon/rad olat/rad))
    ;;
    ;; formula C.6 and C.7 from p16 of Taylor and Bell, "Computations of the Quantities
    ;; Describing the Lunar Librations in the Astronomical Almanac"
    ;;
    (let* ((sun-moon-angle (three-vector:spherical-central-angle decs ras decm ram))
	   ;; because sun is much further than moon, this approx works
	   (moon-phase-angle/deg (- 180d0  (* #.(/ 180 pi) sun-moon-angle)))
	   (d 384400d0) ;; dist to moon (km), average 
	   (ds 149597870.700d0) ;; astronomical unit
	   (cos-e (cos sun-moon-angle))
	   (cos-es (/ (- d (* ds cos-e))
		       (sqrt (+ (expt d 2) (expt ds 2)
				(* -2 d ds cos-e)))))
	   (frac-illum
	    (* 0.5d0 (+ 1d0 cos-es))))
      ;;
      (values frac-illum  moon-phase-angle/deg))))

;; not very well tested
(defun days-from-new-moon-for-observatory (mjd observatory)
  "Returns approximate days to add to MJD to see new moon.  Positive
 means closest new moon is in future.  It is a little inaccurate
 because we evaulate by finite timesteps.  It's probably good
 to about an hour.  It works by searching for phase angle closed to
 180, using MOON-ILLUMINATED-FRACTION-FOR-OBSERVTORY

Returns
 (VALUES DAYS-TO-NEREST-NEW-MOON   ;; negative or positive if prev or next
         DAYS-TO-PREVIOUS-NEW-MOON ;; negative
         DAYS-TO-NEXT-NEW-MOON)    ;; positive"
  (flet ((mjd-new-moon-for-direction (direction &optional (dt-start 0d0))
	   ;;(terpri)
	   (loop with max-span = 32d0 ;; largest deviation from mjd we consider
		 with dt = 0d0
		 with min-aph-angle = 200d0
		 with mjd-best = 99d99
		 for mjd* = (+ mjd (* direction dt-start))
			       then (+ mjd* (* dt direction))
		 until (> (abs (- mjd* mjd)) max-span)
		 for ph-angle = (nth-value 1 (moon-illuminated-fraction-for-observatory
					      mjd* observatory))
		 for aph-angle = (- 180d0 ph-angle) ;; the anti-phase angle, 0 at new moon
		 ;; set a new timestep based on phase-angle
		 do (setf dt (cond ((< aph-angle 5.0) (/ 1d0 24d0)) ;; 1hr
				   ((< aph-angle 10)  0.5)
				   ((< aph-angle 20)  1.00)
				   ((< aph-angle 40)  2.00)
				   (t  3.00)))
		 ;;do (format t "APH: ~,3F DMJD=~,4F dt: ~,3F~%" aph-angle (- mjd* mjd) dt)
		 when (< aph-angle min-aph-angle)
		   do (setf min-aph-angle aph-angle)
		      (setf mjd-best mjd*)
		 finally
		    (return (- mjd-best mjd)))))
    ;; days to next or prev can be > MAX-SPAN so only the lower value
    ;; given by DAYS-TO-NEAREST is trusted
    (let* ((days-to-prev (mjd-new-moon-for-direction -1))
	   (days-to-next (mjd-new-moon-for-direction +1))
	   (days-to-nearest  (if (> (abs days-to-prev) (abs days-to-next))
				 days-to-next
				 days-to-prev)))
      
      ;; now if we're close to the new moon, it's possible the one of
      ;; next/prev that isn't the nearest one is also within this new
      ;; moon, so we fix it by forcing the search to start far away.  This happens
      ;; because the phase angle never hits 180, but is varyingly close.
      (if (and (< (abs days-to-prev) 3) ;; 3 is arbitrary
	       (< (abs days-to-next) 3))
	  (if (= days-to-nearest days-to-prev)
	      ;; fix days-to-next by giving a big START
	      (setf days-to-next
		    (mjd-new-moon-for-direction +1 5d0))
	      ;; else fix days-to-prev
	      (setf days-to-prev
		    (mjd-new-moon-for-direction -1 5d0))))
      
      (values days-to-nearest
	      days-to-prev
	      days-to-next)))) 
	   
    
    
(defun altaz-for-apparent-radec-for-observatory (ra-app dec-app mjd observatory
						 &key (ut1-utc 0d0))
  "Compute ALT,AZ (degrees) for APPARENT RA,DEC (degrees) for observatory.
This does not take into account atmospheric refraction and aberration."
  (let* ((obs (get-observatory observatory :error-on-fail t))
	 (elon/deg (- (observatory-wlongitude obs)))  ;; east long
	 (lat/deg (observatory-latitude obs)))
    (slalib:compute-altaz-for-apparent-radec 
     ra-app dec-app mjd elon/deg lat/deg :ut1-utc ut1-utc)))
    


			  
(defun sun-state-for-observatory (mjd observatory
				  &key (ut1-utc 0d0)
				  (adjust-for-altitude t)
				  (adjust-for-sun-size t)
				  (adjust-for-refraction t))
"Uses Sun position from SLALIB RDPLAN to determine where the sun is, returning
  (VALUES STATE SUN-ALTITUDE SUN-AZIMUTH RA-SUN DEC-SUN)
STATE is one of :SUN-DOWN :SUN-UP :CIVIL-TWILIGHT
                :NAUTICAL-TWILIGHT :ASTRONOMICAL-TWILIGHT
and Sun's alt,az are in degrees.

Applying ADJUST-FOR-ALTITUDE (the default) will adjust the sunset.
The various twilights are not adjusted, because it is unlikely that the
standard definition of a twilight takes altitude into account.

ADJUST-FOR-SUN-SIZE means that sunrise occurs when the sun is a quarter
of a degree below the horizon.

ADJUST-FOR-REFRACTION adjusts the sunset time for an approximation to refraction.

This does not take into account the observatory's altitude-induced parallax, but
that is a minor effect.

The accuracy seems to be within a minute of JPL Horizons, with a
possible asymmetry for sunrise/sunset.  It seems to match staralt to
its minute of precision.  Do not assume that the accuracy is better
than a minute or two.  Apparently, there is no general solution for
atmospheric effects to better than a minute or so - See PhD thesis
of Teresa Wilson, 2018, Michigan Tech, 'Evaluating the Effectiveness
of Current Atmospheric Refraction Models in Predicting Sunrise and
Sunset Times.'" 
  (let* ((obs (observatories:get-observatory observatory))
	 (h (observatories:observatory-altitude obs))
	 (rearth (* 6378.14d3)) 
	 ;; a value of the sun below horizon to make it match staralt
	 (refraction-adj (if adjust-for-refraction
			     (/ 34d0 60) ;; standard refraction
			     0d0))
	 (hd2 (+ (* 2 rearth h) (* h h))) ;; horizon dist squared but <0 for geocentric
	 (horizon-distance  ;; NOTE - can't fail for geocentric station with h<<1
	   (if (plusp hd2)
	       (sqrt hd2)
	       0d0))
	 (sun-angular-adj ;; adjust by radius of sun
	   (if adjust-for-sun-size
	       #.(/ 32d0 60d0 2d0) ;; angular size of sun is 32'
	       0d0))
	 (horizon-adj ;; deg
	   (if (not adjust-for-altitude)
	       0d0
	       (* (/ 180 pi)
		  (asin (/ horizon-distance (+ rearth h)))))))
 
    (multiple-value-bind (ra-sun dec-sun)
	(rdplan-planet-position-for-observatory 10 mjd observatory)
      (multiple-value-bind (alt az)
	  (altaz-for-apparent-radec-for-observatory
	   ra-sun dec-sun mjd observatory :ut1-utc ut1-utc)
	
	(let ((state (cond ((>= alt (- 0 horizon-adj sun-angular-adj refraction-adj))
			    :sun-up)
			   ((>= alt  -6.0)
			    :civil-twilight)
			   ((>= alt -12)
			    :nautical-twilight)
			   ((>= alt -18.0)
			    :astronomical-twilight)
			   (t :sun-down))))
	  (values state alt az ra-sun dec-sun))))))

  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; full ephemeris function

;; structure that is returned
(defstruct ephem
  (elem nil) ;; elements
  (observatory nil) 
  (mjd 0d0 :type double-float)
  (ra 0d0 :type double-float)
  (dec 0d0 :type double-float)
  (ra-apparent 0d0 :type double-float)
  (dec-apparent 0d0 :type double-float)
  (dra/dt 0d0 :type double-float)   ;; rate in arcsec/hr
  (ddec/dt 0d0 :type double-float)
  (rhelio  0d0  :type double-float) ;; heliocentric distance, AU
  (delta 0d0 :type double-float) ;; dist to earth, AU
  (drhelio/dt 0d0 :type double-float) ;; rate of change of rhelio in km/s (like Horizons)
  (ddelta/dt 0d0 :type double-float)  ;; rate of change of rhelio in km/s
  (alt 0d0 :type double-float)
  (az 0d0 :type double-float)
  (airmass 0d0 :type double-float)
  (hour-angle 0d0 :type double-float)
  (pv nil :type (or NULL (simple-array double-float (6)))) ;; pv in AU units
  (true-anomaly 0d0 :type double-float) ;; in degrees
  (phase-angle 0d0 :type double-float)
  ;; elongation is for observatory, not for geocenter
  (elongation-for-observatory 0d0 :type double-float) 
  ;; approx heliocetric pos of earth
  (pv-earth nil :type (or NULL (simple-array double-float (6)))) 
  (ra-moon 0d0 :type double-float)
  (dec-moon 0d0 :type double-float) (alt-moon 0d0 :type double-float)
  ;; approximate - useful for computing moonlight
  (moon-phase-angle 0d0 :type double-float)  
  (moon-frac 0d0 :type double-float)
  (moon-dist 0d0 :type double-float)
  (alt-sun 0d0 :type double-float)
  (az-sun 0d0 :type double-float)
  (ra-sun 0d0 :type double-float)
  (dec-sun 0d0 :type double-float)
  (sun-state nil)
  )




(defun compute-ephem-for-observatory
    (comet-or-univ-elem mjd observatory
     &key
       (perturb t)
       (correct-mjd-to-tt t)
       (dt/mjd 0.001d0)  ;; the time span in days over which to compute rate
       (ut1-utc 0d0)
       (convert-to-mean-epoch 2000d0))
  "Compute an EPHEM structure from COMET-OR-UNIV-ELEM, of type UNIV-ELEM or
COMET-ELEM, giving most useful observational qualities.

Caveats: - DELTA and DDELTA/DT are geocentric, not topocentric like
Horizons.  - Earth position (and all derived quanitities) are computed
using approximate solver."
  (declare (type (or comet-elem univ-elem) comet-or-univ-elem))

  ;; general pv from either comet or universal
  (flet ((compute-pv (elem mjd &key units correct-mjd-to-tt)
	   (cond ((typep elem 'univ-elem)
		  (compute-pv-from-universal-elem
		   elem mjd  
		   :units units :correct-mjd-to-tt correct-mjd-to-tt))
		 ((typep elem 'comet-elem)
		  (compute-pv-from-comet-elem   
		   elem mjd 
		   :units units :correct-mjd-to-tt correct-mjd-to-tt)))))
								  
    (let (pv pv+ ra dec ra-app dec-app ra+ dec+ rhelio rhelio+ drhelio/dt ddelta/dt
	  dra/dt ddec/dt delta true-anomaly pv-earth
	  phase-angle alt az airmass hour-angle ra-moon dec-moon
	  alt-moon moon-frac moon-phase-angle moon-dist alt-sun az-sun
	  ra-sun dec-sun elongation sun-state
	  (elem (if perturb 
		    (cond ((typep comet-or-univ-elem 'comet-elem)
			   (perturb-comet-elem mjd comet-or-univ-elem))
			  (t
			   (perturb-universal-elem mjd comet-or-univ-elem)))
		    comet-or-univ-elem)))
      
      (setf pv (compute-pv elem mjd :units :au    
				    :correct-mjd-to-tt correct-mjd-to-tt))
      (multiple-value-setq (ra dec rhelio ra-app dec-app)
	(compute-radecr-from-pv-for-observatory 
	 mjd observatory pv :units :au :convert-to-mean-epoch convert-to-mean-epoch
	 :compute-apparent-also t))

      ;; compute 2nd ra,dec for rates
      (setf pv+ (compute-pv elem (+ mjd dt/mjd) :units :au   
						:correct-mjd-to-tt correct-mjd-to-tt))
      (multiple-value-setq (ra+ dec+ rhelio rhelio+)
	(compute-radecr-from-pv-for-observatory 
	 (+ mjd dt/mjd) observatory pv+ :units :au
	 :convert-to-mean-epoch convert-to-mean-epoch))

      (multiple-value-setq (dra/dt ddec/dt)
	(%compute-rates-for-ra-dec-pair ra dec ra+ dec+ dt/mjd))
    
      (multiple-value-setq (alt az)
	(slalib-ephem:altaz-for-apparent-radec-for-observatory
	 ra-app dec-app mjd observatory :ut1-utc ut1-utc))
    
      (multiple-value-setq (airmass hour-angle)
	(compute-airmass-for-ra-dec-for-observatory
	 ra dec mjd observatory :ut1-utc ut1-utc))

      (setf rhelio
	    (sqrt (+ (expt (aref pv 0) 2) (expt (aref pv 1) 2) (expt (aref pv 2) 2))))
    
      (setf true-anomaly (compute-true-anomaly-from-pv pv :units :au :pmass 0d0))
    
      (multiple-value-setq (phase-angle pv-earth)
	(compute-phase-angle-for-pv pv mjd :epoch 2000d0))

      (setf delta (sqrt (+ (expt (- (aref pv-earth 0) (aref pv 0)) 2)
			   (expt (- (aref pv-earth 1) (aref pv 1)) 2)
			   (expt (- (aref pv-earth 2) (aref pv 2)) 2))))
    
      ;; compute range rates
      (loop ;; drhelio/dt = d/dt (x^2+y^2+z^2)^(1/2)
	     with dotprod of-type double-float = 0d0
	    with rlen of-type double-float = 0d0
	    for i of-type fixnum below 3
	    do (incf dotprod (* (aref pv i) (aref pv (+ i 3))))
	       (incf rlen (expt (aref pv i) 2))
	    finally (setf drhelio/dt (* (/ dotprod (sqrt rlen)) slalib:+km/au+)))

      (loop ;; ddelta/dt
	    with dotprod of-type double-float = 0d0
	    with rlen of-type double-float = 0d0
	    for i of-type fixnum below 3
	    for j of-type fixnum = (+ i 3)
	    for dri of-type double-float = (- (aref pv i) (aref pv-earth i))
	    for dvi of-type double-float = (- (aref pv j) (aref pv-earth j))
	    do (incf dotprod (* dvi dri))
	       (incf rlen (expt dri 2))
	    finally  (setf ddelta/dt (* (/ dotprod (sqrt rlen)) slalib:+km/au+)))

      (multiple-value-setq (moon-frac moon-phase-angle)
	(slalib-ephem:moon-illuminated-fraction-for-observatory mjd observatory))
      (multiple-value-setq (ra-moon dec-moon)
	(slalib-ephem:moon-position-for-observatory mjd observatory))
      (setf alt-moon (slalib-ephem:altaz-for-apparent-radec-for-observatory
		      ra-moon dec-moon mjd observatory))
      (multiple-value-setq (sun-state alt-sun az-sun ra-sun dec-sun)
	(slalib-ephem:sun-state-for-observatory mjd observatory))
      (setf moon-dist (astro-coords:sky-angle ra dec ra-moon dec-moon :units :degrees))
      (setf alt-moon (slalib-ephem:altaz-for-apparent-radec-for-observatory
		      ra-moon dec-moon mjd observatory))

      (setf elongation ;; for observatory, not geocenter
	    (* +180/pi+
	       (three-vector:spherical-central-angle 
		(* +pi/180+ dec)     (* +pi/180+ ra)
		(* +pi/180+ dec-sun) (* +pi/180+ ra-sun))))
    
      (make-ephem
       :elem elem :observatory (get-observatory observatory) 
       :mjd mjd :ra ra :dec dec :rhelio rhelio
       :ra-apparent ra-app :dec-apparent dec-app
       :dra/dt dra/dt :ddec/dt ddec/dt
       :alt alt :az az :airmass airmass :hour-angle hour-angle
       :rhelio rhelio  :delta delta
       :drhelio/dt drhelio/dt :ddelta/dt ddelta/dt
       :phase-angle phase-angle
       :pv pv :true-anomaly true-anomaly :pv-earth pv-earth
       :ra-moon ra-moon :dec-moon dec-moon :alt-moon alt-moon 
       :moon-frac moon-frac :moon-phase-angle moon-phase-angle
       :moon-dist moon-dist
       :alt-sun alt-sun :az-sun az-sun
       :ra-sun ra-sun :dec-sun dec-sun
       :elongation-for-observatory elongation     
       :sun-state sun-state ))))
  
#| ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

test: 

(defparameter *juno* (slalib-ephem:make-comet-elem-from-jpl 
			       :epoch 2451544.5d0
			       :tp  2452074.4087455366d0
			       :inc  12.96741821245465d0
			       :om 170.1726569883445d0
			       :w 248.031585509157d0
			       :qr  1.978500453239827d0
			       :ec .2584431433514908d0
			       ))

(defparameter *mjd*  56588.3d0)

(slalib-ephem:compute-ephem-for-observatory *juno*
					    *mjd*
					    "mko" :perturb t)

#S(slalib-ephem:ephem
   :elem #S(slalib-ephem:comet-elem
            :id nil
            :epoch 56588.3d0
            :time-peri 56853.37547859128d0
            :orbinc 12.980501774879913d0
            :anode 169.8778697142684d0
            :perih 248.3467514671574d0
            :q 1.988636590798467d0
            :e 0.25528809791586676d0)
   :observatory #S(slalib-ephem:observatory
                   :id "mko"
                   :name "Generic MKO, based on CFHT"
                   :wlongitude 155.47166666666666d0
                   :latitude 19.826666666666668d0
                   :altitude 4215.0d0
                   :timezone 10.0)
   :mjd 56588.3d0
   :ra 307.55487624167876d0
   :dec -13.671214464531083d0
   :dra/dt 29.724831955190552d0
   :ddec/dt -6.369695522767671d0
   :rhelio 2.4816032794918113d0
   :delta 2.1617915504979397d0
   :drhelio/dt -4.81124174596083d0
   :ddelta/dt 17.799801896795426d0
   :alt 40.64223187913429d0
   :az 230.16360282083966d0
   :airmass 1.5333909971516833d0
   :hour-angle 2.456310422009519d0
   :pv #(2.1433000619321216d0 -1.2108097313721564d0 -0.313941834932465d0
         3.592045198615492d-8 1.238339064542415d-7 2.1852008194406173d-8)
   :true-anomaly -88.6696405169201d0
   :phase-angle 23.467859342427918d0
   :pv-earth #(0.8628649799043262d0 0.45436224826501176d0 0.19697008188685663d0
               -1.0228457593735098d-7 1.5775085494891025d-7
               6.83946556693299d-8)
   :ra-moon 79.89702902306223d0
   :dec-moon 19.17955763066669d0
   :alt-moon 1.5075311824476059d0
   :moon-frac 0.7969734494483522d0
   :moon-dist 134.09050960430275d0
   :alt-sun -47.51423566309112d0
   :az-sun 274.70661028696344d0
   :sun-state :sun-down)

************************************************************************************************************************************************************************************************************************
 Date__(UT)__HR:MN     R.A._(J2000.0)_DEC. dRA*cosD d(DEC)/dt Azi_(a-appr)_Elev a-mass  APmag  S-brt               r        rdot            delta      deldot    S-T-O      phi  PAB-LON  PAB-LAT Tru_Anom L_Ap_Hour_Ang
************************************************************************************************************************************************************************************************************************
$$SOE
 2013-Oct-23 07:12  m  307.55513 -13.67123 29.73343  -6.33733 230.0449  40.8170  1.527   9.87   5.43  2.481634540222  -4.8112615 2.16185095648959  18.0541811  23.4642  23.4648 318.2629   4.9098 271.3273   2.443282799
$$EOE
************************************************************************************************************************************************************************************************************************



|#  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




