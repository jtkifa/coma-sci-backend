#|

Return an ephemeris given an orbit

|#


(in-package coma-json-server)


(defun convert-pv-from-equatorial-to-ecliptic (pv)
  (declare (type (simple-array double-float (*))))
  (let ((pvout  (make-array 6 :element-type 'double-float)))
    (multiple-value-bind (x y z)
	(astro-coords:xform-xyz-to-xyz
	 astro-coords:+quaternion-equatorial-j2000-to-ecliptic-j2000+
	 (aref pv 0) (aref pv 1) (aref pv 2))
      (setf (aref pvout 0) x
	    (aref pvout 1) y
	    (aref pvout 2) z))
    ;;
    (multiple-value-bind (vx vy vz)
	(astro-coords:xform-xyz-to-xyz
	 astro-coords:+quaternion-equatorial-j2000-to-ecliptic-j2000+
	 (aref pv 3) (aref pv 4) (aref pv 5))
      (setf (aref pvout 3) vx
	    (aref pvout 4) vy
	    (aref pvout 5) vz))
    ;;
    pvout))
	  
      

(defun fill-orbit-ephem-response (json-resp comet-elem
				  object
				  mjd-start 
				  mjd-end
				  dt/min
				  obs-code centerbody)
  (declare (ignore centerbody))
  (flet ((return-with-error (err-name err-desc)
	   	(setf (json-object-error json-resp)
		      (make-error-object
		       :error err-name
		       :desc err-desc))
	   (return-from fill-orbit-ephem-response))
	 ;;
	 (pushval (hash field-name value)
	   (vector-push-extend
	    value
	    (gethash field-name hash))))
    ;;
    ;;
    (let* ((observatory
	     (or (observatories:get-observatory obs-code)
		 (return-with-error "UNKNOWN-OBSERVATORY"
				    (format nil "OBS-CODE ~A is not known." obs-code))))
	   (univ-elem
	     (or (ignore-errors
		  (slalib-ephem:convert-comet-elem-to-universal-elem
		   comet-elem))
		 (return-with-error
		  "FAILED-TO-CONVERT-ORBIT-TO-UNIVERSAL-ELEMENTS"
		  (format nil "Failed to convert orbit ~A to universal elements in fill-orbit-ephem-response."
			  comet-elem))))
	   (parameters  (json-object-parameters json-resp))
	   (eph         (make-hash-table :test 'equalp)) ;; EPH sub-structure
	   (sunvect     (make-hash-table :test 'equalp))
	   (earthvect   (make-hash-table :test 'equalp)))


      (when (> mjd-start mjd-end)
	(return-with-error "MJD-START-AFTER-MJD-END"
			   (format  nil "Requested EPHEM with MJD-START<MJD-END")))

      ;; set up eph, earthvect, and sunvect
      (setf (gethash "eph" parameters) eph)
      (setf (gethash "earthvect" parameters) earthvect)
      (setf (gethash "sunvect" parameters) sunvect)
      (loop for vect in (list earthvect sunvect)
	    for cb in '("@399" "@500") ;; earth and sun (fixme: barycenter?)
	    for oc in (list obs-code "0") ;; our obs code, and center of sun 
	    do
	       (setf (gethash "centersite" vect) obs-code)
	       (setf (gethash "centerbody" vect) cb) 
	       (setf (gethash "vectorstr" vect) nil)    ;; probably not used
	       (loop
		 for key in '("x" "y" "z" "vx" "vy" "vz")
		 do (setf (gethash key vect) (make-array 0 :adjustable t :fill-pointer t))))
      ;; set up other vectors in eph
      (loop
	for key in *db-eph-fields*
	when (not (member key '("targetbody" "centerbody" "centersite" "ephstr" "tmageph" "nmageph")
			  :test 'equalp))
	  do (setf (gethash key eph) (make-array 0 :adjustable t :fill-pointer t)))
      (setf (gethash "targetbody" eph) object)
      (setf (gethash "centerbody" eph) "@399") ;; earth
      ;; absent fields are NIL
      (loop for field in '("ephstr" "tmageph" "nmageph")
	    do
	       (setf (gethash field eph) nil))
      
      (loop with dt/day = (/ dt/min (* 24d0 60d0))
	    for mjd from mjd-start to mjd-end by dt/day
	    do
	       (multiple-value-bind (ephem err)
		   (ignore-errors (slalib-ephem:compute-ephem-for-observatory
				   univ-elem mjd observatory))
		 (when (not ephem)
		   (return-with-error "SLALIB-ORBIT-FAILURE"
				      (format nil "SLALIB failure: ~A" err)))
		 (pushval eph "ctdatetime" (astro-time:mjd-to-ut-string mjd :frac-seconds t))
		 (pushval eph "jd" (astro-time:mjd-to-jd mjd))
		 (pushval eph "solarpres"
			  (let ((p (slalib-ephem:ephem-alt-sun ephem)))
			    (cond ((> p 0) "*")
				  ((> p -6) "C")  ;; civil twilight
				  ((> p -12) "N") ;; nautical
				  ((> p -18) "A") ;; astronomical
				  (t " "))))      ;; dark
		 (pushval eph "lunarpres"
			  (if (plusp (slalib-ephem:ephem-alt-moon ephem)) "m" " "))

		 
		 (pushval eph "j2kra"
			  (ra-dec:deg->hms-string (slalib-ephem:ephem-ra ephem)))
		 (pushval eph "j2kdec"
			  (ra-dec:deg->dms-string (slalib-ephem:ephem-dec ephem)))
		 (pushval eph "j2kradeg"
			  (slalib-ephem:ephem-ra ephem))
		 (pushval eph "j2kdecdeg"
			  (slalib-ephem:ephem-dec ephem))
		 ;;
		 (pushval eph "appra"
			  (ra-dec:deg->hms-string (slalib-ephem:ephem-ra-apparent ephem)))
		 (pushval eph "appdec"
			  (ra-dec:deg->dms-string (slalib-ephem:ephem-dec-apparent ephem)))
		 (pushval eph "appradeg"
			  (slalib-ephem:ephem-ra-apparent ephem))
		 (pushval eph "appdecdeg"
			  (slalib-ephem:ephem-dec-apparent ephem))
		 ;;
		 (pushval eph "dracosd" (slalib-ephem:ephem-dra/dt ephem))
		 (pushval eph "ddect"  (slalib-ephem:ephem-ddec/dt ephem))
		 ;;
		 (pushval eph "amass" (slalib-ephem:ephem-airmass ephem))
		 ;;
		 (pushval eph "r" (slalib-ephem:ephem-rhelio ephem))
		 (pushval eph "rdot" (slalib-ephem:ephem-drhelio/dt ephem))
		 (pushval eph "delta" (slalib-ephem:ephem-delta ephem))
		 (pushval eph "deldot" (slalib-ephem:ephem-ddelta/dt ephem))
		 ;;
		 (pushval eph "sto" (slalib-ephem:ephem-phase-angle ephem))
		 ;; target-observer-moon angle
		 (pushval eph "tom" (slalib-ephem:ephem-moon-dist ephem))
		 ;; moon illum percent
		 #+nil ;; not the fraction of the object illuminated
		 (pushval eph "illupercent" (* 100 (slalib-ephem:ephem-moon-frac ephem)))
		 ;; illuminated fraction - 1/2(1+cos(PhaseAngle))
		 ;;(pushval eph "illupercent"
		 ;;(* 100 (* 0.5d0
		 ;;    (+ 1 (cos (* (/ pi 180)
		 ;;       (slalib-ephem:ephem-phase-angle ephem)))))))
		 (pushval eph "illupercent"
			  (* 100 (slalib-ephem:ephem-moon-frac ephem)))
						 
		 ;;
		 (multiple-value-bind (glxlon glxlat)
		     (astro-coords:j2000->galactic-ii
		      (slalib-ephem:ephem-ra ephem)
		       (slalib-ephem:ephem-dec ephem))
		   (pushval eph "glxlat" glxlat)
		   (pushval eph "glxlon" glxlon))


		 (let ((true-anomaly (slalib-ephem:ephem-true-anomaly ephem))
		       (light-travel-time/min
			 (* (/ 1 60d0) ;; convert seconds to minutes
			    (/ (* (slalib-ephem:ephem-delta ephem)
				  units:cgs-au) ;; cm
			       units:cgs-c))))
		   (pushval eph "trueanom" true-anomaly)
		   (pushval eph "onewaylttime" light-travel-time/min))
		 
		 ;;
		 ;; fill in sunvect and earthvect
		 (loop for vect in (list sunvect earthvect)
		       for pv-vec in (list
				      ;; PV vector of object
				      (convert-pv-from-equatorial-to-ecliptic
				       (slalib-ephem:ephem-pv ephem))
				      ;; difference vector PV - PV_earth
				      (convert-pv-from-equatorial-to-ecliptic
				       (map 'vector '-
					    (slalib-ephem:ephem-pv ephem)
					    (slalib-ephem:ephem-pv-earth ephem))))
		       do
		       (loop
			 for key in '("x" "y" "z" "vx" "vy" "vz")
			 for i from 1
			 for value across pv-vec
			 for scale = (if (<= i 3)
					 1d0 ;; AU->AU for x,y,z
					 (* 24 3600)) ;; AU/sec --> AU/day for vx,vy,vz
			 do (pushval vect key (* scale value))))))
		 
      
      json-resp)))
	   

#|
;; PROBLEMS comparing to JPL result - these now seem to be fixed

- FIXED PV coordinate system seems to disagree.  X is the same, but not Y and Z
  (JPL uses ecliptic, we use equatorial, so a simple transform?)


One set of velocities might include earth rotation (so drdt, ddeltadt)
  -- this appears to be the case because calling JPL using geocentric coordinates
     gives an answer closer to JPL.


galactic coords are slightly off (0.01 deg)



|#
