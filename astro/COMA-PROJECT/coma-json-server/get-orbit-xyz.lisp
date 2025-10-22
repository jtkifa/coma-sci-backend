

#|

Return ecliptic X,Y,Z for an object, in AU, moving backward and
forward from MJD, and accumulating points spaced in distance by
DR-FRAC times Rhelio, until the orbit is outside RHELIO-MAX, or
until the orbit closes.

Request

{ 
    "TYPE":"REQUEST",
    "ID":"abc123",
    "COMMAND":"get-orbit-xyz",
    
    "PARAMETERS" : {
       // the usual ORBIT object, or strings describing where to obtain the orbit
       "ORBIT": {ID: .. EPOCH-MJD: .. ...}, // or "JPL-ORBIT" or "MPC-ORBIT"
 
       // if the stamp center is determined using MPC-ORBIT, 
       //  JPL-ORBIT, or JPL-EPHEM
       "OBJECT-NAME": "2P", 

       // the MJD from which we start, and plot forward and back in time
       "MJD":55555.0,

       // max distance in AU to plot forward and back in time from MJD
       "RHELIO-MAX": 30.0,

       // orbital size step, relative to distance from sun (approx)
       "DR-FRAC": 0.02
    }
}

Returns:

{
       "TYPE":"response",
       "COMMAND":"get-orbit-xyz",
       "ID":"123abc",
       "PARAMETERS": {
            "MJD":        55555.0, 
            "RHELIO-MAX": 30.0,   // in case a default value was used
            "DR-FRAC":    0.01,   // in case a default value was used
            "MJD-VEC": [50000.0, ...],
            "X-VEC":   [11.0, ...],
            "Y-VEC":   [22.0, ...],
            "Z-VEC":   [33.0, ...],
            // is this orbit closed?
            "IS-CLOSED": false
        }
}



|#


(in-package coma-json-server)

(def-json-command get-orbit-xyz (json-req)
  (with-json-command-setup (json-req)
    (let* ((object-name (get-param "OBJECT-NAME"))
           (mjd (get-param "MJD"))
           (json-orbit (or (get-param "ORBIT")
			   "JPL-ORBIT"))
	   comet-elem comet-elem-error
	   univ-elem ;; we use univ elem to avoid issues for hyperbolics
	   (rhelio-max (get-param "RHELIO-MAX" :default 30d0))
	   (dr-frac (get-param "DR-FRAC" :default 0.01d0)))

      ;; we do it to preserve the source of the orbit error
      (when json-orbit
	(multiple-value-setq (comet-elem comet-elem-error)
	  (get-orbit-using-method
	   json-orbit object-name :mjd mjd)))


      (jcom-test-expr (not json-orbit)
		      "NO-ORBIT-GIVEN"
		      "No ORBIT was given, but ORBIT needs to be 'JPL-ORBIT', 'MPC-ORBIT', or a valid JSON orbit element structure")
      
      ;; if orbit not given, set it to time-peri
      (when (and comet-elem (not mjd))
	(setf mjd (orbital-elements:comet-elem-time-peri comet-elem)))


      (jcom-test-expr (or (not mjd)
			  (not (< 0d0 mjd 200000d0)))
		      "INVALID-MJD"
		      (format nil "Invalid or out of range MJD ~A" mjd))
      ;;
      (jcom-test-expr (and json-orbit (not comet-elem))
		      "FAILED-TO-OBTAIN-ORBIT"
		      (format
		       nil
		       "Failed to parse or obtain provided ORBIT=\"~A\" - error is '~A'"
		       json-orbit comet-elem-error))
      ;;
      (multiple-value-bind (uel error)
	  (ignore-errors
	   (slalib-ephem:convert-comet-elem-to-universal-elem comet-elem))
	(jcom-test-expr
	 (not uel)
	 "FAILED-TO-CONVERT-ORBIT-TO-UNIVERSAL-ELEMENTS"
	 (format nil "Got comet elements, but failed to convert to universal elements with error ~A" error))
	(setf univ-elem uel))
      ;;
      (jcom-test-expr (or (not (realp rhelio-max))
			  (not (< rhelio-max 300)))
		      "INVALID-RHELIO-MAX"
		      "RHELIO-MAX is not a number <300 AU")
      ;;
      (jcom-test-expr (or (not (realp dr-frac))
			  (not (< 0.001 dr-frac 0.2)))
		      "INVALID-DR-FRAC"
		      "RHELIO-MAX is not a number in the range 0.001 to 0.2")
		 ;;
      (multiple-value-bind (mjd-vec x-vec y-vec z-vec is-closed)
	  (ignore-errors
	   (%get-orbit-xyz-internal
	    comet-elem univ-elem mjd rhelio-max dr-frac))
	;;
	;; handle internal error (2nd value)
	(when (not mjd-vec)
	  (let ((%err x-vec))
	    (return-with-error
	     "INTERNAL-ORBIT-CALCULATION-ERROR"
	     (format nil "Internal error computing orbit XYZ: ~A"
		     %err))))

	
	(set-param "MJD" mjd)
	(set-param "RHELIO-MAX"  rhelio-max)
	(set-param "DR-FRAC"  dr-frac)
	(set-param "MJD-VEC"  mjd-vec)
	(set-param "X-VEC"  x-vec)
	(set-param "Y-VEC"  y-vec)
	(set-param "Z-VEC"  z-vec)
	(set-param "IS-CLOSED" (if is-closed 'yason:true 'yason:false ))
	))))
				
;; return (values mjd-vec x-vec y-vec z-vec is-closed)
(defun %get-orbit-xyz-internal (comet-elem univ-elem mjd rhelio-max dr-frac)
  (let* ((rperi (orbital-elements:comet-elem-q comet-elem))
	 (ecc (orbital-elements:comet-elem-e comet-elem))
	 (a/au (orbital-mech:a-from-pericenter-e rperi ecc))
	 (a/mks (* +au/m+ a/au))
	 (period/sec
	   (* 2 pi (sqrt (/ (expt a/mks 3) +gm-sun+))))
	 (period/days
	   (/ period/sec +day/sec+))
	 (raph (if (< ecc 1)
		   (orbital-mech:apocenter-from-a-e a/au ecc)
		   1e6)) ;; 1M AU for non-elliptical orbit
	 ;; if aphelion is inside rhelio-max, close the orbit
	 (is-closed (< raph rhelio-max)))

    (cond (is-closed ;; closed orbit case
	   (multiple-value-bind (mjd-vec x-vec y-vec z-vec)
	       (%get-orbit-xyz/closed univ-elem mjd period/days dr-frac)
	     (values mjd-vec x-vec y-vec z-vec T)))
	  ;;
	  (t ;; not closed
	   (multiple-value-bind (mjd-vec x-vec y-vec z-vec)
	       (%get-orbit-xyz/open univ-elem mjd rhelio-max dr-frac)
	     (values mjd-vec x-vec y-vec z-vec nil))))))


(defun %get-orbit-xyz/closed (univ-elem mjd period/days dr-frac)
  (let ((mjd-start (- mjd (* 0.5d0 period/days)))
	(mjd-end   (+ mjd (* 0.99 (* 0.5d0 period/days)))))
    #+nil (format *error-output* "MJD-START=~A MJD-END=~A~%"
	    mjd-start mjd-end)
    (loop with pv = (make-array 6 :element-type 'double-float)
	  with outlist = nil
	  with %mjd = mjd-start
	  while (< %mjd mjd-end)
	  do
	     (slalib-ephem:compute-pv-from-universal-elem
	      univ-elem %mjd
	      :pv pv
	      :units :au)
	     (let* ((v ;; velocity, AU/day
		      (* +day/sec+
			 (sqrt (+ (expt (aref pv 3) 2) ;; au/sec in PV[3..5]
				  (expt (aref pv 4) 2)
				  (expt (aref pv 5) 2)))))
		    (rh ;; current rhelio, AU
		      (sqrt (+ (expt (aref pv 0) 2) ;; au in PV[0..2]
			       (expt (aref pv 1) 2)
			       (expt (aref pv 2) 2))))
		    ;; next timestep
		    (dt (* dr-frac (/ (max rh 0.1) v)))
		    ;; next mjd
		    (mjd-next (+ %mjd dt)))
	       (setf %mjd mjd-next)
	       (multiple-value-bind (x y z)
		   (equatorial-xyz-to-ecliptic
		    (aref pv 0) (aref pv 1) (aref pv 2))
		 (push (list %mjd x y z) outlist))
	     #+nil(format *error-output* "  V=~F  RH=~F DT=~F MJD=~A~%" v rh dt %mjd))
	     ;;
	  finally
	     (setf outlist (nreverse outlist))
	     (return
	       (values
		(map 'vector 'first outlist)  ;; mjd
		(map 'vector 'second outlist) ;; x
		(map 'vector 'third outlist)  ;; y
		(map 'vector 'fourth outlist) ;; z
		)))))
	       
		   
	      
	  

(defun  %get-orbit-xyz/open (univ-elem mjd rhelio-max dr-frac)
  ;; subroutine that extracts the orbit in one direction (+/- in time).
  ;; the +time direction gets the point at MJD also.
  ;; it returns a time-sorted list of (MJD X Y Z)
  (flet ((get-orbit-one-direction (tsign)
	   (loop with pv = (make-array 6 :element-type 'double-float)
		 with stop = nil
		 with outlist = nil
		 with %mjd = mjd
		 for first-step = t then nil
		 until stop
		 do
		    (slalib-ephem:compute-pv-from-universal-elem
		     univ-elem %mjd
		     :pv pv
		     :units :au)
		    (let* ((v ;; velocity, AU/day
			     (* +day/sec+
				(sqrt
				 (+ (expt (aref pv 3) 2) ;; au/sec in PV[3..5]
				    (expt (aref pv 4) 2)
				    (expt (aref pv 5) 2)))))
			   (rh ;; current rhelio, AU
			     (sqrt (+ (expt (aref pv 0) 2) ;; au in PV[0..2]
				      (expt (aref pv 1) 2)
				      (expt (aref pv 2) 2))))
			   ;; next timestep, in TSIGN direction
			   (dt (* tsign dr-frac (/ (max rh 0.1) v)))
			   ;; next mjd
			   (mjd-next (+ %mjd dt)))
		      (setf %mjd mjd-next)

		      ;; stop the expansion when we exceed rhelio-max
		      (when (> rh rhelio-max)
			(setf stop t))

		      ;; count first step at MJD only for positive TSIGN
		      (when (or (plusp tsign) (not first-step))
			(multiple-value-bind (x y z)
			    (equatorial-xyz-to-ecliptic
			     (aref pv 0) (aref pv 1) (aref pv 2))
			  (push (list %mjd x y z) outlist))))

		 finally
		    (when (plusp tsign)
		      (setf outlist (nreverse outlist)))
		    (return outlist))))

    (let ((full-outlist (append (get-orbit-one-direction -1)
				(get-orbit-one-direction +1))))
      
      (values
       (map 'vector 'first full-outlist)  ;; mjd
       (map 'vector 'second full-outlist) ;; x
       (map 'vector 'third full-outlist)  ;; y
       (map 'vector 'fourth full-outlist) ;; z
       ))))

			   
