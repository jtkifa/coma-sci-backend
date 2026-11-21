#|

   compute projected directions for an orbit.  

   We use X,Y,Z coordinate that are the same as the J2000 heliocentric 
   PV coordinates in SLALIB.

   Additionally, we project onto the sky in the #(RA DEC) directions

   The structure ORBIT-DIRS constains a set of useful directions and projections
   for an orbit at a particular moment.

   The main function is (GENERATE-ORBIT-DIRS-FOR-COMET-ELEM comet-elem 

|#



(in-package slalib-ephem)

(defstruct orbit-dirs 
  ;; inputs
  (mjd -1d99 :type double-float)  ;; input mjd
  (olat -1d99 :type double-float) ;; latitude
  (olon -1d99 :type double-float) ;; longitude East
  (altitude -1d99 :type double-float) ;; altitude in meters
  ;;
  ;; ra and dec at input mjd, J2000
  (mjdtt 0d0 :type double-float)
  (ra  0d0 :type double-float) ;; ra and dec at mjd
  (dec 0d0 :type double-float)
  (delta 0d0 :type double-float) ;; robs in code; distance from observer to object
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; slalib 6d pv array and a copy as scratch; units are km and km/s
  (pv (make-array 6 :element-type 'double-float)  
   :type (simple-array double-float (6)))
  (pvscr (make-array 6 :element-type 'double-float) 
   :type (simple-array double-float (6)))
  ;;
  ;; unit direction vectors on sky, as #(RA-COS-DEC DEC)
  (solar-2vec (make-array 2 :element-type 'double-float)  ;; pointing from sun to object
   :type (simple-array double-float (2)))                 ;   (ie, the R component of pv)
  (orbit-2vec (make-array 2 :element-type 'double-float)  ;; pointing along orbit
   :type (simple-array double-float (2)))
  (plane-2vec  (make-array 2 :element-type 'double-float)  ;; solar cross plane
   :type (simple-array double-float (2)))
  (osolar-2vec (make-array 2 :element-type 'double-float)  ;; pointing to sun
   :type (simple-array double-float (2)))
  ;;
  ;; the same unit vectors, but in 3 dimensional space of rvvec
  (solar-3vec (make-array 3 :element-type 'double-float) ;; pointing from sun, |pv[0],pv[1],pv[2]|
             :type (simple-array double-float (3)))
  (orbit-3vec (make-array 3 :element-type 'double-float) ;; pointing along orbit, |pv[3],pv[4],pv[4]|
             :type (simple-array double-float (3)))
  (plane-3vec  (make-array 3 :element-type 'double-float) ;; solar cross plane - out of orbit dir
             :type (simple-array double-float (3)))
  ;; osolar-3vec is a vector in the orbital direction but perpendicular
  ;; to solar and plane vectors; ie, it is orbit-3vec minus the component in
  ;; in the solar direction, so that solar, osolar, and plane form an orthonormal
  ;; triad
  (osolar-3vec (make-array 3 :element-type 'double-float)  ;; pointing to sun
               :type (simple-array double-float (3)))
  ;; 3 unit vector from earth to comet - useful for dotting into others
  (ec-3vec (make-array 3 :element-type 'double-float)  
               :type (simple-array double-float (3)))
  ;; +ra and +dec 3vecs, orthogonal to ec-3vec
  (ra-3vec (make-array 3 :element-type 'double-float)  
           :type (simple-array double-float (3)))
  (dec-3vec (make-array 3 :element-type 'double-float)  
           :type (simple-array double-float (3)))
  ;; the dot products from earth to the three vectors 
  (solardot 0d0  :type double-float)  ;; ec-3vec . solar-3vec
  (orbitdot 0d0  :type double-float)  ;; ec-3vec . orbit-3vec
  (planedot 0d0  :type double-float)  ;; ec-3vec . plane-3vec
  (osolardot 0d0 :type double-float)  ;; ec-3vec . osolar-3vec ; 
  ;; position angles of the 2d unit vectors on sky, North to East
  (solar-pa 0d0   :type double-float) ;; pa of solar-2vec
  (orbit-pa 0d0   :type double-float) ;; pa of orbit-2vec
  (plane-pa 0d0   :type double-float) ;; pa of plane-2vec
  (osolar-pa 0d0  :type double-float));; pa of osolar-2vec 


(defun generate-orbit-dirs-for-comet-elem-for-observatory
    (comet-elem mjd observatory
     &key
       orbit-dirs
       (correct-mjd-to-tt t))
  (let ((obs (or (observatories:get-observatory observatory)
		 (error "observatory ~A not found" observatory)))
	(orbit-dirs (or orbit-dirs (make-orbit-dirs))))
    ;;
    (setf (orbit-dirs-mjd orbit-dirs) (* 1d0 mjd))
    (setf (orbit-dirs-olat orbit-dirs)
	  (observatories:observatory-latitude obs))
    (setf (orbit-dirs-olon orbit-dirs)
	  (- (observatories:observatory-wlongitude obs)))
    (setf (orbit-dirs-altitude orbit-dirs) 
	  (observatories:observatory-altitude obs))
    ;;
    (generate-orbit-dirs-for-comet-elem comet-elem orbit-dirs
					:correct-mjd-to-tt correct-mjd-to-tt)))


(defun generate-orbit-dirs-for-comet-elem (comet-elem orbit-dirs &key
					   (correct-mjd-to-tt t))
  "Given a COMET-ELEM, fill in an ORBIT-DIRS structure, giving the directions
of various orbit parameters, like the orbit direction, the plane direction, etc.

It is necessary to supply ORBIT-DIRS because the MJD,
OLON,OLAT,ALTITUDE inside ORBIT-DIRS must be set."
  
  (declare (type comet-elem comet-elem)
	   (type orbit-dirs orbit-dirs)
	   ;; turn off speed optimization because subroutines not optimized
	   #+nil(optimize speed)) 

  (when (< (orbit-dirs-mjd orbit-dirs) -1d98)
    (error "MJD not set in orbit dirs"))
  (when (< (orbit-dirs-olat orbit-dirs) -1d98)
    (error "OLAT obervatory latitude not set in orbit dirs"))
  (when (< (orbit-dirs-olon orbit-dirs) -1d98)
    (error "OLAT obervatory longitude not set in orbit dirs"))
  (when (< (orbit-dirs-altitude orbit-dirs) -1d98)
    (error "ALTITUDE obervatory altitude not set in orbit dirs"))
  ;;
  (let* ((mjd (orbit-dirs-mjd orbit-dirs))
	 ;; set mjdtt
	 (mjdtt (setf (orbit-dirs-mjdtt orbit-dirs)
		      (if correct-mjd-to-tt
			  (slalib:correct-mjdut-to-mjdtt mjd)
			  mjd)))
	 (obs-lat (orbit-dirs-olat orbit-dirs))
	 (obs-lon (orbit-dirs-olon orbit-dirs))
	 (altitude (orbit-dirs-altitude orbit-dirs))
	 (ra0 0d0) (dec0 0d0) (robs 0d0)
	 (rvvec (orbit-dirs-pv orbit-dirs))
	 (rvvec2 (orbit-dirs-pvscr orbit-dirs))
	 (solarvec  (orbit-dirs-solar-3vec orbit-dirs))
	 (orbitvec  (orbit-dirs-orbit-3vec orbit-dirs))
	 (planevec  (orbit-dirs-plane-3vec orbit-dirs))
	 (osolarvec (orbit-dirs-osolar-3vec orbit-dirs))
	 (ecvec (orbit-dirs-ec-3vec orbit-dirs)))
    
    (declare (type double-float mjd mjdtt obs-lat obs-lon altitude ra0 dec0 robs)
	     (type (simple-array double-float (*)) rvvec rvvec2 solarvec orbitvec
		   planevec osolarvec ecvec))


    ;; pv is always J2000
    (compute-pv-from-comet-elem comet-elem mjdtt :pv rvvec :units :km  
				:correct-mjd-to-tt nil) ;; don't correct again

    ;; take solar,orbit vecs from rvvec
    (loop for ir from 0 to 2
	  for iv from 3 to 5
	  do (setf (aref solarvec ir) (aref rvvec ir)
		   (aref orbitvec ir) (aref rvvec iv)))
	  
    ;;
    (multiple-value-setq (ra0 dec0 robs)
      (slalib-ephem:compute-radecr-from-pv mjdtt obs-lon obs-lat rvvec :units :km  
					   :convert-to-mean-epoch 2000d0
					   :altitude altitude))
    (setf (orbit-dirs-ra orbit-dirs) ra0)
    (setf (orbit-dirs-dec orbit-dirs) dec0)
    (setf (orbit-dirs-delta orbit-dirs) robs)
    ;;
    ;; normalize direction vectors
    (three-vector:normalize solarvec :vdest solarvec)
    (three-vector:normalize orbitvec :vdest orbitvec)
    ;; define the orbital plane vector as solarvec x orbitvec
    (three-vector:cross-product solarvec orbitvec :vdest planevec)
    ;; planevec is not normalized because solarvec and orbitvec not
    ;; orthogonal, so normalize it
    (three-vector:normalize planevec :vdest planevec)
    ;; now the osolarvec is planevec x solarvec
    (three-vector:cross-product planevec solarvec :vdest osolarvec)
    
    
    #+nil
    (progn
      (format t "solar . orbit=~A~%" (dotprod solarvec orbitvec))
      (format t "solar . plane=~A~%" (dotprod  solarvec planevec))
      (format t "solar . osolar=~A~%" (dotprod solarvec osolarvec))
      (format t "orbit . osolar=~A~%" (dotprod orbitvec osolarvec))
      (format t "plane . osolar=~A~%" (dotprod planevec osolarvec)))



    ;; now for each direction, move out by 0.0001 times the
    ;; observer-object distance in that direction, and compute the new
    ;; ra,dec, get the projected direction, and put it in 2d array xyvec
    (flet ((xyvec-in-direction (vdir xyvec)
	     (declare (type (simple-array double-float (3)) vdir)
		      (type (simple-array double-float (2)) xyvec))
	     (loop 
		for i below 3
		for j from 3 below 6
		do 
		  ;; adjust position by velocity
		  (setf (aref rvvec2 i) (+ (aref rvvec i)
					   (* 0.0001d0 robs (aref vdir i))))
		  ;; also copy vels for light time correction because the central
		  ;; position is light-time corrected so the ends of the direction
		  ;; vectors should be too, I *THINK*
		  (setf (aref rvvec2 j) (aref rvvec j)))
			
	     (multiple-value-bind (ra dec)
		 (slalib-ephem:compute-radecr-from-pv   
		  mjdtt obs-lon  obs-lat rvvec2 :altitude altitude :units :km)
	       (declare  (type double-float ra dec))
	       (multiple-value-bind (x y)
		   (sky-project:tan-project ra dec ra0 dec0 :units :arcsec)
		 (declare  (type double-float x y))
		 (let ((rxy (sqrt (+ (* x x) (* y y)))))
		   (declare (type double-float rxy))
		   (setf (aref xyvec 0) (/ x rxy))
		   (setf (aref xyvec 1) (/ y rxy)))))))
      ;;
      (xyvec-in-direction solarvec  (orbit-dirs-solar-2vec  orbit-dirs))
      (xyvec-in-direction orbitvec  (orbit-dirs-orbit-2vec  orbit-dirs))
      (xyvec-in-direction planevec  (orbit-dirs-plane-2vec  orbit-dirs))
      (xyvec-in-direction osolarvec (orbit-dirs-osolar-2vec orbit-dirs))

      ;; set ecvec, the unit vector pointing from earth to the comet
      (let ((evec (nth-value 3 (slalib:sla-evp mjdtt 2000d0)))) ;; 3vec of earth
	(declare (type (simple-array double-float (3)) evec))
	;; fix evec because it is in AU, not km
	(loop for i below 3 do (setf (aref evec i) (* slalib:+km/au+ (aref evec i))))
	(loop for i below 3
	      do (setf (aref ecvec i)
		       (- (aref rvvec i) (aref evec i)))))
      (three-vector:normalize ecvec :vdest ecvec)
      
      
      ;; now make ra-vec and dec-vec
      (multiple-value-bind (v1 v2)
	  (three-vector:create-two-orthog-vectors 
	   ecvec :v1 (orbit-dirs-ra-3vec orbit-dirs) :v2 (orbit-dirs-dec-3vec orbit-dirs))
	(let ((v2d (make-array 2 :element-type 'double-float)))
	  (xyvec-in-direction v1 v2d)
	  ;; now v2d is the proj of v1 on the sky, and the atan is
	  ;; how much we need to rotate v1,v2 to align them with ra,dec axes
	  (let ((theta (atan (aref v2d 1) (aref v2d 0))))
	    (three-vector:rotate-two-vectors v1 v2 (- theta) :v1-out v1 :v2-out v2))
	  ;; now v1 and v2 should be along ra,dec axes
	  #+nil
	  (progn ;; test code to print projections of v1,v2 - should be #(1 0) and #(0 1)
	    (xyvec-in-direction v1 v2d)
	    (format t "projected V1 after rotation: ~A~%" v2d)
	    (xyvec-in-direction v2 v2d)
	    (format t "projected V2 after rotation: ~A~%" v2d))

	  )))
	  


    
    ;; set the dot products
    (setf (orbit-dirs-solardot orbit-dirs)  (three-vector:dot-product ecvec solarvec))
    (setf (orbit-dirs-orbitdot orbit-dirs)  (three-vector:dot-product ecvec orbitvec))
    (setf (orbit-dirs-planedot orbit-dirs)  (three-vector:dot-product ecvec planevec))
    (setf (orbit-dirs-osolardot orbit-dirs) (three-vector:dot-product ecvec osolarvec))

    ;; set the position angles of the projected vectors, north to east
    (flet ((pos-angle (2vec) 
	     (* +180/PI+ (atan (aref 2vec 0) (aref 2vec 1)))))  ;; atan (x,y)
      ;;
      (setf (orbit-dirs-solar-pa orbit-dirs) 
	    (pos-angle (orbit-dirs-solar-2vec orbit-dirs)))
      (setf (orbit-dirs-orbit-pa orbit-dirs) 
	    (pos-angle (orbit-dirs-orbit-2vec orbit-dirs)))
      (setf (orbit-dirs-plane-pa orbit-dirs) 
	    (pos-angle (orbit-dirs-plane-2vec orbit-dirs)))
      (setf (orbit-dirs-osolar-pa orbit-dirs) 
	    (pos-angle (orbit-dirs-osolar-2vec orbit-dirs))))
      
    ;;
    orbit-dirs))

	    






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; a simpler routine giving just the anti-solar, anti-orbit
;; direction with less fuss, eg for plotting 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tested to agree with JPL for 1 case
(defun compute-sun-and-orbit-directions-for-comet-elem
    (elem mjd &key (observatory "geocenter"))
  "Given comet elements ELEM and an MJD, return the directions of the
solar (FROM-sun, TO-object) vector, the orbit vector (moving BACKWARD
in time), and the ortho (out of plane) vector, defined as cross
product ORBIT x SUN.

Returns N-to-E position angles and normalized 2D #(dRA dDec) direction
vectors as

  (VALUES PA-ANTI-SOLAR PA-ANTI-ORBIT PA-ORTHO
          V-ANTI-SUN    V-ANTI-ORBIT  V-ORTHO)


PA-SOLAR corresponds to JPL Horizons PSAng   
PA-ORBIT corresponds to JPL Horizons PSAMV

Any of these qualities can be NIL if the vector is perfectly
line-of-sight.
"

  (let* ((pv (compute-pv-from-comet-elem elem mjd))
	 (pvsun (copy-seq pv))
	 (pvorbit (copy-seq pv))
	 (pvortho (copy-seq pv)))
    
    ;; adjust pv-sun to be a bit longer than pv
    (dotimes (i 3) (setf (aref pvsun i) (* (aref pvsun i) 1.00001d0)))
    ;; adjust pvorb to be bit behind the orbit (one minute of time)
    (dotimes (i 3) (incf (aref pvorbit i) (* (aref pvorbit (+ i 3)) -1 60d0)))
    ;; compute orthog (out of plane) as ORBIT x SOLAR, and add a bit to PVORTHO
    (let ((v3-ortho (three-vector:cross-product (subseq pv 0 3) (subseq pv 3 6))))
      (three-vector:normalize v3-ortho :vdest v3-ortho)
      (dotimes (i 3) (incf (aref pvortho i) (* 0.00001d0 (aref v3-ortho i)))))
	 
    (multiple-value-bind (ra0 dec0)
	(compute-radecr-from-pv-for-observatory mjd observatory pv)
      (multiple-value-bind (ra-solar dec-solar)
	  (compute-radecr-from-pv-for-observatory mjd observatory pvsun)
	(multiple-value-bind (ra-orbit dec-orbit)
	    (compute-radecr-from-pv-for-observatory mjd observatory pvorbit)
	(multiple-value-bind (ra-ortho dec-ortho)
	    (compute-radecr-from-pv-for-observatory mjd observatory pvortho)

	  ;; make a normalized vector of the direction from ra,dec to ra0,dec0
	  (flet ((make-sky-vec (ra dec) ;; returns NIL if the vector is Line-of-Sight
		   (multiple-value-bind (x y) 
		       (sky-project:tan-project ra dec ra0 dec0 :units :arcsec)
		     (let ((norm (sqrt (+ (expt x 2) (expt y 2)))))
		       (when (plusp norm)
			 (let* ((xx (/ x norm))
				(yy (/ y norm))
				(v (make-array 2 :element-type 'double-float)))
			   (setf (aref v 0) xx
				 (aref v 1) yy)
			   v)))))
		 ;;
		 (compute-pa-for-sky-vec (v)
		   (when v
		     (let ((angle 
			     (* (/ 180 pi) (atan (aref v 0) (aref v 1))))) ;; yes, (atan x y)
		       (when (minusp angle) (incf angle 360d0)) ;; like JPL
		       angle))))
	    
	    (let* ((v-anti-solar   (make-sky-vec ra-solar dec-solar))
		   (v-anti-orbit   (make-sky-vec ra-orbit dec-orbit))
		   (v-ortho   (make-sky-vec ra-ortho dec-ortho))
		   (pa-anti-solar  (compute-pa-for-sky-vec v-anti-solar))
		   (pa-anti-orbit  (compute-pa-for-sky-vec v-anti-orbit))
		   (pa-ortho  (compute-pa-for-sky-vec v-ortho)))
	      
	      (values
	       pa-anti-solar pa-anti-orbit pa-ortho
	       v-anti-solar v-anti-orbit v-ortho)))))))))
		  
    

  
