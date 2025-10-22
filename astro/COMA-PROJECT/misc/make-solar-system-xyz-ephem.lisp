
#|

generate XYZ for the planets, over one orbital period

|#

(asdf:load-system "slalib-ephem")
(asdf:load-system "jpl-horizons")
(asdf:load-system "astro-coords")
(asdf:load-system "astro-time")


(defparameter *planets*
  '((1 . "Mercury") (2 . "Venus") (3 . "Earth") (4 . "Mars")
   (5 . "Jupiter") (6 . "Saturn") (7 . "Uranus") (8 . "Neptune")
    (9 . "Pluto")))

(defparameter *periods* ;; planetary periods in days
  '(88.0	224.7	365.2  687.0	4331	10747	30589	59800	90560))

;; return vectors of x,y,z for a plane, ecliptic coords, AU
(defun get-planet-xyz (iplanet &key (npts 50))
  (let* ((xvec (make-array npts))
	 (yvec (make-array npts))
	 (zvec (make-array npts))
	 (period (nth iplanet *periods*))
	 (kplanet (first (nth iplanet *planets*)))
	 (pv (make-array 6 :element-type 'double-float))
	 ;; choose mjd0 for accuracy of Pluto (see manual)
	 (mjd0 (astro-time:parse-ut-date-and-time-string-to-mjd
		"1885-01-01T00:00:00"))
	 (dmjd (* 1d0 (/ period  npts))))
    (loop for mjd from mjd0 to (+ mjd0 period) by dmjd
	  for i below npts
	  do  (slalib:sla-planet mjd kplanet pv  ;; au units
				 ;; pluto is not good for a full orbit
				 :ignore-out-of-range t)
	      (let ((xyz-vec ;; ecliptic
		     (astro-coords:equatorial-to-ecliptic-xyz
		      (three-vector:make-3vec
		       (aref pv 0) (aref pv 1) (aref pv 2)))))
		(setf (aref xvec i) (aref xyz-vec 0))
		(setf (aref yvec i) (aref xyz-vec 1))
		(setf (aref zvec i) (aref xyz-vec 2))))
    (vector xvec yvec zvec)))
		      
    
    
    
  
(defun make-csv (&key
		   (outfile "planetary-xyz.csv")
		   (npts 50))

  (with-open-file (sout outfile :if-exists :supersede
			:direction :output)

    (loop for (iplan . planet) in *planets*
	  for first-time = t then nil
	  do (format sout "~A~A X, ~A Y, ~A Z"
		     (if first-time "" ", ")
		     planet planet planet))
    (terpri sout)

    (loop with xyz-list 
	    = (loop for iplanet from 0 below 9
		    collect (get-planet-xyz iplanet :npts npts))
	  for i below npts
	  do (loop for first-time = t then nil
		   for xyz-vecs in xyz-list
		   for xvec = (aref xyz-vecs 0)
		   for yvec = (aref xyz-vecs 1)
		   for zvec = (aref xyz-vecs 2)
		   for x = (aref xvec i)
		   for y = (aref yvec i)
		   for z = (aref zvec i)
		   do (format sout "~A~,3F, ~,3F, ~,3F"
			      (if first-time "" ", ")
			      x y z))
	     (terpri sout))))

