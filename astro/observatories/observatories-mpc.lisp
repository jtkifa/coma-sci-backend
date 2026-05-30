
;; package for mbc observatories

(in-package observatories)

(defparameter *mpc-obs-list-file*
  (format nil "~A~A" 
	  (namestring (asdf:system-source-directory
		       (asdf:find-system "observatories")))
	  "data/mpc-obs-codes.txt"))


;; see https://github.com/Bill-Gray/miscell/blob/master/mpc_stat.cpp

(defconstant +earth-major-axis+ 6378140d0)
(defconstant +earth-minor-axis+ 6356755d0)
(defconstant +earth-axis-ratio+ (/ +earth-minor-axis+ +earth-major-axis+))

(defun %lat-alt-to-parallax (lat ht-in-meters)
  (let* ((u (atan (* (sin lat)
		     (/ +earth-axis-ratio+ (cos lat)))))
	 (rho-sin-phi
	   (+ (* +earth-axis-ratio+ (sin u))
	      (* (/ ht-in-meters +earth-major-axis+)
		 (sin lat))))
	 (rho-cos-phi
	   (+ (cos u)
	      (* (/ ht-in-meters +earth-major-axis+)
		 (cos lat)))))
    (values rho-cos-phi rho-sin-phi)))
    

;; iterative solution - ALT is GEODETIC, not GEOCENTRIC
(defun %parallax-to-lat-alt (rho-cos-phi rho-sin-phi)
  (let* ((lat0 (atan rho-sin-phi rho-cos-phi))
	 (rho0 (sqrt (+ (* rho-sin-phi rho-sin-phi)
			(* rho-cos-phi rho-cos-phi))))
	 (tlat lat0)
	 (talt 0d0))
	
    (declare (type double-float lat0 rho0 tlat talt))
    (loop for iter below 8
	  do
	     (multiple-value-bind (rc2 rs2)
		 (%lat-alt-to-parallax tlat talt)
	       (declare (type double-float rc2 rs2))
	       (decf talt (* (- (sqrt (+ (* rc2 rc2) (* rs2 rs2))) rho0)
			     +earth-major-axis+))
	       (decf tlat (- (atan rs2 rc2) lat0))))
	(values tlat talt)))


;; this returns a timezone based only on longitude.
(defun %estimate-timezone-for-longitude (wlong)
  (let ((wlong-fixed (cond ((> wlong 180)
			    (- wlong 360))
			   ((<= wlong -180)
			    (+ wlong 360))
			   (t
			    wlong))))
	  
  (round (/ wlong-fixed 15))))


(defun %parse-mpc-observatory (string)
  (declare (type string string))
  (let* ((code     (string-trim " " (subseq string 0 3)))	
	 (elon-str (string-trim " " (subseq string 4 13)))
	 (cos-str  (string-trim " " (subseq string 13 21)))
	 (sin-str  (string-trim " " (subseq string 21 30)))
	 (desc     (string-trim " " (subseq string 30)))
	 (elon     (ignore-errors (jk-parse-float:parse-float elon-str)))
	 (rhocosphi  (ignore-errors  (jk-parse-float:parse-float cos-str)))
	 (rhosinphi  (ignore-errors   (jk-parse-float:parse-float sin-str))))

    
    (when (and elon rhocosphi rhosinphi) ;; they all have values
      (multiple-value-bind (lat/rad ht-in-meters)
	  (%parallax-to-lat-alt rhocosphi rhosinphi)
      
	(let* ((wlon (let ((x (nth-value 1 (round (- 360 elon) 360))))
		       (if (< x 180) x (- x 360))))
	       (lat (* (/ 180 pi) lat/rad)))
	  (make-observatory
	   :id code
	   :name (format nil "MPC: ~A" desc)
	   :obscode code
	   :wlongitude wlon
	   :latitude lat
	   :altitude ht-in-meters
	   :timezone (%estimate-timezone-for-longitude wlon) ))))))
	   
				      



(defun read-mpc-obseratory-list (&key (infile *mpc-obs-list-file*))
  (with-open-file (s infile)
    (loop for line = (read-line s nil nil)
	  until (not line)
	  for obs = (ignore-errors (%parse-mpc-observatory line))
	  when obs collect obs)))
