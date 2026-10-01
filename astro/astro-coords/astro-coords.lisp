
#|

Coordinate system:

RA,DEC - J2000 and B1950

XYZ RADEC  -  x = cos(ra) cos(dec)
              y = sin(ra) cos(dec)
              z = sin(dec)

GalacticII - latitude lii=0 points at Galactic center
                      lii=90 points in forward motion
                      bii=90 points up at Galactic north

Galactic XYX - this is a proper right handed system
              x = -cos(lii) cos(bii)    - away from GC
              y = -sin(lii) cos(bii)    - AGAINST motion
              z = +sin(bii)             - Galactic North

Galactic UVW - From Binney and Tremaine, this is Galactic XYZ 


 

|#

(in-package astro-coords) 






;; A seems to be an additive offset to a1 (input RA-like coord)
;; B seems to be a tilt between the systems
;; C seems to be an additive offset to a2 (output DEC-like coord)
(defun transform-spherical-systems (a1 d1 A B C)
  (let* ((cd2-ca2 (* (cos d1) (cos (- a1 A))))
         (cd2-sa2 (+ (* (cos d1) (sin (- a1 A)) (cos B))
                     (* (sin d1) (sin B))))
         (sd2 (- (* (sin d1) (cos B))
                 (* (cos d1) (sin (- a1 A)) (sin B))))
         (d2 (asin sd2))
         (a2 (if (zerop (cos d2))
                 0.0
                 (atan cd2-sa2 cd2-ca2))) )
    (values (+ a2 C) d2)))


(defun transform-spherical-systems-deg (a1 d1 A B C)
  (multiple-value-bind (a2 d2)
      (transform-spherical-systems 
       (* a1 0.0174532925199433d0) (* d1 0.0174532925199433d0) 
       A B C)
    (setf a2 (* a2 57.2957795130823d0))
    (setf d2 (* d2 57.2957795130823d0))
    (cond ((< a2 0)
           (setf a2 (+ a2 360d0)))
          ((>= a2 360)
           (setf a2 (- a2 360d0))))          
    (values a2 d2)))



(defun equatorial-to-ecliptic (alpha delta)
  "Convert J2000 equatorial to ecliptic coordinates"
  (transform-spherical-systems-deg alpha delta 0d0 (* (/ pi 180) 23.439281d0) 0d0))

(defun ecliptic-to-equatorial (elong elat)
  "Convert ecliptic to J2000 equatorial coordinates"
  (transform-spherical-systems-deg elong elat 0d0 (* (/ pi 180) -23.439281d0) 0d0))


(defun ecliptic-angle-at-ecliptic-longitude (elong)
  "Compute the angle in degrees that the ecliptic makes at a
particular ecliptic longitude ELONG - ie, if ecliptic longitude
is increased by Delong, producing changes Dalpha,Ddelta, then
return ata[Ddelta/(cos(Dec)*Dalpha)]*180/pi.  This code uses a
numerical derivative and is probably not terribly precise."
  (let ((elongdbl (float elong 1d0))) ;; ensure precision
    (multiple-value-bind (ra1 dec1)
	(ecliptic-to-equatorial (- elongdbl 0.01d0) 0d0)
      (multiple-value-bind (ra2 dec2)
	  (ecliptic-to-equatorial (+ elongdbl 0.01d0) 0d0)
	(let ((ddec (- dec2 dec1))
	      ;; dra is adjusted by cos(dec)
	      (dra  (* (cos (* (/ pi 180) 0.5d0 (+ dec1 dec2)))
		       (- ra2 ra1))))
	(* (/ 180 pi) (atan (/ ddec dra))))))))
				  




(defun galactic-II->b1950 (l-II b-II)
  "convert ra dec to l,b - return (values l b)"
  (transform-spherical-systems-deg 
   l-II b-II 
   0.575958653158129d0   -1.09257611174845d0 4.926191813754d0))

(defun b1950->galactic-II (ra dec)
  "convert ra dec to l,b - return (values l b)"
  (transform-spherical-systems-deg 
   ra dec
   4.926191813754d0 1.09257611174845d0 0.575958653158129d0))

(defun galactic-II->j2000 (l-II b-II)
  "convert galactic l,b to ra,dec - return (values ra dec)"
  (multiple-value-bind (ra dec)
      (galactic-II->b1950 l-II b-II)
    (precess:b1950->j2000 ra dec)))

(defun j2000->galactic-II (ra dec)
  "convert galactic ra,dec to l,b - return (values l b)"
  (multiple-value-setq (ra dec) (precess:j2000->b1950 ra dec))
  (b1950->galactic-II ra dec))

#|
 galactic xyz coordinate system is one in which we are at 0, and
 b-II swings to +z

    +y   (solar motion in -y direction)
    | 
    |          
    -->+x                                 
    X------------------------O  Sun, at x=+8.5d3 kpc
  Gal Cent             l-II / 
  at (0,0,0)               / 
                          / 

|#

(defparameter *rsun-pc-galactic* 8500d0)
(defparameter *solar-velocity-galactic-xyz* #(0d0 0d0 220d0))

(defun galactic-II->galactic-xyz (l-II b-II r/pc &key
				       (center-on-galaxy nil))
  "convert galactic-II coordinates to x,y,z system in which Sun is
 at -8.5kpc,0,0 and b=90 is +z and l=90 points in -y direction 
Tus y is negative rotation.

Units are parsecs

If :center-on-galaxy is T xyz coordinate system has
its origin at the Galactic center, assumed *rsun-pc-galactic* from Earth"
  (declare (type (real -1d7 1d7) l-II b-II)
	   (type real r/pc))
  (let* ((rsun (if center-on-galaxy *rsun-pc-galactic* 0d0))
	 (lrad (* l-II 0.017453292519943295d0))
	 (brad (* b-II 0.017453292519943295d0))
	 (cos-l (cos lrad)) (sin-l (sin lrad)) 
	 (cos-b (cos brad)) (sin-b (sin brad)))
    (declare (type double-float rsun lrad brad cos-l cos-b))
    (values
     ;; -1 because x takes us toward center, +rsun because we are at +rsun
     (+ (* -1d0 r/pc cos-b cos-l) rsun)
     ;; -1 because l swings to negative y
     (* r/pc cos-b sin-l -1d0)
     (* r/pc sin-b))))

(defun radec->galactic-xyz (ra dec r/pc  &key (epoch :j2000)
			       (center-on-galaxy nil))
  "convert ra,dec of epoch :j2000 or :b1950 coordinates to x,y,z system in
 which Sun is at -8.5kpc,0,0 and b=90 is +z and l=90 points in -y direction
 - units are parsecs
if :center-on-galaxy is T  xyz coordinate system has
its origin at the Galactic center, assumed **rsun-pc-galactic* from Earth

The is the same as the Binney and Tremaine (right-handed) UVW system."
  (declare (type real ra dec r/pc))
  (when (eq epoch :j2000) (multiple-value-setq (ra dec)
			   (precess:j2000->b1950 ra dec)))
  (multiple-value-bind (l b)
      (b1950->galactic-ii ra dec)
    (galactic-ii->galactic-xyz l b r/pc :center-on-galaxy center-on-galaxy)))



      


(defun observed-motion->galactic-xyz-motion-OLD
  (v-los ux uy ra dec r &key (epoch :j2000) (delta-t 1.0d0))
  "convert motion as v-los (km/s) ux,uy (arcsec/yr) in ra,dec into a vector
 of vx,vy,vz in km/s - r is distance from us in pc - note that ux,uy already
 have cos(delta) incorporated - ie, ux=cos(delta)*u_alpha
 -- calculation is performed  by computing positions delta-t years apart and
 subtracting - ergo trust only first ~6 digits -
 -- NOTE that all coordinates are in STATIC frame"
  (declare (type (member :j2000 :b1950) epoch)
	   (type real v-los ux uy ra dec r))
  (let* ((delta-t (coerce delta-t 'double-float))
	 (v-los (coerce  v-los 'double-float))
	 (ux  (coerce  ux 'double-float))
	 (uy  (coerce uy 'double-float ))
	 (x0 0.0d0) (y0 0.0d0) (z0 0.0d0) (x1 0.0d0) (y1 0.0d0) (z1 0.0d0) 
	 (dt (* 3.15576d+7 delta-t)) ;; time in seconds
	 ;; change in degrees ra over delta-t yrs
	 (dra (/ (* delta-t #.(/ 1d0 3600d0) ux)
		 (cos (* dec 0.017453292519943295d0))))
	 ;; change in degrees dec over delta-t yrs
	 (ddec  (* delta-t #.(/ 1 3600d0)  uy))
	 (ra1 (+ ra dra))      ;; ra after delta-t
	 (dec1 (+ dec ddec))  ;; dec after delta-t
	 ;; los vel in pc/yr - mult by sec/yr and divide by km/pc
	 (v-los-pc/yr (* 3.15576d+7  (/ v-los 3.09d+13)))
	 (r1 (+ r (* delta-t v-los-pc/yr))))
    (declare (type double-float delta-t v-los ux uy x0 y0 z0 x1 y1 z1 dt
		   dra ddec ra1 dec1 v-los-pc/yr r1))
    (multiple-value-setq (x0 y0 z0)
      (radec->galactic-xyz ra dec r :epoch epoch))
    (multiple-value-setq (x1 y1 z1)
      (radec->galactic-xyz ra1 dec1 r1 :epoch epoch))
    ;; convert back to km/s by dividing by dt to give pc/sec and multiplying by
    ;; the number of km in a pc
    (values (* 3.09d+13 (/ (- x1 x0) dt))
	    (* 3.09d+13 (/ (- y1 y0) dt))
	    (* 3.09d+13 (/ (- z1 z0) dt)))))

;; sun's velocity in galactic xyz system, km/s 
(defparameter *solar-velocity-galactic-xyz* #(0d0 0d0 220d0))

(defun subtract-solar-motion-from-v-los--ra-dec
  (v-los ra dec &key  (epoch :j2000))
  "subtract the motion of the sun from v-los, assuming sun is moving at
   l-II=90, b-II=0, at 220 km/s -  v-los is in km/s
This is somewhat BOGUS because it doesn't subtract LSR"
  (declare (type (member :j2000 :b1950) epoch)
	   (type real ra dec))
  (when (eq epoch :j2000) (multiple-value-setq (ra dec)
			    (precess:j2000->b1950 ra dec)))
  (multiple-value-bind (l b) (b1950->galactic-ii ra dec)
    (subtract-solar-motion-from-v-los--galactic-coords v-los l b)))

(defun subtract-solar-motion-from-v-los--galactic-coords 
  (v-los l-ii b-ii &key (solar-velocity *solar-velocity-galactic-xyz*))
    "subtract the motion of the sun from v-los, assuming sun is moving at
   l-II=90, b-II=0, at 220 km/s -  v-los is in km/s
This is somewhat BOGUS because it doesn't subtract LSR"
  (declare (type (real -1d7 1d7) l-ii b-ii)
	   (type real v-los))
  (let*
      ;; z component of velocity - ie, the component parallel to
      ;; sun's motion
      ((z (* (sin (* 0.0174532925199433d0 l-ii))
	     (cos (* 0.0174532925199433d0 b-ii))))
       ;; dot product of unit vector to object with z coponent
       ;; of velocity - we add this because an object that is
       ;; moving at -220 in the direction of our orbit
       ;; is really stationary wrt the Galaxy
       (dotprod (* (aref solar-velocity 2) z)))
    (+ v-los dotprod)))

	  
      
  
  
  


;; (ac:galactic-ii->j2000 90 0) gives the following for the directions
;; of solar motion
(defparameter *solar-motion-ra* 318.00424416272193d0)
(defparameter *solar-motion-dec* 48.32964230626265d0)
(defparameter *solar-motion-vel* 220d0)
;; motion relative to local standard of rest (see B&T Eq 14)
(defparameter *lsr-motion-ra* 267.4694692373164d0)
(defparameter *lsr-motion-dec* 28.000677953302187d0)
(defparameter *lsr-motion-vel* 16.5d0)

#|

   some definitions of coordinate systems

   The xyz-ra-dec system is CENTERED ON THE SUN and has
     (1 0 0) for dec=90
     (0 1 0) for dec=0, ra=0
     (0 0 1) for dec=0, ra=90

   The xyz-galactic system is CENTERED ON THE SUN and has
     (1 0 0) pointing away from the Galactic Center  (lII=180, bII=0)
     (0 1 0) in the MINUS direction of solar motion (lII=-90, bII=0)
     (0 0 1) upward, N galactic pole (bII=90)
   in this system, the Galactic center is at x=-8500pc
    
   The xyz-galactocentric system is like the 
     xyz-galactic system, except that it is centered on the galaxy


   We transform between systems using 4x4 matrices

|#



  


;; matrix that converts x,y,z in ra,dec system to
;; x,y,z in galactic system
;;                                 [1]
;; if we multiply this matrix as M [0]  we should get the vector in 
;;                                 [0]
;; ra,dec xyz coords pointing away from galactic center
;;
(defparameter *matrix-xyz-galactic--xyz-ra-dec*
  (let ((m (make-array '(3 3) :element-type 'double-float
		       :initial-element 0d0))
	;; ra,dec galactic xhat, pointing away from galactic center
	;; obtained from (ac:galactic-ii->j2000 180 0)
	(rax 86.40485037387879d0) (decx 28.936173110295012d0)
	;; ra,dec of yhat, pointing in reverse direction of solar motion
	;; obtained from (ac:galactic-ii->j2000 -90 0)
	(ray 138.00424416272187d0) (decy -48.3296423062626d0)
	;; ra,dec of zhat, pointing up - (ac:galactic-ii->j2000 0 90)
	(raz 192.8593357281226d0) (decz 27.12825102786797d0))
    (flet ((do-col
	    (ra dec i)
	    (multiple-value-bind (x y z) (quaternion:lon-lat->xyz ra dec)
	      (setf (aref m 0 i) x
		    (aref m 1 i) y
		    (aref m 2 i) z))))
      (do-col rax decx 0)
      (do-col ray decy 1)
      (do-col raz decz 2))
    m))


(defparameter *matrix-xyz-ra-dec--xyz-galactic*
  (let ((m (matrix:copy-dbl-matrix *matrix-xyz-galactic--xyz-ra-dec*)))
    (matrix:invert-matrix-destructive-dbl m)
    m)) 
 

(declaim (inline apply-matrix-to-xyz))
;; use matrix to compute M*XYZ
(defun apply-matrix-to-xyz (m x y z)
  (declare (type (simple-array double-float (3 3)) m)
	   (type double-float x y z)
	   (optimize speed))
  (values (+ (* (aref m 0 0) x) (* (aref m 0 1) y) (* (aref m 0 2) z))
	  (+ (* (aref m 1 0) x) (* (aref m 1 1) y) (* (aref m 1 2) z))
	  (+ (* (aref m 2 0) x) (* (aref m 2 1) y) (* (aref m 2 2) z))))


(defun xyz-galactic->xyz-ra-dec (x y z)
  "convert xyz-galactic to xyz-ra-dec coordinates with a linear transform"
  (declare (type real x y z))
  (apply-matrix-to-xyz *matrix-xyz-galactic--xyz-ra-dec*
		       (float x 1d0) (float y 1d0) (float z 1d0)))

(defun xyz-ra-dec->xyz-galactic (x y z)
  "convert xyz-ra-dec to xyz-galactic coordinates with a linear transform"
  (declare (type real x y z))
  (apply-matrix-to-xyz *matrix-xyz-ra-dec--xyz-galactic*
		       (float x 1d0) (float y 1d0) (float z 1d0)))




(defun correct-xyz-ra-dec-velocity
  (vx vy vz &key (correct-solar t) (correct-lsr t)
      (solar-motion-ra *solar-motion-ra*)
      (solar-motion-dec *solar-motion-dec*)
      (solar-motion-vel *solar-motion-vel*)
      (lsr-motion-ra *lsr-motion-ra*)
      (lsr-motion-dec *lsr-motion-dec*)
      (lsr-motion-vel *lsr-motion-vel*))
  "correct km/s velocity in ra,dec xyz coordinates
for local motion.  By default, corrects for both solar
motion and Local Standard of Rest, but this can be controlled
with keywords :correct-solar and :correct-lsr"
  (declare (type double-float solar-motion-ra solar-motion-dec
		 lsr-motion-ra lsr-motion-dec)
	   (type real vx vy vz))
  (let ((vx (float vx 1d0))
	(vy (float vy 1d0))
	(vz (float vz 1d0))
	(dvx 0d0) (dvy 0d0) (dvz 0d0))
    (declare (type double-float vx vy vz dvx dvy dvz))
    (when correct-solar
      (multiple-value-bind (dvx-solar dvy-solar dvz-solar)
	  (angle->xyz solar-motion-ra solar-motion-dec solar-motion-vel)
	(incf dvx dvx-solar)
	(incf dvy dvy-solar)
	(incf dvz dvz-solar)))
    (when correct-lsr
      (multiple-value-bind (dvx-lsr dvy-lsr dvz-lsr)
	  (angle->xyz lsr-motion-ra lsr-motion-dec lsr-motion-vel)
	(incf dvx dvx-lsr)
	(incf dvy dvy-lsr)
	(incf dvz dvz-lsr)))
    ;;
    ;; we ADD dv to v because what we meausure is
    ;; v_meas = v_true - dv, so we need to compute
    ;; v_true = v_meas + dv
    (values (+ vx dvx) (+ vy dvy) (+ vz dvz))))
    
    
	

(defun observed-motion->ra-dec-xyz-motion
  (v-los/km/s  ux/arcsec/yr  uy/arcsec/yr ra dec r/pc
	       &key (correct-solar t) (correct-lsr t))
  "convert radial and proper motion (in km/s and arcsec/yr),
and distance (in pc) to an x,y,z km/s velocity in xyz-ra-dec
coordinates.  By default, subtracts solar motion and lsr motion,
but this can be controlled with :correct-solar and :correct-lsr"
  (declare (type real v-los/km/s  ux/arcsec/yr  uy/arcsec/yr ra dec r/pc))
  (let* ((v-los (float v-los/km/s 1d0))
	 (ux (float ux/arcsec/yr 1d0))
	 (uy (float uy/arcsec/yr 1d0))
	 (ux/rad/sec (* ux #.(/ pi 180 3600 3.15576d+7)))
	 (uy/rad/sec (* uy #.(/ pi 180 3600 3.15576d+7)))
	 (ra (float ra 1d0))
	 (dec (float dec 1d0))
	 (cos-ra (cos (* 0.017453292519943295d0 ra)))
	 (sin-ra (sin (* 0.017453292519943295d0 ra)))
	 (cos-dec (cos (* 0.017453292519943295d0 dec)))
	 (sin-dec (sin (* 0.017453292519943295d0 dec)))
	 (r (float r/pc 1d0))
	 (r/km (* r 3.08568 (* 1d13))) ;; 3.09d13 km per pc
	 (x (* cos-ra cos-dec))
	 (y (* sin-ra cos-dec))
	 (z sin-dec)
	 (vx 0d0) (vy 0d0) (vz 0d0))
    (declare (type double-float v-los ux uy ux/rad/sec uy/rad/sec
		   r r/km x y z vx vy vz cos-ra sin-ra cos-dec sin-dec)
	     (type (double-float -1d8 1d8) ra dec))
    ;; first compute the component of the velocity in the l.o.s. direction
    (setf vx (* x v-los)
	  vy (* y v-los)
	  vz (* z v-los))
    ;; now the ra component
    (let ((v (* r/km ux/rad/sec)))
      (incf vx (- (*  sin-ra v)))
      (incf vy    (*  cos-ra v)))
    ;; and the dec component
    (let ((v (* r/km uy/rad/sec)))
      (incf vx (- (* cos-ra sin-dec v)))
      (incf vy (- (* sin-ra sin-dec v)))
      (incf vz (* cos-dec v)))
    ;; and return the velocities
    (if (or correct-solar correct-lsr)
	(correct-xyz-ra-dec-velocity vx vy vz
				     :correct-solar correct-solar
				     :correct-lsr correct-lsr)
	(values vx vy vz))))

  
(defun observed-motion->galactic-xyz-motion
  (v-los/km/s  ux/arcsec/yr  uy/arcsec/yr ra dec r/pc
	       &key (correct-solar t) (correct-lsr t))
  "convert radial and proper motion (in km/s and arcsec/yr),
and distance (in pc) to an x,y,z km/s velocity in xyz-galactic
coordinates.   By default, subtracts solar motion and lsr motion,
but this can be controlled with :correct-solar and :correct-lsr"
  (multiple-value-bind (vx vy vz)
      (observed-motion->ra-dec-xyz-motion 
       v-los/km/s  ux/arcsec/yr  uy/arcsec/yr ra dec r/pc
       :correct-solar correct-solar :correct-lsr correct-lsr)
    (declare (type double-float vx vy vz))
    (xyz-ra-dec->xyz-galactic vx vy vz))) 


(declaim (inline %compute-units-factor))
(defun %compute-units-factor (units) ;; take radians to units given
  (declare (type (member :natural :degrees :arcmin :arcsec) units))
  (cond ((eq units :natural) 1d0)
	((eq units :degrees) #.(/ 180d0 pi))
	((eq units :arcmin) #.(* 60 (/ 180d0 pi)))
	((eq units :arcsec) #.(* 3600 (/ 180d0 pi)))
	(t (error "Unknown units"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; compute the angle between two sky positions
(declaim (inline sky-angle sky-angles))

(defun sky-angle (alpha1 delta1 alpha2 delta2 &key (units :arcsec))
  "compute angle between two RA,DEC type angle pairs ALPHA1,DELTA1 and
ALPHA2,DELTA2, by default in arcseconds - uses a arcsin algorithm that
should be accurate for small angles.

If D is the length of the vector connecting the unit vectors
representing ALPHA1,DELTA1 and ALPHA2,DELTA2, then the angle THETA
separating the two unit vectors is THETA=2*asin(D/2)"
  (declare (type double-float alpha1 delta1 alpha2 delta2)
	   (type (member :natural :degrees :arcmin :arcsec) units)
	   (optimize speed))
  (let ((x1 0d0) (y1 0d0) (z1 0d0)
	(x2 0d0) (y2 0d0) (z2 0d0)
	(delta/2 0d0)
	(f (%compute-units-factor units)))

    (declare (type double-float x1 y1 z1 x2 y2 z2 delta/2))
    ;;    
    (multiple-value-setq (x1 y1 z1) (angle->xyz alpha1 delta1))
    (multiple-value-setq (x2 y2 z2) (angle->xyz alpha2 delta2))
    ;;
    (setf delta/2 (min 1d0 ;; avoid (asin 1.00000000001)
		       (* 0.5d0 (sqrt (+ (expt (- x1 x2) 2)
					 (expt (- y1 y2) 2)
					 (expt (- z1 z2) 2))))))
    ;;
    (* 2d0 f (asin (the (double-float 0d0 1d0) delta/2)))))
			 
			 
(defun sky-angles  (alpha1 delta1 alpha2 delta2 &key (units :arcsec))
  "Return the two angles DELTA-ALPHA and DELTA-DELTA so that an RA
slew of DELTA-ALPHA followed by a dec slew of DELTA-DELTA takes us
from from ALPHA1,DELTA1 to ALPHA2,DELTA2."

  (declare (type (double-float 0d0 360d0) alpha1 alpha2)
	   (type (double-float -90d0 90d0) delta1 delta2))


  (let* ((da (- alpha2 alpha1))
	 (dd (- delta2 delta1))
	 (f (* #.(/ pi 180) (%compute-units-factor units)))
	 (cos-delta1 (cos (* (/ pi 180) delta1))))
    (declare (type double-float da dd f cos-delta1))
    ;; avoid effect of going over the ra=0 boundary
    (when (> (abs da) (abs (+ 360d0 da)))
      (setf da (+ da 360d0)))
    (when (> (abs da) (abs (+ -360d0 da)))
      (setf da (+ da -360d0)))
    
    (values (* f da cos-delta1)
	    (* f dd))))


(defun sky-angles-slew (alpha delta dalpha ddelta &key (units :arcsec))
  "Adjust sky angles ALPHA,DELTA by a small angle slew of DALPHA, then a slew 
in DDELTA, when the units of DALPHA,DELTA are the keyword UNITS.

This function complements sky-angles, so that if 
 (sky-angles a1 d1 a2 d2) => (values da dd) 
   then
 (sky-angles-slew a1 d1 da dd) => (values a2 d2)"
    (declare (type (double-float 0d0 360d0) alpha)
	     (type (double-float -90d0 90d0) delta)
	     (type double-float dalpha ddelta))
  (let* ((cosdelta (cos (* (/ pi 180) delta)))
	 (fac (*  (/ pi 180) (%compute-units-factor units))) ;; take Dxx to deg
	 (dalpha/deg (/ dalpha fac))
	 (ddelta/deg (/ ddelta fac))
	 (alpha-new (+ alpha (/ dalpha/deg cosdelta)))
	 (delta-new (+ delta ddelta/deg)))
    (regularize-equatorial-coordinates alpha-new delta-new)))
  
    
    
	  
	   
 



(defun convert-declination-ha-to-alt-az (declination-angle/deg hour-angle/deg 
					 &key (latitude 0d0))
  "Convert DECLINATION-ANGLE and HOUR-ANGLE (positive West),
both in degrees, to ALT above horizin, and AZIMUTH, starting to north
positive to East. Return (VALUES ALT AZ) in degrees.

By default, the observer's latitude is 0d0 (observer is at equator).
The LATITUDE keyword is used to change it."
  ;; the right-handed coordinate system is flat on the earth, Z up, Y North
  ;; 
  ;; rotate UP (Zhat) around X axis, counterclockwise (negative)
  (let ((declination-angle/deg (float declination-angle/deg 1d0))
	(hour-angle/deg (float hour-angle/deg 1d0))
	(latitude (float latitude 1d0)))
    
    (multiple-value-bind (x1 y1 z1)
	(quaternion:rotate-around-vector 0d0 0d0 1d0 ;; zhat
					 1d0 0d0 0d0 ;; xhat
					 (* -1
					    (- declination-angle/deg latitude)
					    (/ pi 180)))
      ;; rotate the resulting vector by +HOUR-ANGLE around yhat
      (multiple-value-bind (x2 y2 z2)
	  (quaternion:rotate-around-vector x1 y1 z1
					   0d0 1d0 0d0
					   (* -1 hour-angle/deg (/ pi 180)))
	;;
	;; truncate if necessary if float roundoff error occurred
	(let ((x2fix (if (> x2 1) 1d0 (if (< x2 -1) -1d0 x2)))
	      (y2fix (if (> y2 1) 1d0 (if (< y2 -1) -1d0 y2)))
	      (z2fix (if (> z2 1) 1d0 (if (< z2 -1) -1d0 z2))))
	;; convert back into spherical ALT-AZ coordinates, minding signs and conventions
	(let* ((az (* (/ 180 pi) (atan x2fix y2fix)))
	       (az360 (if (minusp az) (+ az 360d0) az))
	       (alt (* (/ 180 pi) (asin z2fix))))
	  (values alt az360)))))))
	
				       
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; convert xyz equatorial to xyz ecliptic
;; WARNING not well tested
(let* ((matrix-eq-to-ec
	 (make-array
	  '(3 3)
	  :element-type 'double-float
	  :initial-contents
	  ;; given by transforming elong,elat to ra,dec
	  ;; then ra,dec to xyx
	  '((1d0 0d0 0d0) ;; x=elong=0,elat=0
	    (0d0 0.9174821322657694d0 0.3977769940218479d0) ;; y=90,0
	    (0d0 -0.3977769940218484d0 0.9174821322657691d0));; z=0,90
	  ))
       (matrix-ec-to-eq
	 (matrix:invert-matrix matrix-eq-to-ec)))
      
  (defun equatorial-to-ecliptic-xyz (vec)
    "Convert a double precision 3-vector VEC representing equatorial XYZ
(ie, x direction pointing in RA=0, Z in DEC=90, Y in RA=90)
to a similar vector in the ecliptic XYZ system - useful, eg, for measuring
distance from Z=0 equatorial plane."
    (3vec:3matrix-times-3vector matrix-eq-to-ec vec))

        
  (defun ecliptic-to-equatorial-xyz (vec)
    "Convert a double precision 3-vector VEC representing ecliptic XYZ
(ie, x direction pointing in RA=0, Z in DEC=90, Y in RA=90)
to a similar vector in the equatorial XYZ system."
    (3vec:3matrix-times-3vector matrix-ec-to-eq vec)))
  
