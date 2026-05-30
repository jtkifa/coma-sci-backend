
(in-package orbital-elements)



;; the asteroid elements we use for computations
(defstruct comet-elem
  (id nil) ;; some identifier
  (epoch 0d0 :type double-float)
  ;; in notation of slalib
  (time-peri 0d0 :type double-float) ;; time-peri in PS1 notation
  (orbinc 0d0 :type double-float)    ;; inclination in DEG
  (anode 0d0 :type double-float)     ;; ascending node in DEG
  (perih 0d0 :type double-float)     ;; argument of perih in DEG
  (q 0d0 :type double-float)         ;; q (perihelion dist) in AU
  (e 0d0 :type double-float)         ;; eccentricity
  (nongravs nil)                    ;; an optional nongravs structure
  (data  nil))                       ;; extra data payload
 

(defstruct (comet-elem+err (:include comet-elem));; comet elems + error
  (time-peri-err 0d0 :type double-float) ;; time-peri in PS1 notation
  (orbinc-err 0d0 :type double-float)    ;; inclination in DEG
  (anode-err 0d0 :type double-float)     ;; ascending node in DEG
  (perih-err 0d0 :type double-float)     ;; argument of perih in DEG
  (q-err 0d0 :type double-float)         ;; q (perihelion dist) in AU
  (e-err 0d0 :type double-float))         ;; eccentricity

 
;; 
(defstruct nongravs 
  (dt  0d0 :type double-float)   ;; non-grav lag parameter (days)
  (a1  0d0 :type double-float)   ;; radial     non-grav accel, AU/DAY^2
  (a2  0d0 :type double-float)   ;; tangential non-grav accel, AU/DAY^2
  (a3  0d0 :type double-float))  ;; normal     non-grav accel, AU/DAY^2


;; universal elements in the manner of SLALIB
;; a 13 element vector:
;;  0    Combined mass in solar units
;;  1    energy, 1/a
;;  2    Reference epoch t0 in JD
;;  3,4,5  Position vector at t0 in AU
;;  6,7,8  Velocity vector at t0, in AU/sec
;;  9    Distance from Sun at t0, ini AU
;;  10   r dot v at t0
;;  11   specific angular momentum
;;  12   Basically, timestamp it was last called.  Apparently, a fossil.
;;       Probably, we don't even let SLALIB change it.         
(defstruct univ-elem
  (id nil)
  (velem (make-array 13 :element-type 'double-float)
   :type (simple-array double-float (13)))
  (data  nil))                       ;; extra data payload

  
(defstruct asteroid-elem
  (id nil)
  (epoch 0d0 :type double-float)    ;; t0
  (orbinc 0d0 :type double-float)   ;; in deg
  (anode 0d0 :type double-float)    ;; ascending node in DEG
  (perih 0d0 :type double-float)    ;; argument of perih in DEG
  (a 0d0 :type double-float)        ;; semimajor axis, AU
  (e 0d0 :type double-float)        ;; eccentricity
  (m 0d0 :type double-float)        ;; mean anomaly, degrees
  (data  nil))                       ;; extra data payload

(defstruct (asteroid-elem+err (:include asteroid-elem))
  (orbinc-err 0d0 :type double-float)
  (anode-err 0d0 :type double-float)    
  (perih-err 0d0 :type double-float)   
  (a-err 0d0 :type double-float)        
  (e-err 0d0 :type double-float)        
  (m-err 0d0 :type double-float))        

  
;; descriptors for comets and asteroids, that might
;; be obtained from jpl or mpc
(defstruct object-desc
  (name nil)
  (source nil))

(defstruct (asteroid-desc (:include object-desc))
  (number nil)
  (h nil)
  (g nil)
  (radius nil)
  (period nil) ;; hrs
  (albedo nil))

(defstruct (comet-desc (:include object-desc))
  ;; maybe add stuff here
  )


