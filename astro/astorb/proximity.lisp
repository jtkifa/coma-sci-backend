;;;; proximity.lisp - Proximity search for astorb database

(in-package :astorb)

;;; ============================================================================
;;; Proximity Structure
;;; ============================================================================

(defstruct prox
  "A proximity structure containing the RA,Dec of all asteroids at a given MJD."
  (observatory "uh88")
  (astorb nil)
  ;; MJD at which it is computed
  (mjd 0d0 :type double-float)
  ;; If MJD changes this much, recompute RA,Dec of all asteroids
  (dmjd-recompute 5e0 :type single-float)
  ;; Vectors of RA,Dec - single float for rough precision at this stage
  (ra-vec (make-array 0 :element-type 'single-float) :type (simple-array single-float (*)))
  (dec-vec (make-array 0 :element-type 'single-float) :type (simple-array single-float (*))))

(defmethod print-object ((prox prox) stream)
  (format stream "<ASTORB::PROX MJD=~,3F>" (prox-mjd prox)))


;;; ============================================================================
;;; Fill Proximity Structure
;;; ============================================================================

(defun fill-prox (prox mjd &key
                              (astorb (get-the-astorb))
                              (observatory "uh88"))
  "Fill the PROX structure with computed RA/Dec for all asteroids at MJD."
  (setf (prox-mjd prox) (float mjd 1d0))
  (setf (prox-observatory prox) observatory)
  (setf (prox-astorb prox) astorb)
  (let ((n (astorb-n astorb)))
    (setf (prox-ra-vec prox) (make-array n :element-type 'single-float))
    (setf (prox-dec-vec prox) (make-array n :element-type 'single-float))

    (loop
      with obs = (slalib-ephem:get-observatory observatory)
      with ra-vec = (prox-ra-vec prox)
      with dec-vec = (prox-dec-vec prox)
      for i below n
      for celem = (get-comet-elem-for-nth-asteroid i :astorb astorb)
      do
         (when (not
                (ignore-errors
                 (multiple-value-bind (ra dec)
                     ;; perturb=NIL speeds things up by factor of 3
                     (slalib-ephem:compute-radecr-from-comet-elem-for-observatory
                      celem mjd obs :perturb nil)
                   (setf (aref ra-vec i) (float ra 1.0))
                   (setf (aref dec-vec i) (float dec 1.0))
                   t)))
           ;; Bad orbit calculation
           (setf (aref ra-vec i) -1e10)
           (setf (aref dec-vec i) -1e10))))
  prox)


;;; ============================================================================
;;; Find Nearest Asteroids
;;; ============================================================================

(defun find-nearest-asteroids-in-prox (ra dec mjd &key
                                                       (prox nil)
                                                       (astorb (get-the-astorb))
                                                       (first-pass-tol/deg 2.0)
                                                       (tol 10.0)
                                                       (recompute-on-bad-mjd t))
  "Given a PROX structure (or none), find all asteroids within TOL arcsec
of RA,DEC at MJD. A first pass rejection is done within FIRST-PASS-TOL/DEG
degrees using stored values in PROX, then precise coordinates are computed
for the remaining ones.

Returns a list of ((N-AST DIST/ARCSEC RA DEC) ...) for all candidates,
and returns the filled PROX as the second value.

When RECOMPUTE-ON-BAD-MJD is set, refill the PROX if its MJD boundary is exceeded."
  (declare (type real ra dec mjd first-pass-tol/deg tol)
           (type (or null prox) prox))

  (let* ((ra (float ra 1d0))
         (dec (float dec 1d0))
         (mjd (float mjd 1d0))
         (first-pass-tol/deg (float first-pass-tol/deg 1d0))
         (tol (float tol 1d0))
         (prox (or prox (make-prox)))
         (n-cand-list nil)
         (final-list nil))
    (declare (type double-float ra dec first-pass-tol/deg tol))

    (when (not (prox-astorb prox))
      (fill-prox prox mjd :astorb astorb))

    (when (> (abs (- mjd (prox-mjd prox))) (prox-dmjd-recompute prox))
      (when (not recompute-on-bad-mjd)
        (error "MJD of PROX=~A is not within ~A days of given MJD ~A"
               (prox-mjd prox) (prox-dmjd-recompute prox) mjd))
      (fill-prox prox mjd :astorb (prox-astorb prox)))

    (locally (declare (optimize speed))

      (flet ((ra-dec-to-spherical (ra dec)
               (declare (optimize speed)
                        (type double-float ra dec))
               (let* ((ra/rad (* (/ pi 180) ra))
                      (dec/rad (* (/ pi 180) dec))
                      (cosra (cos ra/rad))
                      (sinra (sin ra/rad))
                      (cosdec (cos dec/rad))
                      (sindec (sin dec/rad))
                      (x (* cosra cosdec))
                      (y (* sinra cosdec))
                      (z sindec))
                 (values x y z))))

        (let* ((ra-prox-vec (prox-ra-vec prox))
               (dec-prox-vec (prox-dec-vec prox))
               (astorb (prox-astorb prox))
               (nast (length ra-prox-vec))
               (x 0d0) (y 0d0) (z 0d0))

          (multiple-value-setq (x y z) (ra-dec-to-spherical (float ra 1d0) (float dec 1d0)))

          ;; First pass: find candidates within first-pass-tol/deg
          (loop
            for i of-type fixnum below nast
            for rap of-type single-float across ra-prox-vec
            for decp of-type single-float across dec-prox-vec
            do
               (multiple-value-bind (xx yy zz)
                   (ra-dec-to-spherical (float rap 1d0) (float decp 1d0))
                 (let* ((dotprod (min 1d0 (max -1d0 (+ (* x xx) (* y yy) (* z zz)))))
                        (angle (* (/ 180 pi) (acos dotprod))))
                   (when (< angle first-pass-tol/deg)
                     (push i n-cand-list)))))

          ;; Second pass: compute precise positions for candidates
          (loop
            for n in n-cand-list
            for celem = (get-comet-elem-for-nth-asteroid n :astorb astorb)
            do
               (multiple-value-bind (rap decp)
                   (slalib-ephem:compute-radecr-from-comet-elem-for-observatory
                    celem mjd (prox-observatory prox) :perturb t)
                 (declare (type double-float rap decp))
                 (multiple-value-bind (xx yy zz)
                     (ra-dec-to-spherical (float rap 1d0) (float decp 1d0))
                   (let* ((dotprod (min 1d0 (max -1d0 (+ (* x xx) (* y yy) (* z zz)))))
                          (angle (* 3600 (/ 180 pi) (acos dotprod))))  ;; arcsec
                     (when (<= angle tol)
                       (push (list n (float angle 1.0) rap decp) final-list))))))

          (values final-list prox))))))
