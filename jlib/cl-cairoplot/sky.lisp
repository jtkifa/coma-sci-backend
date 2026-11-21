;;;; sky.lisp — sky projections, WCS image placement and coordinate grids
;;;;
;;;; A sky plot is drawn in the plane of a projection: :PLOT coordinates
;;;; are the projection plane's (x, y) in degrees, x increasing to the
;;;; east (FITS intermediate world coordinates).  Projections are classes
;;;; (MAKE-PROJECTION :TAN RA0 DEC0 etc.):
;;;;
;;;;   zenithal, about any reference point:  :TAN gnomonic (a camera),
;;;;     :SIN orthographic (a hemisphere, radio), :ARC equidistant,
;;;;     :ZEA equal area (the whole sky), :STG stereographic (conformal)
;;;;   all-sky, about RA0 on the equator:  :AIT Hammer-Aitoff and :MOL
;;;;     Mollweide (equal area ellipses), :CAR plate carree
;;;;
;;;; SET-WINDOW-PROJECTION gives a viewport a projection and a window
;;;; about its reference point (by default its whole boundary, for those
;;;; that have one, and clips drawing to it).  An image whose WCS is
;;;; linear in the same projection (most optical images, in TAN) is
;;;; placed exactly by an affine transform, its WCS CD matrix:
;;;;
;;;;   (let* ((wcs (make-wcs :crval '(150 2.2) :crpix '(80.5 60.5)
;;;;                         :cdelt '(-0.001 0.001) :crota 25))
;;;;          (vp (make-viewport d 0.1 0.9 0.1 0.9)))
;;;;     (set-window-projection vp (wcs-projection wcs) 0.12)
;;;;     (plot-image vp image :wcs wcs)
;;;;     (plot-points vp ras decs :coords :sky)
;;;;     (draw-sky-grid vp))
;;;;
;;;; An image whose WCS is not linear in the plot's projection (another
;;;; tangent point, another projection, distortion terms) is reprojected
;;;; instead: (PLOT-IMAGE VP IMAGE :WCS WCS) resamples it onto the plot,
;;;; and PLOT-CONTOURS with :WCS maps each contour vertex through the WCS.
;;;; A WCS is any object with methods on the generic functions
;;;; WCS-PIXEL-TO-SKY and WCS-SKY-TO-PIXEL (0-based pixels; e.g. an
;;;; adapter to a full WCS library), and optionally WCS-PLANE-TRANSFORM
;;;; for the exact affine case.  The WCS class here is linear in any of
;;;; the projections above.
;;;;
;;;; :COORDS :SKY places points, lines and text by right ascension and
;;;; declination (degrees) through the viewport's projection, exactly.
;;;; DRAW-SKY-GRID traces lines of constant RA and Dec (curved in the
;;;; plane, converging toward a pole) and labels them where they cross
;;;; the frame, or, when a projection's whole boundary is in view, around
;;;; and inside that boundary.

(in-package :cl-cairoplot)

(defconstant +rad+ (/ pi 180))

;;; ============================================================
;;; Coordinate frames
;;; ============================================================
;;;
;;; A frame is a system of longitude and latitude on the sky, related to
;;; equatorial RA and Dec (J2000) by a rotation: :EQUATORIAL, :GALACTIC
;;; (IAU, in the Hipparcos J2000 form) or :ECLIPTIC (mean, J2000).

(defparameter *frame-matrices*
  (let ((e (* 23.4392911d0 (/ pi 180))))
    `((:galactic . #2A((-0.0548755604162154d0 -0.8734370902348850d0 -0.4838350155487132d0)
                       ( 0.4941094278755837d0 -0.4448296299600112d0  0.7469822444972189d0)
                       (-0.8676661490190047d0 -0.1980763734312015d0  0.4559837761750669d0)))
      (:ecliptic . ,(make-array '(3 3) :initial-contents
                                `((1d0 0d0 0d0)
                                  (0d0 ,(cos e) ,(sin e))
                                  (0d0 ,(- (sin e)) ,(cos e)))))))
  "Rotation matrices from equatorial unit vectors to each frame's.")

(defun frame-matrix (frame)
  (or (cdr (assoc frame *frame-matrices*))
      (error "Unknown coordinate frame ~S (:EQUATORIAL, :GALACTIC or :ECLIPTIC)." frame)))

(defun rotate-lonlat (m lon lat transpose)
  "LON, LAT (degrees) rotated by the matrix M (or its transpose)."
  (let* ((l (* lon (/ pi 180))) (b (* lat (/ pi 180)))
         (v (list (* (cos b) (cos l)) (* (cos b) (sin l)) (sin b)))
         (w (loop for r below 3
                  collect (loop for c below 3
                                sum (* (if transpose (aref m c r) (aref m r c)) (nth c v))))))
    (destructuring-bind (x y z) w
      (values (mod (/ (atan y x) (/ pi 180)) 360)
              (/ (asin (max -1 (min 1 z))) (/ pi 180))))))

(defun equatorial-to-frame (frame ra dec)
  "Values: the longitude and latitude in FRAME of RA, DEC (degrees)."
  (if (eq frame :equatorial)
      (values ra dec)
      (rotate-lonlat (frame-matrix frame) ra dec nil)))

(defun frame-to-equatorial (frame lon lat)
  "Values RA DEC (degrees) of longitude LON, latitude LAT in FRAME."
  (if (eq frame :equatorial)
      (values lon lat)
      (rotate-lonlat (frame-matrix frame) lon lat t)))

;;; ============================================================
;;; Projections
;;; ============================================================

(defclass sky-projection ()
  ((ra0 :initarg :ra0 :reader projection-ra0)
   (dec0 :initarg :dec0 :reader projection-dec0)
   (frame :initarg :frame :initform :equatorial :reader projection-frame))
  (:documentation "A projection of the sky about the reference point
   RA0, DEC0 (degrees: longitude and latitude in FRAME) onto a plane
   with coordinates in degrees, x increasing toward increasing
   longitude.  SKY-TO-PLANE and PLANE-TO-SKY take and give equatorial
   RA and Dec whatever the frame."))

(defgeneric sky-to-plane (projection ra dec)
  (:documentation "Values X Y (degrees) of the sky position RA, DEC
   (degrees); X increases toward the east (increasing RA).  NaNs where
   the projection does not reach."))

(defgeneric plane-to-sky (projection x y)
  (:documentation "Values RA DEC (degrees) of the plane point X, Y;
   NaNs outside the projection's boundary."))

(defgeneric projection-boundary (projection)
  (:documentation "The edge of the projected sky as lists XS YS of a
   closed polygon in the plane, or NIL if the projection is unbounded.")
  (:method ((p sky-projection)) nil))

(defun nan2 () (values float-utils:+double-float-nan+ float-utils:+double-float-nan+))

;;; The methods for each projection work in its frame's longitude and
;;; latitude; these convert from and to equatorial.

(defmethod sky-to-plane :around ((p sky-projection) ra dec)
  (cond ((or (nan-p ra) (nan-p dec)) (nan2))
        ((eq (projection-frame p) :equatorial) (call-next-method))
        (t (multiple-value-bind (lon lat) (equatorial-to-frame (projection-frame p) ra dec)
             (call-next-method p lon lat)))))

(defmethod plane-to-sky :around ((p sky-projection) x y)
  (if (or (nan-p x) (nan-p y))
      (nan2)
      (multiple-value-bind (lon lat) (call-next-method)
        (if (or (nan-p lon) (eq (projection-frame p) :equatorial))
            (values lon lat)
            (frame-to-equatorial (projection-frame p) lon lat)))))

;;; Zenithal projections: a point at angular distance C from the
;;; reference point, in direction A (east of north), lands at radius
;;; R(C) in direction A.

(defclass zenithal-projection (sky-projection) ()
  (:documentation "A projection about a reference point, symmetric around it: TAN,
   SIN, ARC, ZEA or STG (see MAKE-PROJECTION)."))

(defgeneric zenithal-radius (projection c)
  (:documentation "R (radians) at angular distance C (radians), or NIL
   where the projection does not reach."))

(defgeneric zenithal-distance (projection r)
  (:documentation "The angular distance C at radius R (radians), or NIL
   beyond the boundary."))

(defgeneric zenithal-limit (projection)
  (:documentation "The greatest radius (radians) reached, or NIL if unbounded.")
  (:method ((p zenithal-projection)) nil))

(defmacro define-zenithal (name code doc (c radius) (r distance) &optional limit)
  `(progn
     (defclass ,name (zenithal-projection) () (:documentation ,doc))
     (defmethod zenithal-radius ((p ,name) ,c) ,radius)
     (defmethod zenithal-distance ((p ,name) ,r) ,distance)
     ,@(when limit `((defmethod zenithal-limit ((p ,name)) ,limit)))
     (setf (get ,code 'projection-class) ',name)))

(define-zenithal tan-projection :tan
  "The gnomonic (TAN) projection: a camera's focal plane.  Great circles
   project to straight lines; it reaches less than 90 degrees out."
  (c (when (< c (/ pi 2)) (tan c)))
  (r (atan r)))

(define-zenithal sin-projection :sin
  "The orthographic (SIN) projection: the hemisphere about the reference
   point as seen from far away."
  (c (when (<= c (/ pi 2)) (sin c)))
  (r (when (<= r 1) (asin r)))
  1)

(define-zenithal arc-projection :arc
  "The zenithal equidistant (ARC) projection: distances from the
   reference point are true."
  (c (when (< c pi) c))
  (r (when (<= r pi) r))
  pi)

(define-zenithal zea-projection :zea
  "The zenithal equal area (ZEA, Lambert) projection: areas are true;
   the whole sky fits in a disk."
  (c (when (< c pi) (* 2 (sin (/ c 2)))))
  (r (when (<= r 2) (* 2 (asin (/ r 2)))))
  2)

(define-zenithal stg-projection :stg
  "The stereographic (STG) projection: conformal, so small shapes are
   true; circles on the sky project to circles."
  (c (when (< c pi) (* 2 (tan (/ c 2)))))
  (r (* 2 (atan (/ r 2)))))

(defmethod sky-to-plane ((p zenithal-projection) ra dec)
  (let* ((a (* +rad+ (- ra (projection-ra0 p))))
         (d (* +rad+ dec)) (d0 (* +rad+ (projection-dec0 p)))
         ;; direction cosines: east, north, toward the reference point
         (x (* (cos d) (sin a)))
         (y (- (* (cos d0) (sin d)) (* (sin d0) (cos d) (cos a))))
         (z (+ (* (sin d0) (sin d)) (* (cos d0) (cos d) (cos a))))
         (sinc (sqrt (+ (* x x) (* y y))))
         (c (atan sinc z))
         (r (zenithal-radius p c)))
    (cond ((null r) (nan2))
          ((zerop sinc) (values 0d0 0d0))
          (t (let ((k (/ r sinc +rad+)))
               (values (* x k) (* y k)))))))

(defmethod plane-to-sky ((p zenithal-projection) xi eta)
  (let* ((x (* +rad+ xi)) (y (* +rad+ eta))
         (d0 (* +rad+ (projection-dec0 p)))
         (rho (sqrt (+ (* x x) (* y y))))
         (c (zenithal-distance p rho)))
    (cond ((null c) (nan2))
          ((zerop rho) (values (projection-ra0 p) (projection-dec0 p)))
          (t (values (mod (+ (projection-ra0 p)
                             (/ (atan (* x (sin c))
                                      (- (* rho (cos d0) (cos c)) (* y (sin d0) (sin c))))
                                +rad+))
                          360)
                     (/ (asin (max -1 (min 1 (+ (* (cos c) (sin d0))
                                                (/ (* y (sin c) (cos d0)) rho)))))
                        +rad+))))))

(defmethod projection-boundary ((p zenithal-projection))
  (let ((r (zenithal-limit p)))
    (when r (ellipse-outline (/ r +rad+) (/ r +rad+)))))

(defun ellipse-outline (a b &optional (n 360))
  (loop for k below n
        for th = (* 2 pi (/ k n))
        collect (* a (cos th)) into xs
        collect (* b (sin th)) into ys
        finally (return (values xs ys))))

;;; All-sky projections, about RA0, DEC0: longitude L and latitude B are
;;; native coordinates, with the reference point at L = B = 0 and native
;;; north toward the frame's north pole.  About the equator L = RA - RA0
;;; in -180..180 and B the declination; otherwise the sky is rotated (as
;;; FITS's default LONPOLE), and the frame's poles fall inside the
;;; ellipse.

(defclass allsky-projection (sky-projection) ()
  (:documentation "A projection of the whole sky: Hammer-Aitoff, Mollweide or plate
   carree (see MAKE-PROJECTION)."))

(defgeneric allsky-forward (projection l b)
  (:documentation "Plane X Y (radians) of longitude L, latitude B (radians)."))

(defgeneric allsky-inverse (projection x y)
  (:documentation "Longitude and latitude (radians) of plane X Y
   (radians), or NIL outside the boundary."))

(defun tilted-p (p)
  "Is the all-sky projection P about a point off its frame's equator?"
  (/= 0 (projection-dec0 p)))

(defun allsky-native (p ra dec)
  "Native L, B (radians) of RA, DEC (degrees, in P's frame)."
  (if (tilted-p p)
      ;; rotate the reference point to L = B = 0, by -RA0 about the pole
      ;; then DEC0 about the new y axis
      (let* ((a (* +rad+ (- ra (projection-ra0 p)))) (d (* +rad+ dec))
             (d0 (* +rad+ (projection-dec0 p)))
             (x (* (cos d) (cos a))) (y (* (cos d) (sin a))) (z (sin d))
             (xn (+ (* x (cos d0)) (* z (sin d0))))
             (zn (- (* z (cos d0)) (* x (sin d0)))))
        (values (atan y xn) (asin (max -1 (min 1 zn)))))
      (values (* +rad+ (- (mod (+ (- ra (projection-ra0 p)) 180) 360) 180))
              (* +rad+ dec))))

(defun allsky-celestial (p l b)
  "RA, DEC (degrees, in P's frame) of native L, B (radians)."
  (if (tilted-p p)
      (let* ((d0 (* +rad+ (projection-dec0 p)))
             (xn (* (cos b) (cos l))) (y (* (cos b) (sin l))) (zn (sin b))
             (x (- (* xn (cos d0)) (* zn (sin d0))))
             (z (+ (* xn (sin d0)) (* zn (cos d0)))))
        (values (mod (+ (projection-ra0 p) (/ (atan y x) +rad+)) 360)
                (/ (asin (max -1 (min 1 z))) +rad+)))
      (values (mod (+ (projection-ra0 p) (/ l +rad+)) 360) (/ b +rad+))))

(defmethod sky-to-plane ((p allsky-projection) ra dec)
  (multiple-value-bind (l b) (allsky-native p ra dec)
    (multiple-value-bind (x y) (allsky-forward p l b)
      (values (/ x +rad+) (/ y +rad+)))))

(defmethod plane-to-sky ((p allsky-projection) xi eta)
  (multiple-value-bind (l b) (allsky-inverse p (* +rad+ xi) (* +rad+ eta))
    (if l
        (allsky-celestial p l b)
        (nan2))))

(defmacro define-allsky (name code doc ((l b) &body forward) ((x y) &body inverse) boundary)
  `(progn
     (defclass ,name (allsky-projection) () (:documentation ,doc))
     (defmethod allsky-forward ((p ,name) ,l ,b) ,@forward)
     (defmethod allsky-inverse ((p ,name) ,x ,y) ,@inverse)
     (defmethod projection-boundary ((p ,name)) ,boundary)
     (setf (get ,code 'projection-class) ',name)))

(define-allsky ait-projection :ait
  "The Hammer-Aitoff (AIT) projection: equal area, the sky in a 2:1
   ellipse with curved parallels."
  ((l b) (let ((g (sqrt (/ 2 (+ 1 (* (cos b) (cos (/ l 2))))))))
           (values (* 2 g (cos b) (sin (/ l 2))) (* g (sin b)))))
  ((x y) (let ((e (+ (expt (/ x (* 2 (sqrt 2d0))) 2) (expt (/ y (sqrt 2d0)) 2))))
           (when (<= e (+ 1 1d-9))
             (let ((z (sqrt (max 0 (- 1 (expt (/ x 4) 2) (expt (/ y 2) 2))))))
               (values (* 2 (atan (/ (* z x) 2) (- (* 2 z z) 1)))
                       (asin (max -1 (min 1 (* y z)))))))))
  (ellipse-outline (/ (* 2 (sqrt 2d0)) +rad+) (/ (sqrt 2d0) +rad+)))

(define-allsky mol-projection :mol
  "The Mollweide (MOL) projection: equal area, the sky in a 2:1 ellipse
   with straight parallels."
  ((l b) (let ((th (mollweide-theta b)))
           (values (* (/ (* 2 (sqrt 2d0)) pi) l (cos th)) (* (sqrt 2d0) (sin th)))))
  ((x y) (let ((s (/ y (sqrt 2d0))))
           (when (<= (abs s) 1)
             (let* ((th (asin s))
                    (b (asin (max -1 (min 1 (/ (+ (* 2 th) (sin (* 2 th))) pi)))))
                    (l (if (zerop (cos th)) 0 (/ (* pi x) (* 2 (sqrt 2d0) (cos th))))))
               (when (<= (abs l) (+ pi 1d-9))
                 (values l b))))))
  (ellipse-outline (/ (* 2 (sqrt 2d0)) +rad+) (/ (sqrt 2d0) +rad+)))

(defun mollweide-theta (b)
  "The auxiliary angle with 2 theta + sin 2 theta = pi sin B."
  (if (>= (abs b) (- (/ pi 2) 1d-12))
      (* (signum b) (/ pi 2))
      (let ((th b) (target (* pi (sin b))))
        (loop repeat 50
              for f = (- (+ (* 2 th) (sin (* 2 th))) target)
              until (< (abs f) 1d-14)
              do (decf th (/ f (+ 2 (* 2 (cos (* 2 th)))))))
        th)))

(define-allsky car-projection :car
  "The plate carree (CAR) projection: RA and Dec as x and y."
  ((l b) (values l b))
  ((x y) (when (and (<= (abs x) pi) (<= (abs y) (/ pi 2))) (values x y)))
  (values (list 180 -180 -180 180) (list 90 90 -90 -90)))

(defun ctype-frame (ctype)
  "The coordinate frame of a FITS CTYPE such as \"GLON-AIT\"."
  (let ((axis (string-upcase (subseq ctype 0 (min 4 (length ctype))))))
    (cond ((member axis '("GLON" "GLAT") :test #'string=) :galactic)
          ((member axis '("ELON" "ELAT") :test #'string=) :ecliptic)
          (t :equatorial))))

(defun make-projection (code ra0 &optional (dec0 0) frame)
  "The projection named by CODE (:TAN :SIN :ARC :ZEA :STG, or all-sky
   :AIT :MOL :CAR, or a FITS CTYPE such as \"RA---SIN\" or
   \"GLON-AIT\") about longitude RA0, latitude DEC0 (degrees; an
   all-sky projection about DEC0 not 0 is tilted, its frame's north
   pole straight up from the center) in FRAME (:EQUATORIAL, :GALACTIC or
   :ECLIPTIC; default from a CTYPE, else :EQUATORIAL)."
  (let* ((frame (or frame (if (stringp code) (ctype-frame code) :equatorial)))
         (code (if (stringp code)
                   (intern (string-upcase (subseq code (max 0 (- (length code) 3)))) :keyword)
                   code))
         (class (or (get code 'projection-class)
                    (error "Unknown sky projection ~S." code))))
    (frame-matrix-or-equatorial frame)
    (make-instance class :ra0 ra0 :dec0 dec0 :frame frame)))

(defun frame-matrix-or-equatorial (frame)
  (unless (eq frame :equatorial) (frame-matrix frame)))

(defun make-tan-projection (ra0 dec0)
  "A gnomonic (TAN) projection about RA0, DEC0 (degrees):
   (MAKE-PROJECTION :TAN RA0 DEC0)."
  (make-projection :tan ra0 dec0))

(defplot sky-to-plot (viewport ra dec)
  "Values: the :PLOT coordinates of RA, DEC through VIEWPORT's projection."
  (sky-to-plane (viewport-sky-projection viewport) ra dec))

(defplot plot-to-sky (viewport x y)
  "Values RA DEC of the :PLOT point X, Y through VIEWPORT's projection."
  (plane-to-sky (viewport-sky-projection viewport) x y))

(defplot set-window-projection (viewport projection &optional half-width (half-height half-width))
  "Give VIEWPORT the sky PROJECTION and a window HALF-WIDTH by
   HALF-HEIGHT degrees (in the plane) about its reference point, east
   (increasing RA) to the left, with equal scales.  Without HALF-WIDTH,
   the window is the projection's whole boundary, and drawing in the
   viewport is clipped to the boundary.  Returns VIEWPORT."
  (setf (viewport-projection viewport) projection)
  (multiple-value-bind (xs ys) (projection-boundary projection)
    (unless half-width
      (unless xs (error "~A has no boundary; give a HALF-WIDTH." projection))
      (setf half-width (reduce #'max (mapcar #'abs xs))
            half-height (reduce #'max (mapcar #'abs ys))))
    (set-window-equal-scales viewport half-width (- half-width) (- half-height) half-height)
    (setf (viewport-clipping-region viewport)
          (if xs
              (region-intersection (viewport-region viewport) (polygon-region xs ys))
              :viewport)))
  viewport)

(defplot set-window-tan (viewport projection half-width &optional (half-height half-width))
  "SET-WINDOW-PROJECTION with a window HALF-WIDTH by HALF-HEIGHT."
  (set-window-projection viewport projection half-width half-height))

;;; ============================================================
;;; WCS
;;; ============================================================

;;; The WCS protocol: any object with methods on WCS-PIXEL-TO-SKY and
;;; WCS-SKY-TO-PIXEL can place an image (by reprojection) and its
;;; contours.  A method on WCS-PLANE-TRANSFORM lets it be placed exactly
;;; by an affine transform when the plot's projection allows.

(defgeneric wcs-pixel-to-sky (wcs i j)
  (:documentation "Values RA DEC (degrees) of 0-based pixel (column I, row J)."))

(defgeneric wcs-sky-to-pixel (wcs ra dec)
  (:documentation "Values: the 0-based pixel (column, row) at RA, DEC
   (NaNs where the WCS cannot see it)."))

(defgeneric wcs-plane-transform (wcs projection)
  (:documentation "The affine from 0-based pixel centers of an image
   with WCS to the plane of PROJECTION, if WCS is exactly linear there;
   else NIL (the image must be reprojected).")
  (:method (wcs projection)
    (declare (ignore wcs projection))
    nil))

(defclass wcs ()
  ((projection :initarg :projection :reader wcs-projection)
   (crpix :initarg :crpix :reader wcs-crpix)
   (cd :initarg :cd :reader wcs-cd))
  (:documentation "A linear world coordinate system: FITS pixel
   (CRPIX, 1-based) is at the projection's reference point, and the CD
   matrix (CD1_1 CD1_2 CD2_1 CD2_2, degrees per pixel) maps pixel
   offsets to the projection plane."))

(defun make-wcs (&key crval crpix cd cdelt (crota 0) (projection :tan) frame)
  "A WCS from FITS-style values: CRVAL (RA DEC, or longitude and
   latitude in FRAME), CRPIX (X Y), and either CD (CD1_1 CD1_2 CD2_1
   CD2_2) or CDELT (D1 D2) with CROTA (degrees), in PROJECTION (a code
   or CTYPE for MAKE-PROJECTION, default :TAN) and FRAME (default from a
   CTYPE, else :EQUATORIAL).  CDELT1 is normally negative (east to the
   left)."
  (let ((cd (or cd
                (destructuring-bind (d1 d2) cdelt
                  (let ((c (cos (* +rad+ crota))) (s (sin (* +rad+ crota))))
                    (list (* d1 c) (- (* d2 s)) (* d1 s) (* d2 c)))))))
    (make-instance 'wcs :projection (make-projection projection (first crval) (second crval)
                                                     frame)
                        :crpix crpix :cd cd)))

(defun wcs-from-header (header)
  "A WCS from a plist of FITS keywords, e.g. (:CRVAL1 150 :CRVAL2 2.2
   :CRPIX1 80.5 :CRPIX2 60.5 :CD1_1 ... ) or with :CDELT1 :CDELT2
   :CROTA2, and :CTYPE1 naming the projection and frame (default
   \"RA---TAN\"; e.g. \"GLON-CAR\" for a galactic map)."
  (flet ((key (k &optional default) (getf header k default)))
    (make-wcs :projection (key :ctype1 "RA---TAN")
              :crval (list (key :crval1) (key :crval2))
              :crpix (list (key :crpix1) (key :crpix2))
              :cd (when (key :cd1_1)
                    (list (key :cd1_1) (key :cd1_2 0) (key :cd2_1 0) (key :cd2_2)))
              :cdelt (list (key :cdelt1) (key :cdelt2))
              :crota (key :crota2 0))))

(defun wcs-pixel-to-plane (wcs i j)
  "Plane coordinates of 0-based pixel (column I, row J)."
  (destructuring-bind (a b c d) (wcs-cd wcs)
    (destructuring-bind (px py) (wcs-crpix wcs)
      (let ((u (- (1+ i) px)) (v (- (1+ j) py)))
        (values (+ (* a u) (* b v)) (+ (* c u) (* d v)))))))

(defmethod wcs-pixel-to-sky ((wcs wcs) i j)
  (multiple-value-call #'plane-to-sky (wcs-projection wcs) (wcs-pixel-to-plane wcs i j)))

(defmethod wcs-sky-to-pixel ((wcs wcs) ra dec)
  (multiple-value-bind (xi eta) (sky-to-plane (wcs-projection wcs) ra dec)
    (destructuring-bind (a b c d) (wcs-cd wcs)
      (destructuring-bind (px py) (wcs-crpix wcs)
        (let ((det (- (* a d) (* b c))))
          (values (+ px -1 (/ (- (* d xi) (* b eta)) det))
                  (+ py -1 (/ (- (* a eta) (* c xi)) det))))))))

(defun same-projection-p (p q)
  (and (eq (class-of p) (class-of q))
       (eq (projection-frame p) (projection-frame q))
       (< (abs (- (projection-ra0 p) (projection-ra0 q))) 1d-9)
       (< (abs (- (projection-dec0 p) (projection-dec0 q))) 1d-9)))

(defmethod wcs-plane-transform ((wcs wcs) projection)
  (when (and projection (same-projection-p projection (wcs-projection wcs)))
    (destructuring-bind (a b c d) (wcs-cd wcs)
      (destructuring-bind (px py) (wcs-crpix wcs)
        ;; xi = a (i + 1 - px) + b (j + 1 - py), eta likewise
        (make-affine a b c d (+ (* a (- 1 px)) (* b (- 1 py)))
                     (+ (* c (- 1 px)) (* d (- 1 py))))))))

(defun viewport-sky-projection (viewport)
  (or (viewport-projection viewport)
      (error "~A has no sky projection (see SET-WINDOW-TAN)." viewport)))

(defun wcs-image-transform (wcs viewport)
  "How pixel centers (column, row, 0-based) of an image with WCS map to
   VIEWPORT's :PLOT coordinates, for PLOT-CONTOURS's and
   PLOT-FILLED-CONTOURS's :TRANSFORM (PLOT-IMAGE takes :WCS instead):
   an affine when the WCS is linear in VIEWPORT's projection, else a
   function of the pixel returning the plot point."
  (let ((projection (viewport-sky-projection viewport)))
    (or (wcs-plane-transform wcs projection)
        (lambda (i j)
          (multiple-value-call #'sky-to-plane projection (wcs-pixel-to-sky wcs i j))))))

(defun wcs-pixel-scale (wcs projection i j)
  "The side, in PROJECTION's plane, of a square with the area of pixel
   I, J of an image with WCS."
  (flet ((plane (i j) (multiple-value-call #'sky-to-plane projection
                        (wcs-pixel-to-sky wcs i j))))
    (multiple-value-bind (x0 y0) (plane i j)
      (multiple-value-bind (x1 y1) (plane (1+ i) j)
        (multiple-value-bind (x2 y2) (plane i (1+ j))
          (sqrt (abs (- (* (- x1 x0) (- y2 y0)) (* (- x2 x0) (- y1 y0))))))))))

(defun reproject-image (array wcs viewport &key (interpolation :nearest) (max-size 2000)
                                                 resolution)
  "Resample the 2D ARRAY, with WCS, onto a grid in VIEWPORT's :PLOT
   coordinates covering the part of its window the image overlaps.  The
   grid step is RESOLUTION plot units (default the image's pixel size at
   its center, or 1/600 of the window if that is smaller), at most
   MAX-SIZE samples a side.  INTERPOLATION
   :NEAREST or :BILINEAR; outside the image, NaN.  Returns the new array
   and the X-RANGE and Y-RANGE (outer edges) to plot it with."
  (destructuring-bind (h w) (array-dimensions array)
    (let* ((projection (viewport-sky-projection viewport))
           (xs '()) (ys '()) (partial nil))
      ;; the image's extent in the plane, from points along its edges and
      ;; across it (an all-sky image's edges may all lie on a seam)
      (flet ((edge-point (i j)
               (multiple-value-bind (x y)
                   (multiple-value-call #'sky-to-plane projection (wcs-pixel-to-sky wcs i j))
                 (if (or (nan-p x) (nan-p y))
                     (setf partial t)
                     (progn (push x xs) (push y ys))))))
        (let ((n 64))
          (dotimes (k (1+ n))
            (let ((u (- (* k (/ w n)) 0.5)) (v (- (* k (/ h n)) 0.5)))
              (edge-point u -0.5) (edge-point u (- h 0.5))
              (edge-point -0.5 v) (edge-point (- w 0.5) v)
              (dotimes (q (1+ n))
                (edge-point u (- (* q (/ h n)) 0.5)))))))
      (multiple-value-bind (wx1 wx2 wy1 wy2) (viewport-window viewport)
        (let* ((wxa (min wx1 wx2)) (wxb (max wx1 wx2))
               (wya (min wy1 wy2)) (wyb (max wy1 wy2))
               ;; the overlap of the outline's bounds and the window (all
               ;; the window if part of the image is beyond the projection)
               ;; padded, as the samples may fall short of the extremes
               (pad (* 0.02 (max (- wxb wxa) (- wyb wya))))
               (xa (if (or partial (null xs)) wxa (max wxa (- (reduce #'min xs) pad))))
               (xb (if (or partial (null xs)) wxb (min wxb (+ (reduce #'max xs) pad))))
               (ya (if (or partial (null ys)) wya (max wya (- (reduce #'min ys) pad))))
               (yb (if (or partial (null ys)) wyb (min wyb (+ (reduce #'max ys) pad)))))
          (when (or (>= xa xb) (>= ya yb))
            (return-from reproject-image nil))
          (let* ((fine (/ (max (- wxb wxa) (- wyb wya)) 600))
                 (pixel (wcs-pixel-scale wcs projection (floor w 2) (floor h 2)))
                 (step (max (or resolution (if (nan-p pixel) fine (min pixel fine)))
                            (/ (- xb xa) max-size) (/ (- yb ya) max-size)))
                 (nx (max 1 (ceiling (- xb xa) step)))
                 (ny (max 1 (ceiling (- yb ya) step)))
                 (dx (/ (- xb xa) nx)) (dy (/ (- yb ya) ny))
                 (out (make-array (list ny nx) :element-type 'double-float
                                               :initial-element float-utils:+double-float-nan+)))
            (flet ((at (i j) (float (aref array j i) 1d0)))
              (dotimes (q ny)
                (dotimes (p nx)
                  (multiple-value-bind (i j)
                      (multiple-value-bind (ra dec)
                          (plane-to-sky projection (+ xa (* dx (+ p 0.5))) (+ ya (* dy (+ q 0.5))))
                        ;; beyond the projection's boundary: no sky there
                        (if (nan-p ra) (nan2) (wcs-sky-to-pixel wcs ra dec)))
                    (unless (or (nan-p i) (nan-p j))
                      (setf (aref out q p)
                            (ecase interpolation
                              (:nearest
                               (let ((i (round i)) (j (round j)))
                                 (if (and (< -1 i w) (< -1 j h))
                                     (at i j)
                                     float-utils:+double-float-nan+)))
                              (:bilinear
                               ;; within half a pixel of the edge, clamp
                               (if (and (<= -0.5 i (- w 0.5)) (<= -0.5 j (- h 0.5)))
                                   (let* ((i (max 0 (min (- w 1) i))) (j (max 0 (min (- h 1) j)))
                                          (i0 (min (floor i) (max 0 (- w 2))))
                                          (j0 (min (floor j) (max 0 (- h 2))))
                                          (i1 (min (1+ i0) (1- w))) (j1 (min (1+ j0) (1- h)))
                                          (fi (- i i0)) (fj (- j j0)))
                                     (+ (* (- 1 fi) (- 1 fj) (at i0 j0)) (* fi (- 1 fj) (at i1 j0))
                                        (* (- 1 fi) fj (at i0 j1)) (* fi fj (at i1 j1))))
                                   float-utils:+double-float-nan+))))))))
            (values out (list xa xb) (list ya yb)))))))))

;;; ============================================================
;;; Coordinate grids
;;; ============================================================

(defun sky-extent (viewport &optional (frame (projection-frame (viewport-projection viewport))))
  "Values LON-MIN LON-MAX LAT-MIN LAT-MAX of the sky in VIEWPORT's
   window, as longitude and latitude in FRAME (longitude unwrapped about
   the projection's reference point; the whole circle if a pole is
   inside, and, if both are, the whole sky, in the projection's own
   frame just short of the seam opposite the reference point; then the
   fifth value is true)."
  (multiple-value-bind (x1 x2 y1 y2) (viewport-window viewport)
    (let* ((projection (viewport-projection viewport))
           ;; in the projection's own frame (and not tilted), the seam
           ;; is the meridian opposite the reference point
           (own (and (eq frame (projection-frame projection))
                     (not (and (typep projection 'allsky-projection)
                               (tilted-p projection)))))
           (lon0 (if own
                     (projection-ra0 projection)
                     (multiple-value-call #'equatorial-to-frame frame
                       (plane-to-sky projection 0 0))))
           (lons '()) (lats '()) (n 24)
           (ex (* 1d-9 (abs (- x2 x1)))) (ey (* 1d-9 (abs (- y2 y1)))))
      (dotimes (a (1+ n))
        (dotimes (b (1+ n))
          (multiple-value-bind (ra dec)
              (plane-to-sky projection (+ x1 (* a (/ (- x2 x1) n))) (+ y1 (* b (/ (- y2 y1) n))))
            (unless (nan-p ra)
              (multiple-value-bind (lon lat) (equatorial-to-frame frame ra dec)
                (push (+ lon0 (- (mod (+ (- lon lon0) 180) 360) 180)) lons)
                (push lat lats))))))
      (flet ((inside-p (lat)
               ;; is the pole at LAT inside the window?
               (multiple-value-bind (x y)
                   (multiple-value-call #'sky-to-plane projection
                     (frame-to-equatorial frame lon0 lat))
                 (and (not (nan-p x))
                      (<= (- (min x1 x2) ex) x (+ (max x1 x2) ex))
                      (<= (- (min y1 y2) ey) y (+ (max y1 y2) ey))))))
        (let ((north (inside-p 90)) (south (inside-p -90)))
          (cond ((and north south)
                 (if own
                     (values (+ lon0 -180 1d-7) (- (+ lon0 180) 1d-7) -90 90 t)
                     (values 0 360 -90 90)))
                (north (values 0 360 (reduce #'min lats) 90))
                (south (values 0 360 -90 (reduce #'max lats)))
                (t (values (reduce #'min lons) (reduce #'max lons)
                           (reduce #'min lats) (reduce #'max lats)))))))))

(defun boundary-in-window-p (viewport)
  "Is the whole boundary of VIEWPORT's projection inside its window?"
  (multiple-value-bind (xs ys) (projection-boundary (viewport-projection viewport))
    (when xs
      (multiple-value-bind (x1 x2 y1 y2) (viewport-window viewport)
        (let ((ex (* 1d-6 (abs (- x2 x1)))) (ey (* 1d-6 (abs (- y2 y1)))))
          (and (every (lambda (x) (<= (- (min x1 x2) ex) x (+ (max x1 x2) ex))) xs)
               (every (lambda (y) (<= (- (min y1 y2) ey) y (+ (max y1 y2) ey))) ys)))))))

(defplot draw-projection-boundary (viewport)
  "Draw the edge of VIEWPORT's projected sky, if it has one, in the
   current style (clipped to the viewport's rectangle)."
  (multiple-value-bind (xs ys) (projection-boundary (viewport-sky-projection viewport))
    (when xs
      (with-viewport (viewport :clip nil)
        (let ((device (viewport-device viewport)))
          (apply-clip device (viewport-region viewport))
          (plot-polygon device xs ys))))))

(defun trace-sky-runs (projection frame fn n jump)
  "The sky curve (FUNCALL FN F) => longitude, latitude in FRAME, F from
   0 to 1 in N steps, in PROJECTION's plane: a list of runs (XS YS
   LATS), split where the
   curve leaves the projection (NaN) or jumps more than JUMP (across an
   all-sky projection's seam)."
  (let ((runs '()) (xs '()) (ys '()) (decs '()))
    (flet ((flush ()
             (when (cdr xs) (push (list (nreverse xs) (nreverse ys) (nreverse decs)) runs))
             (setf xs '() ys '() decs '())))
      (dotimes (k (1+ n))
        (multiple-value-bind (lon dec) (funcall fn (/ k n))
          (multiple-value-bind (x y)
              (multiple-value-call #'sky-to-plane projection (frame-to-equatorial frame lon dec))
            (cond ((or (nan-p x) (nan-p y)) (flush))
                  (t (when (and xs (> (sqrt (+ (expt (- x (first xs)) 2) (expt (- y (first ys)) 2)))
                                      jump))
                       (flush))
                     (push x xs) (push y ys) (push dec decs))))))
      (flush))
    (nreverse runs)))

(defun on-boundary-p (projection runs)
  "Do the traced RUNS lie along a zenithal PROJECTION's boundary?"
  (let ((limit (and (typep projection 'zenithal-projection) (zenithal-limit projection))))
    (and limit runs
         (loop for (xs ys) in runs
               always (every (lambda (x y) (> (sqrt (+ (* x x) (* y y))) (* 0.999 (/ limit +rad+))))
                             xs ys)))))

(defun edge-crossings (xs ys edge-value along-x lo hi)
  "Where the plot polyline XS, YS crosses the line x = EDGE-VALUE (ALONG-X
   NIL) or y = EDGE-VALUE (ALONG-X T) between LO and HI: a list of the
   other coordinate at each crossing."
  (loop for (xa xb) on xs for (ya yb) on ys
        while xb
        for va = (if along-x ya xa) for vb = (if along-x yb xb)
        when (and (not (nan-p va)) (not (nan-p vb))
                  (or (and (<= va edge-value) (> vb edge-value))
                      (and (> va edge-value) (<= vb edge-value))))
          append (let* ((f (/ (- edge-value va) (- vb va)))
                        (other (if along-x (+ xa (* f (- xb xa))) (+ ya (* f (- yb ya))))))
                   (when (<= (min lo hi) other (max lo hi)) (list other)))))

(defun frame-axis-names (frame)
  (ecase frame
    (:equatorial (values "right ascension" "declination"))
    (:galactic (values "galactic longitude" "galactic latitude"))
    (:ecliptic (values "ecliptic longitude" "ecliptic latitude"))))

(defplot draw-sky-grid (viewport &key (grid t) ra-target dec-target size font
                                    (grid-color :gray) (grid-width 0.5) (frame t)
                                    coordinates (labels t)
                                    (lon-sides '(:bottom :top :right :left))
                                    (lat-sides '(:left :right :bottom :top))
                                    (x-label :auto) (y-label :auto)
                                    (tick-length nil))
  "Draw lines of constant longitude and latitude in COORDINATES (a
   frame, :EQUATORIAL, :GALACTIC or :ECLIPTIC; default the projection's
   own) through VIEWPORT's projection (if GRID), the frame (if FRAME),
   and sexagesimal labels (if LABELS): right ascension in hours, other
   longitudes in degrees.  A second call with other COORDINATES
   overlays a second grid, e.g. galactic over equatorial, with its
   labels on other sides (LON-SIDES '(:TOP), LAT-SIDES '(:RIGHT)) and
   FRAME NIL.
   Usually each longitude line (about RA-TARGET of them, default 4) is
   labeled at the first edge in LON-SIDES it crosses, each latitude
   line (about DEC-TARGET, default 4) at the first in LAT-SIDES, with
   ticks TICK-LENGTH canvas units long (default 0.6 of SIZE), and the
   axes titled X-LABEL and Y-LABEL (default from COORDINATES; NIL for
   none) beside the first of LON-SIDES and LAT-SIDES.
   When the projection's whole boundary is in the window (as after
   SET-WINDOW-PROJECTION without a size), the frame is that boundary
   instead, longitude lines (default about 8) are labeled beyond their
   ends at the boundary, or below the equator if they run pole to pole,
   and latitude lines (default about 6) left of their left ends, or on
   their lowest point if they close around a pole.  SIZE is the text size
   (default DEFAULT-TEXT-SIZE).  Lines use GRID-COLOR, GRID-WIDTH."
  (let* ((device (viewport-device viewport))
         (projection (viewport-sky-projection viewport))
         (coordinates (or coordinates (projection-frame projection)))
         (size (or size (default-text-size device)))
         (tick-length (or tick-length (* 0.6 size)))
         (gap (* 0.5 size))
         (full (boundary-in-window-p viewport)))
    (multiple-value-bind (ramin ramax decmin decmax seam) (sky-extent viewport coordinates)
      (if full
          (setf decmin -90 decmax 90)
          ;; trace lines a little beyond the window, so they surely cross
          ;; its edges (the extent is sampled, and rounded by rotations)
          (let ((dlat (* 0.02 (- decmax decmin))) (dlon (* 0.02 (- ramax ramin))))
            (setf decmin (max -90 (- decmin dlat)) decmax (min 90 (+ decmax dlat)))
            (when (< (- ramax ramin) 359)
              (setf ramin (- ramin dlon) ramax (+ ramax dlon)))))
      (multiple-value-bind (x1 x2 y1 y2) (viewport-window viewport)
        (let ((jump (* 0.2 (max (abs (- x2 x1)) (abs (- y2 y1)))))
              (ra-ticks (funcall (sexagesimal-ticks :unit (if (eq coordinates :equatorial)
                                                              :hours
                                                              :longitude)
                                                    :target (or ra-target (if full 8 4)))
                                 ramin ramax))
              (dec-ticks (remove-if (lambda (m) (>= (abs (first m)) 90))
                                    (funcall (sexagesimal-ticks :sign t
                                                                :target (or dec-target
                                                                            (if full 6 4)))
                                             decmin decmax)))
              (lines '()))            ; (kind label value runs)
          (when (> (- ramax ramin) 359)
            ;; around a pole: one line per RA, not both 0h and 24h; and
            ;; none along an all-sky projection's seam
            (setf ra-ticks (remove-if (lambda (m)
                                        (or (>= (- (first m) ramin) 359.999)
                                            (and seam (or (< (abs (- (first m) ramin)) 1d-4)
                                                          (< (abs (- (first m) ramax)) 1d-4)))))
                                      ra-ticks)))
          (dolist (m ra-ticks)
            (push (list :ra (second m) (first m)
                        (trace-sky-runs projection coordinates
                                        (lambda (f) (values (first m) (+ decmin (* f (- decmax decmin)))))
                                        180 jump))
                  lines))
          (dolist (m dec-ticks)
            (let ((runs (trace-sky-runs projection coordinates
                                        (lambda (f) (values (+ ramin (* f (- ramax ramin))) (first m)))
                                        360 jump)))
              ;; not one lying along the boundary (as the equator of SIN
              ;; about a pole)
              (unless (on-boundary-p projection runs)
                (push (list :dec (second m) (first m) runs) lines))))
          ;; RA lines first, so their labels win where labels would overlap
          (setf lines (nreverse lines))
          (with-viewport (viewport)
            (when grid
              (with-style (device :color grid-color :line-width grid-width)
                (without-footprint
                  (loop for (nil nil nil runs) in lines
                        do (loop for (xs ys) in runs do (plot-polyline device xs ys)))))))
          (when frame
            (unless full (draw-frame viewport))
            (draw-projection-boundary viewport))
          (when labels
            (with-viewport (viewport :clip nil)
              (with-font (device (or font (current-font device)) :size size)
                (if full
                    (label-full-sky viewport lines gap coordinates)
                    (label-sky-frame viewport lines gap tick-length size x1 x2 y1 y2
                                     lon-sides lat-sides))
                ;; axis titles beyond the numbers
                (unless full
                  (multiple-value-bind (lon-name lat-name) (frame-axis-names coordinates)
                    (let ((x-label (if (eq x-label :auto) lon-name x-label))
                          (y-label (if (eq y-label :auto) lat-name y-label)))
                      (multiple-value-bind (left right bottom top)
                          (viewport-rectangle viewport :canvas)
                        (multiple-value-bind (w ascent descent) (text-size device "0")
                          (declare (ignore w))
                          (when x-label
                            (if (eq (first lon-sides) :top)
                                (put-text device (/ (+ left right) 2)
                                          (- top gap ascent descent gap)
                                          x-label :halign :center :valign :bottom)
                                (put-text device (/ (+ left right) 2)
                                          (+ bottom gap ascent descent gap)
                                          x-label :halign :center :valign :top)))
                          (when y-label
                            (if (eq (first lat-sides) :right)
                                (put-text device (+ right gap (* 4.5 size)) (/ (+ top bottom) 2)
                                          y-label :angle (- (/ pi 2)) :halign :center
                                                  :valign :top)
                                (put-text device (- left gap (* 4.5 size)) (/ (+ top bottom) 2)
                                          y-label :angle (- (/ pi 2)) :halign :center
                                                  :valign :bottom))))))))))))))
    viewport))

(defun label-sky-frame (viewport lines gap tick-length size x1 x2 y1 y2 lon-sides lat-sides)
  "Label grid LINES where they cross the frame of window X1..X2, Y1..Y2,
   with ticks (see DRAW-SKY-GRID)."
  (let ((device (viewport-device viewport)))
    ;; each line is labeled where it crosses the first edge, in order of
    ;; preference, that it crosses at all (near a pole most RA lines
    ;; leave through the sides or top)
    (loop with placed = '()     ; (side position width) of labels drawn
          for (kind text nil runs) in lines
          do (loop for side in (if (eq kind :ra) lon-sides lat-sides)
                   for crossings = (loop for (xs ys) in runs
                                         append (ecase side
                                                  ((:bottom :top)
                                                   (edge-crossings xs ys (if (eq side :bottom) y1 y2)
                                                                   t x1 x2))
                                                  ((:left :right)
                                                   (edge-crossings xs ys (if (eq side :left) x1 x2)
                                                                   nil y1 y2))))
                   when crossings
                     do (dolist (other crossings)
                          (multiple-value-bind (cx cy)
                              (ecase side
                                (:bottom (plot-to-canvas viewport other y1))
                                (:top (plot-to-canvas viewport other y2))
                                (:left (plot-to-canvas viewport x1 other))
                                (:right (plot-to-canvas viewport x2 other)))
                            ;; a tick inward, then the label outside, unless
                            ;; it would overlap a label already on that edge
                            (multiple-value-bind (dx dy)
                                (ecase side (:bottom (values 0 -1)) (:top (values 0 1))
                                  (:left (values 1 0)) (:right (values -1 0)))
                              (draw-line device cx cy (+ cx (* dx tick-length))
                                         (+ cy (* dy tick-length)))
                              (let* ((along (if (member side '(:bottom :top)) cx cy))
                                     ;; extent along the edge: width, or a line's height
                                     (extent (if (member side '(:bottom :top))
                                                 (text-size device text)
                                                 (* 1.2 size))))
                                (unless (find-if (lambda (p)
                                                   (and (eq (first p) side)
                                                        (< (abs (- (second p) along))
                                                           (+ (/ (+ (third p) extent) 2)
                                                              (* 0.5 size)))))
                                                 placed)
                                  (push (list side along extent) placed)
                                  (put-text device (- cx (* dx gap)) (- cy (* dy gap)) text
                                            :halign (case side (:left :right) (:right :left)
                                                      (t :center))
                                            :valign (case side (:bottom :top) (:top :bottom)
                                                      (t :center))))))))
                        (return)))))

(defun label-full-sky (viewport lines gap frame)
  "Label grid LINES around and inside the boundary of VIEWPORT's
   projection (see DRAW-SKY-GRID)."
  (let* ((device (viewport-device viewport))
         (projection (viewport-projection viewport))
         (placed '()))                  ; (left right top bottom) of labels drawn
    (flet ((put-text (device x y text &key (halign :left) (valign :bottom) halo)
             ;; skip a label that would overlap one already drawn
             (multiple-value-bind (w ascent descent) (text-size device text)
               (let* ((h (+ ascent descent))
                      (left (ecase halign (:left x) (:center (- x (/ w 2))) (:right (- x w))))
                      (top (ecase valign (:top y) (:center (- y (/ h 2))) (:bottom (- y h))))
                      (box (list left (+ left w) top (+ top h))))
                 (unless (find-if (lambda (b)
                                    (and (< (first b) (second box)) (< (first box) (second b))
                                         (< (third b) (fourth box)) (< (third box) (fourth b))))
                                  placed)
                   (push box placed)
                   (put-text device x y text :halign halign :valign valign :halo halo))))))
    (multiple-value-bind (ox oy) (plot-to-canvas viewport 0 0)
      (loop for (kind text value runs) in lines
            do (ecase kind
                 (:ra
                  (let ((end (loop for (xs ys decs) in runs
                                   ;; an end of the line that is not at a pole
                                   thereis (loop for k in (list 0 (1- (length xs)))
                                                 when (< (abs (nth k decs)) 89.9)
                                                   return (list (nth k xs) (nth k ys))))))
                    (if end
                        ;; beyond the end, outward from the center
                        (multiple-value-bind (cx cy) (apply #'plot-to-canvas viewport end)
                          (let* ((dx (- cx ox)) (dy (- cy oy))
                                 (l (max 1d-9 (sqrt (+ (* dx dx) (* dy dy)))))
                                 (ux (/ dx l)) (uy (/ dy l)))
                            (put-text device (+ cx (* ux gap)) (+ cy (* uy gap)) text
                                      :halign (cond ((> ux 0.5) :left) ((< ux -0.5) :right)
                                                    (t :center))
                                      :valign (cond ((> uy 0.5) :top) ((< uy -0.5) :bottom)
                                                    (t :center)))))
                        ;; pole to pole: below the equator
                        (multiple-value-bind (x y)
                            (multiple-value-call #'sky-to-plane projection
                              (frame-to-equatorial frame value 0))
                          (unless (nan-p x)
                            (multiple-value-bind (cx cy) (plot-to-canvas viewport x y)
                              (put-text device cx (+ cy (* 0.5 gap)) text
                                        :halign :center :valign :top :halo t)))))))
                 (:dec
                  (let* ((points (loop for (xs ys) in runs
                                       nconc (mapcar (lambda (x y)
                                                       (multiple-value-list
                                                        (plot-to-canvas viewport x y)))
                                                     xs ys)))
                         (closed (and (= (length runs) 1)
                                      (let ((p (first points)) (q (car (last points))))
                                        (< (+ (abs (- (first p) (first q)))
                                              (abs (- (second p) (second q))))
                                           1d-3)))))
                    (when points
                      (if closed
                          ;; around a pole: on its lowest point
                          (destructuring-bind (cx cy)
                              (reduce (lambda (p q) (if (> (second q) (second p)) q p)) points)
                            (put-text device cx cy text :halign :center :valign :center :halo t))
                          ;; left of its leftmost end
                          (destructuring-bind (cx cy)
                              (reduce (lambda (p q) (if (< (first q) (first p)) q p))
                                      (loop for (xs ys) in runs
                                            collect (multiple-value-list
                                                     (plot-to-canvas viewport (first xs) (first ys)))
                                            collect (multiple-value-list
                                                     (plot-to-canvas viewport (car (last xs))
                                                                     (car (last ys))))))
                            (put-text device (- cx gap) cy text
                                      :halign :right :valign :center))))))))))))
