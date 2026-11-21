;;;; polar.lisp — polar plots
;;;;
;;;; SET-WINDOW-POLAR gives a viewport a polar mapping; after it, every
;;;; plotting function takes :COORDS :POLAR with points as (ANGLE
;;;; RADIUS), the angle in degrees.  Lines and polygons in :POLAR follow
;;;; the angle (a segment between two radii at different angles is a
;;;; spiral arc), so wedges and circles come out round.  The mapping:
;;;;
;;;;   ZERO       where angle 0 points on the page: :EAST (right, the
;;;;              mathematical convention), :NORTH (up, for compass
;;;;              bearings and position angles), :WEST, :SOUTH, or
;;;;              degrees counterclockwise from east
;;;;   DIRECTION  :COUNTERCLOCKWISE or :CLOCKWISE (compass bearings)
;;;;   R-MIN, R-MAX  the radii at the center and at the rim (R-MIN need
;;;;              not be 0, e.g. for decibels)
;;;;   SECTOR     (FROM TO), the angles shown, default the whole circle
;;;;
;;;; :PLOT coordinates are then the unit disk: the rim at distance 1 from
;;;; the center.  DRAW-POLAR-GRID draws the rim, circles of constant
;;;; radius labeled along one spoke, and spokes labeled around the rim.
;;;; PLOT-POLAR-BARS draws wedges, e.g. a wind rose.

(in-package :cl-cairoplot)

(defstruct polar r-min r-max zero sign sector)

(defun viewport-polar-mapping (viewport)
  (or (viewport-polar viewport)
      (error "~A has no polar mapping (see SET-WINDOW-POLAR)." viewport)))

(defun polar-direction (polar angle)
  "The direction on the page (degrees counterclockwise from east) of
   ANGLE."
  (+ (polar-zero polar) (* (polar-sign polar) angle)))

(defplot polar-to-plot (viewport angle radius)
  "Values: the :PLOT coordinates of ANGLE (degrees), RADIUS through
   VIEWPORT's polar mapping."
  (let* ((p (viewport-polar-mapping viewport))
         (rho (/ (- radius (polar-r-min p)) (- (polar-r-max p) (polar-r-min p))))
         (phi (* (/ pi 180) (polar-direction p angle))))
    (values (* rho (cos phi)) (* rho (sin phi)))))

(defplot plot-to-polar (viewport x y)
  "Values ANGLE (degrees, 0..360) and RADIUS of the :PLOT point X, Y."
  (let* ((p (viewport-polar-mapping viewport))
         (phi (* (/ 180 pi) (atan y x))))
    (values (mod (* (polar-sign p) (- phi (polar-zero p))) 360)
            (+ (polar-r-min p) (* (sqrt (+ (* x x) (* y y)))
                                  (- (polar-r-max p) (polar-r-min p)))))))

(defun densify-polar (angles radii &optional (step 2))
  "ANGLES and RADII as lists with points added between neighbors, at
   most STEP degrees apart, so lines between them follow the angle."
  (let ((as (coerce angles 'list)) (rs (coerce radii 'list))
        (out-a '()) (out-r '()))
    (loop for (a0 a1) on as
          for (r0 r1) on rs
          do (if (null a1)
                 (progn (push a0 out-a) (push r0 out-r))
                 (let ((n (max 1 (ceiling (abs (- a1 a0)) step))))
                   (dotimes (k n)
                     (push (+ a0 (* k (/ (- a1 a0) n))) out-a)
                     (push (+ r0 (* k (/ (- r1 r0) n))) out-r)))))
    (values (nreverse out-a) (nreverse out-r))))

(defun polar-sector-angles (polar &optional (step 1))
  "Angles from the sector's start to its end (or 0..360), STEP apart."
  (destructuring-bind (a b) (or (polar-sector polar) '(0 360))
    (let ((n (max 2 (ceiling (abs (- b a)) step))))
      (loop for k to n collect (+ a (* k (/ (- b a) n)))))))

(defun polar-outline (polar)
  "The shown region as lists XS YS of :PLOT coordinates: the disk, or
   the sector's wedge."
  (let ((xs '()) (ys '()))
    (dolist (a (polar-sector-angles polar))
      (let ((phi (* (/ pi 180) (polar-direction polar a))))
        (push (cos phi) xs) (push (sin phi) ys)))
    (when (polar-sector polar)
      (push 0 xs) (push 0 ys))
    (values (nreverse xs) (nreverse ys))))

(defplot set-window-polar (viewport r-max &key (r-min 0) (zero :east)
                                             (direction :counterclockwise) sector (margin 0.16))
  "Give VIEWPORT a polar mapping (see the top of polar.lisp) from R-MIN
   at the center to R-MAX at the rim, and a window with equal scales
   around the disk or SECTOR, with MARGIN (a fraction of the radius) for
   labels.  Drawing is clipped to the disk or sector.  Returns VIEWPORT."
  (let ((polar (make-polar :r-min r-min :r-max r-max
                           :zero (case zero (:east 0) (:north 90) (:west 180) (:south 270)
                                   (t zero))
                           :sign (ecase direction (:counterclockwise 1) (:clockwise -1))
                           :sector sector)))
    (setf (viewport-polar viewport) polar)
    (multiple-value-bind (xs ys) (polar-outline polar)
      (let ((xs (if sector (cons 0 xs) xs)) (ys (if sector (cons 0 ys) ys)))
        (set-window-equal-scales viewport
                                 (- (reduce #'min xs) margin) (+ (reduce #'max xs) margin)
                                 (- (reduce #'min ys) margin) (+ (reduce #'max ys) margin)))
      (setf (viewport-clipping-region viewport)
            (region-intersection (viewport-region viewport) (polygon-region xs ys))))
    viewport))

(defun text-anchor-for (phi)
  "HALIGN and VALIGN for text placed outward in the direction PHI
   (radians) from a point."
  (values (cond ((> (cos phi) 0.3) :left) ((< (cos phi) -0.3) :right) (t :center))
          (cond ((> (sin phi) 0.3) :bottom) ((< (sin phi) -0.3) :top) (t :center))))

(defplot draw-polar-grid (viewport &key (angle-step 30) radii (r-target 4) angle-labels
                                        r-format r-label-angle
                                        (grid-color '(0.55 0.55 0.55 0.55)) (frame t)
                                        (labels t) size title)
  "Draw VIEWPORT's polar grid: circles at RADII (default round values,
   about R-TARGET) and spokes every ANGLE-STEP degrees in GRID-COLOR,
   the rim (FRAME) in the current style, and with LABELS the angles
   around the rim (ANGLE-LABELS, a list of texts for the spokes in
   order, or default \"30°\") and the radii (as ticks are labeled, or by
   R-FORMAT, a function of the value) along the spoke at R-LABEL-ANGLE
   (default halfway to the first spoke).  SIZE is the text size
   (default 0.7 of DEFAULT-TEXT-SIZE); TITLE goes above."
  (let* ((device (viewport-device viewport))
         (p (viewport-polar-mapping viewport))
         (size (or size (* 0.7 (default-text-size device))))
         (r-min (polar-r-min p)) (r-max (polar-r-max p))
         (eps (* 1d-9 (abs (- r-max r-min))))
         ;; (radius text) of the labeled circles, the rim included if round
         (ticks (if radii
                    (mapcar (lambda (r) (list r (default-value-text r))) radii)
                    (loop for (r text) in (axis-ticks (min r-min r-max) (max r-min r-max)
                                                      :target r-target)
                          when (and (> (abs (- r r-min)) eps)
                                    (<= (min r-min r-max) r (max r-min r-max)))
                            collect (list r text))))
         (radii (loop for (r) in ticks when (> (abs (- r r-max)) eps) collect r))
         (sector (polar-sector p))
         (spokes (destructuring-bind (a b) (or sector '(0 360))
                   (loop for k from (ceiling (min a b) angle-step)
                           to (floor (max a b) angle-step)
                         for angle = (* k angle-step)
                         unless (and (null sector) (>= angle 360))
                           collect angle)))
         (rim-angles (polar-sector-angles p)))
    (with-style (viewport :color grid-color :line-width 0.5)
      (without-footprint
        (dolist (r radii)
          (plot-polyline viewport rim-angles (mapcar (constantly r) rim-angles) :coords :polar))
        (dolist (a spokes)
          (plot-line viewport a r-min a r-max :coords :polar))))
    (when frame
      (with-viewport (viewport :clip nil)
        (multiple-value-bind (xs ys) (polar-outline p)
          (plot-polygon device xs ys))))
    (when labels
      (with-viewport (viewport :clip nil)
       ;; noted, so legends placed :BEST avoid them
       (let ((*record-footprint* t))
        (loop for a in spokes
              for k from 0
              do (let ((phi (* (/ pi 180) (polar-direction p a))))
                   (multiple-value-bind (halign valign) (text-anchor-for phi)
                     (plot-text device (* 1.04 (cos phi)) (* 1.04 (sin phi))
                                (cond ((consp angle-labels) (nth k angle-labels))
                                      ;; a sector's angles as given, so signed
                                      (sector (format nil "~D°" (round a)))
                                      (t (format nil "~D°" (round (mod a 360)))))
                                :halign halign :valign valign :size size))))
        (let ((label-angle (or r-label-angle
                               (let ((a (or (second spokes) angle-step)))
                                 (/ (+ (first spokes) a) 2)))))
          (loop for (r text) in ticks
                do (multiple-value-bind (x y) (polar-to-plot viewport label-angle r)
                     (plot-text device x y (if r-format (funcall r-format r) text)
                                :halign :center :valign :center :size (* 0.85 size)
                                :halo t)))))))
    (when title
      (multiple-value-bind (x1 x2 y1 y2) (viewport-window viewport)
        (declare (ignore y1))
        (with-viewport (viewport :clip nil)
          (plot-text device (/ (+ x1 x2) 2) y2 title :halign :center :valign :bottom
                                                     :size (* 1.15 size)))))
    viewport))

(defplot plot-polar-bars (device angles values &key width (bottoms 0) color colors
                                               (gap-color :white) (gap-width 1))
  "Draw a wedge for each of VALUES centered on ANGLES (degrees), WIDTH
   degrees wide (default 0.9 of the angles' spacing), from BOTTOMS (a
   radius or a sequence) out to BOTTOM + VALUE, so successive calls with
   BOTTOMS stack them.  Each is filled with COLOR, its element of
   COLORS, or the current color, and outlined in GAP-COLOR."
  (let* ((angles (coerce angles 'list)) (values (coerce values 'list))
         (width (or width
                    (* 0.9 (if (rest angles)
                               (reduce #'min (mapcar (lambda (a b) (abs (- b a)))
                                                     angles (rest angles)))
                               360)))))
    (loop for a in angles for v in values for k from 0
          do (let* ((r0 (nth-spec bottoms k)) (r1 (+ r0 v))
                    (a0 (- a (/ width 2))) (a1 (+ a (/ width 2)))
                    (xs (list a0 a1 a1 a0)) (ys (list r0 r0 r1 r1))
                    (fill (or (nth-spec colors k) color)))
               (if fill
                   (with-color (device fill) (plot-polygon device xs ys :coords :polar :fill t))
                   (plot-polygon device xs ys :coords :polar :fill t))
               (when (and gap-color (plusp gap-width))
                 (with-style (device :color gap-color :line-width gap-width)
                   (plot-polygon device xs ys :coords :polar)))))))
