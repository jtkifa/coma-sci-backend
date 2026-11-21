;;;; drawing.lisp — drawing on a plot device in any coordinates
;;;;
;;;; Each function takes &KEY COORDS (default :PLOT, i.e. the current
;;;; viewport's window; :SKY for right ascension and declination through
;;;; the viewport's projection; :POLAR for angle and radius, see
;;;; polar.lisp) and draws with the current canvas style, so
;;;; WITH-COLOR, WITH-LINE-WIDTH, WITH-FONT etc. apply.  Line widths and
;;;; text sizes stay in canvas units whatever the coordinates.

(in-package :cl-cairoplot)

(defun curved-to-plot-sequences (device xs ys coords &optional densify)
  "Sequences XS, YS in COORDS :SKY (right ascension and declination
   through the current viewport's projection) or :POLAR (angle and
   radius, see polar.lisp) as :PLOT coordinates.  With DENSIFY, :POLAR
   lines are first divided so they curve with the angle."
  (let ((vp (current-viewport device)) (pxs '()) (pys '())
        (to-plot (ecase coords (:sky #'sky-to-plot) (:polar #'polar-to-plot))))
    (when (and densify (eq coords :polar))
      (setf (values xs ys) (densify-polar xs ys)))
    (map nil (lambda (a b)
               (multiple-value-bind (x y) (funcall to-plot vp a b)
                 (push x pxs) (push y pys)))
         xs ys)
    (values (nreverse pxs) (nreverse pys))))

(defmacro convert-sky-sequences (device coords &rest pairs)
  "If the variable COORDS is :SKY or :POLAR, convert each pair of
   sequence variables in PAIRS ((XS YS) ...) to :PLOT (lines in :POLAR
   following the angle) and set COORDS to :PLOT."
  `(when (member ,coords '(:sky :polar))
     ,@(loop for (xs ys) in pairs
             collect `(setf (values ,xs ,ys)
                            (curved-to-plot-sequences ,device ,xs ,ys ,coords t)))
     (setf ,coords :plot)))

(defplot plot-line (device x1 y1 x2 y2 &key (coords :plot))
  "Stroke the line from (X1,Y1) to (X2,Y2) in COORDS. In :SKY, a straight
   line between the projected points; in :POLAR, a curve following the
   angle."
  (when (eq coords :polar)
    ;; a line in angle and radius curves
    (return-from plot-line
      (plot-polyline device (list x1 x2) (list y1 y2) :coords :polar)))
  (when (eq coords :sky)
    ;; a straight segment between the projected points
    (let ((vp (current-viewport device)))
      (setf (values x1 y1) (sky-to-plot vp x1 y1)
            (values x2 y2) (sky-to-plot vp x2 y2)
            coords :plot)))
  ;; one lock for the whole batch
  (with-batched-graphics (device)
    (new-path device)
    (polyline-path* device (list x1 x2) (list y1 y2) coords)
    (stroke device)))

(defplot plot-polyline (device xs ys &key (coords :plot))
  "Stroke line segments joining the points of sequences XS and YS."
  (convert-sky-sequences device coords (xs ys))
  ;; one lock for the whole batch
  (with-batched-graphics (device)
    (new-path device)
    (polyline-path* device xs ys coords)
    (stroke device)))

(defmacro with-plot-rotation ((device degrees x y &key (coords :plot) follow-skew)
                              &body body)
  "Run BODY with all drawing, text included, rotated DEGREES
   counterclockwise on the page about the point (X,Y) in COORDS.  The
   rotation is applied on the page (to cairo's transform), so shapes
   keep their shape however unequal the x and y scales of COORDS are,
   and line widths are unaffected.  Coordinates inside BODY are
   interpreted as if unrotated.
   With FOLLOW-SKEW, and COORDS skewed by SET-TRANSFORM, the rotation
   happens in the plot's unskewed geometry and is then skewed like the
   rest of the plot (see plot-device.lisp).  That transform is not
   rigid, so lines stroked and text drawn inside BODY are sheared too."
  (let ((d (gensym "DEVICE")) (m (gensym "M")) (c (gensym "COORDS")))
    ;; enter the viewport first: entering one inside BODY would reset
    ;; cairo's transform, rotation and all
    `(let ((,d ,device) (,c ,coords))
       (with-target (,d ,c)
         (with-saved-state (,d)
           (let ((,m (shape-rotation ,d ,degrees ,x ,y ,c ,follow-skew)))
             ;; TRANSFORM takes cairo's order: xx yx xy yy x0 y0
             (transform ,d (aref ,m 0) (aref ,m 2) (aref ,m 1) (aref ,m 3)
                        (aref ,m 4) (aref ,m 5)))
           ,@body)))))

(defun vertex-mean (seq)
  (/ (reduce #'+ seq) (length seq)))

;;; Rotated rectangles and polygons transform their points before the
;;; path is built (rather than rotating cairo's transform), so line
;;; widths stay uniform even when FOLLOW-SKEW makes the rotation
;;; non-rigid.

(defplot plot-polygon (device xs ys &key fill (coords :plot) (angle 0) about
                                       follow-skew)
  "Stroke, or fill if FILL, the closed polygon with vertices XS, YS.
   ANGLE rotates it, keeping its shape, by that many degrees
   counterclockwise on the page about ABOUT, a list (X Y) in COORDS,
   by default the mean of the vertices.  With FOLLOW-SKEW and skewed
   COORDS, it is rotated in the plot's unskewed geometry instead, then
   skewed with the plot."
  (when (eq coords :polar)
    ;; close it explicitly, so the closing side also follows the angle
    (setf xs (append (coerce xs 'list) (list (elt xs 0)))
          ys (append (coerce ys 'list) (list (elt ys 0)))))
  (convert-sky-sequences device coords (xs ys))
  (let ((post (unless (zerop angle)
                (destructuring-bind (px py)
                    (or about (list (vertex-mean xs) (vertex-mean ys)))
                  (shape-rotation device angle px py coords follow-skew)))))
    ;; one lock for the whole batch
    (with-batched-graphics (device)
      (new-path device)
      (polygon-path* device xs ys coords post)
      (finish-shape device fill))))

(defplot plot-rectangle (device x1 y1 x2 y2 &key fill (coords :plot) (angle 0)
                                               about follow-skew)
  "Stroke, or fill if FILL, the rectangle with corners (X1,Y1), (X2,Y2).
   ANGLE rotates it, keeping its shape, by that many degrees
   counterclockwise on the page about ABOUT, a list (X Y) in COORDS,
   by default its center.  With FOLLOW-SKEW and skewed COORDS, it is
   rotated in the plot's unskewed geometry instead, then skewed with
   the plot."
  (let ((post (unless (zerop angle)
                (destructuring-bind (px py)
                    (or about (list (/ (+ x1 x2) 2) (/ (+ y1 y2) 2)))
                  (shape-rotation device angle px py coords follow-skew)))))
    ;; one lock for the whole batch
    (with-batched-graphics (device)
      (new-path device)
      (rectangle-path* device x1 y1 x2 y2 coords post)
      (finish-shape device fill))))

(defplot plot-ellipse (device xc yc a b &key fill (coords :plot) (angle 0)
                                           (a-scale :x) (b-scale :y) follow-skew)
  "Stroke, or fill if FILL, an ellipse centered at (XC,YC) in COORDS
   with semi-axes A and B.  Think of it as drawn in two steps:
     1. A lies along the x axis of COORDS and B exactly perpendicular
        to it on the page, each measured in x units of COORDS if its
        scale (A-SCALE, B-SCALE) is :X or in y units if :Y.  The
        defaults (:X and :Y) give the ellipse whose semi-axes are A in
        x and B in y; A-SCALE and B-SCALE both :X with A = B gives a
        true circle.
     2. The ellipse is rotated ANGLE degrees counterclockwise on the
        page about its center, keeping its shape.
   So the shape never depends on the angle, even when the x and y
   scales of COORDS differ.
   If COORDS are skewed (a SET-TRANSFORM with shear), FOLLOW-SKEW adds
   3. The ellipse is skewed like the rest of the plot, as if steps 1
      and 2 happened in the plot before it was skewed.  At ANGLE 0 with
      the default scales this is exactly the image of the ellipse
      (x/A)^2 + (y/B)^2 = 1 of COORDS.
   Without skew, FOLLOW-SKEW changes nothing.  With COORDS :SKY, only
   the center is in right ascension and declination; A and B are in
   :PLOT units (degrees on the projection plane)."
  (when (member coords '(:sky :polar))
    (setf (values xc yc) (funcall (if (eq coords :sky) #'sky-to-plot #'polar-to-plot)
                                  (current-viewport device) xc yc)
          coords :plot))
  ;; one lock for the path building and finish
  (with-batched-graphics (device)
    (new-path device)
    (multiple-value-call #'ellipse-path-canvas device
      (ellipse-vectors device xc yc a b coords :angle angle :a-scale a-scale
                       :b-scale b-scale :follow-skew follow-skew))
    (finish-shape device fill)))

(defplot plot-circle (device xc yc r &key fill (coords :plot) round follow-skew)
  "Stroke, or fill if FILL, a circle centered at (XC,YC) in COORDS.
   By default R is a radius in COORDS in both x and y, so the result is
   an ellipse on the page wherever the x and y scales of COORDS differ
   (e.g. :DEVICE on a non-square page, or most :PLOT windows).  With
   ROUND, it is a true circle on the page whose radius is R measured
   along the x axis of COORDS.  FOLLOW-SKEW is as for PLOT-ELLIPSE."
  (plot-ellipse device xc yc r r :fill fill :coords coords
                                 :a-scale :x :b-scale (if round :x :y)
                                 :follow-skew follow-skew))

(defplot plot-text (device x y string &key (coords :plot) (angle 0)
                                         (halign :left) (valign :baseline)
                                         font size (halo *halo*))
  "Draw STRING anchored at (X,Y) in COORDS, as PUT-TEXT, except that
   ANGLE is in degrees counterclockwise (as in PGPLOT).  HALO outlines
   it (see WITH-HALO).  Returns the text's width in canvas units."
  (multiple-value-bind (cx cy) (to-canvas device coords x y)
    (let ((width (put-text device cx cy string
                           :angle (* angle (/ pi -180)) ; canvas y runs downward
                           :halign halign :valign valign :font font :size size
                           :halo halo))
          (fp (recording-footprint device)))
      (when (and fp (numberp width))
        ;; the text's box, ignoring rotation
        (let* ((h (or size (font-size (current-font device))))
               (x1 (- cx (* (halign-fraction halign) width)))
               (y1 (ecase valign
                     (:top cy) (:center (- cy (* 0.5 h)))
                     (:bottom (- cy h)) (:baseline (- cy (* 0.75 h))))))
          (note-box fp x1 y1 (+ x1 width) (+ y1 h))))
      width)))
