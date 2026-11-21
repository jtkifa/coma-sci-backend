;;;; plot3d.lisp — 3D plots projected into a viewport
;;;;
;;;; A VIEW3D projects a 3D data box into a viewport's :PLOT coordinates,
;;;; so 3D drawing is ordinary 2D drawing afterward: rotated viewports,
;;;; clipping, fonts, markers, halos and colormaps all apply.
;;;;
;;;;   (let ((view (make-view3d vp :x-range '(0 10) :y-range '(0 10)
;;;;                               :z-range '(0 1))))
;;;;     (with-viewport (vp)
;;;;       (draw-box3d view :part :back)
;;;;       (plot-floor-contours view array)
;;;;       (plot-surface view array :colormap :viridis)
;;;;       (draw-box3d view :part :front)))
;;;;
;;;; The camera looks at the box from AZIMUTH (degrees about the z axis,
;;;; from the x axis) and ELEVATION (degrees above the x-y plane), as in
;;;; matplotlib; DISTANCE, if given, adds perspective.  BOX-ASPECT sets the
;;;; drawn proportions of the box (default x : y : z = 1 : 1 : 0.6).
;;;; MAKE-VIEW3D sets the viewport's window to fit the projected box with
;;;; equal scales, so shapes are not distorted.
;;;;
;;;; Hidden surfaces use the painter's algorithm: surface cells are drawn
;;;; back to front, which is exact for a single surface z = f(x,y).  Draw
;;;; what lies behind the surface (back box edges, floor contours) before
;;;; it and what lies in front after it.

(in-package :cl-cairoplot)

(defclass view3d ()
  ((viewport :initarg :viewport :reader view3d-viewport)
   (x-range :initarg :x-range :reader view3d-x-range)
   (y-range :initarg :y-range :reader view3d-y-range)
   (z-range :initarg :z-range :reader view3d-z-range)
   (azimuth :initarg :azimuth :reader view3d-azimuth)
   (elevation :initarg :elevation :reader view3d-elevation)
   (box-aspect :initarg :box-aspect :reader view3d-box-aspect)
   (distance :initarg :distance :reader view3d-distance)
   ;; camera basis in normalized box coordinates: screen right, screen
   ;; up, and toward the camera
   (right :reader view3d-right) (up :reader view3d-up) (toward :reader view3d-toward))
  (:documentation "A view of a 3D box of data into a viewport, from an azimuth and an
   elevation (see MAKE-VIEW3D)."))

(defplot make-view3d (viewport &key (x-range '(0 1)) (y-range '(0 1)) (z-range '(0 1))
                                  (azimuth -60) (elevation 30)
                                  (box-aspect '(1 1 0.6)) distance (margin 0.05))
  "A 3D view of the box X-RANGE x Y-RANGE x Z-RANGE (lists (LO HI)) into
   VIEWPORT, whose window is set to fit the projected box (plus MARGIN, a
   fraction) with equal scales.  See the top of plot3d.lisp."
  (let* ((phi (* azimuth (/ pi 180))) (theta (* elevation (/ pi 180)))
         (view (make-instance 'view3d :viewport viewport
                                      :x-range x-range :y-range y-range :z-range z-range
                                      :azimuth azimuth :elevation elevation
                                      :box-aspect box-aspect :distance distance)))
    (setf (slot-value view 'right) (list (- (sin phi)) (cos phi) 0)
          (slot-value view 'up) (list (- (* (sin theta) (cos phi)))
                                      (- (* (sin theta) (sin phi)))
                                      (cos theta))
          (slot-value view 'toward) (list (* (cos theta) (cos phi))
                                          (* (cos theta) (sin phi))
                                          (sin theta)))
    ;; fit the window around the eight projected corners
    (let ((xs '()) (ys '()))
      (dolist (corner (box-corners view))
        (multiple-value-bind (sx sy) (apply #'project3d view corner)
          (push sx xs) (push sy ys)))
      (let* ((x1 (reduce #'min xs)) (x2 (reduce #'max xs))
             (y1 (reduce #'min ys)) (y2 (reduce #'max ys))
             (mx (* margin (- x2 x1))) (my (* margin (- y2 y1))))
        (set-window-equal-scales viewport (- x1 mx) (+ x2 mx) (- y1 my) (+ y2 my))))
    view))

(defun box-corners (view)
  (destructuring-bind (x1 x2) (view3d-x-range view)
    (destructuring-bind (y1 y2) (view3d-y-range view)
      (destructuring-bind (z1 z2) (view3d-z-range view)
        (loop for x in (list x1 x2)
              nconc (loop for y in (list y1 y2)
                          nconc (loop for z in (list z1 z2) collect (list x y z))))))))

(defun normalize3d (view x y z)
  "Data point (X,Y,Z) in box coordinates centered on 0, scaled by
   BOX-ASPECT."
  (flet ((n (v range k)
           (destructuring-bind (lo hi) range
             (* k (- (/ (- v lo) (- hi lo)) 1/2)))))
    (destructuring-bind (ax ay az) (view3d-box-aspect view)
      (values (n x (view3d-x-range view) ax)
              (n y (view3d-y-range view) ay)
              (n z (view3d-z-range view) az)))))

(defun dot3 (v a b c)
  (+ (* (first v) a) (* (second v) b) (* (third v) c)))

(defun project-normalized (view a b c)
  "Values SX SY DEPTH of the point (A,B,C) in box coordinates (see
   NORMALIZE3D)."
  (let ((sx (dot3 (view3d-right view) a b c))
        (sy (dot3 (view3d-up view) a b c))
        (depth (dot3 (view3d-toward view) a b c))
        (distance (view3d-distance view)))
    (if distance
        (let ((k (/ distance (- distance depth))))   ; nearer is larger
          (values (* k sx) (* k sy) depth))
        (values sx sy depth))))

(defun project3d (view x y z)
  "Values: the :PLOT coordinates SX SY of data point (X,Y,Z), and its
   DEPTH (larger is nearer the camera)."
  (multiple-value-call #'project-normalized view (normalize3d view x y z)))

(defun project-sequences (view xs ys zs)
  "Projected :PLOT coordinates of the points XS, YS, ZS as two vectors."
  (let* ((n (min (length xs) (length ys) (length zs)))
         (sxs (make-array n :element-type 'double-float))
         (sys (make-array n :element-type 'double-float)))
    (dotimes (k n)
      (multiple-value-bind (sx sy) (project3d view (elt xs k) (elt ys k) (elt zs k))
        (setf (aref sxs k) (float sx 1d0) (aref sys k) (float sy 1d0))))
    (values sxs sys)))

;;; ============================================================
;;; Lines, points, text
;;; ============================================================
;;; Draw these inside WITH-VIEWPORT of the view's viewport.

;;; The drawing functions below draw in the view's viewport, unclipped
;;; (a box seen at an angle, and its labels, reach past the rectangle).

(defmacro with-view3d ((view) &body body)
  "Run BODY drawing in VIEW's viewport, entered without clipping unless
   it already is."
  `(call-with-view3d ,view (lambda () ,@body)))

(defun call-with-view3d (view function)
  (let ((viewport (view3d-viewport view)))
    (if (viewport-entered-p viewport)
        (funcall function)
        (with-viewport (viewport :clip nil) (funcall function)))))

(defun plot3d-polyline (view xs ys zs)
  "Stroke line segments joining the 3D points XS, YS, ZS."
  (with-view3d (view)
    (multiple-value-bind (sxs sys) (project-sequences view xs ys zs)
      (plot-polyline (viewport-device (view3d-viewport view)) sxs sys))))

(defun plot3d-points (view xs ys zs &rest keys &key marker size angle font halo)
  "Draw markers at the 3D points XS, YS, ZS; keywords as PLOT-POINTS."
  (declare (ignore marker size angle font halo))
  (with-view3d (view)
    (multiple-value-bind (sxs sys) (project-sequences view xs ys zs)
      (apply #'plot-points (viewport-device (view3d-viewport view)) sxs sys keys))))

(defun plot3d-text (view x y z text &rest keys &key halign valign angle font size halo)
  "Draw TEXT at the 3D point (X,Y,Z), upright on the page; keywords as
   PLOT-TEXT."
  (declare (ignore halign valign angle font size halo))
  (with-view3d (view)
    (multiple-value-bind (sx sy) (project3d view x y z)
      (apply #'plot-text (viewport-device (view3d-viewport view)) sx sy text keys))))

;;; ============================================================
;;; The box
;;; ============================================================

(defun box-edges (view)
  "The 12 edges of the data box, each a list of two corners."
  (let ((corners (box-corners view)))
    (loop for (a . rest) on corners
          nconc (loop for b in rest
                      ;; corners differing in exactly one coordinate
                      when (= 1 (count nil (mapcar #'= a b)))
                        collect (list a b)))))

(defun draw-box3d (view &key (part :all))
  "Stroke the edges of the data box with the current style: PART :BACK
   (the three edges meeting at the corner farthest from the camera, to
   draw before a surface), :FRONT (the others, after it) or :ALL."
  (with-view3d (view)
    (let* ((device (viewport-device (view3d-viewport view)))
           (far (first (sort (box-corners view) #'<
                             :key (lambda (c) (nth-value 2 (apply #'project3d view c)))))))
      (dolist (edge (box-edges view))
        (let ((back (member far edge :test #'equal)))
          (when (or (eq part :all) (if (eq part :back) back (not back)))
            (multiple-value-bind (sxs sys)
                (project-sequences view (mapcar #'first edge) (mapcar #'second edge)
                                   (mapcar #'third edge))
              (plot-polyline device sxs sys))))))))

;;; ============================================================
;;; Surfaces
;;; ============================================================

(defun grid-coordinate (index n range)
  "The data coordinate of grid node INDEX (possibly fractional) of N
   spanning RANGE."
  (destructuring-bind (lo hi) range
    (if (= n 1) lo (+ lo (* index (/ (- hi lo) (1- n)))))))

(defun unit3 (x y z)
  (let ((len (sqrt (+ (* x x) (* y y) (* z z)))))
    (if (zerop len) (values 0 0 1) (values (/ x len) (/ y len) (/ z len)))))

(defun plot-surface (view array &key x-range y-range (colormap :viridis) vmin vmax
                                  (scale :linear) reverse color-array
                                  (shade 0.6) (light '(-1 1 2)) smooth
                                  edges (edge-color :black) (edge-width 0.4)
                                  (fill t) contours contour-color contour-colormap
                                  (contour-width 0.8))
  "Draw the surface z = ARRAY[row, column], rows along y and columns
   along x, the grid nodes spanning X-RANGE and Y-RANGE (default the
   view's).
   Color: each cell (or, with SMOOTH, each node) through COLORMAP by
   height, or by COLOR-ARRAY (same shape) if given, with VMIN, VMAX,
   SCALE and REVERSE as for PLOT-IMAGE.  SHADE (0..1) darkens surface
   turned away from LIGHT, a direction in data-box axes; NIL for none.
   SMOOTH blends colors across cells (mesh gradients) instead of flat
   facets.  EDGES strokes cell edges in EDGE-COLOR, EDGE-WIDTH wide;
   FILL NIL fills cells with the background instead, a hidden-line
   wireframe (both imply flat cells).
   CONTOURS (a list of levels, a number of levels, or T) draws contour
   lines on the surface, hidden correctly behind nearer parts of it, in
   CONTOUR-COLOR (default the current color) or colored by
   CONTOUR-COLORMAP, CONTOUR-WIDTH wide.
   Cells are drawn back to front.  Returns the VMIN and VMAX used."
  (with-view3d (view)
    (destructuring-bind (h w) (array-dimensions array)
      (let* ((device (viewport-device (view3d-viewport view)))
             (x-range (or x-range (view3d-x-range view)))
             (y-range (or y-range (view3d-y-range view)))
             (values-array (or color-array array))
             (cm (parse-colormap (if reverse (reverse-colormap colormap) colormap)))
             (f (scale-function scale))
             (smooth (and smooth fill (not edges)))
             (levels (cond ((null contours) nil)
                           ((listp contours) (sort (copy-list contours) #'<))
                           ((integerp contours) (contour-levels array contours))
                           (t (contour-levels array 8))))
             (contour-cm (and contour-colormap (parse-colormap contour-colormap))))
        (multiple-value-bind (lo hi) (if (and vmin vmax)
                                         (values vmin vmax)
                                         (image-limits values-array))
          (let* ((vmin (or vmin lo)) (vmax (or vmax hi))
                 (range (if (= vmax vmin) 1 (- vmax vmin)))
                 (sx (make-array (list h w))) (sy (make-array (list h w)))
                 (depth (make-array (list h w)))
                 (nodes (make-array (list h w)))       ; normalized (a b c)
                 (cell-normals (make-array (list (max 1 (1- h)) (max 1 (1- w)))))
                 (light (multiple-value-list (apply #'unit3 light))))
            (labels ((xi (i) (grid-coordinate i w x-range))
                     (yj (j) (grid-coordinate j h y-range))
                     (fraction (v) (funcall f (max 0 (min 1 (/ (- v vmin) range)))))
                     (shading (nx ny nz)
                       (if shade
                           (+ (- 1 shade) (* shade (abs (dot3 light nx ny nz))))
                           1))
                     (shaded-color (value normal)
                       (multiple-value-bind (r g b) (colormap-rgb cm (fraction value))
                         (let ((k (apply #'shading normal)))
                           (list (* r k) (* g k) (* b k) 1))))
                     (canvas-xy (j i)
                       (multiple-value-list (to-canvas device :plot (aref sx j i) (aref sy j i)))))
              ;; project every node once
              (dotimes (j h)
                (dotimes (i w)
                  (setf (aref nodes j i)
                        (multiple-value-list (normalize3d view (xi i) (yj j) (aref array j i))))
                  (multiple-value-bind (px py pd) (apply #'project-normalized view (aref nodes j i))
                    (setf (aref sx j i) px (aref sy j i) py (aref depth j i) pd))))
              ;; cell normals: the cross product of the cell's diagonals
              (dotimes (j (1- h))
                (dotimes (i (1- w))
                  (destructuring-bind ((ax ay az) (bx by bz) (cx cy cz) (dx dy dz))
                      (list (aref nodes j i) (aref nodes (1+ j) (1+ i))
                            (aref nodes j (1+ i)) (aref nodes (1+ j) i))
                    (let ((ux (- bx ax)) (uy (- by ay)) (uz (- bz az))
                          (vx (- dx cx)) (vy (- dy cy)) (vz (- dz cz)))
                      (setf (aref cell-normals j i)
                            (multiple-value-list
                             (unit3 (- (* uy vz) (* uz vy)) (- (* uz vx) (* ux vz))
                                    (- (* ux vy) (* uy vx)))))))))
              (flet ((node-normal (j i)
                       ;; the mean of the normals of the cells around node (i,j)
                       (let ((sum (list 0 0 0)))
                         (loop for (cj ci) in (list (list (1- j) (1- i)) (list (1- j) i)
                                                    (list j (1- i)) (list j i))
                               when (and (< -1 cj (1- h)) (< -1 ci (1- w)))
                               do (setf sum (mapcar #'+ sum (aref cell-normals cj ci))))
                         (multiple-value-list (apply #'unit3 sum))))
                     (level-color (level)
                       (cond (contour-cm
                              (multiple-value-call #'list
                                (colormap-rgb contour-cm
                                              (if (cdr levels)
                                                  (/ (- level (first levels))
                                                     (- (car (last levels)) (first levels)))
                                                  0.5))))
                             (t contour-color))))
                (let ((cells '()))
                  ;; cells: (depth j i segments), SEGMENTS ((color x1 y1 x2 y2) ...)
                  ;; in :PLOT coordinates
                  (dotimes (j (1- h))
                    (dotimes (i (1- w))
                      (let ((zs (list (aref array j i) (aref array j (1+ i))
                                      (aref array (1+ j) (1+ i)) (aref array (1+ j) i))))
                        (unless (some #'nan-p zs)
                          (push (list (/ (+ (aref depth j i) (aref depth j (1+ i))
                                            (aref depth (1+ j) (1+ i)) (aref depth (1+ j) i))
                                         4)
                                      j i
                                      (loop for level in levels
                                            nconc (loop for ((u1 v1) (u2 v2))
                                                        in (apply #'cell-contour-segments
                                                                  (append zs (list level)))
                                                        collect (multiple-value-bind (x1 y1)
                                                                    (project3d view (xi (+ i u1))
                                                                               (yj (+ j v1)) level)
                                                                  (multiple-value-bind (x2 y2)
                                                                      (project3d view (xi (+ i u2))
                                                                                 (yj (+ j v2)) level)
                                                                    (list (level-color level)
                                                                          x1 y1 x2 y2))))))
                                cells)))))
                  (setf cells (sort cells #'< :key #'first))
                  ;; A cell's contour segments are drawn once all its neighbors
                  ;; are painted: neighbors drawn after it would cover the half
                  ;; of the line that overhangs into them (they do not hide the
                  ;; cell itself: a single surface does not overlap its neighbors)
                  (let ((order (make-hash-table :test 'equal))
                        (due (make-hash-table)))
                    (loop for cell in cells for k from 0
                          do (setf (gethash (list (second cell) (third cell)) order) k))
                    (loop for (nil j i segments) in cells
                          when segments
                          do (let ((last (loop for dj from -1 to 1
                                               maximize (loop for di from -1 to 1
                                                              maximize (gethash (list (+ j dj) (+ i di))
                                                                                order -1)))))
                               (setf (gethash last due) (append (gethash last due) segments))))
                    (setf cells (loop for (d j i) in cells for k from 0
                                      collect (list d j i (gethash k due)))))
                  (flet ((draw-segments (segments)
                           (with-style (device :line-width contour-width :line-cap :round)
                             (loop for (color x1 y1 x2 y2) in segments
                                   do (with-saved-state (device)
                                        (when color (set-color device color))
                                        (plot-line device x1 y1 x2 y2)))))
                         (corners (j i)
                           (list (list j i) (list j (1+ i)) (list (1+ j) (1+ i)) (list (1+ j) i))))
                    (if smooth
                        ;; one mesh gradient, painted in batches: a batch ends
                        ;; where a cell's contour segments must be drawn
                        ;; before the nearer cells hide them
                        (let ((node-colors (make-array (list h w)))
                              (batch '()))
                          (dotimes (j h)
                            (dotimes (i w)
                              (setf (aref node-colors j i)
                                    (shaded-color (aref values-array j i) (node-normal j i)))))
                          (flet ((flush ()
                                   (when batch
                                     (paint-mesh device (nreverse batch))
                                     (setf batch '()))))
                            (loop for (nil j i segments) in cells
                                  do (let ((points (mapcar (lambda (c) (apply #'canvas-xy c))
                                                           (corners j i))))
                                       (push (list (mapcar #'first points) (mapcar #'second points)
                                                   (mapcar (lambda (c) (apply #'aref node-colors c))
                                                           (corners j i)))
                                             batch))
                                  (when segments
                                    (flush)
                                    (draw-segments segments)))
                            (flush)))
                        (loop for (nil j i segments) in cells
                              do (let* ((cs (corners j i))
                                        (xs (mapcar (lambda (c) (apply #'aref sx c)) cs))
                                        (ys (mapcar (lambda (c) (apply #'aref sy c)) cs))
                                        (value (/ (reduce #'+ (mapcar (lambda (c)
                                                                        (apply #'aref values-array c))
                                                                      cs))
                                                  4))
                                        (rgb (shaded-color value (aref cell-normals j i))))
                                   (with-saved-state (device)
                                     ;; FILL NIL: cells in the background color hide
                                     ;; what is behind them: a hidden-line wireframe
                                     (set-color device (if fill rgb
                                                           (or (canvas-background device) :white)))
                                     (plot-polygon device xs ys :fill t)
                                     (if (or edges (not fill))
                                         (with-style (device :color edge-color
                                                             :line-width edge-width)
                                           (plot-polygon device xs ys))
                                         ;; stroking in the fill color closes
                                         ;; antialiasing seams
                                         (progn (set-line-width device 0.5)
                                                (plot-polygon device xs ys))))
                                   (when segments (draw-segments segments))))))))
              (values vmin vmax))))))))

(defun plot-floor-contours (view array &rest keys &key x-range y-range z &allow-other-keys)
  "Draw contours of ARRAY (nodes as PLOT-SURFACE) on the plane at height
   Z (default the bottom of the box), before the surface.  Other
   keywords are as PLOT-CONTOURS (:LEVELS, :N, :COLOR, :COLORMAP,
   :NEGATIVE-DASH), except labels."
  (with-view3d (view)
    (destructuring-bind (h w) (array-dimensions array)
      (let* ((device (viewport-device (view3d-viewport view)))
             (x-range (or x-range (view3d-x-range view)))
             (y-range (or y-range (view3d-y-range view)))
             (z (or z (first (view3d-z-range view))))
             (levels (sort (copy-list (or (getf keys :levels)
                                          (contour-levels array (or (getf keys :n) 8))))
                           #'<))
             (colormap (getf keys :colormap))
             (cm (and colormap (parse-colormap colormap)))
             (color (getf keys :color))
             (lo (first levels)) (hi (car (last levels))))
        (loop for level in levels
              for k from 0
              do (with-saved-state (device)
                   (cond (cm (set-color device (multiple-value-call #'list
                                                 (colormap-rgb cm (if (= hi lo) 0.5
                                                                      (/ (- level lo) (- hi lo)))))))
                         ((consp color) (set-color device (nth (mod k (length color)) color)))
                         (color (set-color device color)))
                   (when (and (getf keys :negative-dash) (minusp level))
                     (set-dash device (getf keys :negative-dash)))
                   (loop for (xs ys closed) in (contour-lines array level)
                         do (let ((dxs (map 'list (lambda (i) (grid-coordinate i w x-range)) xs))
                                  (dys (map 'list (lambda (j) (grid-coordinate j h y-range)) ys)))
                              (when closed
                                (setf dxs (append dxs (list (first dxs)))
                                      dys (append dys (list (first dys)))))
                              (plot3d-polyline view dxs dys
                                               (make-list (length dxs) :initial-element z))))))
        levels))))

;;; ============================================================
;;; Axes
;;; ============================================================

(defun far-corner (view)
  "The data-box corner farthest from the camera."
  (first (sort (box-corners view) #'<
               :key (lambda (c) (nth-value 2 (apply #'project3d view c))))))

(defun outward-alignment (view a b c da db dc)
  "HALIGN and VALIGN for text placed outward from box point (A,B,C) in
   direction (DA,DB,DC): text extends away from the box."
  (multiple-value-bind (x0 y0) (project-normalized view a b c)
    (multiple-value-bind (x1 y1) (project-normalized view (+ a da) (+ b db) (+ c dc))
      (let* ((dx (- x1 x0)) (dy (- y1 y0)) (len (sqrt (+ (* dx dx) (* dy dy)))))
        (if (zerop len)
            (values :center :center)
            (let ((dx (/ dx len)) (dy (/ dy len)))
              (values (cond ((> dx 0.35) :left) ((< dx -0.35) :right) (t :center))
                      (cond ((> dy 0.35) :bottom) ((< dy -0.35) :top) (t :center)))))))))

(defun draw-axes3d (view &key (part :all) grid x-label y-label z-label size
                           (box t) (tick-length 0.025))
  "Draw the box, grid, ticks, numbers and labels of VIEW's axes, as
   matplotlib places them: x and y along the floor edges nearest the
   camera, z up the leftmost vertical edge.  PART :BACK draws what lies
   behind a surface (back box edges and, with GRID, grid lines on the
   floor and the two back walls); :FRONT the rest; :ALL both.  SIZE is
   the text size (default 0.8 of DEFAULT-TEXT-SIZE); TICK-LENGTH is a
   fraction of the box.  Lines use the current style."
  (with-view3d (view)
    (let* ((device (viewport-device (view3d-viewport view)))
           (size (or size (* 0.8 (default-text-size device))))
           (far (far-corner view)))
      (destructuring-bind ((x1 x2) (y1 y2) (z1 z2))
          (list (view3d-x-range view) (view3d-y-range view) (view3d-z-range view))
        (flet ((ticks (lo hi) (axis-ticks lo hi))
               (line (p q)
                 (multiple-value-bind (sxs sys)
                     (project-sequences view (list (first p) (first q)) (list (second p) (second q))
                                        (list (third p) (third q)))
                   (plot-polyline device sxs sys))))
          (when (member part '(:back :all))
            (when box (draw-box3d view :part :back))
            (when grid
              (destructuring-bind (fx fy fz) far
                (declare (ignore fz))
                (with-style (device :color :light-gray :line-width 0.5)
                  (loop for (x) in (ticks x1 x2)
                        do (line (list x y1 z1) (list x y2 z1))     ; floor
                        (line (list x fy z1) (list x fy z2)))    ; wall y = fy
                  (loop for (y) in (ticks y1 y2)
                        do (line (list x1 y z1) (list x2 y z1))
                        (line (list fx y z1) (list fx y z2)))    ; wall x = fx
                  (loop for (z) in (ticks z1 z2)
                        do (line (list x1 fy z) (list x2 fy z))
                        (line (list fx y1 z) (list fx y2 z)))))))
          (when (member part '(:front :all))
            (when box (draw-box3d view :part :front))
            (with-font (device (current-font device) :size size)
              ;; one axis: ticks at POSITIONS along the edge from P at parameter
              ;; values, outward direction OUT in box coordinates
              (flet ((axis (majors point-at out label mid &optional (label-offset 7))
                       (destructuring-bind (oa ob oc) out
                         (dolist (m majors)
                           (destructuring-bind (a b c) (apply #'list (funcall point-at (first m)))
                             (multiple-value-bind (x0 y0) (project-normalized view a b c)
                               (multiple-value-bind (xt yt)
                                   (project-normalized view (+ a (* tick-length oa))
                                                       (+ b (* tick-length ob))
                                                       (+ c (* tick-length oc)))
                                 (plot-line device x0 y0 xt yt)))
                             (multiple-value-bind (halign valign)
                                 (outward-alignment view a b c oa ob oc)
                               (multiple-value-bind (xn yn)
                                   (project-normalized view (+ a (* 2.5 tick-length oa))
                                                       (+ b (* 2.5 tick-length ob))
                                                       (+ c (* 2.5 tick-length oc)))
                                 (plot-text device xn yn (second m) :halign halign
                                            :valign valign)))))
                         (when label
                           (destructuring-bind (a b c) (funcall point-at mid)
                             (multiple-value-bind (halign valign)
                                 (outward-alignment view a b c oa ob oc)
                               (multiple-value-bind (xl yl)
                                   (project-normalized view (+ a (* label-offset tick-length oa))
                                                       (+ b (* label-offset tick-length ob))
                                                       (+ c (* label-offset tick-length oc)))
                                 (plot-text device xl yl label :halign halign
                                            :valign valign))))))))
                (flet ((nearer (u v key)
                         ;; of two values, the one whose edge is nearer the camera
                         (if (> (funcall key u) (funcall key v)) u v))
                       (depth-at (x y z) (nth-value 2 (project3d view x y z))))
                  ;; x along the floor edge nearest the camera
                  (let ((y (nearer y1 y2 (lambda (y) (depth-at (/ (+ x1 x2) 2) y z1)))))
                    (axis (ticks x1 x2)
                          (lambda (x) (multiple-value-list (normalize3d view x y z1)))
                          (list 0 (if (= y y2) 1 -1) 0) x-label (/ (+ x1 x2) 2)))
                  (let ((x (nearer x1 x2 (lambda (x) (depth-at x (/ (+ y1 y2) 2) z1)))))
                    (axis (ticks y1 y2)
                          (lambda (y) (multiple-value-list (normalize3d view x y z1)))
                          (list (if (= x x2) 1 -1) 0 0) y-label (/ (+ y1 y2) 2)))
                  ;; z up the leftmost vertical edge, outward along its diagonal
                  (destructuring-bind (cx cy)
                      (first (sort (list (list x1 y1) (list x1 y2) (list x2 y1) (list x2 y2)) #'<
                                   :key (lambda (c) (nth-value 0 (project3d view (first c)
                                                                            (second c) z1)))))
                    (axis (ticks z1 z2)
                          (lambda (z) (multiple-value-list (normalize3d view cx cy z)))
                          (list (if (= cx x2) 0.7 -0.7) (if (= cy y2) 0.7 -0.7) 0)
                          ;; farther out: z numbers are wider than tall
                          z-label (/ (+ z1 z2) 2) 11)))))))))))
