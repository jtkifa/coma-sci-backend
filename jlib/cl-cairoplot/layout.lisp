;;;; layout.lisp — grids of panels and insets
;;;;
;;;; (MAKE-PANEL-GRID DEVICE 2 3) returns a 2 x 3 array of viewports, row 0
;;;; at the top, filling the page between margins with gaps between
;;;; panels (as PGPLOT's PGSUBP or matplotlib's subplots).  For panels
;;;; that share axes, draw numbers only on the outer edges:
;;;;   (draw-box vp :numbers (panel-number-sides grid row col))
;;;; and with no gaps between panels (:HSPACE 0 :WSPACE 0) they touch.

(in-package :cl-cairoplot)

(defun make-panel-grid (device rows cols &key (left 0.1) (right 0.97) (bottom 0.1)
                                             (top 0.92) (hspace 0.08) (wspace 0.08)
                                             window)
  "A ROWS x COLS array of viewports of DEVICE (row 0 at the top),
   spanning LEFT..RIGHT and BOTTOM..TOP in :DEVICE coordinates with
   gaps HSPACE between rows and WSPACE between columns (fractions of the
   page).  WINDOW (X1 X2 Y1 Y2), if given, is set on every panel."
  (let* ((grid (make-array (list rows cols)))
         (w (/ (- right left (* wspace (1- cols))) cols))
         (h (/ (- top bottom (* hspace (1- rows))) rows)))
    (dotimes (r rows grid)
      (dotimes (c cols)
        (let ((x1 (+ left (* c (+ w wspace))))
              (y2 (- top (* r (+ h hspace)))))
          (setf (aref grid r c)
                (make-viewport device x1 (+ x1 w) (- y2 h) y2
                               :window (or window '(0 1 0 1)))))))))

(defun panel-number-sides (grid row col &key (x-sides '(:bottom)) (y-sides '(:left)))
  "The sides of panel ROW, COL that get numbers when panels share axes:
   X-SIDES on the bottom row, Y-SIDES in the left column."
  (destructuring-bind (rows cols) (array-dimensions grid)
    (declare (ignore cols))
    (append (when (= row (1- rows)) x-sides)
            (when (= col 0) y-sides))))

(defun panel-omitted-ends (grid row col)
  "For DRAW-BOX's :OMIT-END-NUMBERS: where touching panels meet, the
   number at the left end of x (all but the first column) and the top
   end of y (all but the top row), which would collide with the
   neighbor's."
  (declare (ignore grid))
  (append (when (> col 0) '(:x-low))
          (when (> row 0) '(:y-high))))

(defplot draw-panel-label (viewport text &key (position :upper-left) size halo)
  "Write TEXT, e.g. \"(a)\", just inside a corner (:UPPER-LEFT,
   :UPPER-RIGHT, :LOWER-LEFT, :LOWER-RIGHT) of VIEWPORT."
  (let* ((device (viewport-device viewport))
         (size (or size (default-text-size device)))
         (inset (* 0.5 size)))
    (with-viewport (viewport :clip nil)
      (multiple-value-bind (left right bottom top) (viewport-rectangle viewport :canvas)
        (let ((upper (member position '(:upper-left :upper-right)))
              (leftp (member position '(:upper-left :lower-left))))
          (put-text device (if leftp (+ left inset) (- right inset))
                    (if upper (+ top inset) (- bottom inset))
                    text :halign (if leftp :left :right) :valign (if upper :top :bottom)
                         :size size :halo halo))))
    viewport))

;;; ============================================================
;;; Insets
;;; ============================================================
;;;
;;; An inset is a small viewport inside another, e.g. a zoom on part of
;;; the plot.  ADD-INSET places it (by fractions of the parent or in the
;;; parent's plot coordinates), clears its background and, with ZOOM,
;;; marks the region it shows in the parent: a rectangle and two lines
;;; to the inset's corners (DRAW-ZOOM-INDICATOR).  Parents are assumed
;;; unrotated.

(defplot add-inset (viewport x1 x2 y1 y2 &key (coords :fraction) window
                                              (background :white) zoom)
  "Make and return a viewport inside VIEWPORT spanning X1..X2, Y1..Y2 in
   COORDS: :FRACTION of VIEWPORT's rectangle (0..1 across and up) or
   VIEWPORT's :PLOT coordinates.  WINDOW (X1 X2 Y1 Y2) sets its window.
   BACKGROUND (NIL for none) fills it, hiding what is drawn beneath.
   ZOOM (with WINDOW) draws DRAW-ZOOM-INDICATOR."
  (let ((device (viewport-device viewport)))
    (multiple-value-bind (dx1 dx2 dy1 dy2)
        (ecase coords
          (:fraction
           (multiple-value-bind (px1 px2 py1 py2) (viewport-rectangle viewport :device)
             (flet ((across (f) (+ px1 (* f (- px2 px1))))
                    (up (f) (+ py1 (* f (- py2 py1)))))
               (values (across x1) (across x2) (up y1) (up y2)))))
          (:plot
           (with-viewport (viewport :clip nil)
             (multiple-value-bind (ax ay) (convert-coords device x1 y1 :plot :device)
               (multiple-value-bind (bx by) (convert-coords device x2 y2 :plot :device)
                 (values ax bx ay by))))))
      (let ((inset (make-viewport device dx1 dx2 dy1 dy2 :window (or window '(0 1 0 1)))))
        (when background
          (with-color (device background)
            (plot-rectangle device dx1 dy1 dx2 dy2 :coords :device :fill t)))
        (when (and zoom window)
          (draw-zoom-indicator viewport inset))
        inset))))

(defun segment-crosses-rectangle-p (ax ay bx by rectangle)
  "True if the segment A-B passes through the inside of RECTANGLE
   (X1 X2 Y1 Y2), not just touching it."
  (destructuring-bind (x1 x2 y1 y2) rectangle
    (let ((ex (* 1d-6 (abs (- x2 x1)))) (ey (* 1d-6 (abs (- y2 y1)))))
      (loop for k from 1 below 100
            for f = (/ k 100)
            for x = (+ ax (* f (- bx ax)))
            for y = (+ ay (* f (- by ay)))
              thereis (and (< (+ (min x1 x2) ex) x (- (max x1 x2) ex))
                           (< (+ (min y1 y2) ey) y (- (max y1 y2) ey)))))))

(defplot draw-zoom-indicator (viewport inset &key (color :gray) (line-width 0.8))
  "Mark on VIEWPORT the region INSET shows (INSET's window, in
   VIEWPORT's plot coordinates) with a rectangle, and join its corners
   to the matching corners of INSET with lines that cross neither."
  (let ((device (viewport-device viewport)))
    (multiple-value-bind (wx1 wx2 wy1 wy2) (viewport-window inset)
      (with-style (device :color color :line-width line-width)
        (plot-rectangle viewport wx1 wy1 wx2 wy2)
        (multiple-value-bind (rx1 ry1 rx2 ry2)
            (with-viewport (viewport :clip nil)
              (multiple-value-bind (ax ay) (convert-coords device (min wx1 wx2) (min wy1 wy2)
                                                           :plot :device)
                (multiple-value-bind (bx by) (convert-coords device (max wx1 wx2) (max wy1 wy2)
                                                             :plot :device)
                  (values (min ax bx) (min ay by) (max ax bx) (max ay by)))))
          (multiple-value-bind (ix1 ix2 iy1 iy2) (viewport-rectangle inset :device)
            (let ((region (list rx1 rx2 ry1 ry2)) (box (list ix1 ix2 iy1 iy2)))
              ;; the same corner of each: lower left, lower right, ...
              (loop for (fx fy) in '((0 0) (1 0) (0 1) (1 1))
                    for ax = (if (zerop fx) rx1 rx2) for ay = (if (zerop fy) ry1 ry2)
                    for bx = (if (zerop fx) ix1 ix2) for by = (if (zerop fy) iy1 iy2)
                    unless (or (segment-crosses-rectangle-p ax ay bx by region)
                               (segment-crosses-rectangle-p ax ay bx by box))
                      do (plot-line device ax ay bx by :coords :device)))))))
    inset))
