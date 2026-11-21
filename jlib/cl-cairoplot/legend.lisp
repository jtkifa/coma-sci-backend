;;;; legend.lisp — legends
;;;;
;;;; (DRAW-LEGEND VP '(("data" :marker :filled-circle :color :blue)
;;;;                   ("model" :line t :color :red :dash (4 2))
;;;;                   ("1 sigma" :fill (0 0 1 0.2))))
;;;;
;;;; draws a legend for VIEWPORT: one row per entry, a sample of the
;;;; entry's style then its label, on a translucent box.  An entry is a
;;;; label (text: a string or runs) followed by style keywords:
;;;;   :LINE T          a line sample, with :LINE-WIDTH and :DASH
;;;;   :MARKER m        a marker sample (any PLOT-POINTS marker), :SIZE
;;;;   :FILL color      a filled patch in that color
;;;;   :COLOR c         the color of the line and marker (default current)
;;;; Several can combine, e.g. :LINE T :MARKER :CIRCLE.
;;;;
;;;; POSITION :BEST (the default) puts the legend where it covers least
;;;; of what has been drawn in the viewport (its footprint, see
;;;; viewport.lisp): the fewest line segments, points, text boxes and
;;;; area of filled shapes, trying the corners, the middles of the sides
;;;; and the center, in matplotlib's order of preference.

(in-package :cl-cairoplot)

(defparameter +legend-positions+
  '(:upper-right :upper-left :lower-left :lower-right :center-right :center-left
    :lower-center :upper-center :center)
  "The places :BEST tries, most preferred first.")

(defun segment-hits-rectangle-p (x1 y1 x2 y2 rx1 ry1 rx2 ry2)
  "True if the segment (X1,Y1)-(X2,Y2) meets the rectangle RX1..RX2,
   RY1..RY2 (Liang-Barsky clipping)."
  (declare (type double-float x1 y1 x2 y2 rx1 ry1 rx2 ry2))
  (let ((t0 0d0) (t1 1d0) (dx (- x2 x1)) (dy (- y2 y1)))
    (declare (type double-float t0 t1 dx dy))
    (flet ((edge (p q)
             (declare (type double-float p q))
             ;; NIL once the segment is known to miss
             (cond ((zerop p) (>= q 0))
                   (t (let ((r (/ q p)))
                        (if (< p 0)
                            (when (<= r t1) (setf t0 (max t0 r)) t)
                            (when (>= r t0) (setf t1 (min t1 r)) t)))))))
      (and (edge (- dx) (- x1 rx1)) (edge dx (- rx2 x1))
           (edge (- dy) (- y1 ry1)) (edge dy (- ry2 y1))
           (<= t0 t1)))))

(defun point-in-polygon-p (x y xs ys)
  "True if (X,Y) is inside the polygon XS, YS (even-odd rule)."
  (let ((inside nil) (n (length xs)))
    (loop for i below n
          for j = (1- n) then (1- i)
          do (let ((xi (aref xs i)) (yi (aref ys i)) (xj (aref xs j)) (yj (aref ys j)))
               (when (and (not (eq (> yi y) (> yj y)))
                          (< x (+ xi (/ (* (- xj xi) (- y yi)) (- yj yi)))))
                 (setf inside (not inside)))))
    inside))

(defun footprint-overlap (footprint x1 y1 x2 y2)
  "How much of FOOTPRINT the rectangle X1..X2, Y1..Y2 (canvas) covers:
   one for each segment, point and polygon side in it, up to 40 for the
   fraction of it inside polygons, and ten for each text box it
   touches."
  (let ((score 0)
        (x1 (float x1 1d0)) (y1 (float y1 1d0)) (x2 (float x2 1d0)) (y2 (float y2 1d0)))
    (let ((s (footprint-segments footprint)))
      (loop for i from 0 below (fill-pointer s) by 4
            when (segment-hits-rectangle-p (aref s i) (aref s (+ i 1)) (aref s (+ i 2))
                                           (aref s (+ i 3)) x1 y1 x2 y2)
              do (incf score)))
    (let ((p (footprint-points footprint)))
      (loop for i from 0 below (fill-pointer p) by 2
            when (and (<= (- x1 3) (aref p i) (+ x2 3)) (<= (- y1 3) (aref p (1+ i)) (+ y2 3)))
              do (incf score)))
    ;; the share of a 5 x 5 grid of sample points inside any polygon
    (let ((near (loop for (xs . ys) in (footprint-polygons footprint)
                      when (and (<= (reduce #'min xs) x2) (>= (reduce #'max xs) x1)
                                (<= (reduce #'min ys) y2) (>= (reduce #'max ys) y1))
                        collect (cons xs ys)))
          (covered 0))
      (loop for fx in '(0.1 0.3 0.5 0.7 0.9)
            do (loop for fy in '(0.1 0.3 0.5 0.7 0.9)
                     for x = (+ x1 (* fx (- x2 x1))) for y = (+ y1 (* fy (- y2 y1)))
                     when (loop for (xs . ys) in near thereis (point-in-polygon-p x y xs ys))
                       do (incf covered)))
      (incf score (* 40 (/ covered 25)))
      (loop for (xs . ys) in near
            do (loop for i below (length xs)
                     for j = (if (zerop i) (1- (length xs)) (1- i))
                     when (segment-hits-rectangle-p (aref xs j) (aref ys j) (aref xs i) (aref ys i)
                                                    x1 y1 x2 y2)
                       do (incf score))))
    (loop for (bx1 by1 bx2 by2) in (footprint-boxes footprint)
          when (and (<= bx1 x2) (>= bx2 x1) (<= by1 y2) (>= by2 y1))
            do (incf score 10))
    score))

(defplot draw-legend (viewport entries &key (position :best) size font
                                          (frame t) (background '(1 1 1 0.85))
                                          (frame-color :gray))
  "Draw a legend of ENTRIES (see the top of legend.lisp) for VIEWPORT.
   POSITION is :BEST (where it covers least of the plot), :UPPER-RIGHT,
   :UPPER-LEFT, :LOWER-RIGHT, :LOWER-LEFT, :UPPER-CENTER, :LOWER-CENTER,
   :CENTER-LEFT, :CENTER-RIGHT or :CENTER (inside the viewport),
   :OUTSIDE-RIGHT (beside it), or a list (X Y) in :DEVICE coordinates
   for the box's upper left corner.  SIZE is the text size (default 0.8
   of DEFAULT-TEXT-SIZE).  BACKGROUND fills the
   box (NIL for none) and FRAME outlines it in FRAME-COLOR."
  (let* ((device (viewport-device viewport))
         (size (or size (* 0.8 (default-text-size device))))
         (pad (* 0.5 size)) (sample (* 2.2 size)) (gap (* 0.5 size))
         (row (* 1.4 size)))
    (with-viewport (viewport :clip nil)
      (with-font (device (or font (current-font device)) :size size)
        (let* ((widths (mapcar (lambda (e) (text-size device (first e))) entries))
               (box-w (+ pad sample gap (reduce #'max widths :initial-value 0) pad))
               (box-h (+ pad (* row (length entries)) pad)))
          (multiple-value-bind (left right bottom top) (viewport-rectangle viewport :canvas)
            (let ((inset (* 0.6 size)))
              (flet ((corner (position)
                       ;; the box's upper left corner (canvas y runs down)
                       (if (consp position)
                           (to-canvas device :device (first position) (second position))
                           (values (case position
                                     ((:upper-left :lower-left :center-left) (+ left inset))
                                     (:outside-right (+ right inset))
                                     ((:upper-center :lower-center :center)
                                      (- (/ (+ left right) 2) (/ box-w 2)))
                                     (t (- right inset box-w)))
                                   (case position
                                     ((:lower-left :lower-right :lower-center)
                                      (- bottom inset box-h))
                                     ((:center-left :center-right :center)
                                      (- (/ (+ top bottom) 2) (/ box-h 2)))
                                     (t (+ top inset)))))))
                (when (eq position :best)
                  (let ((footprint (viewport-footprint viewport)))
                    (setf position
                          (if footprint
                              (let ((best nil) (best-score nil))
                                (dolist (p +legend-positions+ best)
                                  (multiple-value-bind (x y) (corner p)
                                    (let ((score (footprint-overlap footprint x y
                                                                    (+ x box-w) (+ y box-h))))
                                      (when (or (null best-score) (< score best-score))
                                        (setf best p best-score score))))))
                              :upper-right))))
                (multiple-value-bind (x0 y0) (corner position)
                  ;; later legends avoid this one
                  (unless (or (consp position) (eq position :outside-right))
                    (note-box (or (viewport-footprint viewport)
                                  (setf (viewport-footprint viewport) (make-footprint)))
                              x0 y0 (+ x0 box-w) (+ y0 box-h)))
                  (when background
                    (with-color (device background)
                      (draw-rectangle device x0 y0 box-w box-h :fill t)))
                  (when frame
                    (with-style (device :color frame-color :line-width 0.5)
                      (draw-rectangle device x0 y0 box-w box-h)))
                  (loop for entry in entries
                        for k from 0
                        for cy = (+ y0 pad (* row (+ k 0.5)))
                        for sx1 = (+ x0 pad) for sx2 = (+ x0 pad sample)
                        do (destructuring-bind (label &key line marker fill color line-width dash
                                                      (size (* 0.35 size)))
                               entry
                             (with-saved-state (device)
                               (when fill
                                 (with-color (device fill)
                                   (draw-rectangle device sx1 (- cy (* 0.35 row)) sample
                                                   (* 0.7 row) :fill t)))
                               (when color (set-color device color))
                               (when line
                                 (with-saved-state (device)
                                   (when line-width (set-line-width device line-width))
                                   (when dash (set-dash device dash))
                                   (draw-line device sx1 cy sx2 cy)))
                               (when marker
                                 (plot-point device (/ (+ sx1 sx2) 2) cy :marker marker :size size
                                                                         :coords :canvas)))
                             (put-text device (+ sx2 gap) cy label :valign :center))))))))))
    viewport))
