;;;; pie.lisp — pie and donut charts
;;;;
;;;; PIE-CHART draws a whole chart in a viewport: wedges in series
;;;; colors (*CATEGORY-COLORS* in order, so at most eight; fold small
;;;; ones into "other") separated by thin gaps, starting at 12 o'clock
;;;; and running clockwise; percentages inside the wedges that have room
;;;; for them; and names outside, with leader lines, nudged apart where
;;;; they would collide.  INNER > 0 makes a donut, with CENTER-TEXT in
;;;; its hole.  Text is in the current color, or white on dark wedges.

(in-package :cl-cairoplot)

(defun color-luminance (spec)
  "Relative luminance of the color SPEC, 0 (black) .. 1 (white)."
  (let ((c (parse-color spec)))
    (flet ((linear (v) (if (<= v 0.04045) (/ v 12.92) (expt (/ (+ v 0.055) 1.055) 2.4))))
      (+ (* 0.2126 (linear (color-red c))) (* 0.7152 (linear (color-green c)))
         (* 0.0722 (linear (color-blue c)))))))

(defun contrasting-ink (spec)
  "White on a dark color SPEC, else NIL (keep the current color)."
  (when (< (color-luminance spec) 0.18) :white))

(defun spread-labels (ys gap)
  "YS (canvas, increasing downward) moved apart so neighbors are at
   least GAP apart, each moving as little as possible; same order."
  (let* ((order (sort (loop for y in ys for k from 0 collect (cons y k)) #'< :key #'car))
         (placed (mapcar #'car order)))
    ;; push down past each predecessor, then pull clusters back up by
    ;; their mean overshoot so they stay centered on their wedges
    (let ((prev nil))
      (loop for cell on placed
            do (when (and prev (< (car cell) (+ prev gap)))
                 (setf (car cell) (+ prev gap)))
               (setf prev (car cell))))
    (let ((shift (/ (loop for y in placed for (y0) in order sum (- y y0))
                    (max 1 (length placed)))))
      (setf placed (mapcar (lambda (y) (- y shift)) placed)))
    (let ((result (make-list (length ys))))
      (loop for y in placed for (nil . k) in order do (setf (nth k result) y))
      result)))

(defplot pie-chart (viewport values &key names colors (inner 0) (start 90) (clockwise t)
                                    (gap-color :white) (gap-width 1.5)
                                    (percentages t) (percent-format "~D%") (labels t)
                                    center-text size title)
  "Draw a pie (or, with INNER a fraction of the radius, a donut) of
   VALUES in VIEWPORT, whose window it sets.  NAMES label the wedges
   outside (LABELS NIL leaves them to a legend: the entries are
   returned); COLORS default to *CATEGORY-COLORS*.  Wedges start at
   START degrees (90: 12 o'clock) and run CLOCKWISE, separated by
   GAP-WIDTH lines of GAP-COLOR.  PERCENTAGES writes each wedge's share,
   formatted with PERCENT-FORMAT (given the rounded percentage), where
   it fits.  CENTER-TEXT goes in a donut's hole; TITLE above.  SIZE is
   the text size.  Returns legend entries for DRAW-LEGEND."
  (let* ((device (viewport-device viewport))
         (values (mapcar (lambda (v) (float v 1d0)) (coerce values 'list)))
         (n (length values))
         (total (reduce #'+ values))
         (colors (category-colors n colors))
         (size (or size (* 0.9 (default-text-size device))))
         (sign (if clockwise -1 1))
         (wedges '())
         (radius nil)
         (highest nil))                 ; canvas y of the topmost name
    ;; the largest radius (canvas units) leaving room for the names
    ;; beside and the title above, then a window with equal scales
    (multiple-value-bind (left right bottom top) (viewport-rectangle viewport :canvas)
      (let* ((w (/ (abs (- right left)) 2)) (h (/ (abs (- bottom top)) 2))
             (name-width (if (and labels names)
                             (+ 12 (* (/ size (font-size (current-font device)))
                                      (reduce #'max names :key (lambda (s) (text-size device s)))))
                             0))
             (above (+ (if (and labels names) (* 1.5 size) 0) (if title (* 1.8 size) 0))))
        (setf radius (max 1 (min (/ (- w name-width) (if (plusp name-width) 1.16 1.04))
                                 (/ (- h above) (if (plusp name-width) 1.12 1.04)))))
        (set-window viewport (- (/ w radius)) (/ w radius) (- (/ h radius)) (/ h radius))))
    ;; angles in degrees, counterclockwise from +x
    (let ((angle start))
      (dolist (v values)
        (let ((sweep (* sign 360 (/ v total))))
          (push (list angle (+ angle sweep)) wedges)
          (incf angle sweep))))
    (setf wedges (nreverse wedges))
    (flet ((arc-points (a0 a1 r)
             (let ((steps (max 2 (ceiling (abs (- a1 a0)) 2))))
               (loop for k to steps
                     for a = (* (/ pi 180) (+ a0 (* k (/ (- a1 a0) steps))))
                     collect (list (* r (cos a)) (* r (sin a)))))))
      (flet ((wedge-outline (a0 a1)
               (let ((outer (arc-points a0 a1 1))
                     (in (if (plusp inner) (reverse (arc-points a0 a1 inner)) (list (list 0 0)))))
                 (let ((points (append outer in)))
                   (values (mapcar #'first points) (mapcar #'second points))))))
        ;; wedges, then the gaps between them
        (loop for (a0 a1) in wedges
              for color in colors
              do (multiple-value-bind (xs ys) (wedge-outline a0 a1)
                   (with-color (viewport color)
                     (plot-polygon viewport xs ys :fill t))))
        (when (and gap-color (plusp gap-width) (> n 1))
          (with-style (viewport :color gap-color :line-width gap-width)
            (loop for (a0) in wedges
                  for a = (* a0 (/ pi 180))
                  do (plot-line viewport (* inner (cos a)) (* inner (sin a)) (cos a) (sin a)))))))
    ;; percentages inside wedges with room for them
    (when percentages
      (let ((radius (if (plusp inner) (/ (+ 1 inner) 2) 0.62)))
        (loop for (a0 a1) in wedges
              for v in values
              for color in colors
              for mid = (* (/ pi 180) (/ (+ a0 a1) 2))
              for text = (format nil percent-format (round (* 100 v) total))
              ;; the arc at RADIUS must hold the text, and the ring its height
              when (and (> (* radius (abs (- a1 a0)) (/ pi 180)) 0.28)
                        (or (zerop inner) (> (- 1 inner) 0.18)))
                do (let ((ink (contrasting-ink color)))
                     (flet ((draw ()
                              (plot-text viewport (* radius (cos mid)) (* radius (sin mid)) text
                                         :halign :center :valign :center :size size)))
                       (if ink (with-color (viewport ink) (draw)) (draw)))))))
    ;; names outside, with leader lines, spread apart on each side
    (when (and labels names)
      (let ((gap (* 1.25 size)) (right '()) (left '()))
        (with-viewport (viewport :clip nil)
          (loop for (a0 a1) in wedges
                for name in names
                for mid = (* (/ pi 180) (/ (+ a0 a1) 2))
                do (multiple-value-bind (cx cy) (to-canvas device :plot (* 1.12 (cos mid))
                                                          (* 1.12 (sin mid)))
                     (if (>= (cos mid) 0)
                         (push (list name mid cy cx) right)
                         (push (list name mid cy cx) left))))
          (dolist (side (list right left))
            (let ((spread (spread-labels (mapcar #'third side) gap)))
              (loop for (name mid) in side
                    for cy in spread
                    do (multiple-value-bind (ex ey) (to-canvas device :plot (* 1.02 (cos mid))
                                                              (* 1.02 (sin mid)))
                         (multiple-value-bind (lx) (to-canvas device :plot
                                                              (* (if (>= (cos mid) 0) 1.14 -1.14)
                                                                 (max (abs (cos mid)) 0.35))
                                                              0)
                           (let ((rightp (>= (cos mid) 0)))
                             (with-style (device :line-width 0.6 :color :gray)
                               (new-path device)
                               (move-to device ex ey)
                               (line-to device lx cy)
                               (line-to device (+ lx (if rightp 4 -4)) cy)
                               (stroke device))
                             (setf highest (if highest (min highest cy) cy))
                             (put-text device (+ lx (if rightp 7 -7)) cy name
                                       :halign (if rightp :left :right) :valign :center
                                       :size size))))))))))
    (when (and center-text (plusp inner))
      (plot-text viewport 0 0 center-text :halign :center :valign :center :size (* 1.2 size)))
    (when title
      ;; just above the pie and any names over it
      (with-viewport (viewport :clip nil)
        (multiple-value-bind (cx cy) (to-canvas device :plot 0 1)
          (put-text device cx (min (- cy (* 0.8 size)) (if highest (- highest (* 1.5 size)) cy))
                    title :halign :center :valign :bottom :size (* 1.15 size)))))
    (loop for name in names for color in colors collect (list name :fill color))))
