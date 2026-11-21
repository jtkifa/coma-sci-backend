;;;; areas.lisp — step plots and stacked areas
;;;;
;;;;   STEP-POINTS        the staircase through points, or over bin edges
;;;;   PLOT-STEP          a step line, optionally filled down to a baseline
;;;;   STACK-SERIES       several series stacked into layers on a baseline:
;;;;                      :ZERO, :CENTER (symmetric about zero) or :WIGGLE
;;;;                      (a streamgraph), optionally as percentages
;;;;   PLOT-STACKED-AREA  the layers filled in series colors (as BAR-CHART:
;;;;                      *CATEGORY-COLORS* in order), separated by thin
;;;;                      gaps, named by labels inside them and/or a legend
;;;;
;;;; A staircase steps between points at the point (WHERE :POST, the value
;;;; holds until the next point), before it (:PRE) or halfway (:MID).
;;;; Given one more X than Y, the Xs are bin edges and each value spans
;;;; its bin, as a histogram.
;;;;
;;;; Layers stack in the order given, or ORDER :INSIDE-OUT (the usual
;;;; streamgraph order: series peaking earliest in the middle, later ones
;;;; added alternately above and below); colors and names stay with
;;;; their series either way.

(in-package :cl-cairoplot)

(defun step-points (xs ys &key (where :mid))
  "The corners of the staircase through the points XS, YS as two lists,
   stepping at each point (WHERE :POST), before it (:PRE) or halfway
   between points (:MID).  If XS has one more element than YS, XS are
   bin edges and each value spans its bin (WHERE is ignored)."
  (let* ((xs (coerce xs 'vector)) (ys (coerce ys 'vector))
         (n (length ys)) (sx '()) (sy '()))
    (flet ((corner (x y) (push x sx) (push y sy)))
      (if (= (length xs) (1+ n))
          (dotimes (k n)
            (corner (aref xs k) (aref ys k))
            (corner (aref xs (1+ k)) (aref ys k)))
          (progn
            (corner (aref xs 0) (aref ys 0))
            (loop for k from 1 below n
                  for b = (ecase where
                            (:post (aref xs k))
                            (:pre (aref xs (1- k)))
                            (:mid (/ (+ (aref xs (1- k)) (aref xs k)) 2)))
                  do (corner b (aref ys (1- k)))
                     (corner b (aref ys k)))
            (when (> n 1) (corner (aref xs (1- n)) (aref ys (1- n)))))))
    (values (nreverse sx) (nreverse sy))))

(defplot plot-step (device xs ys &key (where :mid) fill (baseline 0) (coords :plot))
  "Stroke the staircase through XS, YS (see STEP-POINTS for WHERE and
   bin edges) in the current style.  FILL, a color (T for the current
   one), first fills under it down to BASELINE.  Returns the corners as
   two lists."
  (multiple-value-bind (sx sy) (step-points xs ys :where where)
    (when fill
      (if (eq fill t)
          (plot-fill-between device sx sy :y2 baseline :coords coords)
          (with-color (device fill)
            (plot-fill-between device sx sy :y2 baseline :coords coords))))
    (plot-polyline device sx sy :coords coords)
    (values sx sy)))

;;; ============================================================
;;; Stacking
;;; ============================================================

(defun stack-order (series order)
  "The indices of SERIES in stacking order, bottom first: ORDER NIL for
   as given, :INSIDE-OUT (see the top of the file), or a list of
   indices."
  (let ((n (length series)))
    (cond ((null order) (loop for i below n collect i))
          ((listp order) order)
          ((eq order :inside-out)
           (let* ((peaks (loop for s in series
                               collect (let ((v (coerce s 'vector)))
                                         (position (reduce #'max v) v))))
                  (by-peak (sort (loop for i below n collect i) #'< :key (lambda (i) (nth i peaks))))
                  (top 0) (bottom 0) (tops '()) (bottoms '()))
             (dolist (i by-peak)
               (let ((sum (reduce #'+ (nth i series))))
                 (if (< top bottom)
                     (progn (incf top sum) (push i tops))
                     (progn (incf bottom sum) (push i bottoms)))))
             ;; BOTTOMS were added outward downward, TOPS outward upward
             (append bottoms (reverse tops))))
          (t (error "Unknown stacking ORDER ~S." order)))))

(defun stack-series (series &key (baseline :zero) normalize order)
  "Stack SERIES (a list of equally long sequences of values >= 0, the
   first at the bottom, or in ORDER, see STACK-ORDER) into layers.  BASELINE is :ZERO, :CENTER (the
   stack symmetric about zero) or :WIGGLE (a streamgraph: the baseline
   chosen to minimize the layers' slopes, after Byron & Wattenberg 2008,
   for evenly spaced points, centered about zero on average).  NORMALIZE
   T scales each column to total 100 (percentages), a number to total
   that.  Returns values LAYERS, a list of (LOWER UPPER) double-float
   vectors, and the LOW and HIGH of the whole stack."
  (let* ((fs (mapcar (lambda (i)
                       (map '(vector double-float) (lambda (v) (float v 1d0)) (nth i series)))
                     (stack-order series order)))
         (m (reduce #'min fs :key #'length))
         (totals (make-array m :element-type 'double-float :initial-element 0d0))
         (base (make-array m :element-type 'double-float :initial-element 0d0)))
    (dolist (f fs)
      (dotimes (j m) (incf (aref totals j) (aref f j))))
    (when normalize
      (let ((sum (if (eq normalize t) 100d0 (float normalize 1d0))))
        (dolist (f fs)
          (dotimes (j m)
            (setf (aref f j) (if (zerop (aref totals j)) 0d0
                                 (/ (* sum (aref f j)) (aref totals j))))))
        (dotimes (j m)
          (unless (zerop (aref totals j)) (setf (aref totals j) sum)))))
    (ecase baseline
      (:zero)
      (:center (dotimes (j m) (setf (aref base j) (- (/ (aref totals j) 2)))))
      (:wiggle
       ;; g'(j) = -(sum_i f_i (f_i'/2 + sum_{k<i} f_k')) / sum_i f_i
       (loop for j from 1 below m
             do (let ((s1 0d0) (s2 0d0) (below 0d0))
                  (dolist (f fs)
                    (let ((df (- (aref f j) (aref f (1- j)))))
                      (incf s1 (aref f j))
                      (incf s2 (* (aref f j) (+ below (/ df 2))))
                      (incf below df)))
                  (setf (aref base j) (- (aref base (1- j))
                                         (if (zerop s1) 0d0 (/ s2 s1))))))
       ;; center the stream on zero on average
       (let ((shift (/ (loop for j below m sum (+ (aref base j) (/ (aref totals j) 2)))
                       (max m 1))))
         (dotimes (j m) (decf (aref base j) shift)))))
    (let ((lower (copy-seq base)) (layers '()) (lo 0d0) (hi 0d0))
      (dolist (f fs)
        (let ((upper (make-array m :element-type 'double-float)))
          (dotimes (j m) (setf (aref upper j) (+ (aref lower j) (aref f j))))
          (push (list lower upper) layers)
          (setf lower upper)))
      (setf layers (nreverse layers))
      (when (plusp m)
        (setf lo (reduce #'min (first (first layers)))
              hi (reduce #'max (second (car (last layers))))))
      (values layers lo hi))))

;;; ============================================================
;;; Stacked areas
;;; ============================================================

(defun stacked-label-place (device xs lower upper width size coords)
  "The index of the point where a label WIDTH wide and SIZE high fits
   best inside the layer LOWER..UPPER (the most room to spare over the
   label's width), or NIL if it fits nowhere."
  (let* ((m (length lower))
         (cx (make-array m)) (thick (make-array m)))
    (dotimes (j m)
      (multiple-value-bind (x y1) (to-canvas device coords (elt xs j) (aref lower j))
        (multiple-value-bind (x2 y2) (to-canvas device coords (elt xs j) (aref upper j))
          (declare (ignore x2))
          (setf (aref cx j) x (aref thick j) (abs (- y1 y2))))))
    (let ((best nil) (best-room 0) (half (/ width 2))
          (left (min (aref cx 0) (aref cx (1- m))))
          (right (max (aref cx 0) (aref cx (1- m)))))
      (dotimes (j m best)
        ;; clear of the ends by half a text height
        (when (<= (+ left half (* 0.5 size)) (aref cx j) (- right half (* 0.5 size)))
          (let ((room (loop for k below m
                            when (<= (abs (- (aref cx k) (aref cx j))) (+ half 3))
                              minimize (aref thick k))))
            (when (and (>= room (* 1.5 size)) (> room best-room))
              (setf best j best-room room))))))))

(defplot plot-stacked-area (device xs series &key names colors (baseline :zero) normalize
                                                  order step (where :mid) (gap-color :white)
                                                  (gap-width 1) labels label-size
                                                  (coords :plot))
  "Stack SERIES (sequences of values >= 0 at XS, the first at the
   bottom, or in ORDER, see STACK-ORDER; see STACK-SERIES for BASELINE
   and NORMALIZE) and fill each layer in its series' color: COLORS, or
   *CATEGORY-COLORS* in order.  Layers are separated by GAP-WIDTH lines
   of GAP-COLOR.  STEP draws them as staircases (WHERE as for
   STEP-POINTS; with one more X than values per series, XS are bin
   edges).  LABELS writes each of NAMES inside its layer where it fits
   (at LABEL-SIZE, in the current color).  Returns legend entries for
   DRAW-LEGEND, top layer first, and the LOW and HIGH of the stack."
  (let* ((indices (stack-order series order))
         (colors (let ((all (category-colors (length series) colors)))
                   (mapcar (lambda (i) (nth i all)) indices)))
         (names (and names (mapcar (lambda (i) (nth i names)) indices))))
    (multiple-value-bind (layers lo hi)
        (stack-series series :baseline baseline :normalize normalize :order indices)
      (let* ((edges (= (length xs) (1+ (length (first (first layers))))))
             (size (or label-size (* 0.9 (default-text-size device)))))
        (flet ((outline (ys)
                 ;; the boundary curve over XS
                 (if (or step edges)
                     (step-points xs ys :where where)
                     (values (coerce xs 'list) (coerce ys 'list)))))
          (loop for (lower upper) in layers
                for color in colors
                do (multiple-value-bind (ux uy) (outline upper)
                     (multiple-value-bind (lx ly) (outline lower)
                       (with-color (device color)
                         (plot-polygon device (append ux (reverse lx)) (append uy (reverse ly))
                                       :fill t :coords coords)))))
          (when (and gap-color (plusp gap-width))
            (with-style (device :color gap-color :line-width gap-width)
              (loop for (lower) in (rest layers)
                    do (multiple-value-bind (x y) (outline lower)
                         (plot-polyline device x y :coords coords)))))
          (when (and labels names)
            ;; label positions at points, or at bin centers over edges
            (let ((label-xs (if edges
                                (loop for k below (1- (length xs))
                                      collect (/ (+ (elt xs k) (elt xs (1+ k))) 2))
                                xs)))
              (loop for (lower upper) in layers
                    for name in names
                    for width = (* (text-size device name)
                                   (/ size (font-size (current-font device))))
                    for j = (stacked-label-place device label-xs lower upper width size coords)
                    when j
                      do (plot-text device (elt label-xs j) (/ (+ (aref lower j) (aref upper j)) 2)
                                    name :halign :center :valign :center :size size
                                    :coords coords)))))
        (values (loop for name in (reverse names)
                      for color in (reverse colors)
                      collect (list name :fill color))
                lo hi)))))
