;;;; vectors.lisp — vector fields: arrows and streamlines
;;;;
;;;;   PLOT-VECTORS       an arrow (U,V) at each point (X,Y), as
;;;;                      matplotlib's quiver; headless for polarization
;;;;                      or other orientations (:HEADS NIL :PIVOT :MIDDLE)
;;;;   PLOT-VECTOR-FIELD  arrows of a function (X Y) -> (VALUES U V) on a
;;;;                      grid, scaled so the longest nearly fills a cell
;;;;   PLOT-STREAMLINES   evenly spaced streamlines of such a function,
;;;;                      with arrowheads showing the direction
;;;;
;;;; Vectors are in plot units: the arrow at (X,Y) runs to (X + S U, Y +
;;;; S V) for the scale S, so with unequal x and y scales its direction
;;;; on the page follows the window (use SET-WINDOW-EQUAL-SCALES for true
;;;; angles).  Arrowheads are in canvas units, shrunk on short arrows.

(in-package :cl-cairoplot)

(defun stroke-arrow (device cx1 cy1 cx2 cy2 head-size head-angle)
  "Draw an arrow between canvas points with a filled head HEAD-SIZE long
   (none if NIL), shrunk to 40% of a short arrow's length."
  (let ((length (sqrt (+ (expt (- cx2 cx1) 2) (expt (- cy2 cy1) 2)))))
    (if (and head-size (plusp length))
        (let ((head (min head-size (* 0.4 length))))
          (new-path device)
          (multiple-value-bind (ex ey) (arrowhead-path device cx1 cy1 cx2 cy2 head head-angle)
            (fill-path device)
            (new-path device)
            (move-to device cx1 cy1)
            (line-to device ex ey)
            (stroke device)))
        (progn (new-path device)
               (move-to device cx1 cy1)
               (line-to device cx2 cy2)
               (stroke device)))))

(defplot plot-vectors (device xs ys us vs &key (scale 1) (pivot :tail) (heads t) head-size
                                             (head-angle 22) colormap vmin vmax colors
                                             (coords :plot))
  "Draw an arrow for each vector (U,V) of sequences US, VS at the points
   XS, YS, SCALE plot units per unit of U and V.  PIVOT puts the point at
   the arrow's :TAIL, :MIDDLE or :TIP.  HEADS NIL draws plain segments
   (orientations, e.g. polarization).  HEAD-SIZE is in canvas units
   (default 0.5 of DEFAULT-TEXT-SIZE).  COLORMAP colors the arrows by
   length, VMIN..VMAX (default the lengths' range); COLORS gives each a
   color instead; otherwise they are in the current style.  Returns
   VMIN and VMAX of the lengths, for DRAW-COLORBAR."
  (let* ((xs (coerce xs 'list)) (ys (coerce ys 'list))
         (us (coerce us 'list)) (vs (coerce vs 'list))
         (lengths (mapcar (lambda (u v) (sqrt (+ (* u u) (* v v)))) us vs))
         (head-size (and heads (or head-size (* 0.5 (default-text-size device)))))
         (lo nil) (hi nil))
    (when colormap
      (multiple-value-setq (colors lo hi)
        (values-to-colors lengths :colormap colormap :vmin vmin :vmax vmax)))
    (loop for x in xs for y in ys for u in us for v in vs
          for k from 0
          do (let* ((du (* scale u)) (dv (* scale v))
                    (f (ecase pivot (:tail 0) (:middle 0.5) (:tip 1)))
                    (x1 (- x (* f du))) (y1 (- y (* f dv))))
               (multiple-value-bind (cx1 cy1) (to-canvas device coords x1 y1)
                 (multiple-value-bind (cx2 cy2) (to-canvas device coords (+ x1 du) (+ y1 dv))
                   (let ((color (and colors (nth k colors))))
                     (if color
                         (with-color (device color)
                           (stroke-arrow device cx1 cy1 cx2 cy2 head-size head-angle))
                         (stroke-arrow device cx1 cy1 cx2 cy2 head-size head-angle)))))))
    (values lo hi)))

(defun grid-centers (lo hi n)
  "N cell centers evenly spanning LO..HI."
  (loop for k below n collect (+ lo (* (+ k 0.5) (/ (- hi lo) n)))))

(defplot plot-vector-field (device function x-range y-range &rest keys
                                   &key (n 20) ny scale (pivot :middle) (coords :plot)
                                   &allow-other-keys)
  "Draw arrows of FUNCTION, of X and Y returning values U and V, at the
   centers of an N by NY grid (NY default from the window's shape) over
   X-RANGE and Y-RANGE, lists (LO HI).  SCALE defaults to make the
   longest arrow 0.9 of a cell.  Arrows are centered on the grid points
   (PIVOT :MIDDLE).  Other keywords go to PLOT-VECTORS."
  (destructuring-bind (x1 x2) x-range
    (destructuring-bind (y1 y2) y-range
      (let* ((ny (or ny
                     ;; cells about square on the page
                     (multiple-value-bind (ax ay) (to-canvas device coords x1 y1)
                       (multiple-value-bind (bx by) (to-canvas device coords x2 y2)
                         (max 1 (round (* n (/ (abs (- by ay)) (max 1d-9 (abs (- bx ax)))))))))))
             (xs '()) (ys '()) (us '()) (vs '()))
        (dolist (y (grid-centers y1 y2 ny))
          (dolist (x (grid-centers x1 x2 n))
            (multiple-value-bind (u v) (funcall function x y)
              (push x xs) (push y ys) (push u us) (push v vs))))
        (let* ((longest (loop for u in us for v in vs maximize (sqrt (+ (* u u) (* v v)))))
               (cell (min (/ (abs (- x2 x1)) n) (/ (abs (- y2 y1)) ny)))
               (scale (or scale (if (plusp longest) (/ (* 0.9 cell) longest) 1))))
          (apply #'plot-vectors device xs ys us vs :scale scale :pivot pivot :coords coords
                 (loop for (k v) on keys by #'cddr
                       unless (member k '(:n :ny :scale :pivot :coords)) append (list k v))))))))

;;; ============================================================
;;; Streamlines
;;; ============================================================
;;;
;;; As matplotlib's streamplot: the domain is divided into a grid of
;;; cells (30 x 30 times DENSITY); from each empty cell a streamline is
;;; traced both ways (second-order Runge-Kutta, at unit speed in cell
;;; units so lines advance evenly) until it leaves the domain, stalls,
;;; or enters a cell another line (or itself) already crossed.  The
;;; cells it crosses are then taken, which keeps lines evenly spaced.

(defun trace-streamline (function x y x-range y-range nx ny taken own direction)
  "The points (X Y) of a streamline from X, Y followed in DIRECTION (+1
   or -1), marking the cells it enters in OWN; stops on leaving the
   domain, stalling or entering a cell of TAKEN or OWN."
  (destructuring-bind (x1 x2) x-range
    (destructuring-bind (y1 y2) y-range
      (let* ((cw (/ (- x2 x1) nx)) (ch (/ (- y2 y1) ny))
             (step 0.2) (points (list (list x y)))
             (cell (list (floor (- x x1) cw) (floor (- y y1) ch))))
        (flet ((velocity (x y)
                 ;; in cell units per unit time, normalized to unit speed
                 (multiple-value-bind (u v) (funcall function x y)
                   (let* ((gu (/ u cw)) (gv (/ v ch))
                          (speed (sqrt (+ (* gu gu) (* gv gv)))))
                     (if (< speed 1d-12)
                         (values nil nil)
                         (values (/ (* direction gu) speed) (/ (* direction gv) speed)))))))
          (loop repeat (* 4 (+ nx ny) (round 1 step))
                do (multiple-value-bind (gu gv) (velocity x y)
                     (unless gu (return))
                     (let ((mx (+ x (* 0.5 step gu cw))) (my (+ y (* 0.5 step gv ch))))
                       (unless (and (<= x1 mx x2) (<= y1 my y2)) (return))
                       (multiple-value-bind (hu hv) (velocity mx my)
                         (unless hu (return))
                         (setf x (+ x (* step hu cw)) y (+ y (* step hv ch))))))
                   (unless (and (<= x1 x x2) (<= y1 y y2)) (return))
                   (let ((here (list (min (1- nx) (floor (- x x1) cw))
                                     (min (1- ny) (floor (- y y1) ch)))))
                     (unless (equal here cell)
                       (when (or (gethash here taken) (gethash here own)) (return))
                       (setf (gethash here own) t cell here)))
                   (push (list x y) points)))
        (nreverse points)))))

(defplot plot-streamlines (device function x-range y-range &key (density 1) (arrows t)
                                                             head-size (min-length 3)
                                                             (coords :plot))
  "Draw streamlines of FUNCTION (X Y -> U V) over X-RANGE and Y-RANGE,
   lists (LO HI), about 30 x DENSITY lines across (see above), in the
   current style.  ARROWS puts an arrowhead (HEAD-SIZE canvas units,
   default 0.5 of DEFAULT-TEXT-SIZE) midway along each line.  Lines
   crossing fewer than MIN-LENGTH cells are dropped."
  (destructuring-bind (x1 x2) x-range
    (destructuring-bind (y1 y2) y-range
      (let* ((nx (max 2 (round (* 30 density)))) (ny nx)
             (taken (make-hash-table :test 'equal))
             (head-size (or head-size (* 0.5 (default-text-size device)))))
        (dotimes (j ny)
          (dotimes (i nx)
            (unless (gethash (list i j) taken)
              (let* ((own (make-hash-table :test 'equal))
                     (x (+ x1 (* (+ i 0.5) (/ (- x2 x1) nx))))
                     (y (+ y1 (* (+ j 0.5) (/ (- y2 y1) ny)))))
                (setf (gethash (list i j) own) t)
                (let* ((ahead (trace-streamline function x y x-range y-range nx ny taken own 1))
                       (behind (trace-streamline function x y x-range y-range nx ny taken own -1))
                       (line (append (reverse (rest behind)) ahead)))
                  (when (>= (hash-table-count own) min-length)
                    (maphash (lambda (cell v) (declare (ignore v)) (setf (gethash cell taken) t))
                             own)
                    (plot-polyline device (mapcar #'first line) (mapcar #'second line)
                                   :coords coords)
                    (when (and arrows (> (length line) 2))
                      (let* ((m (floor (length line) 2))
                             (a (nth (1- m) line)) (b (nth m line)))
                        (multiple-value-bind (ax ay) (to-canvas device coords (first a) (second a))
                          (multiple-value-bind (bx by) (to-canvas device coords (first b) (second b))
                            (new-path device)
                            (arrowhead-path device ax ay bx by head-size 22)
                            (fill-path device)))))))))))))))
