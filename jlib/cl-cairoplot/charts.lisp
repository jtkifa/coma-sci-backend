;;;; charts.lisp — error bars, filled areas, histograms and arrows
;;;;
;;;; PLOT-ERROR-BARS (as PGPLOT's PGERRB), PLOT-FILL-BETWEEN, HISTOGRAM
;;;; and PLOT-HISTOGRAM (as PGHIST/PGBIN), PLOT-ARROW (as PGARRO) and
;;;; ANNOTATE.  All take &KEY COORDS (default :PLOT) and draw with the
;;;; current style.  Sizes of caps and arrowheads are in canvas units,
;;;; so they look the same whatever the plot scales, and they are drawn
;;;; square to their line on the page.
;;;;
;;;; On a log axis (window in log10 units), pass log10 values, e.g. the
;;;; log10 of y - error and y + error as the lower and upper ends.

(in-package :cl-cairoplot)

(defun nth-value-of (spec k)
  "SPEC if a number, else its K-th element (NIL spec: NIL)."
  (cond ((null spec) nil)
        ((realp spec) spec)
        (t (elt spec k))))

;;; ============================================================
;;; Arrows
;;; ============================================================

(defun arrowhead-path (device x1 y1 x2 y2 head-size head-angle)
  "Add a closed arrowhead at canvas point (X2,Y2), pointing away from
   (X1,Y1).  Returns the canvas point where the shaft should end."
  (let* ((dx (- x2 x1)) (dy (- y2 y1)) (len (sqrt (+ (* dx dx) (* dy dy)))))
    (if (zerop len)
        (values x2 y2)
        (let* ((ux (/ dx len)) (uy (/ dy len))
               (a (* head-angle (/ pi 180)))
               (back (* head-size (cos a))) (side (* head-size (sin a))))
          (move-to device x2 y2)
          (line-to device (- x2 (* back ux) (* side uy)) (- y2 (* back uy) (- (* side ux))))
          (line-to device (- x2 (* back ux) (- (* side uy))) (- y2 (* back uy) (* side ux)))
          (close-path device)
          (values (- x2 (* 0.8 back ux)) (- y2 (* 0.8 back uy)))))))

(defplot plot-arrow (device x1 y1 x2 y2 &key (coords :plot) head-size (head-angle 22)
                                         (fill t) both)
  "Draw an arrow from (X1,Y1) to (X2,Y2) in COORDS, with a head HEAD-SIZE
   canvas units long (default 0.6 of DEFAULT-TEXT-SIZE) and HEAD-ANGLE
   degrees each side, filled unless FILL is NIL; BOTH puts a head at
   each end."
  (let ((head-size (or head-size (* 0.6 (default-text-size device)))))
    (multiple-value-bind (cx1 cy1) (to-canvas device coords x1 y1)
      (multiple-value-bind (cx2 cy2) (to-canvas device coords x2 y2)
        (new-path device)
        (multiple-value-bind (ex ey) (arrowhead-path device cx1 cy1 cx2 cy2 head-size head-angle)
          (multiple-value-bind (sx sy)
              (if both
                  (arrowhead-path device cx2 cy2 cx1 cy1 head-size head-angle)
                  (values cx1 cy1))
            (if fill (fill-path device) (stroke device))
            (new-path device)
            (move-to device sx sy)
            (line-to device ex ey)
            (stroke device)))))))

(defplot annotate (device x y text tx ty &key (coords :plot) (gap 3) head-size halo
                                           (halign :center) (valign :center) size font)
  "Write TEXT at (TX,TY) with an arrow from it to the point (X,Y), both
   in COORDS.  The arrow starts GAP canvas units outside the text's box."
  (multiple-value-bind (px py) (to-canvas device coords x y)
    (multiple-value-bind (cx cy) (to-canvas device coords tx ty)
      (let ((width (put-text device cx cy text :halign halign :valign valign
                                               :font font :size size :halo halo)))
        (multiple-value-bind (w ascent descent)
            (with-saved-state (device)
              (when (or font size) (set-font device (or font (current-font device)) :size size))
              (text-size device text))
          (declare (ignore w))
          ;; start where the line to the point leaves the text's box
          (let* ((left (- cx (* (halign-fraction halign) width)))
                 (mid-x (+ left (/ width 2)))
                 (mid-y (ecase valign
                          (:center cy) (:baseline (- cy (/ (- ascent descent) 2)))
                          (:top (+ cy (/ (+ ascent descent) 2)))
                          (:bottom (- cy (/ (+ ascent descent) 2)))))
                 (hw (+ (/ width 2) gap)) (hh (+ (/ (+ ascent descent) 2) gap))
                 (dx (- px mid-x)) (dy (- py mid-y))
                 (k (min (if (zerop dx) most-positive-double-float (/ hw (abs dx)))
                         (if (zerop dy) most-positive-double-float (/ hh (abs dy))))))
            (when (< k 1)
              (plot-arrow device (+ mid-x (* k dx)) (+ mid-y (* k dy)) px py
                          :coords :canvas :head-size head-size))))))))

;;; ============================================================
;;; Error bars
;;; ============================================================

(defplot plot-error-bars (device xs ys &key yerr xerr yerr-lower yerr-upper
                                         xerr-lower xerr-upper cap-size
                                         (coords :plot) halo)
  "Draw error bars at the points XS, YS.  YERR (a number or a sequence)
   gives symmetric y errors; YERR-LOWER and YERR-UPPER asymmetric ones
   (overriding YERR on their side); likewise XERR, XERR-LOWER,
   XERR-UPPER.  Each bar ends in a cap CAP-SIZE canvas units across
   (default 0.4 of DEFAULT-TEXT-SIZE; 0 for none).  HALO outlines the
   bars (see WITH-HALO)."
  (let ((m (coords-transform device coords))
        (cap (or cap-size (* 0.4 (default-text-size device))))
        (n (min (length xs) (length ys))))
    ;; build-path is called inside with-batched-graphics
    (flet ((build-path ()
             (new-path/tus device)
             (dotimes (k n)
               (let ((x (elt xs k)) (y (elt ys k)))
                 (flet ((bar (x1 y1 x2 y2)
                          (multiple-value-bind (ax ay) (affine-apply m x1 y1)
                            (multiple-value-bind (bx by) (affine-apply m x2 y2)
                              (move-to/tus device ax ay)
                              (line-to/tus device bx by)
                              (let* ((dx (- bx ax)) (dy (- by ay))
                                     (len (sqrt (+ (* dx dx) (* dy dy)))))
                                (when (and (plusp cap) (plusp len))
                                  ;; caps square to the bar on the page
                                  (let ((px (* (/ cap 2) (/ (- dy) len)))
                                        (py (* (/ cap 2) (/ dx len))))
                                    (dolist (end (list (list ax ay) (list bx by)))
                                      (destructuring-bind (ex ey) end
                                        (move-to/tus device (- ex px) (- ey py))
                                        (line-to/tus device (+ ex px) (+ ey py)))))))))))
                   (let ((lo (or (nth-value-of yerr-lower k) (nth-value-of yerr k)))
                         (hi (or (nth-value-of yerr-upper k) (nth-value-of yerr k))))
                     (when (or lo hi) (bar x (- y (or lo 0)) x (+ y (or hi 0)))))
                   (let ((lo (or (nth-value-of xerr-lower k) (nth-value-of xerr k)))
                         (hi (or (nth-value-of xerr-upper k) (nth-value-of xerr k))))
                     (when (or lo hi) (bar (- x (or lo 0)) y (+ x (or hi 0)) y))))))))
      ;; each build + stroke wrapped in one lock acquisition
      (do-halo-layers (device halo (line-width device) color width)
        (with-batched-graphics (device)
          (build-path)
          (stroke-halo device color width (line-width device))))
      (with-batched-graphics (device)
        (build-path)
        (stroke device)))))

;;; ============================================================
;;; Filled areas
;;; ============================================================

(defplot plot-fill-between (device xs y1s &key (y2 0) (coords :plot) outline)
  "Fill the area between the curves (XS, Y1S) and (XS, Y2) with the
   current color; Y2 is a sequence, or a number for a baseline (default
   0).  OUTLINE also strokes both curves."
  (let* ((n (min (length xs) (length y1s)))
         (lower (loop for k below n collect (nth-value-of y2 k)))
         (xs (coerce (subseq xs 0 n) 'list))
         (upper (coerce (subseq y1s 0 n) 'list)))
    (plot-polygon device (append xs (reverse xs)) (append upper (reverse lower))
                  :fill t :coords coords)
    (when outline
      (plot-polyline device xs upper :coords coords)
      (plot-polyline device xs lower :coords coords))))

;;; ============================================================
;;; Histograms
;;; ============================================================

(defun histogram (values &key (bins :auto) range weights)
  "Bin VALUES (a sequence; NaNs ignored).  BINS is a number of equal
   bins, a sequence of bin edges, or :AUTO (the Freedman-Diaconis rule,
   at least 1 bin).  RANGE, a list (LO HI), limits the bins (default the
   data's range); values outside are dropped, and the last bin includes
   its upper edge.  WEIGHTS, a sequence, weights each value.  Returns
   values EDGES (N+1) and COUNTS (N), double-float vectors."
  (let* ((data (remove-if #'nan-p (coerce values 'list)))
         (weights (and weights (coerce weights 'list)))
         (lo (if range (first range) (reduce #'min data)))
         (hi (if range (second range) (reduce #'max data)))
         (hi (if (= hi lo) (+ lo 1) hi))
         (edges
           (if (typep bins 'sequence)
               (map '(vector double-float) (lambda (e) (float e 1d0)) bins)
               (let ((n (if (eq bins :auto)
                            (let* ((sorted (sort (map 'vector (lambda (v) (float v 1d0)) data) #'<))
                                   (count (length sorted))
                                   (iqr (- (aref sorted (floor (* 0.75 (1- count))))
                                           (aref sorted (floor (* 0.25 (1- count))))))
                                   (width (* 2 iqr (expt count -1/3))))
                              (if (plusp width)
                                  (max 1 (ceiling (- hi lo) width))
                                  (max 1 (ceiling (sqrt count)))))
                            bins)))
                 (let ((e (make-array (1+ n) :element-type 'double-float)))
                   (dotimes (k (1+ n) e)
                     (setf (aref e k) (float (+ lo (* k (/ (- hi lo) n))) 1d0)))))))
         (nbins (1- (length edges)))
         (counts (make-array nbins :element-type 'double-float :initial-element 0d0)))
    (loop for v in data
          for k from 0
          for w = (if weights (float (nth k weights) 1d0) 1d0)
          do (let ((bin (cond ((< v (aref edges 0)) nil)
                              ((> v (aref edges nbins)) nil)
                              ((= v (aref edges nbins)) (1- nbins))
                              (t (1- (or (position-if (lambda (e) (> e v)) edges) 1))))))
               (when bin (incf (aref counts bin) w))))
    (values edges counts)))

(defplot plot-histogram (device values &key (bins :auto) range weights edges counts
                                          (style :step) normalize cumulative (baseline 0)
                                          (bar-width 0.8) (coords :plot))
  "Bin VALUES (see HISTOGRAM; or give EDGES and COUNTS directly) and draw
   the histogram with the current style.  STYLE :STEP outlines it,
   :FILLED fills it (the current color; outline it separately), :BARS
   draws separate bars BAR-WIDTH of each bin wide.  NORMALIZE :DENSITY
   divides by the total and the bin width (area 1), :FRACTION by the
   total.  CUMULATIVE sums the counts.  BASELINE is the bottom (use a
   small positive number on a log axis).  Returns the EDGES and the
   COUNTS drawn."
  (multiple-value-bind (edges counts)
      (if (and edges counts)
          (values (map '(vector double-float) (lambda (e) (float e 1d0)) edges)
                  (map '(vector double-float) (lambda (c) (float c 1d0)) counts))
          (histogram values :bins bins :range range :weights weights))
    (let* ((n (length counts))
           (total (reduce #'+ counts))
           (heights (make-array n :element-type 'double-float)))
      (dotimes (k n)
        (setf (aref heights k)
              (let ((c (aref counts k)))
                (ecase normalize
                  ((nil) c)
                  (:fraction (if (zerop total) 0d0 (/ c total)))
                  (:density (if (zerop total) 0d0
                                (/ c total (- (aref edges (1+ k)) (aref edges k)))))))))
      (when cumulative
        (loop for k from 1 below n do (incf (aref heights k) (aref heights (1- k)))))
      (ecase style
        ((:step :filled)
         ;; the outline: up the first edge, across each bin, down the last
         (let ((xs (list (aref edges 0))) (ys (list baseline)))
           (dotimes (k n)
             (push (aref edges k) xs) (push (aref heights k) ys)
             (push (aref edges (1+ k)) xs) (push (aref heights k) ys))
           (push (aref edges n) xs) (push baseline ys)
           (setf xs (nreverse xs) ys (nreverse ys))
           (if (eq style :filled)
               (plot-polygon device xs ys :fill t :coords coords)
               (plot-polyline device xs ys :coords coords))))
        (:bars
         (dotimes (k n)
           (let* ((x1 (aref edges k)) (x2 (aref edges (1+ k)))
                  (pad (* (- 1 bar-width) (/ (- x2 x1) 2))))
             (plot-rectangle device (+ x1 pad) baseline (- x2 pad) (aref heights k)
                             :fill t :coords coords)))))
      (values edges heights))))

;;; ============================================================
;;; Colors and sizes from values (for PLOT-POINTS :COLORS, :SIZES)
;;; ============================================================

(defun value-range (values vmin vmax)
  "VMIN and VMAX, defaulting to the range of the non-NaN VALUES."
  (let ((data (remove-if #'nan-p (coerce values 'list))))
    (values (or vmin (reduce #'min data)) (or vmax (reduce #'max data)))))

(defun values-to-colors (values &key (colormap :viridis) vmin vmax (scale :linear) reverse)
  "A list of colors (R G B), one per element of VALUES, through COLORMAP
   with VMIN, VMAX (default the values' range), SCALE and REVERSE as for
   PLOT-IMAGE; NaNs give NIL.  Second and third values: VMIN and VMAX,
   for DRAW-COLORBAR."
  (multiple-value-bind (lo hi) (value-range values vmin vmax)
    (let ((cm (parse-colormap (if reverse (reverse-colormap colormap) colormap)))
          (f (scale-function scale))
          (range (if (= hi lo) 1 (- hi lo))))
      (values (map 'list (lambda (v)
                           (unless (nan-p v)
                             (multiple-value-list
                              (colormap-rgb cm (funcall f (max 0 (min 1 (/ (- v lo) range))))))))
                   values)
              lo hi))))

(defun values-to-sizes (values &key (min-size 1.5) (max-size 8) vmin vmax)
  "A list of marker sizes, one per element of VALUES, from MIN-SIZE at
   VMIN to MAX-SIZE at VMAX (default the values' range), with marker
   area proportional to the value.  Second and third values: VMIN, VMAX."
  (multiple-value-bind (lo hi) (value-range values vmin vmax)
    (let ((range (if (= hi lo) 1 (- hi lo))))
      (values (map 'list (lambda (v)
                           (let ((f (if (nan-p v) 0 (max 0 (min 1 (/ (- v lo) range))))))
                             (sqrt (+ (* min-size min-size)
                                      (* f (- (* max-size max-size) (* min-size min-size)))))))
                   values)
              lo hi))))

;;; ============================================================
;;; 2D histograms
;;; ============================================================

(defun histogram2d (xs ys &key (bins 40) x-range y-range weights)
  "Bin the points XS, YS (NaNs ignored) on a grid: BINS is a number for
   both axes or a list (NX NY); X-RANGE and Y-RANGE (lists (LO HI))
   default to the data's.  WEIGHTS weights each point.  Returns values
   COUNTS, a double-float array (NY NX) with rows along y as PLOT-IMAGE
   expects, X-EDGES and Y-EDGES."
  (destructuring-bind (nx ny) (if (consp bins) bins (list bins bins))
    (multiple-value-bind (x1 x2) (if x-range (values-list x-range) (value-range xs nil nil))
      (multiple-value-bind (y1 y2) (if y-range (values-list y-range) (value-range ys nil nil))
        (let ((counts (make-array (list ny nx) :element-type 'double-float :initial-element 0d0))
              (x-edges (make-array (1+ nx) :element-type 'double-float))
              (y-edges (make-array (1+ ny) :element-type 'double-float)))
          (dotimes (k (1+ nx)) (setf (aref x-edges k) (float (+ x1 (* k (/ (- x2 x1) nx))) 1d0)))
          (dotimes (k (1+ ny)) (setf (aref y-edges k) (float (+ y1 (* k (/ (- y2 y1) ny))) 1d0)))
          (dotimes (k (min (length xs) (length ys)))
            (let ((x (elt xs k)) (y (elt ys k)))
              (unless (or (nan-p x) (nan-p y))
                (let ((i (floor (* nx (/ (- x x1) (- x2 x1)))))
                      (j (floor (* ny (/ (- y y1) (- y2 y1))))))
                  ;; the top edges belong to the last bins
                  (when (= x x2) (setf i (1- nx)))
                  (when (= y y2) (setf j (1- ny)))
                  (when (and (< -1 i nx) (< -1 j ny))
                    (incf (aref counts j i) (if weights (float (elt weights k) 1d0) 1d0)))))))
          (values counts x-edges y-edges))))))

(defplot plot-histogram2d (device xs ys &key (bins 40) x-range y-range weights
                                          (colormap :viridis) vmin vmax (scale :linear)
                                          reverse empty (interpolation :nearest) (coords :plot))
  "Bin the points XS, YS (see HISTOGRAM2D) and draw the counts as an
   image through COLORMAP (VMIN default 0, VMAX the largest count;
   SCALE, REVERSE as PLOT-IMAGE).  Empty bins are left transparent
   unless EMPTY is T (then they take the colormap's low end).  Returns
   VMIN, VMAX and the COUNTS array."
  (multiple-value-bind (counts x-edges y-edges)
      (histogram2d xs ys :bins bins :x-range x-range :y-range y-range :weights weights)
    (let ((image (if empty
                     counts
                     (let ((a (make-array (array-dimensions counts) :element-type 'double-float)))
                       (dotimes (k (array-total-size counts) a)
                         (let ((c (row-major-aref counts k)))
                           (setf (row-major-aref a k)
                                 (if (zerop c) float-utils:+double-float-nan+ c))))))))
      (multiple-value-bind (lo hi)
          (plot-image device image :colormap colormap :scale scale :reverse reverse
                                   :vmin (or vmin 0)
                                   :vmax (or vmax (max 1 (reduce #'max (make-array (array-total-size counts)
                                                                                   :element-type 'double-float
                                                                                   :displaced-to counts))))
                                   :x-range (list (aref x-edges 0) (aref x-edges (1- (length x-edges))))
                                   :y-range (list (aref y-edges 0) (aref y-edges (1- (length y-edges))))
                                   :interpolation interpolation :coords coords)
        (values lo hi counts)))))
