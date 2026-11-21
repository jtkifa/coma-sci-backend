;;;; categorical.lisp — bar charts, box plots and violin plots
;;;;
;;;; Charts over categories, which sit at positions 0, 1, 2, ... along
;;;; the category axis (labeled by CATEGORY-TICKS in DRAW-BOX):
;;;;
;;;;   PLOT-BARS        bars at given positions, with error bars and
;;;;                    value labels (the building block)
;;;;   BAR-CHART        a whole chart in a viewport: grouped or stacked
;;;;                    series, vertical or horizontal, window, axes,
;;;;                    category names and legend
;;;;   PLOT-BOX-PLOT    box-and-whisker plots of datasets (BOX-STATS)
;;;;   PLOT-VIOLINS     violin plots (mirrored kernel density estimates,
;;;;                    KDE), with the box inside
;;;;
;;;; Series colors come from *CATEGORY-COLORS* in order (never cycled
;;;; past its eight: fold extra series together instead).  Bars that
;;;; touch, and stacked segments, are separated by a thin GAP-COLOR
;;;; outline; value labels are in the current (text) color, not the
;;;; bar's.

(in-package :cl-cairoplot)

(defparameter *category-colors*
  '("#2a78d6" "#eb6834" "#1baf7a" "#eda100" "#e87ba4" "#008300" "#6250d6" "#e34948")
  "Series colors in order: blue, orange, aqua, yellow, magenta, green,
   violet, red (a palette checked for color-vision deficiency, adjacent
   pairs distinct).")

(defun category-colors (n &optional colors)
  "N series colors: COLORS if given, else the first N of *CATEGORY-COLORS*."
  (let ((colors (or colors *category-colors*)))
    (when (> n (length colors))
      (error "~D series but only ~D colors; give :COLORS, or combine series." n
             (length colors)))
    (subseq colors 0 n)))

(defun nth-spec (spec k)
  "SPEC if an atom (a number or a color keyword), else its K-th element."
  (if (and spec (or (consp spec) (vectorp spec)) (not (stringp spec)))
      (elt spec k)
      spec))

(defun default-value-text (value)
  "VALUE as text: an integer as is, else to 3 significant figures."
  (if (or (integerp value) (= value (fround value)))
      (format nil "~D" (round value))
      (let* ((digits (max 0 (- 2 (floor (log (abs value) 10))))))
        (format nil "~,vF" digits (float value 1d0)))))

;;; ============================================================
;;; Bars
;;; ============================================================

(defun error-ends (errors k)
  "Values LOWER UPPER of the error spec ERRORS for bar K: a number, a
   sequence of numbers, or a sequence of (LOWER UPPER)."
  (let ((e (nth-spec errors k)))
    (cond ((null e) (values 0 0))
          ((consp e) (values (first e) (second e)))
          (t (values e e)))))

(defplot plot-bars (device positions values &key (width 0.8) (bottoms 0) (orientation :vertical)
                                                color colors (gap-color :white) (gap-width 1)
                                                outline errors cap-size value-labels
                                                (value-format #'default-value-text) texts
                                                label-size (bars t) (coords :plot))
  "Draw a bar at each of POSITIONS (along the category axis) from its
   bottom (BOTTOMS, a number or sequence) to bottom + its VALUE, WIDTH
   wide (a number or sequence).  ORIENTATION :VERTICAL (columns) or
   :HORIZONTAL (bars along x).  Fill: COLORS (a color per bar), else
   COLOR, else the current color.  GAP-COLOR outlines each bar
   GAP-WIDTH canvas units wide (NIL for none), separating touching bars;
   OUTLINE, a color, strokes it with the current line width.  ERRORS (a
   number, a sequence, or a sequence of (LOWER UPPER)) adds error bars
   at the ends, CAP-SIZE wide.  VALUE-LABELS writes each value, by
   VALUE-FORMAT (or the strings TEXTS), just beyond its bar's end (and
   error bar), in LABEL-SIZE text.  BARS NIL draws only the labels and
   error bars."
  (let* ((vertical (ecase orientation (:vertical t) (:horizontal nil)))
         (n (min (length positions) (length values)))
         (positions (coerce positions 'list)) (values (coerce values 'list))
         (label-size (or label-size (* 0.8 (default-text-size device))))
         (pad (* 0.3 label-size)))
    (flet ((corners (k)
             (let* ((p (nth k positions)) (v (nth k values))
                    (b (nth-spec bottoms k)) (w (/ (nth-spec width k) 2)))
               (if vertical
                   (values (- p w) b (+ p w) (+ b v))
                   (values b (- p w) (+ b v) (+ p w))))))
      (dotimes (k (if bars n 0))
        (multiple-value-bind (x1 y1 x2 y2) (corners k)
          (with-saved-state (device)
            (let ((fill (or (nth-spec colors k) color)))
              (when fill (set-color device fill)))
            (plot-rectangle device x1 y1 x2 y2 :fill t :coords coords))
          (when gap-color
            (with-style (device :color gap-color :line-width gap-width)
              (plot-rectangle device x1 y1 x2 y2 :coords coords)))
          (when outline
            (with-color (device outline)
              (plot-rectangle device x1 y1 x2 y2 :coords coords)))))
      ;; error bars and labels at the ends
      (let ((ends (loop for k below n
                        collect (+ (nth-spec bottoms k) (nth k values)))))
        (when errors
          (let ((lower (loop for k below n collect (nth-value 0 (error-ends errors k))))
                (upper (loop for k below n collect (nth-value 1 (error-ends errors k)))))
            (if vertical
                (plot-error-bars device (subseq positions 0 n) ends
                                 :yerr-lower lower :yerr-upper upper :cap-size cap-size
                                 :coords coords)
                (plot-error-bars device ends (subseq positions 0 n)
                                 :xerr-lower lower :xerr-upper upper :cap-size cap-size
                                 :coords coords))))
        (when value-labels
          (with-font (device (current-font device) :size label-size)
            (loop for k below n
                  for v = (nth k values)
                  for p = (nth k positions)
                  ;; beyond the end, and beyond its error bar, away from the bottom
                  for out = (multiple-value-bind (lo hi) (error-ends errors k)
                              (if (minusp v) (- (nth k ends) lo) (+ (nth k ends) hi)))
                  do (multiple-value-bind (cx cy)
                         (if vertical (to-canvas device coords p out) (to-canvas device coords out p))
                       ;; which way is "beyond" on the page
                       (multiple-value-bind (bx by)
                           (if vertical
                               (to-canvas device coords p (nth-spec bottoms k))
                               (to-canvas device coords (nth-spec bottoms k) p))
                         (let* ((dx (- cx bx)) (dy (- cy by))
                                (l (max 1d-9 (sqrt (+ (* dx dx) (* dy dy)))))
                                (ux (/ dx l)) (uy (/ dy l)))
                           (put-text device (+ cx (* ux pad)) (+ cy (* uy pad))
                                     (if texts (elt texts k) (funcall value-format v))
                                     :halign (cond ((> ux 0.5) :left) ((< ux -0.5) :right)
                                                   (t :center))
                                     :valign (cond ((> uy 0.5) :top) ((< uy -0.5) :bottom)
                                                   (t :center)))))))))))))

(defplot bar-chart (viewport categories data &key names (mode :grouped) (orientation :vertical)
                                                 colors (width 0.8) errors value-labels
                                                 (value-format #'default-value-text)
                                                 value-range category-angle
                                                 category-label value-label title
                                                 (legend :best) (grid t) size
                                                 (gap-color :white))
  "Draw a bar chart filling VIEWPORT: its window, bars, axes, category
   names and (for several named series) legend.  CATEGORIES are the
   names (strings or runs) along the category axis; DATA is one series
   (a list of numbers, one per category) or a list of series.  MODE
   :GROUPED puts each category's series side by side within WIDTH;
   :STACKED piles them (positive values up, negative down).
   ORIENTATION :VERTICAL (columns, categories along x) or :HORIZONTAL
   (categories down the y axis, first at the top).  NAMES label the
   series in the legend at LEGEND (a DRAW-LEGEND position, by default
   :BEST, or NIL).
   COLORS default to *CATEGORY-COLORS* in order.  ERRORS, one spec per
   series (as for PLOT-BARS), adds error bars (grouped mode).
   VALUE-LABELS writes values at the bar ends (totals when stacked).
   VALUE-RANGE (LO HI) sets the value axis (default from the data, with
   room for labels).  CATEGORY-ANGLE slants the category names (see
   DRAW-BOX's X-NUMBERS).  CATEGORY-LABEL, VALUE-LABEL and TITLE name
   the axes and chart; GRID draws lines across the value axis.  SIZE is
   the text size.  Returns VIEWPORT."
  (let* ((device (viewport-device viewport))
         (series (if (every #'realp data) (list data) data))
         (n-series (length series))
         (n-cat (length categories))
         (vertical (eq orientation :vertical))
         (colors (category-colors n-series colors))
         (layout '())                  ; (positions values bottoms width color errors)
         (up (make-list n-cat :initial-element 0))     ; stack ends, when stacked
         (down (make-list n-cat :initial-element 0)))
    (ecase mode
      (:grouped
       (let ((w (/ width n-series)))
         (loop for values in series
               for s from 0
               for color in colors
               do (push (list (loop for k below n-cat
                                    collect (+ k (- (/ width 2)) (* w (+ s 0.5))))
                              values 0 w color (nth-spec errors s))
                        layout))))
      (:stacked
       (loop for values in series
             for color in colors
             do (let ((bottoms (loop for v in values
                                     for k from 0
                                     collect (if (minusp v) (nth k down) (nth k up)))))
                  (loop for v in values
                        for k from 0
                        do (if (minusp v) (incf (nth k down) v) (incf (nth k up) v)))
                  (push (list (loop for k below n-cat collect k) values bottoms width color nil)
                        layout)))))
    (setf layout (nreverse layout))
    ;; the value axis: the bars' ends and error bars, and 0, with room
    (destructuring-bind (lo hi)
        (or value-range
            (let ((ends (list 0)))
              (loop for (nil values bottoms nil nil errs) in layout
                    do (loop for v in values
                             for k from 0
                             for end = (+ (nth-spec bottoms k) v)
                             do (multiple-value-bind (l u) (error-ends errs k)
                                  (push (- end l) ends) (push (+ end u) ends))))
              (let* ((lo (reduce #'min ends)) (hi (reduce #'max ends))
                     (room (* (- hi lo) 0.05))
                     (lo (if (minusp lo) (- lo room) lo))
                     (hi (if (plusp hi) (+ hi room) hi)))
                (if value-labels
                    (label-room viewport layout mode up down vertical value-format lo hi)
                    (list lo hi)))))
      ;; the category axis with room between the outer bars and the frame
      (let ((c1 -0.65) (c2 (- n-cat 0.35)))
       (if vertical
           (set-window viewport c1 c2 lo hi)
           (set-window viewport lo hi c2 c1))
       (with-viewport (viewport)
        (when grid
          (with-style (device :color :very-light-gray :line-width 0.5)
            (dolist (m (axis-ticks lo hi))
              (if vertical
                  (plot-line device c1 (first m) c2 (first m))
                  (plot-line device (first m) c1 (first m) c2)))))
        (loop for (positions values bottoms w color errs) in layout
              do (plot-bars device positions values :bottoms bottoms :width w :color color
                                                    :orientation orientation :errors errs
                                                    :gap-color gap-color
                                                    :value-labels (and value-labels
                                                                       (eq mode :grouped))
                                                    :value-format value-format))
        (when (and value-labels (eq mode :stacked))
          ;; each total beyond its stack's end (its negative end if negative)
          (let ((totals (loop for k below n-cat
                              collect (loop for (nil values) in layout sum (nth k values)))))
            (plot-bars device (loop for k below n-cat collect k)
                       (loop for total in totals for u in up for d in down
                             collect (if (minusp total) d u))
                       :bars nil :orientation orientation :value-labels t
                       :texts (mapcar value-format totals))))
        ;; the baseline
        (if vertical
            (plot-line device c1 0 c2 0)
            (plot-line device 0 c1 0 c2))))
      ;; value ticks outside the frame and on one side only, so they never
      ;; cross the bars; the grid carries the values across
      (if vertical
          (draw-box viewport :ticks '(:left) :numbers '(:bottom :left) :tick-direction :out
                             :minor nil
                             :x-ticks (category-ticks categories) :x-numbers (or category-angle
                                                                                  :horizontal)
                             :x-label category-label :y-label value-label :title title :size size)
          (draw-box viewport :ticks '(:bottom) :numbers '(:bottom :left) :tick-direction :out
                             :minor nil
                             :y-ticks (category-ticks categories)
                             :x-label value-label :y-label category-label :title title
                             :size size))
      (when (and legend names (> n-series 1))
        (draw-legend viewport (mapcar (lambda (name color) (list name :fill color)) names colors)
                     :position legend)))
    viewport))

(defun label-room (viewport layout mode up down vertical value-format lo hi)
  "The value range LO..HI widened so value labels beyond the bar ends
   fit inside VIEWPORT: the fraction of the axis a label needs, F, is
   measured on the page, and the new end E solves (E - end) = F (E - LO)."
  (let* ((device (viewport-device viewport))
         (size (* 0.8 (default-text-size device)))
         (texts-up '()) (texts-down '()))
    (if (eq mode :stacked)
        (loop for u in up for dn in down
              for total = (+ u dn)
              do (if (minusp total)
                     (push (funcall value-format total) texts-down)
                     (push (funcall value-format total) texts-up)))
        (loop for (nil values) in layout
              do (dolist (v values)
                   (if (minusp v)
                       (push (funcall value-format v) texts-down)
                       (push (funcall value-format v) texts-up)))))
    (multiple-value-bind (left right bottom top) (viewport-rectangle viewport :canvas)
      (with-font (device (current-font device) :size size)
        (flet ((need (texts)
                 ;; page extent of the longest label along the value axis
                 (if (null texts)
                     0
                     (+ (* 0.6 size)
                        (if vertical
                            (multiple-value-bind (w ascent descent) (text-size device "0")
                              (declare (ignore w))
                              (+ ascent descent))
                            (reduce #'max (mapcar (lambda (text) (text-size device text))
                                                  texts)))))))
          (let* ((length (if vertical (abs (- bottom top)) (abs (- right left))))
                 (f-up (/ (need texts-up) length))
                 (f-down (/ (need texts-down) length))
                 (hi (if texts-up (/ (- hi (* f-up lo)) (- 1 f-up)) hi))
                 (lo (if texts-down (/ (- lo (* f-down hi)) (- 1 f-down)) lo)))
            (list lo hi)))))))

;;; ============================================================
;;; Box plots
;;; ============================================================

(defun quantile (sorted q)
  "The Q quantile (0..1) of the SORTED vector, interpolating linearly."
  (let* ((n (length sorted)) (h (* q (1- n)))
         (i (floor h)) (f (- h i)))
    (if (>= i (1- n))
        (aref sorted (1- n))
        (+ (* (- 1 f) (aref sorted i)) (* f (aref sorted (1+ i)))))))

(defun box-stats (values &key (whisker 1.5))
  "A plist of the box-plot statistics of VALUES (NaNs dropped): :N,
   :MEAN, :MEDIAN, :Q1, :Q3, :LOW and :HIGH (the whisker ends: the most
   extreme values within WHISKER times the interquartile range of the
   box) and :OUTLIERS (the rest)."
  (let* ((sorted (sort (map 'vector (lambda (v) (float v 1d0))
                            (remove-if #'nan-p (coerce values 'list)))
                       #'<))
         (n (length sorted)))
    (when (zerop n) (error "No values for a box plot."))
    (let* ((q1 (quantile sorted 0.25)) (q3 (quantile sorted 0.75))
           (iqr (- q3 q1))
           (lo-fence (- q1 (* whisker iqr))) (hi-fence (+ q3 (* whisker iqr)))
           (inside (remove-if-not (lambda (v) (<= lo-fence v hi-fence)) sorted)))
      (list :n n :mean (/ (reduce #'+ sorted) n) :median (quantile sorted 0.5)
            :q1 q1 :q3 q3
            :low (reduce #'min inside) :high (reduce #'max inside)
            :outliers (coerce (remove-if (lambda (v) (<= lo-fence v hi-fence)) sorted) 'list)))))

(defplot plot-box-plot (device positions datasets &key (width 0.5) (orientation :vertical)
                                                      colors color (fill-alpha 0.35)
                                                      (whisker 1.5) (show-outliers t)
                                                      (outlier-marker :circle) outlier-size
                                                      means (mean-marker :diamond)
                                                      (coords :plot))
  "Draw a box-and-whisker plot of each of DATASETS (sequences of
   numbers) at POSITIONS: a box from the first to the third quartile
   with a line at the median, whiskers to the most extreme values within
   WHISKER times the box's height, and the outliers beyond as
   OUTLIER-MARKERs (if SHOW-OUTLIERS).  MEANS marks the mean with MEAN-MARKER.  Each box is
   WIDTH wide, in its color (COLORS, one per dataset, else COLOR, else
   the current color): filled at FILL-ALPHA and outlined.  ORIENTATION
   :VERTICAL or :HORIZONTAL.  Returns the BOX-STATS of each dataset."
  (let ((vertical (ecase orientation (:vertical t) (:horizontal nil)))
        (all '()))
    (loop for data in (coerce datasets 'list)
          for p in (coerce positions 'list)
          for k from 0
          for stats = (box-stats data :whisker whisker)
          for c = (or (nth-spec colors k) color)
          do (push stats all)
             (destructuring-bind (&key median q1 q3 low high outliers mean &allow-other-keys)
                 stats
               (flet ((line (v1 a1 v2 a2)
                        ;; a segment from (value V1, across A1) to (V2, A2)
                        (if vertical
                            (plot-line device (+ p a1) v1 (+ p a2) v2 :coords coords)
                            (plot-line device v1 (+ p a1) v2 (+ p a2) :coords coords)))
                      (rect (v1 v2 &key fill)
                        (let ((h (/ width 2)))
                          (if vertical
                              (plot-rectangle device (- p h) v1 (+ p h) v2 :fill fill :coords coords)
                              (plot-rectangle device v1 (- p h) v2 (+ p h) :fill fill
                                                                           :coords coords)))))
                 (with-saved-state (device)
                   (when c (set-color device c))
                   (let ((h (/ width 2)) (cap (/ width 4)))
                     (with-opacity-group (device fill-alpha)
                       (rect q1 q3 :fill t))
                     (rect q1 q3)
                     ;; whiskers with caps
                     (line q1 0 low 0) (line q3 0 high 0)
                     (line low (- cap) low cap) (line high (- cap) high cap)
                     ;; the median, across the box
                     (with-line-width (device (* 2 (line-width device)))
                       (line median (- h) median h))
                     (when (and show-outliers outliers)
                       (if vertical
                           (plot-points device (make-list (length outliers) :initial-element p)
                                        outliers :marker outlier-marker :size outlier-size
                                                 :coords coords)
                           (plot-points device outliers
                                        (make-list (length outliers) :initial-element p)
                                        :marker outlier-marker :size outlier-size
                                        :coords coords)))
                     (when means
                       (if vertical
                           (plot-point device p mean :marker mean-marker :coords coords)
                           (plot-point device mean p :marker mean-marker :coords coords))))))))
    (nreverse all)))

;;; ============================================================
;;; Violin plots
;;; ============================================================

(defun kde-bandwidth (sorted)
  "Silverman's rule of thumb for the Gaussian kernel width of SORTED."
  (let* ((n (length sorted))
         (mean (/ (reduce #'+ sorted) n))
         (sd (sqrt (/ (reduce #'+ (map 'list (lambda (v) (expt (- v mean) 2)) sorted))
                      (max 1 (1- n)))))
         (iqr (- (quantile sorted 0.75) (quantile sorted 0.25)))
         (spread (if (plusp iqr) (min sd (/ iqr 1.34)) sd)))
    (* 0.9 (if (plusp spread) spread 1d0) (expt n -0.2))))

(defun kde (values &key bandwidth (points 100) (cut 2))
  "A Gaussian kernel density estimate of VALUES: values GRID and
   DENSITY (vectors of POINTS), the grid spanning the data plus CUT
   bandwidths each side, the density integrating to 1."
  (let* ((sorted (sort (map 'vector (lambda (v) (float v 1d0))
                            (remove-if #'nan-p (coerce values 'list)))
                       #'<))
         (n (length sorted))
         (bw (or bandwidth (kde-bandwidth sorted)))
         (lo (- (aref sorted 0) (* cut bw))) (hi (+ (aref sorted (1- n)) (* cut bw)))
         (grid (make-array points :element-type 'double-float))
         (density (make-array points :element-type 'double-float))
         (norm (/ 1 (* n bw (sqrt (* 2 pi))))))
    (dotimes (k points)
      (let ((x (+ lo (* (- hi lo) (/ k (1- points))))))
        (setf (aref grid k) x
              (aref density k) (* norm (loop for v across sorted
                                             sum (exp (* -0.5 (expt (/ (- x v) bw) 2))))))))
    (values grid density)))

(defplot plot-violins (device positions datasets &key (width 0.8) (orientation :vertical)
                                                     colors color (fill-alpha 0.5)
                                                     bandwidth (cut 2) (scale :width)
                                                     (inner :box) (coords :plot))
  "Draw a violin for each of DATASETS at POSITIONS: its kernel density
   estimate (KDE, with BANDWIDTH, default Silverman's rule, extending CUT
   bandwidths beyond the data) mirrored about the position.  SCALE
   :WIDTH makes every violin WIDTH wide at its widest; :AREA gives them
   equal areas (the widest WIDTH wide); :COUNT makes areas proportional
   to the number of values.  INNER :BOX draws a thin interquartile box,
   whiskers and a white median dot; :QUARTILES dashed lines at the
   quartiles; NIL nothing.  Colors as for PLOT-BOX-PLOT, filled at
   FILL-ALPHA.  Returns the BOX-STATS of each dataset."
  (let* ((vertical (ecase orientation (:vertical t) (:horizontal nil)))
         (datasets (coerce datasets 'list))
         (estimates (mapcar (lambda (d)
                              (multiple-value-list (kde d :bandwidth bandwidth :cut cut)))
                            datasets))
         (peaks (mapcar (lambda (e) (reduce #'max (second e))) estimates))
         (counts (mapcar #'length datasets))
         (max-peak (reduce #'max peaks))
         (max-scaled (reduce #'max (mapcar #'* peaks counts)))
         (all '()))
    (loop for data in datasets
          for (grid density) in estimates
          for peak in peaks
          for count in counts
          for p in (coerce positions 'list)
          for k from 0
          for c = (or (nth-spec colors k) color)
          for half = (/ width 2
                        (ecase scale
                          (:width peak)
                          (:area max-peak)
                          (:count (/ max-scaled count))))
          do (let ((stats (box-stats data))
                   (vs (coerce grid 'list))
                   (as (map 'list (lambda (d) (* d half)) density)))
               (push stats all)
               (with-saved-state (device)
                 (when c (set-color device c))
                 (let ((across (append (mapcar (lambda (a) (+ p a)) as)
                                       (reverse (mapcar (lambda (a) (- p a)) as))))
                       (along (append vs (reverse vs))))
                   (with-opacity-group (device fill-alpha)
                     (if vertical
                         (plot-polygon device across along :fill t :coords coords)
                         (plot-polygon device along across :fill t :coords coords)))
                   (if vertical
                       (plot-polygon device across along :coords coords)
                       (plot-polygon device along across :coords coords)))
                 (destructuring-bind (&key median q1 q3 low high &allow-other-keys) stats
                   (flet ((line (v1 a1 v2 a2)
                            (if vertical
                                (plot-line device (+ p a1) v1 (+ p a2) v2 :coords coords)
                                (plot-line device v1 (+ p a1) v2 (+ p a2) :coords coords)))
                          (width-at (v)
                            ;; the violin's half width at value V
                            (let ((i (position-if (lambda (g) (>= g v)) grid)))
                              (if i (* half (aref density i)) 0))))
                     (ecase inner
                       ((nil))
                       (:box
                        (let ((bw (* 0.06 width)))
                          (with-color (device :dark-gray)
                            (line low 0 high 0)
                            (if vertical
                                (plot-rectangle device (- p bw) q1 (+ p bw) q3 :fill t
                                                                               :coords coords)
                                (plot-rectangle device q1 (- p bw) q3 (+ p bw) :fill t
                                                                               :coords coords)))
                          (with-color (device :white)
                            (if vertical
                                (plot-point device p median :marker :filled-circle :size 2
                                                            :coords coords :halo nil)
                                (plot-point device median p :marker :filled-circle :size 2
                                                            :coords coords :halo nil)))))
                       (:quartiles
                        (with-dash (device '(3 2))
                          (dolist (v (list q1 q3))
                            (let ((a (width-at v))) (line v (- a) v a))))
                        (let ((a (width-at median))) (line median (- a) median a)))))))))
    (nreverse all)))
