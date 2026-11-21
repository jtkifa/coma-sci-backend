;;;; contours.lisp — contour lines of 2D arrays, as PGPLOT's PGCONT/PGCONL
;;;;
;;;; (PLOT-CONTOURS DEVICE ARRAY :LEVELS '(1 2 4 8)) draws contours of
;;;; the array PLOT-IMAGE would draw, with the same pixel geometry
;;;; (X-RANGE, Y-RANGE, ORIGIN), so contours overlay an image exactly.
;;;; CONTOUR-LINES returns the lines themselves.
;;;;
;;;; Contours are found by marching squares on the grid of pixel
;;;; centers, linearly interpolated along cell edges; a saddle cell is
;;;; resolved by the average of its corners, and cells with a NaN corner
;;;; are skipped.  Segments are joined into polylines, so dashes run
;;;; continuously and labels can follow the line.

(in-package :cl-cairoplot)

;;; ============================================================
;;; Marching squares
;;; ============================================================

;; For each corner pattern (bit 0: corner (i,j) >= level, bit 1:
;; (i+1,j), bit 2: (i+1,j+1), bit 3: (i,j+1)), the pairs of cell edges
;; joined: :B bottom, :R right, :T top, :L left.  Saddles 5 and 10 are
;; listed for a center below the level; above it they swap (see below).
(defparameter +contour-cases+
  #(() ((:l :b)) ((:b :r)) ((:l :r)) ((:r :t)) ((:l :b) (:r :t)) ((:b :t)) ((:l :t))
    ((:t :l)) ((:b :t)) ((:b :r) (:t :l)) ((:r :t)) ((:r :l)) ((:b :r)) ((:l :b)) ()))

(defun contour-lines (array level)
  "Contours of the 2D ARRAY at LEVEL as a list of (XS YS CLOSED-P), XS and
   YS double-float vectors in pixel coordinates (column, row) of ARRAY.
   CLOSED-P lines end where they began (the first point is not repeated)."
  (destructuring-bind (h w) (array-dimensions array)
    (let ((points (make-hash-table))          ; edge key -> (x . y)
          (links (make-hash-table)))          ; edge key -> joined edge keys
      (labels ((z (i j) (row-major-aref array (+ (* j w) i)))
               ;; horizontal edge (i,j)-(i+1,j) and vertical (i,j)-(i,j+1)
               (h-key (i j) (* 2 (+ (* j w) i)))
               (v-key (i j) (1+ (* 2 (+ (* j w) i))))
               (crossing (z1 z2) (/ (- level z1) (- z2 z1)))
               (edge (name i j)
                 (let ((key (ecase name
                              (:b (h-key i j)) (:t (h-key i (1+ j)))
                              (:l (v-key i j)) (:r (v-key (1+ i) j)))))
                   (unless (gethash key points)
                     (setf (gethash key points)
                           (ecase name
                             (:b (cons (+ i (crossing (z i j) (z (1+ i) j))) j))
                             (:t (cons (+ i (crossing (z i (1+ j)) (z (1+ i) (1+ j))))
                                       (1+ j)))
                             (:l (cons i (+ j (crossing (z i j) (z i (1+ j))))))
                             (:r (cons (1+ i) (+ j (crossing (z (1+ i) j)
                                                             (z (1+ i) (1+ j)))))))))
                   key)))
        (dotimes (j (1- h))
          (dotimes (i (1- w))
            (let ((a (z i j)) (b (z (1+ i) j)) (c (z (1+ i) (1+ j))) (d (z i (1+ j))))
              (unless (some #'nan-p (list a b c d))
                (let* ((index (logior (if (>= a level) 1 0) (if (>= b level) 2 0)
                                      (if (>= c level) 4 0) (if (>= d level) 8 0)))
                       (pairs (aref +contour-cases+ index)))
                  ;; saddle with its center above the level: the high
                  ;; corners connect, so cut off the low ones instead
                  (when (and (member index '(5 10))
                             (>= (/ (+ a b c d) 4) level))
                    (setf pairs (aref +contour-cases+ (if (= index 5) 10 5))))
                  (loop for (e1 e2) in pairs
                        do (let ((k1 (edge e1 i j)) (k2 (edge e2 i j)))
                             (push k2 (gethash k1 links))
                             (push k1 (gethash k2 links)))))))))
        ;; walk the links into polylines: open ones from their ends first
        (let ((visited (make-hash-table)) (lines '()))
          (flet ((walk (start)
                   (let ((keys (list start)) (previous nil) (current start))
                     (setf (gethash start visited) t)
                     (loop
                       (let ((next (find-if (lambda (k) (and (not (eql k previous))
                                                             (not (gethash k visited))))
                                            (gethash current links))))
                         (unless next (return))
                         (setf (gethash next visited) t)
                         (push next keys)
                         (setf previous current current next)))
                     (let* ((keys (nreverse keys))
                            (closed (and (cddr keys)
                                         (member start (gethash current links))))
                            (n (length keys))
                            (xs (make-array n :element-type 'double-float))
                            (ys (make-array n :element-type 'double-float)))
                       (loop for k in keys for p = (gethash k points) for m from 0
                             do (setf (aref xs m) (float (car p) 1d0)
                                      (aref ys m) (float (cdr p) 1d0)))
                       (push (list xs ys (and closed t)) lines)))))
            (loop for k being the hash-keys of links using (hash-value ks)
                  when (and (= (length ks) 1) (not (gethash k visited)))
                    do (walk k))
            (loop for k being the hash-keys of links
                  unless (gethash k visited)
                    do (walk k)))
          lines)))))

(defun cell-contour-segments (a b c d level)
  "Contour segments at LEVEL across one grid cell with corner values A
   at (0,0), B at (1,0), C at (1,1) and D at (0,1), as a list of
   ((U1 V1) (U2 V2)) in the cell's own 0..1 coordinates."
  (if (some #'nan-p (list a b c d))
      '()
      (let* ((index (logior (if (>= a level) 1 0) (if (>= b level) 2 0)
                            (if (>= c level) 4 0) (if (>= d level) 8 0)))
             (pairs (aref +contour-cases+ index)))
        ;; saddle with its center above the level, as in CONTOUR-LINES
        (when (and (member index '(5 10)) (>= (/ (+ a b c d) 4) level))
          (setf pairs (aref +contour-cases+ (if (= index 5) 10 5))))
        (flet ((point (edge)
                 (flet ((f (z1 z2) (/ (- level z1) (- z2 z1))))
                   (ecase edge
                     (:b (list (f a b) 0))
                     (:t (list (f d c) 1))
                     (:l (list 0 (f a d)))
                     (:r (list 1 (f b c)))))))
          (loop for (e1 e2) in pairs collect (list (point e1) (point e2)))))))

(defun contour-levels (array n &key vmin vmax)
  "About N round levels strictly between VMIN and VMAX (default the
   range of ARRAY)."
  (multiple-value-bind (lo hi) (image-limits array)
    (let* ((lo (or vmin lo)) (hi (or vmax hi))
           (step (nice-step (- hi lo) (1+ n))))
      (remove-if-not (lambda (v) (< lo v hi)) (multiples-in step lo hi)))))

;;; ============================================================
;;; Drawing
;;; ============================================================

(defun level-label (level levels format)
  (if format
      (funcall format level)
      ;; enough decimals for the spacing of the levels
      (let ((step (if (cdr levels)
                      (reduce #'min (mapcar (lambda (a b) (abs (- b a)))
                                            levels (rest levels)))
                      (abs level))))
        (format-decimal level (if (zerop step) 1 step)))))

;;; Labels are placed with a PATH-WALKER, a polyline in canvas units
;;; measured by arc length.

(defstruct (path-walker (:constructor %make-path-walker (xs ys cumulative)))
  xs ys cumulative)

(defun make-path-walker (cxs cys closed)
  "A walker over the canvas polyline CXS, CYS (closed: back to the start)."
  (let* ((xs (if closed (concatenate '(vector double-float) cxs (subseq cxs 0 1)) cxs))
         (ys (if closed (concatenate '(vector double-float) cys (subseq cys 0 1)) cys))
         (n (length xs))
         (cumulative (make-array n :element-type 'double-float :initial-element 0d0)))
    (loop for k from 1 below n
          do (setf (aref cumulative k)
                   (+ (aref cumulative (1- k))
                      (sqrt (+ (expt (- (aref xs k) (aref xs (1- k))) 2)
                               (expt (- (aref ys k) (aref ys (1- k))) 2))))))
    (%make-path-walker xs ys cumulative)))

(defun walker-length (walker)
  (let ((c (path-walker-cumulative walker))) (aref c (1- (length c)))))

(defun walker-point (walker s)
  "The point at arc length S (clamped to the line)."
  (let* ((xs (path-walker-xs walker)) (ys (path-walker-ys walker))
         (c (path-walker-cumulative walker)) (n (length c))
         (k (max 1 (min (1- n) (or (position-if (lambda (v) (>= v s)) c) (1- n)))))
         (s0 (aref c (1- k))) (s1 (aref c k))
         (f (if (= s1 s0) 0 (max 0 (min 1 (/ (- s s0) (- s1 s0)))))))
    (values (+ (aref xs (1- k)) (* f (- (aref xs k) (aref xs (1- k)))))
            (+ (aref ys (1- k)) (* f (- (aref ys k) (aref ys (1- k))))))))

(defun walker-angle (walker s0 s1)
  "Canvas direction of the chord from arc length S0 to S1."
  (multiple-value-bind (x0 y0) (walker-point walker s0)
    (multiple-value-bind (x1 y1) (walker-point walker s1)
      (atan (- y1 y0) (- x1 x0)))))

(defun walker-turning (walker s0 s1 &optional (steps 8))
  "Total turning (radians) of the line between arc lengths S0 and S1."
  (let ((ds (/ (- s1 s0) steps)) (previous nil) (total 0))
    (dotimes (k steps total)
      (let ((a (walker-angle walker (+ s0 (* k ds)) (+ s0 (* (1+ k) ds)))))
        (when previous
          (incf total (abs (- (mod (+ (- a previous) pi) (* 2 pi)) pi))))
        (setf previous a)))))

(defun label-sites (walker spacing width &key (max-turning 1.2))
  "Arc lengths for labels WIDTH long, about every SPACING: each is the
   least-bent spot near its nominal place, and spots bending more than
   MAX-TURNING radians over the label are skipped."
  (let* ((total (walker-length walker))
         (half (/ width 2))
         (low half) (high (- total half)))
    (when (> total (* 1.5 width))
      (loop for nominal from (min (/ spacing 2) (/ total 2)) below high by spacing
            for best = (loop with best-s = nil and best-turn = nil
                             for k from -4 to 4
                             for s = (+ nominal (* k (/ spacing 16)))
                             when (<= low s high)
                               do (let ((turn (walker-turning walker (- s half) (+ s half))))
                                    (when (or (null best-turn) (< turn best-turn))
                                      (setf best-s s best-turn turn)))
                             finally (return (and best-s (< best-turn max-turning) best-s)))
            when best collect best))))

(defun flips-p (angle)
  "True if text along canvas direction ANGLE would read upside down, or
   down a (nearly) vertical line: such text is turned half a turn, so
   vertical labels read bottom to top as an axis label does."
  (let ((edge (- (/ pi 2) 0.05)))        ; about 3 degrees short of vertical
    (or (>= angle edge) (< angle (- (+ edge 0.1))))))

(defun upright (angle)
  "ANGLE turned by half a turn if FLIPS-P."
  (if (flips-p angle)
      (if (plusp angle) (- angle pi) (+ angle pi))
      angle))

;;; Gaps are sized from the label's ink: half its advance width and
;;; HALF-HEIGHT (the larger reach of its ink above or below the line),
;;; plus small margins, so they hide as little of the line as possible.

(defun gap-margins (label-size)
  "Values: the margin beyond the ends of a label, and above and below."
  (values (* 0.15 label-size) (* 0.08 label-size)))

(defun straight-label-gap (x y angle width half-height label-size)
  "The box around a straight label, as a polygon region in :CANVAS."
  (multiple-value-bind (end-margin side-margin) (gap-margins label-size)
    (straight-gap-box x y angle (+ (/ width 2) end-margin) (+ half-height side-margin))))

(defun straight-gap-box (x y angle hw hh)
  (let* ((c (cos angle)) (s (sin angle))
         (corners `((,(- hw) ,(- hh)) (,hw ,(- hh)) (,hw ,hh) (,(- hw) ,hh))))
    (polygon-region (loop for (u v) in corners collect (+ x (* c u) (- (* s v))))
                    (loop for (u v) in corners collect (+ y (* s u) (* c v)))
                    :coords :canvas)))

(defun curved-label-gap (walker s width half-height label-size)
  "The strip along WALKER around a curved label centered at arc length S."
  (multiple-value-bind (end-margin side-margin) (gap-margins label-size)
    (curved-gap-strip walker s (+ (/ width 2) end-margin) (+ half-height side-margin))))

(defun curved-gap-strip (walker s half-length hh)
  (let* ((a0 (- s half-length)) (a1 (+ s half-length))
         (steps 12) (upper '()) (lower '()))
    (loop for k from 0 to steps
          for a = (+ a0 (* k (/ (- a1 a0) steps)))
          do (multiple-value-bind (x y) (walker-point walker a)
               (let* ((angle (walker-angle walker (- a 1) (+ a 1)))
                      (nx (- (sin angle))) (ny (cos angle)))
                 (push (list (+ x (* hh nx)) (+ y (* hh ny))) upper)
                 (push (list (- x (* hh nx)) (- y (* hh ny))) lower))))
    (let ((points (append (reverse upper) lower)))
      (polygon-region (mapcar #'first points) (mapcar #'second points) :coords :canvas))))

(defun label-half-height (device text)
  "How far TEXT's ink reaches above or below its center line, when drawn
   with :VALIGN :CENTER in the current font."
  (multiple-value-bind (xb yb w h) (text-extents device text)
    (declare (ignore xb w))
    (multiple-value-bind (ascent descent) (font-extents device)
      ;; :CENTER puts the baseline (ascent - descent)/2 below the anchor
      (let ((baseline (/ (- ascent descent) 2)))
        (max (abs (+ baseline yb)) (abs (+ baseline yb h)))))))

(defun label-inside-clip-p (device walker s x y width half-height)
  "True if the box of a label centered at (X,Y), arc length S, is inside
   the current clip region."
  (let* ((angle (walker-angle walker (- s (/ width 2)) (+ s (/ width 2))))
         (c (cos angle)) (sn (sin angle))
         (hw (/ width 2)))
    (every (lambda (corner)
             (destructuring-bind (u v) corner
               (in-clip-p device (+ x (* c u) (- (* sn v))) (+ y (* sn u) (* c v)))))
           `((,(- hw) ,(- half-height)) (,hw ,(- half-height))
             (,hw ,half-height) (,(- hw) ,half-height)))))

(defun draw-curved-label (device walker s text width &optional halo)
  "Draw TEXT centered at arc length S along WALKER, one character at a
   time, each turned to the line under it, reading left to right; with
   HALO, all the characters' halos first."
  (let ((reversed (flips-p (walker-angle walker (- s (/ width 2)) (+ s (/ width 2))))))
    ;; set per halo layer by DO-HALO-LAYERS below
    (let (halo-color halo-width)
      (flet ((pass (halo-pass)
        (let ((x-offset 0))
          (loop for ch across text
                for glyph = (string ch)
                for advance = (text-size device glyph)
                do (let* ((center (+ x-offset (/ advance 2)))
                          ;; walk the line forward, or backward if it runs right to left
                          (a (if reversed
                                 (- (+ s (/ width 2)) center)
                                 (+ (- s (/ width 2)) center)))
                          (d (max 1 (/ advance 2)))
                          (angle (if reversed
                                     (walker-angle walker (+ a d) (- a d))
                                     (walker-angle walker (- a d) (+ a d)))))
                     (multiple-value-bind (x y) (walker-point walker a)
                       (if halo-pass
                           ;; this character's halo only (the text itself in the
                           ;; halo color), opaque: the whole layer is blended below
                           (with-saved-state (device)
                             (set-color device halo-color)
                             (put-text device x y glyph :angle angle :halign :center
                                                        :valign :center
                                                        :halo (list :color halo-color
                                                                    :width halo-width
                                                                    :opacity 1 :soft nil)))
                           (put-text device x y glyph :angle angle :halign :center
                                                      :valign :center :halo nil)))
                     (incf x-offset advance))))))
        (do-halo-layers (device halo (* 0.08 (font-size (current-font device)))
                                layer-color layer-width)
          (setf halo-color layer-color halo-width layer-width)
          (pass t))
        (pass nil)))))

(defun pixel-mapping (device coords transform)
  "A function of a pixel position (column, row) returning the canvas
   point, through TRANSFORM to COORDS: TRANSFORM an affine (or list (A B
   C D E F)), or a function of the pixel returning the COORDS point
   (e.g. from WCS-IMAGE-TRANSFORM)."
  (let ((to-canvas (coords-transform device coords)))
    (if (functionp transform)
        (lambda (i j) (multiple-value-call #'affine-apply to-canvas (funcall transform i j)))
        (let ((m (affine-compose to-canvas (as-affine transform))))
          (lambda (i j) (affine-apply m i j))))))

(defplot plot-contours (device array &key levels (n 8) x-range y-range (origin :lower)
                                       transform wcs (coords :plot) color colormap
                                       negative-dash labels label-size label-spacing
                                       label-format (halo *halo*))
  "Draw contours of the 2D ARRAY at LEVELS (default about N round
   levels, see CONTOUR-LEVELS), placed as PLOT-IMAGE places ARRAY
   (X-RANGE, Y-RANGE, ORIGIN, TRANSFORM or WCS, COORDS); TRANSFORM may
   also be a function of the pixel (column, row) returning the COORDS
   point, and WCS maps every vertex through the image's WCS onto the
   current viewport's sky projection.
   Style: COLOR, a color spec or a list cycled over the levels (default
   the current color); or COLORMAP, coloring each level by its place in
   the range of LEVELS.  NEGATIVE-DASH, a dash pattern, dashes contours
   below zero.  Line width is the current one.
   LABELS writes each level along its contours, with a gap in the line:
   T (or :STRAIGHT) as straight text along the line, :CURVED with each
   character following the curve.  Labels go about every LABEL-SPACING
   canvas units (default a third of the page width) at the least-bent
   spot nearby, skipping sharp bends and spots that would overlap
   another label, in LABEL-SIZE text (default 0.6 of DEFAULT-TEXT-SIZE).
   The text is LABEL-FORMAT applied to the level, or the level with
   enough decimals.  HALO outlines lines and labels (see WITH-HALO;
   default width 0.7 of the line width).  Returns the levels drawn."
  (check-type labels (member nil t :straight :curved))
  (when wcs (setf transform (wcs-image-transform wcs (current-viewport device))))
  (destructuring-bind (h w) (array-dimensions array)
    (let* ((levels (sort (copy-list (or levels (contour-levels array n))) #'<))
           (cmap (and colormap (parse-colormap colormap)))
           (lo (first levels)) (hi (car (last levels)))
           (curved (eq labels :curved))
           (label-size (or label-size (* 0.6 (default-text-size device))))
           (label-spacing (or label-spacing (/ (canvas-width device) 3))))
      (destructuring-bind (x1 x2) (or x-range (list -0.5 (- w 0.5)))
        (destructuring-bind (y1 y2) (or y-range (list -0.5 (- h 0.5)))
          ;; pixel coordinates (column, row) -> drawing space, for pixel
          ;; centers: column i is centered at x1 + (i + 1/2) * (x2 - x1)/w
          (let* ((sx (/ (- x2 x1) w)) (sy (/ (- y2 y1) h))
                 (m (pixel-mapping device coords
                                   (or transform
                                       (ecase origin
                                         (:lower (make-affine sx 0 0 sy (+ x1 (/ sx 2))
                                                              (+ y1 (/ sy 2))))
                                         (:upper (make-affine sx 0 0 (- sy) (+ x1 (/ sx 2))
                                                              (- y2 (/ sy 2))))))))
                 (contours            ; (level color closed cxs cys)
                   (loop for level in levels
                         for k from 0
                         for level-color = (cond (cmap (multiple-value-call #'list
                                                         (colormap-rgb cmap
                                                                       (if (= hi lo) 0.5
                                                                           (/ (- level lo) (- hi lo))))))
                                                 ((consp color) (nth (mod k (length color)) color))
                                                 (t color))
                         nconc (loop for (xs ys closed) in (contour-lines array level)
                                     collect (let ((cxs (make-array (length xs) :element-type 'double-float))
                                                   (cys (make-array (length xs) :element-type 'double-float)))
                                               (dotimes (q (length xs))
                                                 (multiple-value-bind (cx cy)
                                                     (funcall m (aref xs q) (aref ys q))
                                                   (setf (aref cxs q) cx (aref cys q) cy)))
                                               (list level level-color closed cxs cys)))))
                 (placed '()))   ; (text x y width color walker s contour half-height)
            ;; choose label sites
            (when labels
              (with-font (device (current-font device) :size label-size)
                (loop for (level level-color closed cxs cys) in contours
                      for contour from 0
                      for text = (level-label level levels label-format)
                      for width = (text-size device text)
                      for half-height = (label-half-height device text)
                      for walker = (make-path-walker cxs cys closed)
                      do (loop for s in (label-sites walker label-spacing width)
                               do (multiple-value-bind (x y) (walker-point walker s)
                                    ;; skip a label partly outside the clip region
                                    ;; (it would be cut off) or overlapping one
                                    ;; already placed
                                    (unless (or (not (label-inside-clip-p device walker s x y width
                                                                          half-height))
                                                (find-if (lambda (p)
                                                       (destructuring-bind (ptext px py pw &rest more) p
                                                         (declare (ignore ptext more))
                                                         (< (sqrt (+ (expt (- x px) 2) (expt (- y py) 2)))
                                                            (+ (/ (+ width pw) 2) label-size))))
                                                     placed))
                                      (push (list text x y width level-color walker s
                                                  contour half-height)
                                            placed)))))))
            ;; set per halo layer by DO-HALO-LAYERS below
            (let (halo-color halo-width)
              ;; all the line halos first (HALO-PASS), as one layer, then the lines
              (flet ((pass (halo-pass)
                (loop for (level level-color closed cxs cys) in contours
                      for contour from 0
                      ;; each contour is cut only by the gaps of its own labels,
                      ;; so a label never blanks a neighboring contour
                      for gaps = (loop for (nil x y width nil walker s c half-height) in placed
                                       when (= c contour)
                                         collect (region-outside
                                                  (if curved
                                                      (curved-label-gap walker s width half-height
                                                                        label-size)
                                                      (straight-label-gap
                                                       x y (upright (walker-angle walker
                                                                                  (- s (/ width 2))
                                                                                  (+ s (/ width 2))))
                                                       width half-height label-size))))
                      ;; with-saved-state holds the lock; direct path ops for efficiency
                      do (with-saved-state (device)
                           (when gaps (apply-clip device (apply #'region-intersection gaps)))
                           (when level-color (set-color device level-color))
                           (when (and negative-dash (minusp level) (not halo-pass))
                             (set-dash device negative-dash))
                           (new-path/tus device)
                           (move-to/tus device (aref cxs 0) (aref cys 0))
                           (loop for q from 1 below (length cxs)
                                 do (line-to/tus device (aref cxs q) (aref cys q)))
                           (when closed (close-path/tus device))
                           (if halo-pass
                               (stroke-halo device halo-color halo-width (line-width device))
                               (stroke/tus device))))))
                (do-halo-layers (device halo (* 0.7 (line-width device))
                                        layer-color layer-width)
                  (setf halo-color layer-color halo-width layer-width)
                  (pass t))
                (pass nil)))
            (when placed
              (with-font (device (current-font device) :size label-size)
                (loop for (text x y width level-color walker s) in placed
                      do (with-saved-state (device)
                           (when level-color (set-color device level-color))
                           (if curved
                               (draw-curved-label device walker s text width halo)
                               (put-text device x y text
                                         :angle (upright (walker-angle walker (- s (/ width 2))
                                                                       (+ s (/ width 2))))
                                         :halign :center :valign :center :halo halo))))))
            levels))))))

;;; ============================================================
;;; Filled contours
;;; ============================================================
;;;
;;; Each grid cell is split into two triangles, over which the values
;;; are interpolated linearly, so a triangle's part above a level is a
;;; convex polygon.  For each level, the parts of all triangles above it
;;; are filled as one path (so pieces of a band meet without seams), and
;;; the levels are filled upward, each over the last: every band
;;; boundary is then covered by the next fill, leaving no hairline gaps.

(defun clip-triangle-above (points values level)
  "The part of the triangle POINTS (three (X Y)) whose linearly
   interpolated VALUES are >= LEVEL, as a list of points (possibly
   empty)."
  (let ((out '()))
    (loop for k below 3
          for p = (nth k points) for q = (nth (mod (1+ k) 3) points)
          for vp = (nth k values) for vq = (nth (mod (1+ k) 3) values)
          do (when (>= vp level) (push p out))
             (when (or (and (>= vp level) (< vq level)) (and (< vp level) (>= vq level)))
               (let ((f (/ (- level vp) (- vq vp))))
                 (push (list (+ (first p) (* f (- (first q) (first p))))
                             (+ (second p) (* f (- (second q) (second p)))))
                       out))))
    (nreverse out)))

(defun filled-contour-levels (array n)
  "About N round bands spanning ARRAY's range: N+1-ish level edges from
   a round number at or below the minimum to one at or above the maximum."
  (multiple-value-bind (lo hi) (image-limits array)
    (let ((step (nice-step (- hi lo) n)))
      ;; multiples of STEP, not a running sum, so 0 is exactly 0
      (loop for k from (floor lo step)
            for v = (* k step)
            collect v
            until (>= v hi)))))

(defun band-colors (levels colormap colors)
  "One color per band between consecutive LEVELS: from the list COLORS
   (cycled), else COLORMAP sampled at each band's middle."
  (let ((bands (1- (length levels))))
    (if colors
        (loop for k below bands collect (nth (mod k (length colors)) colors))
        (let ((cm (parse-colormap colormap)))
          (loop for k below bands
                collect (multiple-value-list
                         (colormap-rgb cm (if (= bands 1) 0.5 (/ (+ k 0.5) bands)))))))))

(defplot plot-filled-contours (device array &key levels (n 8) (colormap :viridis) colors
                                              x-range y-range (origin :lower) transform
                                              wcs (coords :plot))
  "Fill the bands between consecutive LEVELS of the 2D ARRAY (default
   about N round bands covering its range, see FILLED-CONTOUR-LEVELS),
   each band colored by COLORMAP at its middle or by the list COLORS.
   Values below the first level are left unfilled; values above the
   last get the last band's color.  The array is placed as PLOT-IMAGE
   places it (X-RANGE, Y-RANGE, ORIGIN, or TRANSFORM or WCS as for
   PLOT-CONTOURS, COORDS), with values at the pixel centers.  Returns
   the LEVELS, for DRAW-COLORBAR :LEVELS."
  (when wcs (setf transform (wcs-image-transform wcs (current-viewport device))))
  (destructuring-bind (h w) (array-dimensions array)
    (let* ((levels (sort (copy-list (or levels (filled-contour-levels array n))) #'<))
           (band-colors (band-colors levels colormap colors)))
      (destructuring-bind (x1 x2) (or x-range (list -0.5 (- w 0.5)))
        (destructuring-bind (y1 y2) (or y-range (list -0.5 (- h 0.5)))
          (let* ((sx (/ (- x2 x1) w)) (sy (/ (- y2 y1) h))
                 (m (pixel-mapping device coords
                                   (or transform
                                       (ecase origin
                                         (:lower (make-affine sx 0 0 sy (+ x1 (/ sx 2))
                                                              (+ y1 (/ sy 2))))
                                         (:upper (make-affine sx 0 0 (- sy) (+ x1 (/ sx 2))
                                                              (- y2 (/ sy 2))))))))
                 (points (make-array (list h w))))
            (dotimes (j h)
              (dotimes (i w)
                (setf (aref points j i) (multiple-value-list (funcall m i j)))))
            ;; with-saved-state holds the lock; direct path ops for efficiency
            (loop for level in (butlast levels)
                  for color in band-colors
                  do (with-saved-state (device)
                       (set-color device color)
                       (new-path/tus device)
                       (dotimes (j (1- h))
                         (dotimes (i (1- w))
                           (let ((v00 (aref array j i)) (v10 (aref array j (1+ i)))
                                 (v11 (aref array (1+ j) (1+ i))) (v01 (aref array (1+ j) i)))
                             (unless (or (some #'nan-p (list v00 v10 v11 v01))
                                         ;; corners beyond a sky projection
                                         (some (lambda (c) (nan-p (first (apply #'aref points c))))
                                               (list (list j i) (list j (1+ i))
                                                     (list (1+ j) i) (list (1+ j) (1+ i)))))
                               (dolist (tri (list (list (list j i) (list j (1+ i)) (list (1+ j) (1+ i)))
                                                  (list (list j i) (list (1+ j) (1+ i)) (list (1+ j) i))))
                                 (let ((part (clip-triangle-above
                                              (mapcar (lambda (c) (apply #'aref points c)) tri)
                                              (mapcar (lambda (c) (apply #'aref array c)) tri)
                                              level)))
                                   (when (cddr part)
                                     (move-to/tus device (first (first part)) (second (first part)))
                                     (dolist (p (rest part)) (line-to/tus device (first p) (second p)))
                                     (close-path/tus device))))))))
                       (fill-path/tus device)))
            levels))))))
