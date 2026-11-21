;;;; axes.lisp — frames, ticks, numeric labels and axis titles
;;;;
;;;; DRAW-BOX does what PGPLOT's PGBOX and PGLAB do, for a viewport: a
;;;; frame, major and minor ticks, numbers, an optional grid, and axis
;;;; labels and a title placed clear of the numbers.  Options are
;;;; keywords rather than PGPLOT's option letters.
;;;;
;;;; A log axis follows PGPLOT: the window holds log10 of the values, so
;;;; a window from 0 to 3 is labeled 1 to 1000.
;;;;
;;;; Sizes default to DEFAULT-TEXT-SIZE, 1/40 of the smaller canvas
;;;; dimension (PGPLOT's character height), and tick lengths and gaps
;;;; scale with the text size.

(in-package :cl-cairoplot)

(defun default-text-size (canvas)
  "The text size used when none is given: a fortieth of the smaller of
   CANVAS's width and height."
  (/ (min (canvas-width canvas) (canvas-height canvas)) 40))

;;; ============================================================
;;; Tick values
;;; ============================================================

(defun nice-step (span target)
  "A step of 1, 2 or 5 times a power of ten giving about TARGET
   intervals over SPAN."
  (let* ((raw (/ span target))
         (magnitude (expt 10d0 (floor (log raw 10))))
         (f (/ raw magnitude)))
    (* magnitude (cond ((< f 1.5) 1) ((< f 3) 2) ((< f 7) 5) (t 10)))))

(defun default-subdivisions (step)
  "Minor intervals per major STEP: 4 for 2×10^n (halves), else 5."
  (let ((leading (round (/ step (expt 10d0 (floor (log (abs step) 10)))))))
    (if (= leading 2) 4 5)))

(defun multiples-in (step lo hi)
  "Multiples of STEP from LO to HI inclusive, with slack for float
   (including single-float) rounding."
  (let ((eps 1d-6))
    (loop for k from (ceiling (- (/ lo step) eps)) to (floor (+ (/ hi step) eps))
          collect (* k step))))

(defun decimals-for (step)
  "Digits after the point needed to show multiples of STEP exactly."
  (loop for d from 0 below 12
        for scaled = (* (abs step) (expt 10 d))
        when (< (abs (- scaled (round scaled))) (* 1d-6 scaled))
          return d
        finally (return 12)))

(defun format-decimal (value step)
  (let ((d (decimals-for step)))
    (if (zerop d)
        (format nil "~D" (round value))
        (format nil "~,vF" d (float value 1d0)))))

(defun power-of-ten-text (exponent &optional mantissa)
  "Runs for 10^EXPONENT, or MANTISSA×10^EXPONENT."
  (list (if mantissa (format nil "~A×10" mantissa) "10")
        (list (format nil "~D" exponent) :scale 0.6 :rise 0.5)))

(defun linear-tick-labels (values step)
  "Text for each of VALUES (multiples of STEP): plain decimals, or
   mantissa×10^n with a common exponent for very large or small values."
  (let* ((biggest (reduce #'max values :key #'abs :initial-value 0))
         (exponent (if (plusp biggest) (floor (log biggest 10)) 0)))
    (if (<= -3 exponent 4)
        (mapcar (lambda (v) (format-decimal v step)) values)
        (let ((scale (expt 10d0 exponent)))
          (mapcar (lambda (v)
                    (if (< (abs v) (* 1d-9 step))
                        "0"
                        (power-of-ten-text
                         exponent (format-decimal (/ v scale) (/ step scale)))))
                  values)))))

(defun linear-ticks (lo hi &key step subdivisions (target 5))
  "Ticks for a linear axis from LO to HI: values MAJORS (a list of
   (POSITION TEXT)) and MINORS (a list of positions)."
  (let ((lo (min lo hi)) (hi (max lo hi)))
    (let* ((step (or step (nice-step (- hi lo) target)))
           (sub (or subdivisions (default-subdivisions step)))
           ;; snap values that should be zero
           (majors (mapcar (lambda (v) (if (< (abs v) (* 1d-9 step)) 0d0 v))
                           (multiples-in step lo hi)))
           (minors (if (> sub 1)
                       (remove-if (lambda (v)
                                    (< (abs (- (/ v step) (round v step))) 1d-6))
                                  (multiples-in (/ step sub) lo hi))
                       '())))
      (values (mapcar #'list majors (linear-tick-labels majors step))
              minors))))

(defun log-tick-text (exponent)
  (if (<= -2 exponent 3)
      (format-decimal (expt 10 exponent) (expt 10d0 (min exponent 0)))
      (power-of-ten-text exponent)))

(defun log-ticks (lo hi &key (target 5))
  "Ticks for a log axis whose window LO..HI is in log10 units: values
   MAJORS (a list of (POSITION TEXT)) and MINORS (positions)."
  (let ((lo (min lo hi)) (hi (max lo hi)))
    (if (< (- hi lo) 1)
        ;; under a decade: linear ticks on the values, placed logarithmically
        (multiple-value-bind (majors minors)
            (linear-ticks (expt 10d0 lo) (expt 10d0 hi) :target target)
          (values (loop for (v text) in majors
                        when (plusp v) collect (list (log v 10d0) text))
                  (loop for v in minors when (plusp v) collect (log v 10d0))))
        (let* ((step (max 1 (ceiling (- (/ (- hi lo) target) 1d-9))))
               (decades (loop for e from (ceiling (- lo 1d-9)) to (floor (+ hi 1d-9))
                              collect e)))
          (values (loop for e in decades
                        when (zerop (mod e step))
                          collect (list e (log-tick-text e)))
                  (if (= step 1)
                      (loop for e from (floor lo) to (floor hi)
                            nconc (loop for k from 2 to 9
                                        for p = (+ e (log k 10d0))
                                        when (<= lo p hi) collect p))
                      (remove-if (lambda (e) (zerop (mod e step))) decades)))))))

(defun axis-ticks (lo hi &key log step subdivisions (target 5) format)
  "Major ticks as a list of (POSITION TEXT) and minor tick positions for
   an axis over LO..HI.  FORMAT, a function of the value (10^position on
   a log axis) returning text, overrides the labels."
  (multiple-value-bind (majors minors)
      (if log
          (log-ticks lo hi :target target)
          (linear-ticks lo hi :step step :subdivisions subdivisions
                              :target target))
    (values (if format
                (loop for (p) in majors
                      collect (list p (funcall format (if log (expt 10d0 p) p))))
                majors)
            minors)))

;;; ============================================================
;;; Drawing
;;; ============================================================

(defun side-geometry (side left right top bottom)
  "For SIDE, values: the edge coordinate (canvas y for :BOTTOM/:TOP, x
   for :LEFT/:RIGHT), the inward direction (DX DY), and whether ticks
   along it are placed by x."
  (ecase side
    (:bottom (values bottom 0 -1 t))
    (:top    (values top    0  1 t))
    (:left   (values left   1  0 nil))
    (:right  (values right -1  0 nil))))

(defun stroke-ticks (device side positions length direction box)
  "Stroke ticks of LENGTH at canvas POSITIONS along SIDE of BOX
   (LEFT RIGHT TOP BOTTOM)."
  (multiple-value-bind (edge dx dy along-x) (apply #'side-geometry side box)
    (let ((a (ecase direction (:in 0) (:out (- length)) (:both (- length))))
          (b (ecase direction (:in length) (:out 0) (:both length))))
      ;; one lock for the whole batch of ticks
      (with-batched-graphics (device)
        (new-path device)
        (dolist (p positions)
          (multiple-value-bind (x y) (if along-x (values p edge) (values edge p))
            (move-to device (+ x (* a dx)) (+ y (* a dy)))
            (line-to device (+ x (* b dx)) (+ y (* b dy)))))
        (stroke device)))))

(defplot draw-frame (viewport)
  "Draw VIEWPORT's rectangle, rotated with it, in the current style."
  (with-viewport (viewport :clip nil)
    (multiple-value-bind (left right bottom top) (viewport-rectangle viewport :canvas)
      (plot-rectangle (viewport-device viewport) left bottom right top
                      :coords :canvas))))

(defun trim-majors (majors lo hi low-key high-key omit)
  "MAJORS without the ones at the window end LO (if LOW-KEY is in OMIT)
   or HI (if HIGH-KEY is)."
  (let ((eps (* 1d-6 (abs (- hi lo)))))
    (remove-if (lambda (m)
                 (or (and (member low-key omit) (< (abs (- (first m) lo)) eps))
                     (and (member high-key omit) (< (abs (- (first m) hi)) eps))))
               majors)))

(defplot draw-box (viewport &key (frame t)
                               (ticks '(:bottom :top :left :right))
                               (numbers '(:bottom :left))
                               (minor t) (tick-direction :in)
                               grid zero-axes
                               x-log y-log x-step y-step
                               x-subdivisions y-subdivisions
                               x-format y-format
                               (x-numbers :horizontal) (y-numbers :horizontal)
                               x-label y-label title
                               size font omit-end-numbers x-ticks y-ticks)
  "Draw a frame, ticks, numbers and labels around VIEWPORT, rotated with
   it if it is rotated.
   TICKS and NUMBERS list the sides (:BOTTOM :TOP :LEFT :RIGHT) that get
   them.  TICK-DIRECTION is :IN, :OUT or :BOTH; MINOR enables minor
   ticks.  GRID is NIL, T (major) or :MINOR (major and minor), drawn in
   light gray.  ZERO-AXES draws the lines x=0 and y=0 when inside.
   X-LOG/Y-LOG make log axes (window in log10 units).  X-STEP/Y-STEP set
   the major interval and X-SUBDIVISIONS/Y-SUBDIVISIONS the minor
   intervals per major; X-FORMAT/Y-FORMAT are functions from a value to
   its text.  Y-NUMBERS is :HORIZONTAL or :VERTICAL (along the axis);
   X-NUMBERS is :HORIZONTAL (a number that would overlap the one before
   is skipped) or an angle in degrees to slant them, ending at their
   ticks (for long category names, see CATEGORY-TICKS).
   X-LABEL, Y-LABEL and TITLE are text (strings or runs).
   OMIT-END-NUMBERS lists window ends (:X-LOW :X-HIGH :Y-LOW :Y-HIGH)
   whose numbers are left out, e.g. where touching panels meet.
   X-TICKS and Y-TICKS, functions of the window's (LO HI) returning
   values MAJORS ((POSITION TEXT) ...) and MINORS (positions) as
   AXIS-TICKS does, replace the standard ticks, e.g. SEXAGESIMAL-TICKS.  SIZE is the
   text size; FONT a font spec.  Lines use the current style."
  ;; a rotated viewport is fine (everything turns with it); what cannot
  ;; be boxed is a plot rotated or skewed *within* the viewport
  (unless (viewport-axis-aligned-p viewport)
    (error "DRAW-BOX needs plot axes along the viewport's edges, but ~A ~
            has a rotated or skewed SET-TRANSFORM." viewport))
  (let* ((device (viewport-device viewport))
         (size (or size (default-text-size device)))
         (major-length (* 0.6 size))
         (minor-length (* 0.3 size))
         (gap (* 0.5 size))
         ;; how far ticks reach out of the frame on each side
         (outs (loop for side in '(:bottom :top :left :right)
                     append (list side (if (and (member side ticks)
                                                (member tick-direction '(:out :both)))
                                           major-length 0)))))
    (multiple-value-bind (wx1 wx2 wy1 wy2) (viewport-window viewport)
      (multiple-value-bind (x-majors x-minors)
          (if x-ticks
              (funcall x-ticks wx1 wx2)
              (axis-ticks wx1 wx2 :log x-log :step x-step
                                  :subdivisions x-subdivisions :format x-format))
        (multiple-value-bind (y-majors y-minors)
            (if y-ticks
                (funcall y-ticks wy1 wy2)
                (axis-ticks wy1 wy2 :log y-log :step y-step
                                    :subdivisions y-subdivisions :format y-format))
          (flet ((cx (x) (nth-value 0 (plot-to-canvas viewport x wy1)))
                 (cy (y) (nth-value 1 (plot-to-canvas viewport wx1 y))))
            (let* ((left (min (cx wx1) (cx wx2)))
                   (right (max (cx wx1) (cx wx2)))
                   (top (min (cy wy1) (cy wy2)))
                   (bottom (max (cy wy1) (cy wy2)))
                   (box (list left right top bottom))
                   (margins (list :bottom 0 :top 0 :left 0 :right 0)))
              (with-viewport (viewport :clip nil)
                (with-font (device (or font (current-font device)) :size size)
                  ;; grid and zero axes, under everything else
                  (when grid
                    (with-style (device :color :light-gray :line-width 0.5)
                      (dolist (x (append (mapcar #'first x-majors)
                                         (when (eq grid :minor) x-minors)))
                        (plot-line device x wy1 x wy2))
                      (dolist (y (append (mapcar #'first y-majors)
                                         (when (eq grid :minor) y-minors)))
                        (plot-line device wx1 y wx2 y))))
                  (when zero-axes
                    (when (< (min wx1 wx2) 0 (max wx1 wx2))
                      (plot-line device 0 wy1 0 wy2))
                    (when (< (min wy1 wy2) 0 (max wy1 wy2))
                      (plot-line device wx1 0 wx2 0)))
                  (when frame
                    (draw-frame viewport))
                  ;; ticks
                  (dolist (side ticks)
                    (let* ((along-x (member side '(:bottom :top)))
                           (majors (if along-x x-majors y-majors))
                           (minors (if along-x x-minors y-minors))
                           (to-canvas (if along-x #'cx #'cy)))
                      (stroke-ticks device side
                                    (mapcar (lambda (m) (funcall to-canvas (first m)))
                                            majors)
                                    major-length tick-direction box)
                      (when minor
                        (stroke-ticks device side (mapcar to-canvas minors)
                                      minor-length tick-direction box))))
                  ;; numbers, recording how far they reach from each side
                  (multiple-value-bind (ignore ascent descent) (text-size device "0")
                    (declare (ignore ignore))
                    (let ((height (+ ascent descent)))
                      (dolist (side numbers)
                        (let ((reach 0))
                          (ecase side
                            ((:bottom :top)
                             (if (realp x-numbers)
                                 ;; slanted, reading upward, ending at the tick
                                 (let ((angle (* x-numbers (/ pi 180))))
                                   (dolist (m (trim-majors x-majors wx1 wx2 :x-low :x-high
                                                           omit-end-numbers))
                                     (let ((w (text-size device (second m))))
                                       (put-text device (cx (first m))
                                                 (if (eq side :bottom)
                                                     (+ bottom (getf outs side) gap)
                                                     (- top (getf outs side) gap))
                                                 (second m)
                                                 :angle (- angle)
                                                 :halign (if (eq side :bottom) :right :left)
                                                 :valign :center)
                                       (setf reach (max reach (+ (* w (sin angle))
                                                                 (* height (cos angle))))))))
                                 (let ((last-right nil))
                                   ;; left to right on the page, skipping a
                                   ;; number that would overlap the last drawn
                                   (dolist (m (sort (copy-list
                                                     (trim-majors x-majors wx1 wx2 :x-low :x-high
                                                                  omit-end-numbers))
                                                    #'< :key (lambda (m) (cx (first m)))))
                                     (let* ((x (cx (first m)))
                                            (w (text-size device (second m))))
                                       (unless (and last-right (< (- x (/ w 2)) (+ last-right gap)))
                                         (setf last-right (+ x (/ w 2)))
                                         (put-text device x
                                                   (if (eq side :bottom)
                                                       (+ bottom (getf outs side) gap)
                                                       (- top (getf outs side) gap))
                                                   (second m)
                                                   :halign :center
                                                   :valign (if (eq side :bottom) :top :bottom)))))
                                   (setf reach height))))
                            ((:left :right)
                             (dolist (m (trim-majors y-majors wy1 wy2 :y-low :y-high
                                                     omit-end-numbers))
                               (let* ((sign (if (eq side :left) -1 1))
                                      (x (+ (if (eq side :left) left right)
                                            (* sign (+ (getf outs side) gap))))
                                      (y (cy (first m))))
                                 (setf reach
                                       (max reach
                                            (if (eq y-numbers :vertical)
                                                (progn
                                                  (put-text device x y (second m)
                                                            :angle (- (/ pi 2))
                                                            :halign :center
                                                            :valign (if (eq side :left)
                                                                        :bottom :top))
                                                  height)
                                                (put-text device x y (second m)
                                                          :halign (if (eq side :left)
                                                                      :right :left)
                                                          :valign :center))))))))
                          (setf (getf margins side) (+ gap reach)))))
                  ;; axis labels and title, beyond ticks and numbers
                  (let ((xc (/ (+ left right) 2))
                        (yc (/ (+ top bottom) 2)))
                    (when x-label
                      (put-text device xc (+ bottom (getf outs :bottom) (getf margins :bottom) gap)
                                x-label :halign :center :valign :top))
                    (when y-label
                      (put-text device (- left (getf outs :left) (getf margins :left) gap) yc
                                y-label :angle (- (/ pi 2))
                                        :halign :center :valign :bottom))
                    (when title
                      (put-text device xc (- top (getf outs :top) (getf margins :top) gap)
                                title :halign :center :valign :bottom))))))))))))
  viewport)
;;; ============================================================
;;; Category axes
;;; ============================================================

(defun category-ticks (categories &key (start 0))
  "A tick generator for DRAW-BOX's :X-TICKS or :Y-TICKS naming
   CATEGORIES (strings or runs) at the positions START, START + 1, ...
   (as BAR-CHART, PLOT-BOX-PLOT and PLOT-VIOLINS place them), with no
   minor ticks."
  (lambda (a b)
    (values (loop for c in categories
                  for k from start
                  when (<= (- (min a b) 1d-9) k (+ (max a b) 1d-9))
                    collect (list k c))
            '())))

;;; ============================================================
;;; Sexagesimal axes
;;; ============================================================
;;;
;;; For right ascension and declination: plot coordinates are degrees;
;;; ticks fall on round sexagesimal values and are labeled 12h30m15s
;;; (h, m, s raised) for UNIT :HOURS, or +45°30'15" for :DEGREES.

(defparameter +sexagesimal-steps+
  ;; in units of seconds (of time or of arc), below an hour or degree
  '(1 2 5 10 15 20 30 60 120 300 600 900 1200 1800))

(defparameter +hour-steps+ '(3600 7200 10800 14400 21600 43200)
  "Steps of 1, 2, 3, 4, 6 and 12 hours, in seconds.")

(defparameter +degree-steps+
  '(3600 7200 18000 36000 54000 72000 108000 162000 216000 324000)
  "Steps of 1, 2, 5, 10, 15, 20, 30, 45, 60 and 90 degrees, in seconds.")

(defun sexagesimal-text (value unit resolution sign)
  "Text for VALUE (hours or degrees) to RESOLUTION seconds: runs with
   raised h m s for :HOURS, or degrees, ' and \" for :DEGREES (or
   :LONGITUDE, wrapped to 0..360), with a + sign if SIGN and VALUE is
   positive."
  (let* ((value (case unit                                    ; longitudes wrap
                  (:hours (mod value 24))
                  (:longitude (mod value 360))
                  (t value)))
         (total (round (* (abs value) 3600) resolution))
         (total (if (eq unit :longitude) (mod total (round (* 360 3600) resolution)) total))
         (seconds (* total resolution))
         (d (floor seconds 3600)) (m (floor (mod seconds 3600) 60)) (s (mod seconds 60))
         (show-m (< resolution 3600)) (show-s (< resolution 60))
         (prefix (cond ((minusp value) "-") ((and sign (plusp total)) "+") (t ""))))
    (flet ((up (x) (list x :scale 0.65 :rise 0.45)))
      (if (eq unit :hours)
          (append (list (format nil "~A~D" prefix d) (up "h"))
                  (when show-m (list (format nil "~2,'0D" m) (up "m")))
                  (when show-s (list (format nil "~2,'0D" s) (up "s"))))
          (append (list (format nil "~A~D°" prefix d))
                  (when show-m (list (format nil "~2,'0D'" m)))
                  (when show-s (list (format nil "~2,'0D\"" s))))))))

(defun sexagesimal-ticks (&key (unit :degrees) (target 5) sign)
  "A tick generator for DRAW-BOX's :X-TICKS or :Y-TICKS on an axis in
   degrees: about TARGET ticks at round sexagesimal steps, labeled in
   UNIT :HOURS (degrees / 15, as right ascension), :DEGREES (as
   declination, with SIGN adding + to positive values) or :LONGITUDE
   (degrees wrapped to 0..360)."
  (lambda (a b)
    (let* ((lo (min a b)) (hi (max a b))
           (scale (if (eq unit :hours) 1/15 1))     ; degrees -> displayed units
           (span (* 3600 scale (- hi lo)))          ; in seconds
           (steps (append +sexagesimal-steps+
                          (if (eq unit :hours) +hour-steps+ +degree-steps+)))
           (step (or (find-if (lambda (s) (<= (/ span s) (* 1.4 target))) steps)
                     (car (last steps))))
           (sub (cond ((member step '(2 20 120 1200 7200 14400 72000)) 4)
                      ((member step '(15 900 10800 54000 162000 43200)) 3)
                      ((member step '(30 1800 21600 108000 216000 324000)) 3)
                      (t 5)))
           (step-degrees (/ step 3600 scale))
           (majors (multiples-in step-degrees lo hi))
           (minors (remove-if (lambda (v) (< (abs (- (/ v step-degrees) (round v step-degrees))) 1d-6))
                              (multiples-in (/ step-degrees sub) lo hi))))
      (values (mapcar (lambda (v) (list v (sexagesimal-text (* v scale) unit step sign)))
                      majors)
              minors))))

;;; ============================================================
;;; Secondary axes
;;; ============================================================

(defun invert-monotonic (function target lo hi)
  "The X in LO..HI where the monotonic FUNCTION reaches TARGET, by
   bisection."
  (let ((increasing (< (funcall function lo) (funcall function hi))))
    (loop repeat 60
          for mid = (/ (+ lo hi) 2)
          do (if (eq increasing (< (funcall function mid) target))
                 (setf lo mid)
                 (setf hi mid))
          finally (return (/ (+ lo hi) 2)))))

(defplot draw-secondary-axis (viewport side function &key inverse label ticks format size font
                                                       (minor t) (tick-direction :in))
  "Draw an axis on SIDE (:TOP or :RIGHT) of VIEWPORT in other units:
   FUNCTION maps the window's x (for :TOP) or y (for :RIGHT) to the
   secondary value and must be monotonic; INVERSE, if given, maps back
   (else it is found numerically).  Ticks come from TICKS (a generator
   as for DRAW-BOX's :X-TICKS) or round secondary values (FORMAT as
   AXIS-TICKS), with numbers outside and LABEL beyond them.  Ask
   DRAW-BOX not to tick and number that side."
  (let* ((device (viewport-device viewport))
         (size (or size (default-text-size device)))
         (major-length (* 0.6 size)) (minor-length (* 0.3 size)) (gap (* 0.5 size))
         (out (if (member tick-direction '(:out :both)) major-length 0)))
    (multiple-value-bind (wx1 wx2 wy1 wy2) (viewport-window viewport)
      (multiple-value-bind (p1 p2) (if (eq side :top) (values wx1 wx2) (values wy1 wy2))
        (let ((s1 (funcall function p1)) (s2 (funcall function p2)))
          (multiple-value-bind (majors minors)
              (if ticks
                  (funcall ticks (min s1 s2) (max s1 s2))
                  (axis-ticks (min s1 s2) (max s1 s2) :format format))
            (flet ((primary (s) (if inverse
                                    (funcall inverse s)
                                    (invert-monotonic function s (min p1 p2) (max p1 p2)))))
              (multiple-value-bind (left right bottom top) (viewport-rectangle viewport :canvas)
                (declare (ignore bottom))
                (with-viewport (viewport :clip nil)
                  (with-font (device (or font (current-font device)) :size size)
                    (flet ((at (s)         ; canvas position along the side
                             (let ((p (primary s)))
                               (if (eq side :top)
                                   (nth-value 0 (plot-to-canvas viewport p wy1))
                                   (nth-value 1 (plot-to-canvas viewport wx1 p))))))
                      ;; ticks
                      (dolist (set (list (list (mapcar #'first majors) major-length)
                                         (when minor (list minors minor-length))))
                        (when set
                          (destructuring-bind (values length) set
                            (stroke-ticks device side (mapcar #'at values) length
                                          tick-direction (list left right top
                                                               (nth-value 1 (plot-to-canvas
                                                                             viewport wx1 wy1)))))))
                      ;; numbers, then the label beyond the farthest one
                      (let ((reach 0))
                        (dolist (m majors)
                          (let ((c (at (first m))))
                            (if (eq side :top)
                                (progn (put-text device c (- top out gap) (second m)
                                                 :halign :center :valign :bottom)
                                       (setf reach (max reach (nth-value 1 (text-size device "0"))
                                                        (+ (nth-value 1 (text-size device "0"))
                                                           (nth-value 2 (text-size device "0"))))))
                                (setf reach (max reach
                                                 (put-text device (+ right out gap) c (second m)
                                                           :halign :left :valign :center))))))
                        (when label
                          (if (eq side :top)
                              (put-text device (/ (+ left right) 2) (- top out gap reach gap)
                                        label :halign :center :valign :bottom)
                              (put-text device (+ right out gap reach gap)
                                        (/ (+ top (nth-value 1 (plot-to-canvas viewport wx1 wy1))) 2)
                                        label :angle (- (/ pi 2)) :halign :center
                                              :valign :top))))))))))))))
  viewport)
