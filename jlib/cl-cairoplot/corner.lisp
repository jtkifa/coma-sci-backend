;;;; corner.lisp — corner plots of samples
;;;;
;;;; CORNER-PLOT shows every pair of parameters of a set of samples (e.g.
;;;; from MCMC), as the corner.py package does: a triangle of panels with
;;;; each parameter's histogram on the diagonal, titled with its median
;;;; and the spread to its 16th and 84th percentiles, and below it each
;;;; pair's density as filled contours enclosing 1 and 2 sigma of the
;;;; samples (the fractions 1 - exp(-s^2/2) a 2D normal holds within s
;;;; sigma: 39% and 86%), with the samples outside the outer contour as
;;;; dots.  TRUTHS marks known values.  Panels share axes; numbers and
;;;; names are on the outer edges.

(in-package :cl-cairoplot)

(defun gaussian-smooth (array sigma)
  "ARRAY (2D, double-float) blurred by a Gaussian of SIGMA cells, as a
   new array (edges treated as empty)."
  (if (<= sigma 0)
      array
      (destructuring-bind (ny nx) (array-dimensions array)
        (let* ((half (ceiling (* 3 sigma)))
               (kernel (let ((k (loop for i from (- half) to half
                                      collect (exp (- (/ (* i i) (* 2 sigma sigma)))))))
                         (let ((sum (reduce #'+ k))) (mapcar (lambda (v) (/ v sum)) k))))
               (tmp (make-array (list ny nx) :element-type 'double-float :initial-element 0d0))
               (out (make-array (list ny nx) :element-type 'double-float :initial-element 0d0)))
          (dotimes (j ny)
            (dotimes (i nx)
              (loop for w in kernel for d from (- half)
                    for ii = (+ i d)
                    when (< -1 ii nx) do (incf (aref tmp j i) (* w (aref array j ii))))))
          (dotimes (j ny)
            (dotimes (i nx)
              (loop for w in kernel for d from (- half)
                    for jj = (+ j d)
                    when (< -1 jj ny) do (incf (aref out j i) (* w (aref tmp jj i))))))
          out))))

(defun sigma-thresholds (density sigmas)
  "The density values enclosing the fractions of the total mass a 2D
   normal holds within each of SIGMAS, in the same order."
  (let* ((values (sort (copy-seq (make-array (array-total-size density)
                                             :displaced-to density
                                             :element-type 'double-float))
                       #'>))
         (total (reduce #'+ values)))
    (loop for s in sigmas
          collect (let ((target (* total (- 1 (exp (* -0.5 s s))))) (sum 0d0))
                    (or (loop for v across values
                              do (incf sum v)
                              when (>= sum target) return v)
                        0d0)))))

(defun mix-white (color fraction)
  "COLOR lightened toward white by FRACTION (0: as is, 1: white)."
  (let ((c (parse-color color)))
    (flet ((mix (v) (+ v (* fraction (- 1 v)))))
      (list (mix (color-red c)) (mix (color-green c)) (mix (color-blue c))))))

(defun draw-corner-title (device cx cy name sorted size)
  "Write NAME = median, with the spreads to the 84th and 16th
   percentiles stacked after it (raised and lowered), centered on the
   canvas point CX and bottom at CY."
  (let* ((median (quantile sorted 0.5))
         (plus (- (quantile sorted 0.84) median))
         (minus (- median (quantile sorted 0.16)))
         ;; two significant figures of the smaller spread set the decimals
         (digits (max 0 (- 1 (floor (log (max 1d-300 (min plus minus)) 10)))))
         (main (append (if (stringp name) (list name) name)
                       (list (format nil " = ~,vF" digits median))))
         (up (format nil "+~,vF" digits plus))
         (down (format nil "-~,vF" digits minus))
         (small (* 0.7 size)))
    (with-saved-state (device)
      (set-font device (current-font device) :size size)
      (let ((main-width (text-size device main)))
        (set-font device (current-font device) :size small)
        (let* ((width (+ main-width (max (text-size device up) (text-size device down))))
               (x (- cx (/ width 2)))
               (base (- cy (* 0.35 size))))  ; room below for the lowered spread
          (put-text device x base main :size size)
          (put-text device (+ x main-width) (- base (* 0.45 size)) up :size small)
          (put-text device (+ x main-width) (+ base (* 0.3 size)) down :size small))))))

(defun corner-plot (device samples &key names (bins 30) ranges truths
                                        (sigmas '(1 2)) (smooth 1) (color "#2a78d6")
                                        (truth-color "#eb6834") (points t) (titles t)
                                        (left 0.1) (right 0.97) (bottom 0.1) (top 0.94)
                                        (space 0.012) size)
  "Draw a corner plot of SAMPLES, a list with a sequence of values for
   each parameter (all equally long), filling LEFT..RIGHT, BOTTOM..TOP
   of DEVICE (see the top of corner.lisp).  NAMES label the parameters;
   RANGES, a list of (LO HI) or NIL for each, default to the central 99%
   of the samples widened by 10%; BINS bins each axis.  The 2D densities are smoothed by SMOOTH
   bins and contoured at SIGMAS, filled in shades of COLOR; POINTS draws
   the samples outside the outer contour.  TRUTHS, a list of values (NIL
   to skip one), are marked in TRUTH-COLOR.  TITLES puts each median and
   spread over its histogram.  Returns the grid of viewports (only those
   on and below the diagonal are drawn)."
  (let* ((p (length samples))
         (data (mapcar (lambda (s) (map '(vector double-float) (lambda (v) (float v 1d0)) s))
                       samples))
         (sorted (mapcar (lambda (v) (sort (copy-seq v) #'<)) data))
         ;; by default the central 99% of the samples, widened 10%
         (ranges (loop for k below p
                       for s in sorted
                       collect (or (nth k ranges)
                                   (let* ((lo (quantile s 0.005)) (hi (quantile s 0.995))
                                          (pad (* 0.1 (- hi lo))))
                                     (list (- lo pad) (+ hi pad))))))
         (grid (make-panel-grid device p p :left left :right right :bottom bottom :top top
                                           :hspace space :wspace space))
         (size (or size (* 0.7 (default-text-size device))))
         (sigmas (sort (copy-list sigmas) #'<))
         ;; filled bands, outermost lightest
         (shades (loop for k below (length sigmas)
                       collect (mix-white color (* 0.75 (/ (- (length sigmas) k)
                                                           (length sigmas)))))))
    (dotimes (row p)
      (dotimes (col (1+ row))
        (let* ((vp (aref grid row col))
               (xr (nth col ranges)) (yr (nth row ranges))
               (truth-x (nth col truths)) (truth-y (nth row truths)))
          (if (= row col)
              ;; the histogram of one parameter
              (let* ((counts (nth-value 1 (histogram (nth col data) :bins bins :range xr)))
                     (peak (reduce #'max counts)))
                (set-window vp (first xr) (second xr) 0 (* 1.1 peak))
                (with-style (vp :color color :line-width 1.2)
                  (plot-histogram vp (nth col data) :bins bins :range xr))
                (with-style (vp :color color :dash '(4 3) :line-width 0.8)
                  (dolist (q '(0.16 0.5 0.84))
                    (let ((v (quantile (nth col sorted) q)))
                      (plot-line vp v 0 v (* 1.1 peak)))))
                (when truth-x
                  (with-style (vp :color truth-color :line-width 1.2)
                    (plot-line vp truth-x 0 truth-x (* 1.1 peak))))
                (when (and titles names)
                  (with-viewport (vp :clip nil)
                    (multiple-value-bind (cx cy)
                        (to-canvas device :plot (/ (+ (first xr) (second xr)) 2) (* 1.1 peak))
                      (draw-corner-title device cx (- cy (* 0.3 size)) (nth col names)
                                         (nth col sorted) size)))))
              ;; the density of a pair
              (multiple-value-bind (counts) (histogram2d (nth col data) (nth row data)
                                                         :bins bins :x-range xr :y-range yr)
                (set-window vp (first xr) (second xr) (first yr) (second yr))
                (let* ((density (gaussian-smooth counts smooth))
                       (thresholds (sigma-thresholds density sigmas))
                       ;; ascending levels: outer contour first
                       (levels (reverse thresholds))
                       (peak (loop for k below (array-total-size density)
                                   maximize (row-major-aref density k))))
                  (when points
                    (let ((outer (first levels)) (xs '()) (ys '()))
                      (loop for x across (nth col data) for y across (nth row data)
                            do (let ((i (floor (* bins (/ (- x (first xr)) (- (second xr) (first xr))))))
                                     (j (floor (* bins (/ (- y (first yr)) (- (second yr) (first yr)))))))
                                 (when (or (not (< -1 i bins)) (not (< -1 j bins))
                                           (< (aref density j i) outer))
                                   (push x xs) (push y ys))))
                      (with-color (vp '(0.2 0.2 0.2 0.6))
                        (plot-points vp xs ys :marker :filled-circle :size 0.7))))
                  (plot-filled-contours vp density :levels (append levels (list (* 1.01 peak)))
                                                   :colors shades :x-range xr :y-range yr)
                  (with-style (vp :color color :line-width 0.8)
                    (plot-contours vp density :levels levels :x-range xr :y-range yr)))
                (with-style (vp :color truth-color :line-width 1.2)
                  (when truth-x (plot-line vp truth-x (first yr) truth-x (second yr)))
                  (when truth-y (plot-line vp (first xr) truth-y (second xr) truth-y)))
                (when (and truth-x truth-y)
                  (with-color (vp truth-color)
                    (plot-point vp truth-x truth-y :marker :filled-square :size 2.5)))))
          ;; axes: numbers and names only on the outer edges
          (let ((bottom-row (= row (1- p))) (first-col (= col 0)))
            (draw-box vp :size size :x-numbers 45
                         :numbers (append (when bottom-row '(:bottom))
                                          (when (and first-col (/= row col)) '(:left)))
                         :ticks (if (= row col) '(:bottom :top) '(:bottom :top :left :right))
                         :x-label (and bottom-row names (nth col names))
                         :y-label (and first-col (/= row col) names (nth row names)))))))
    grid))
