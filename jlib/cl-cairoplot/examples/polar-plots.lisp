;;;; polar-plots.lisp — example polar-plots
;;;;
;;;; Polar plots: after SET-WINDOW-POLAR every plotting function takes
;;;; :COORDS :POLAR (angle in degrees, radius).  Curves in the
;;;; mathematical convention (0° east, counterclockwise); a wind rose
;;;; with compass bearings (0° north, clockwise) as stacked wedges; a
;;;; half-disk sector of an antenna pattern in decibels (the center at
;;;; -40 dB); and a visual binary's orbit by position angle (from north
;;;; through east) and separation, with the observations.
;;;;
;;;; Uses: set-window-polar (:zero, :direction, :r-min, :sector),
;;;;       draw-polar-grid (:angle-labels, :r-format), plot-polyline,
;;;;       plot-points, plot-polar-bars (:bottoms), draw-legend (placed
;;;;       :best, clear of the bars and labels)

(in-package :cl-cairoplot-examples)

(define-example polar-plots (d :size 900 :aspect 0.9)
  "Polar curves, a wind rose of stacked wedges with compass bearings, a
   sector in decibels, and a binary star's orbit by position angle."
  (page-title d "Polar plots")
  (let ((panels (make-panel-grid d 2 2 :left 0.04 :right 0.96 :bottom 0.03 :top 0.92
                                       :hspace 0.06 :wspace 0.06))
        (angles (loop for a from 0 to 360 by 1 collect a)))
    ;; curves: a cardioid-like limaçon and a three-petal rose
    (let ((vp (aref panels 0 0)))
      (set-window-polar vp 2)
      (draw-polar-grid vp :title "r = 1 + 0.8 cos θ,  r = 1.6 |sin 3θ|")
      (with-style (vp :color "#2a78d6" :line-width 1.5)
        (plot-polyline vp angles (mapcar (lambda (a) (+ 1 (* 0.8 (cos (* a (/ pi 180)))))) angles)
                       :coords :polar))
      (with-style (vp :color "#eb6834" :line-width 1.5)
        (plot-polyline vp angles (mapcar (lambda (a) (* 1.6 (abs (sin (* 3 a (/ pi 180))))))
                                         angles)
                       :coords :polar)))
    ;; a wind rose: frequency (%) by bearing, stacked by speed
    (let ((vp (aref panels 0 1))
          (bearings (loop for k below 16 collect (* 22.5 k)))
          (calm '(3 2 2 1 1 1 2 3 4 5 5 4 3 3 3 3))
          (breeze '(2 1 1 1 0.5 0.5 1 2 3 5 6 5 3 2 2 2))
          (strong '(1 0.5 0.3 0.2 0.2 0.3 0.5 1 2 3 4 3 1.5 1 0.8 0.8)))
      (set-window-polar vp 16 :zero :north :direction :clockwise :margin 0.32)
      (draw-polar-grid vp :angle-step 45 :r-format (lambda (r) (format nil "~D%" (round r)))
                          :angle-labels '("N" "NE" "E" "SE" "S" "SW" "W" "NW")
                          :title "wind rose: compass bearings")
      (let ((colors (category-colors 3)) (bottoms (make-list 16 :initial-element 0)))
        (loop for series in (list calm breeze strong)
              for color in colors
              do (plot-polar-bars vp bearings series :bottoms bottoms :color color)
                 (setf bottoms (mapcar #'+ bottoms series)))
        (draw-legend vp (mapcar (lambda (name color) (list name :fill color))
                                '("> 8 m/s" "3-8 m/s" "< 3 m/s") (reverse colors))
                     :size 9)))
    ;; a sector in decibels
    (let ((vp (aref panels 1 0))
          (beam (loop for a from -90 to 90 by 0.5 collect a)))
      (set-window-polar vp 0 :r-min -40 :zero :north :direction :clockwise :sector '(-90 90))
      (draw-polar-grid vp :r-format (lambda (r) (format nil "~D dB" (round r)))
                          :title "sector -90°..90°, radius -40..0 dB")
      (with-style (vp :color "#2a78d6" :line-width 1.5)
        (plot-polyline vp beam
                       (mapcar (lambda (a)
                                 (let* ((u (* 8 (sin (* a (/ pi 180)))))
                                        (g (if (< (abs u) 1d-9) 1 (expt (/ (sin u) u) 2))))
                                   (max -40 (* 10 (log (max g 1d-6) 10)))))
                               beam)
                       :coords :polar)))
    ;; a visual binary: position angle from north through east, as on the sky
    (let ((vp (aref panels 1 1)))
      (set-window-polar vp 1.2 :zero :north)
      (draw-polar-grid vp :angle-step 45 :r-format (lambda (r) (format nil "~,1F\"" r))
                          :title "binary orbit: position angle and separation")
      (flet ((orbit (phase)
               ;; an inclined ellipse on the sky: x east, y north (arcsec)
               (let* ((e (* 2 pi phase))
                      (x (+ 0.25 (* 0.75 (cos e)))) (y (* 0.45 (sin e)))
                      (c (cos 0.6)) (s (sin 0.6))
                      (east (- (* c x) (* s y))) (north (+ (* s x) (* c y))))
                 (values (mod (* (/ 180 pi) (atan east north)) 360)
                         (sqrt (+ (* east east) (* north north)))))))
        (let ((pas '()) (seps '()))
          (loop for k to 360
                do (multiple-value-bind (pa sep) (orbit (/ k 360))
                     (push pa pas) (push sep seps)))
          ;; join the orbit in :PLOT space, where it is an ellipse
          (multiple-value-bind (xs ys)
              (let ((xs '()) (ys '()))
                (loop for pa in pas for sep in seps
                      do (multiple-value-bind (x y) (polar-to-plot vp pa sep)
                           (push x xs) (push y ys)))
                (values xs ys))
            (with-style (vp :color "#2a78d6" :line-width 1.2) (plot-polyline vp xs ys))))
        (let ((obs-pa '()) (obs-sep '()))
          (loop for phase in '(0.05 0.18 0.31 0.47 0.6 0.72 0.88)
                do (multiple-value-bind (pa sep) (orbit phase)
                     (push pa obs-pa) (push sep obs-sep)))
          (with-color (vp "#eb6834")
            (plot-points vp obs-pa obs-sep :coords :polar :marker :filled-circle :size 3.5)))
        (with-color (vp :black) (plot-points vp '(0) '(0) :coords :polar :marker :star :size 7))))))
