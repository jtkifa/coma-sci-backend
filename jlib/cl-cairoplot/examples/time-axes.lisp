;;;; time-axes.lisp — example time-axes
;;;;
;;;; Date and time axes with TIME-TICKS, which labels an axis of times
;;;; (MJD, JD, Unix or universal time) at round calendar steps, from
;;;; seconds to years, concisely: the month at its start, the year at
;;;; January, the date at midnight, context on the first day or month
;;;; tick; numbers that would collide are skipped.  A
;;;; light curve over four months has MJD numbers below and calendar
;;;; dates above; one night's altitude curve crosses midnight; a
;;;; fifteen-year series in Unix seconds gets year ticks; and a
;;;; three-minute occultation gets seconds.
;;;;
;;;; Uses: time-ticks (:scale), iso-to-mjd, calendar-to-mjd, draw-box
;;;;       (:x-ticks, a second call for the top axis), plot-points,
;;;;       plot-error-bars, plot-polyline, plot-fill-between,
;;;;       plot-rectangle

(in-package :cl-cairoplot-examples)

(define-example time-axes (d :size 900 :aspect 0.75)
  "Date and time axes: a light curve with MJD below and dates above, one
   night across midnight in hours, fifteen years of monthly data in
   Unix seconds, and a three-minute occultation in seconds."
  (page-title d "Date and time axes")
  (let ((panels (make-panel-grid d 2 2 :left 0.1 :right 0.97 :bottom 0.09 :top 0.85
                                       :hspace 0.2 :wspace 0.12)))
    ;; a light curve: MJD numbers below, calendar dates above
    (let* ((vp (aref panels 0 0))
           (t0 (iso-to-mjd "2024-03-01")) (t1 (iso-to-mjd "2024-07-01"))
           (times (loop for k below 70
                        collect (+ t0 (* (- t1 t0) (+ 0.5 (* 0.98 (noise (* 7 k))))))))
           (mags (mapcar (lambda (time k)
                           (+ 14.2 (* 0.35 (sin (* 2 pi (/ (- time t0) 3.71))))
                              (* 0.04 (gaussian-noise (+ 500 k)))))
                         times (loop for k below 70 collect k))))
      (set-window vp t0 t1 14.75 13.65)
      (plot-error-bars vp times mags :yerr 0.04 :cap-size 0)
      (with-color (vp "#2a78d6")
        (plot-points vp times mags :marker :filled-circle :size 2.5))
      (draw-box vp :ticks '(:bottom :left :right) :x-label "MJD" :y-label "V (mag)")
      (draw-box vp :frame nil :ticks '(:top) :numbers '(:top) :minor t
                   :x-ticks (time-ticks :scale :mjd) :title "light curve: MJD below, dates above"))
    ;; one night, across midnight
    (let* ((vp (aref panels 0 1))
           (t0 (iso-to-mjd "2024-03-14T19:00")) (t1 (iso-to-mjd "2024-03-15T06:00"))
           (dusk (iso-to-mjd "2024-03-14T20:10")) (dawn (iso-to-mjd "2024-03-15T04:50"))
           (times (loop for k to 100 collect (+ t0 (* (- t1 t0) (/ k 100)))))
           (alts (mapcar (lambda (time)
                           (* (/ 180 pi)
                              (asin (+ (* 0.42 0.80) (* 0.91 0.60 (cos (* 2 pi (- time 60384.03))))))))
                         times)))
      (set-window vp t0 t1 0 90)
      (with-color (vp :very-light-gray)
        (plot-rectangle vp t0 0 dusk 90 :fill t)
        (plot-rectangle vp dawn 0 t1 90 :fill t))
      (with-style (vp :color "#2a78d6" :line-width 1.5)
        (plot-polyline vp times alts))
      (draw-box vp :x-ticks (time-ticks :scale :mjd) :x-label "UTC, night of 2024 Mar 14"
                   :y-label "altitude (deg)"
                   :title "one night (twilight shaded)"))
    ;; fifteen years of monthly values, the axis in Unix seconds
    (let* ((vp (aref panels 1 0))
           (months (loop for k below 180
                         collect (multiple-value-bind (y m) (floor k 12)
                                   (time-to-unix (calendar-to-mjd (+ 2010 y) (1+ m) 15) :mjd))))
           (values (loop for k below 180
                         collect (+ 20 (* 0.05 k) (* 6 (sin (* 2 pi (/ k 12))))
                                    (* 1.5 (gaussian-noise (+ 900 k)))))))
      (set-window vp (time-to-unix (calendar-to-mjd 2010 1 1) :mjd)
                  (time-to-unix (calendar-to-mjd 2025 1 1) :mjd) 0 45)
      (with-color (vp "#1baf7a")
        (plot-fill-between vp months values))
      (with-style (vp :color "#008300" :line-width 0.8)
        (plot-polyline vp months values))
      (draw-box vp :x-ticks (time-ticks :scale :unix) :y-label "clear hours per month"
                   :title "fifteen years (axis in Unix seconds)"))
    ;; three minutes of an occultation
    (let* ((vp (aref panels 1 1))
           (t0 (iso-to-mjd "2024-03-14T21:30:00")) (t1 (iso-to-mjd "2024-03-14T21:33:00"))
           (times (loop for k to 180 collect (+ t0 (/ k 86400d0))))
           (flux (loop for k to 180
                       collect (+ (if (<= 84 k 97) 0.08 1.0) (* 0.03 (gaussian-noise (+ 3000 k)))))))
      (set-window vp t0 t1 -0.1 1.25)
      (with-color (vp "#eb6834")
        (plot-histogram vp nil :edges (cons (- (first times) (/ 0.5 86400))
                                            (mapcar (lambda (time) (+ time (/ 0.5 86400))) times))
                               :counts flux))
      (draw-box vp :x-ticks (time-ticks :scale :mjd) :x-label "UTC, 2024 Mar 14"
                   :y-label "normalized flux" :title "an occultation, 1 s cadence"))))
