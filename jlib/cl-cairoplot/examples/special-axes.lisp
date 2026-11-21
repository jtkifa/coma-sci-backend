;;;; special-axes.lisp — example special-axes
;;;;
;;;; Secondary axes in other units (DRAW-SECONDARY-AXIS): wavelength
;;;; below with frequency on top, flux on the left with AB magnitude on
;;;; the right; and sexagesimal axes (SEXAGESIMAL-TICKS) on a field
;;;; set up with SET-WINDOW-SKY: right ascension in hours, increasing to
;;;; the left and compressed by cos(dec), and declination, with a
;;;; 2' x 2' square on the sky drawn square to show the scaling.
;;;;
;;;; Uses: make-viewport, plot-polyline, with-style,
;;;;       draw-box, draw-secondary-axis, plot-image, plot-points,
;;;;       with-color, set-window-sky, sexagesimal-ticks

(in-package :cl-cairoplot-examples)

(define-example special-axes (d :size 900 :aspect 0.5)
  "Secondary axes (frequency over wavelength, AB magnitude beside flux)
   and sexagesimal right ascension and declination axes on a sky field
   with RA scaled by cos(dec)."
  (page-title d "Secondary and sexagesimal axes")
  ;; a spectrum: flux density against wavelength in microns
  (let ((vp (make-viewport d 0.09 0.42 0.14 0.8 :window '(1 5 5 40))))
    (multiple-value-bind (xs ys)
        (samples (lambda (um) (+ 10 (* 25 (exp (- (/ (expt (- um 2.2) 2) 0.08))))
                                 (* 3 (sin (* 3 um)))))
                 1 5 400)
      (with-style (vp :color :blue :line-width 1.2) (plot-polyline vp xs ys)))
    (draw-box vp :ticks '(:bottom :left) :x-label "wavelength (μm)" :y-label "flux (μJy)")
    ;; frequency in THz = c / wavelength
    (draw-secondary-axis vp :top (lambda (um) (/ 299.792458 um))
                         :inverse (lambda (thz) (/ 299.792458 thz)) :label "frequency (THz)")
    ;; AB magnitude = 23.9 - 2.5 log10(flux in microjansky)
    (draw-secondary-axis vp :right (lambda (f) (- 23.9 (* 2.5 (log f 10))))
                         :label "AB magnitude"))
  ;; a field in right ascension and declination, at dec +62 where an RA
  ;; degree is about half as long as a Dec degree
  (let* ((ra0 187.70) (dec0 62.39)
         (vp (make-viewport d 0.64 0.96 0.14 0.8))
         ;; 0.12 x 0.09 degrees on the sky; DRA is the RA half-range
         (dra (set-window-sky vp ra0 dec0 0.12 0.09)))
    ;; the image, square pixels on the sky, spans the same RA range
    (plot-image vp (sky-image) :colormap :gray :scale :asinh :vmin 0 :vmax 300 :reverse t
                               :x-range (list (+ ra0 dra) (- ra0 dra))
                               :y-range (list (- dec0 0.09) (+ dec0 0.09)))
    (with-color (vp :red)
      (plot-points vp (list ra0) (list dec0) :marker :cross :size 6))
    ;; a square 2' x 2' on the sky: 2' of Dec, but 2'/cos(dec) of RA;
    ;; drawn square on the page, it shows the RA scaling is right
    (let* ((ra ra0) (dec (- dec0 0.06))
           (half 1/60) (half-ra (/ half (cos (* dec0 (/ pi 180))))))
      (with-style (vp :color :blue :line-width 1.2)
        (plot-rectangle vp (- ra half-ra) (- dec half) (+ ra half-ra) (+ dec half)))
      (plot-text vp ra (+ dec half) "2' × 2'" :halign :center :valign :bottom :size 11
                                             :halo t))
    (draw-box vp :x-ticks (sexagesimal-ticks :unit :hours :target 4)
                 :y-ticks (sexagesimal-ticks :sign t :target 4)
                 :x-label "right ascension" :y-label "declination"
                 :title "set-window-sky at dec +62°")))
