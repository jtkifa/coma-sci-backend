;;;; errorbars-and-legends.lisp — example errorbars-and-legends
;;;;
;;;; A light curve with error bars, a model curve and its 1-sigma band
;;;; (FILL-BETWEEN), an annotation and a legend; a log-log spectrum with
;;;; asymmetric errors and an upper limit; and arrows.
;;;;
;;;; Uses: make-viewport, plot-fill-between, plot-polyline,
;;;;       plot-error-bars, plot-points, annotate, draw-box, draw-legend,
;;;;       plot-arrow, plot-text

(in-package :cl-cairoplot-examples)

(define-example errorbars-and-legends (d :size 900 :aspect 0.62)
  "Error bars, a model with a shaded 1-sigma band, an annotation arrow
   and a legend; asymmetric errors on a log-log plot; arrows."
  (page-title d "Error bars, filled bands, legends and annotations")
  ;; light curve: decaying flux with a flare
  (let* ((ts (loop for k from 0 below 40 collect (+ 0.5 (* 0.5 k))))
         (model (lambda (time) (+ 2 (* 6 (exp (- (/ time 6)))))))
         (errs (mapcar (lambda (time) (+ 0.3 (* 0.04 time))) ts))
         (fluxes (loop for time in ts for err in errs for k from 0
                       collect (+ (funcall model time) (* err (gaussian-noise k))
                                  (if (<= 12 time 13.5) 2.5 0))))
         (vp (make-viewport d 0.07 0.6 0.17 0.88 :window '(0 20.5 0 10))))
    (multiple-value-bind (mxs mys) (samples model 0 20.5 200)
      (with-color (vp '(1 0.5 0 0.25))
        (plot-fill-between vp mxs (map 'vector (lambda (y) (+ y 0.8)) mys)
                           :y2 (map 'vector (lambda (y) (- y 0.8)) mys)))
      (with-style (vp :color :orange :line-width 1.5) (plot-polyline vp mxs mys))
      (with-color (vp :blue)
        (plot-error-bars vp ts fluxes :yerr errs :xerr 0.25 :cap-size 0)
        (plot-points vp ts fluxes :marker :filled-circle :size 3))
      (annotate vp 12.5 7.4 "flare" 16 7.6 :size 14))
    (draw-box vp :x-label "time (days)" :y-label "flux (mJy)" :title "light curve")
    (draw-legend vp '(("observed" :marker :filled-circle :color :blue)
                      ("model" :line t :color :orange :line-width 1.5)
                      ("model 1σ band" :fill (1 0.5 0 0.25)))))
  ;; spectrum on log axes: errors as log10 of (value - err, value + err)
  (let* ((nus (loop for k from 0 below 8 collect (expt 10 (+ 9 (* 0.4 k)))))
         (fluxes (mapcar (lambda (nu) (* 3e3 (expt (/ nu 1e9) -0.7))) nus))
         (vp (make-viewport d 0.7 0.97 0.17 0.88 :window '(8.8 12.2 1 3.8))))
    (with-color (vp :dark-gray)
      (plot-error-bars vp (mapcar (lambda (nu) (log nu 10)) nus)
                       (mapcar (lambda (f) (log f 10)) fluxes)
                       :yerr-lower (mapcar (lambda (f) (- (log f 10) (log (* 0.7 f) 10))) fluxes)
                       :yerr-upper (mapcar (lambda (f) (- (log (* 1.2 f) 10) (log f 10))) fluxes))
      (plot-points vp (mapcar (lambda (nu) (log nu 10)) nus)
                   (mapcar (lambda (f) (log f 10)) fluxes) :marker :square :size 3)
      ;; an upper limit: a downward arrow
      (plot-arrow vp 12 1.9 12 1.45 :head-size 6)
      (plot-text vp 11.95 1.95 "limit" :halign :right :size 10))
    (draw-box vp :x-log t :y-log t :x-label "ν (Hz)" :y-label "S (mJy)"
                 :title "asymmetric errors"))
  ;; arrows
  (plot-arrow d 0.1 0.03 0.3 0.03 :coords :device)
  (plot-arrow d 0.35 0.03 0.55 0.03 :coords :device :fill nil)
  (plot-arrow d 0.6 0.03 0.8 0.03 :coords :device :both t)
  (plot-text d 0.84 0.03 "plot-arrow" :coords :device :valign :center :size 11))
