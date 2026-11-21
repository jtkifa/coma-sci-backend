;;;; insets.lisp — example insets
;;;;
;;;; Insets: small viewports inside a plot.  A spectrum with a zoom on
;;;; the Hα region, the inset placed by fractions of the plot; and an
;;;; image with a zoom on a star, the inset placed in pixel coordinates.
;;;; ADD-INSET clears the inset's background, and with :ZOOM marks the
;;;; region it shows with a rectangle and lines to its corners.
;;;;
;;;; Uses: add-inset (:coords :fraction/:plot, :window, :zoom),
;;;;       draw-zoom-indicator, plot-polyline, plot-image, draw-box

(in-package :cl-cairoplot-examples)

(defun mock-spectrum ()
  "Wavelengths (Å) and fluxes of a synthetic spectrum: a sloped
   continuum, absorption lines, and Hα with the [N II] doublet."
  (let ((ws (loop for w from 4000 to 7000 by 0.5 collect w)))
    (flet ((line (w w0 sigma depth) (* depth (exp (- (/ (expt (- w w0) 2) (* 2 sigma sigma)))))))
      (values ws
              (loop for w in ws for k from 0
                    collect (+ 1 (* 0.15 (/ (- w 4000) 3000))
                               (- (line w 4861 4 0.3)) (- (line w 5175 6 0.2))
                               (- (line w 5890 3 0.25))
                               (line w 6563 2.5 2.4) (line w 6548 2.2 0.35)
                               (line w 6584 2.2 0.9)
                               (* 0.02 (gaussian-noise (+ k 9000)))))))))

(define-example insets (d :size 900 :aspect 0.48)
  "A spectrum with a zoom inset on Hα and [N II], and an image with a
   zoom inset on a star."
  (page-title d "Insets")
  (let ((vp (make-viewport d 0.07 0.55 0.13 0.86 :window '(4000 7000 0.5 3.8))))
    (multiple-value-bind (ws fs) (mock-spectrum)
      (with-style (vp :color "#2a78d6" :line-width 0.8) (plot-polyline vp ws fs))
      (draw-box vp :x-label "wavelength (Å)" :y-label "relative flux"
                   :title "zoom inset placed by fractions")
      (let ((inset (add-inset vp 0.06 0.5 0.5 0.95 :window '(6520 6610 0.8 3.6) :zoom t)))
        (with-style (inset :color "#2a78d6" :line-width 1) (plot-polyline inset ws fs))
        (plot-text inset 6563 3.3 "Hα" :halign :center :size 10)
        (plot-text inset 6584 2.15 "[N II]" :halign :center :size 10)
        (draw-box inset :size 9 :x-step 30))))
  (let ((vp (make-viewport d 0.64 0.98 0.13 0.86)))
    (set-window-equal-scales vp -0.5 159.5 -0.5 119.5)
    (let ((sky (sky-image)))
      (plot-image vp sky :colormap :gray :scale :asinh :vmin 0 :vmax 400 :reverse t)
      (draw-box vp :x-label "x (pixels)" :y-label "y (pixels)"
                   :title "zoom inset placed in pixels")
      ;; the faint star at (120, 40), shown in the upper left
      (let ((inset (add-inset vp 5 65 75 115 :coords :plot :window '(110 130 33 48) :zoom t)))
        (plot-image inset sky :colormap :gray :scale :asinh :vmin 0 :vmax 200 :reverse t)
        (draw-box inset :size 8 :ticks '() :numbers '())))))
