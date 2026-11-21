;;;; viewport-targets.lisp — example viewport-targets
;;;;
;;;; Plotting functions take a device or a viewport.  Given a viewport
;;;; they draw in it, entering it (its frame and clip) for the call, so
;;;; panels need no WITH-VIEWPORT; given the device they draw in its
;;;; selected viewport, which SETF of DEVICE-VIEWPORT changes.  Here the
;;;; left panel is drawn by passing its viewport, the curve clipped at
;;;; the window's edge; the right one is rotated and still needs no
;;;; WITH-VIEWPORT, text and rotated markers included.  Page text given
;;;; the device in :DEVICE coordinates stays on the page.
;;;;
;;;; Uses: plot-polyline, plot-points, plot-fill-between, plot-image,
;;;;       plot-text, with-plot-rotation, draw-box (given a viewport),
;;;;       (setf device-viewport), make-viewport :angle

(in-package :cl-cairoplot-examples)

(define-example viewport-targets (d :size 900 :aspect 0.6)
  "Plotting into viewports by passing them to the plotting functions:
   a clipped curve and points in one panel, an image, text and rotated
   markers in a rotated one, and a selected viewport used through the
   device, all without WITH-VIEWPORT."
  (page-title d "Drawing into viewports without WITH-VIEWPORT")
  (let ((left (make-viewport d 0.08 0.46 0.12 0.82 :window '(0 10 -1.5 1.5)))
        (right (make-viewport d 0.6 0.9 0.2 0.72 :window '(0 1 0 1) :angle 15)))
    ;; the left panel, given its viewport; the curve overshoots the window
    (multiple-value-bind (xs ys) (samples (lambda (x) (* 1.9 (exp (/ x -9)) (sin (* 1.4 x))))
                                          0 10)
      (with-color (d '(0.16 0.47 0.84 0.2))
        (plot-fill-between left xs ys))
      (with-style (d :color "#2a78d6" :line-width 1.5)
        (plot-polyline left xs ys)))
    (with-color (d "#eb6834")
      (plot-points left '(1 3 5 7 9) '(1.0 -0.6 0.4 -0.3 0.2) :marker :filled-square))
    (draw-box left :x-label "x" :y-label "y" :title "(plot-polyline vp ...), clipped")
    ;; the right panel, rotated: image, text and markers turn with it
    (plot-image right (wave-image) :colormap :viridis :x-range '(0 1) :y-range '(0 1))
    (with-plot-rotation (right 30 0.3 0.7)
      (with-color (d :white)
        (plot-points right '(0.3) '(0.7) :marker :square :size 14)))
    (plot-text right 0.5 0.1 "text in the frame" :halign :center :size 11
                                                 :halo '(:color :white :width 2.5))
    (draw-box right :title "rotated 15°, no with-viewport")
    ;; select the left panel: calls given the device now draw there
    (setf (device-viewport d) left)
    (plot-text d 5 1.25 "drawn through the device" :halign :center :size 10)
    ;; page coordinates are not tied to any viewport
    (plot-text d 0.5 0.03 "(:coords :device text is on the page)" :coords :device
                                                                   :halign :center :size 9)))
