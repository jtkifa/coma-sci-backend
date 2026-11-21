;;;; colors.lisp — example colors
;;;;
;;;; The named colors, and transparency.
;;;;
;;;; Uses: with-color, plot-rectangle, plot-text, color, plot-circle

(in-package :cl-cairoplot-examples)

(define-example colors (d :aspect 0.5)
  "The named colors, and transparency."
  (page-title d "Named colors")
  (let ((names (sort (loop for k being the hash-keys of *named-colors* collect k)
                     #'string< :key #'symbol-name)))
    (loop for name in names
          for i from 0
          for x = (+ 0.03 (* 0.24 (mod i 4)))
          for y = (- 0.85 (* 0.075 (floor i 4)))
          do (with-color (d name) (plot-rectangle d x y (+ x 0.05) (+ y 0.05)
                                                  :coords :device :fill t))
             (plot-rectangle d x y (+ x 0.05) (+ y 0.05) :coords :device)
             (plot-text d (+ x 0.06) (+ y 0.025) (format nil "~(~S~)" name)
                        :coords :device :valign :center :size 11)))
  (loop for (color x) in '((:red 0.62) (:green 0.7) (:blue 0.78))
        do (with-color (d (append (cdr (assoc color '((:red 1 0 0) (:green 0 1 0)
                                                      (:blue 0 0 1))))
                                  '(0.4)))
             (plot-circle d x 0.22 0.05 :coords :device :round t :fill t))))
