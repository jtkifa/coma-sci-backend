;;;; markers.lisp — example markers
;;;;
;;;; Every named marker, PGPLOT's symbol numbers, and a scatter plot in a
;;;; rotated viewport (markers turn with it and keep their shape).
;;;;
;;;; Uses: plot-points, marker-names, plot-point, plot-text,
;;;;       with-color, draw-box, make-viewport

(in-package :cl-cairoplot-examples)

(define-example markers (d :aspect 0.9)
  "Every named marker, PGPLOT's symbol numbers, and a scatter plot in a
   rotated viewport (markers turn with it and keep their shape)."
  (page-title d "Markers (plot-points)")
  ;; named markers
  (loop for name in (marker-names)
        for i from 0
        for x = (+ 0.05 (* 0.19 (mod i 5)))
        for y = (- 0.9 (* 0.045 (floor i 5)))
        do (plot-point d x y :marker name :coords :device)
           (plot-text d (+ x 0.025) y (format nil "~(~S~)" name)
                      :coords :device :valign :center :size 10))
  ;; PGPLOT symbol numbers
  (plot-text d 0.05 0.635 "PGPLOT symbol numbers:" :coords :device :size 11)
  (loop for code in (append (loop for i from 0 to 27 collect i)
                            (loop for i from -3 downto -8 collect i))
        for i from 0
        for x = (+ 0.05 (* 0.0525 (mod i 17)))
        for y = (- 0.58 (* 0.07 (floor i 17)))
        do (plot-point d x y :marker code :coords :device)
           (plot-text d x (- y 0.03) (format nil "~D" code)
                      :coords :device :halign :center :size 8))
  ;; scatter plot, unrotated and rotated
  (let ((*random-state* (sb-ext:seed-random-state 42)))
    (let* ((xs (loop repeat 60 collect (random 10.0)))
           (ys (mapcar (lambda (x) (+ (* 0.8 x) (- (random 3.0) 1.5))) xs)))
      (flet ((panel (vp)
               (with-color (vp :blue) (plot-points vp xs ys :marker :filled-circle))
               (with-color (vp :red)
                 (plot-points vp '(2 5 8) '(1.6 4 6.4) :marker :star :size 7))
               (draw-box vp :x-label "x" :y-label "y")))
        (panel (make-viewport d 0.1 0.42 0.08 0.4 :window '(0 10 -2 10)))
        (panel (make-viewport d 0.62 0.92 0.08 0.38 :window '(0 10 -2 10) :angle 20))))))
