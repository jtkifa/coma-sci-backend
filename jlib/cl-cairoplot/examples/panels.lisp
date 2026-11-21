;;;; panels.lisp — example panels
;;;;
;;;; A grid of panels sharing axes (MAKE-PANEL-GRID): no gaps, numbers
;;;; only on the outer edges, panel labels (a)..(f), and one legend
;;;; outside the grid.
;;;;
;;;; Uses: make-panel-grid, plot-polyline, plot-points,
;;;;       with-style, with-color, draw-box, panel-number-sides,
;;;;       panel-omitted-ends, draw-panel-label, draw-legend

(in-package :cl-cairoplot-examples)

(define-example panels (d :size 900 :aspect 0.6)
  "A 2 x 3 grid of panels sharing axes, with no gaps, numbers only on
   the outer edges, panel labels and a legend outside the grid."
  (page-title d "Panel grid with shared axes")
  (let ((grid (make-panel-grid d 2 3 :left 0.08 :right 0.82 :bottom 0.1 :top 0.9
                                     :hspace 0 :wspace 0 :window '(0 10 -1.4 1.4))))
    (dotimes (row 2)
      (dotimes (col 3)
        (let* ((vp (aref grid row col))
               (k (+ (* 3 row) col))
               (omega (+ 0.6 (* 0.25 k))))
          (multiple-value-bind (xs ys)
              (samples (lambda (x) (* (exp (/ x -12)) (sin (* omega x)))) 0 10 300)
            (with-style (vp :color :blue :line-width 1.2) (plot-polyline vp xs ys)))
          (let ((xs (loop for i from 0 below 20 collect (+ 0.25 (* 0.5 i)))))
            (with-color (vp :red)
              (plot-points vp xs (loop for x in xs for i from 0
                                       collect (+ (* (exp (/ x -12)) (sin (* omega x)))
                                                  (* 0.15 (gaussian-noise (+ i (* 50 k))))))
                           :marker :filled-circle :size 2.5)))
          (draw-box vp :numbers (panel-number-sides grid row col) :size 11
                       :omit-end-numbers (panel-omitted-ends grid row col)
                       :x-label (when (= row 1) "time")
                       :y-label (when (= col 0) "signal"))
          (draw-panel-label vp (format nil "(~C)" (code-char (+ 97 k))) :size 13))))
    (draw-legend (aref grid 0 2) '(("model" :line t :color :blue :line-width 1.2)
                                   ("data" :marker :filled-circle :color :red))
                 :position :outside-right)))
