;;;; box-and-violin-plots.lisp — example box-and-violin-plots
;;;;
;;;; Distributions by category: box-and-whisker plots (quartile box,
;;;; median, whiskers to 1.5 IQR, outliers, means), violin plots (kernel
;;;; density estimates with a box inside, or quartile lines), a
;;;; horizontal box plot, and violins scaled by count with the points
;;;; overlaid.  The data are synthetic seeing, color and residual
;;;; distributions, including a bimodal one that a box plot hides and a
;;;; violin shows.
;;;;
;;;; Uses: plot-box-plot (:means, outliers, :orientation), plot-violins
;;;;       (:inner :box/:quartiles, :scale :count), box-stats,
;;;;       category-ticks, draw-box, plot-points

(in-package :cl-cairoplot-examples)

(defun normal-sample (n mean sigma seed)
  (loop for k below n collect (+ mean (* sigma (gaussian-noise (+ seed k))))))

(defun distribution-data ()
  "Five samples: narrow, wide, skewed (with outliers), bimodal, small."
  (list (normal-sample 200 0.8 0.08 100)
        (normal-sample 200 1.1 0.2 1000)
        (append (mapcar (lambda (v) (+ 0.7 (expt (abs v) 1.6))) (normal-sample 200 0 0.45 2000))
                '(2.6 2.9 3.1))
        (append (normal-sample 120 0.75 0.07 3000) (normal-sample 120 1.35 0.09 4000))
        (normal-sample 25 1.0 0.15 5000)))

(define-example box-and-violin-plots (d :size 900 :aspect 0.75)
  "Box plots and violin plots of five distributions (one bimodal, one
   skewed with outliers): vertical boxes with means, violins with an
   inner box, a horizontal box plot, and count-scaled violins with
   their points."
  (page-title d "Box plots and violin plots")
  (let* ((names '("site A" "site B" "site C" "site D" "site E"))
         (data (distribution-data))
         (positions '(0 1 2 3 4))
         (colors (category-colors 5))
         (panels (make-panel-grid d 2 2 :left 0.1 :right 0.97 :bottom 0.1 :top 0.9
                                        :hspace 0.16 :wspace 0.12)))
    (flet ((categorical-box (vp title &key horizontal)
             (if horizontal
                 (draw-box vp :ticks '(:bottom :top) :y-ticks (category-ticks names)
                              :x-label "seeing (arcsec)" :title title)
                 (draw-box vp :ticks '(:left :right) :x-ticks (category-ticks names)
                              :y-label "seeing (arcsec)" :title title))))
      ;; box plots, means as diamonds
      (let ((vp (aref panels 0 0)))
        (set-window vp -0.5 4.5 0.3 3.3)
        (plot-box-plot vp positions data :colors colors :means t)
        (categorical-box vp "box plots (means as diamonds)"))
      ;; violins with a box inside: site D's two peaks show
      (let ((vp (aref panels 0 1)))
        (set-window vp -0.5 4.5 0.3 3.3)
        (plot-violins vp positions data :colors colors)
        (categorical-box vp "violins with inner boxes"))
      ;; horizontal box plot, first category on top
      (let ((vp (aref panels 1 0)))
        (set-window vp 0.3 3.3 4.5 -0.5)
        (plot-box-plot vp positions data :colors colors :orientation :horizontal :width 0.55)
        (categorical-box vp "horizontal" :horizontal t))
      ;; violins scaled by count, quartile lines, points overlaid
      (let ((vp (aref panels 1 1)))
        (set-window vp -0.5 4.5 0.3 3.3)
        (plot-violins vp positions data :colors colors :scale :count :inner :quartiles
                                        :fill-alpha 0.3)
        (loop for sample in data
              for p in positions
              do (with-color (vp '(0.15 0.15 0.15 0.5))
                   (plot-points vp (loop for k below (length sample)
                                         collect (+ p (* 0.25 (noise (+ 77 k (* 1000 p))))))
                                sample :marker :filled-circle :size 1.2 :halo nil)))
        (categorical-box vp "scaled by count, quartiles, points")))))
