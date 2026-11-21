;;;; step-and-area-charts.lisp — example step-and-area-charts
;;;;
;;;; Step plots and stacked areas: the three places a staircase can step
;;;; between points; a filled staircase over bin edges (photon counts);
;;;; stacked steps over monthly bins with a legend; stacked areas named
;;;; by labels inside the layers; the same as percentages; and a
;;;; streamgraph.  The data are synthetic asteroid discoveries by survey.
;;;;
;;;; Uses: plot-step (:where, :fill, bin edges), step-points,
;;;;       plot-stacked-area (:step, :labels, :normalize, :baseline
;;;;       :wiggle, :order :inside-out), stack-series, draw-legend
;;;;       (:outside-right), make-panel-grid

(in-package :cl-cairoplot-examples)

(defun survey-discoveries ()
  "Values: years 1995..2024, survey names, and synthetic discoveries
   (thousands per year) by survey, one series each."
  (let ((years (loop for y from 1995 to 2024 collect y)))
    (flet ((bump (peak width height)
             (mapcar (lambda (y) (* height (exp (- (/ (expt (- y peak) 2) (* 2 width width))))))
                     years))
           (rise (mid width height)
             (mapcar (lambda (y) (/ height (+ 1 (exp (/ (- mid y) width))))) years)))
      (values years
              '("LINEAR" "Spacewatch" "NEAT" "Catalina" "Pan-STARRS" "other")
              (list (bump 2001 3 20)
                    (bump 2002 7 5)
                    (bump 2004 2.5 6)
                    (mapcar #'+ (rise 2007 1.5 14) (bump 2016 4 6))
                    (rise 2016 2 26)
                    (mapcar (lambda (y) (+ 1.5 (* 0.1 (- y 1995)))) years))))))

(define-example step-and-area-charts (d :size 960 :aspect 0.62)
  "Step plots (:pre, :mid, :post, and filled over bin edges) and
   stacked areas: stacked steps with a legend, labeled layers,
   percentages and a streamgraph."
  (page-title d "Step plots and stacked areas")
  (let ((panels (make-panel-grid d 2 3 :left 0.07 :right 0.87 :bottom 0.09 :top 0.88
                                       :hspace 0.2 :wspace 0.08)))
    ;; where the staircase steps, offset so all three show
    (let ((vp (aref panels 0 0))
          (xs '(0 1 2 3 4 5 6 7))
          (ys '(1 2.2 1.5 2.8 2 1.2 2.5 1.8)))
      (set-window vp -0.5 8.5 0 10)
      (loop for where in '(:post :mid :pre)
            for offset in '(6 3 0)
            for color in (category-colors 3)
            do (let ((shifted (mapcar (lambda (y) (+ y offset)) ys)))
                 (with-style (vp :color color :line-width 1.5)
                   (plot-step vp xs shifted :where where))
                 (plot-points vp xs shifted :marker :filled-circle :size 2.5)
                 (plot-text vp 7.2 (+ offset (car (last ys))) (format nil "~(~S~)" where)
                            :valign :center :size 10)))
      (draw-box vp :title "(plot-step ... :where ...)" :numbers '(:bottom)))
    ;; counts in 30 s bins: bin edges, filled
    (let* ((vp (aref panels 0 1))
           (edges (loop for k from 0 to 20 collect (* 30 k)))
           (counts (loop for k below 20
                         collect (round (+ 40 (* 6 (gaussian-noise (+ 300 k)))
                                           (* 70 (exp (- (/ (expt (- k 8) 2) 4)))))))))
      (set-window vp 0 600 0 130)
      (with-style (vp :color "#2a78d6" :line-width 1.2)
        (plot-step vp edges counts :fill '(0.16 0.47 0.84 0.2)))
      (draw-box vp :title "bin edges, :fill" :x-label "time (s)" :y-label "counts per 30 s"))
    ;; stacked steps over monthly bins
    (let ((vp (aref panels 0 2))
          (months (loop for k from 0 to 12 collect k))
          (hours '((40 38 30 22 18 12 10 14 20 30 36 42)
                   (20 24 26 30 28 22 18 20 26 28 24 22)
                   (12 10 14 12 8 6 8 10 12 14 12 10))))
      (set-window vp 0 12 0 120)
      (draw-legend vp (plot-stacked-area vp months hours
                                         :names '("imaging" "spectroscopy" "engineering"))
                   :position :upper-right :size 9)
      (draw-box vp :title "stacked steps over bin edges" :x-label "month" :y-label "hours"
                   :x-step 3))
    (multiple-value-bind (years names surveys) (survey-discoveries)
      ;; stacked, labeled inside the layers
      (let ((vp (aref panels 1 0)))
        (set-window vp 1995 2024 0 (* 1.05 (nth-value 2 (stack-series surveys))))
        (plot-stacked-area vp years surveys :names names :labels t)
        (draw-box vp :title ":labels t" :x-label "year"
                     :y-label "discoveries (thousands/yr)"))
      ;; as percentages
      (let ((vp (aref panels 1 1)))
        (set-window vp 1995 2024 0 100)
        (plot-stacked-area vp years surveys :names names :normalize t :labels t)
        (draw-box vp :title ":normalize t" :x-label "year" :y-label "share (%)"))
      ;; a streamgraph, inside-out; its vertical position means nothing,
      ;; so no y numbers.  One legend serves the three survey panels.
      (let ((vp (aref panels 1 2)))
        (multiple-value-bind (layers lo hi)
            (stack-series surveys :baseline :wiggle :order :inside-out)
          (declare (ignore layers))
          (set-window vp 1995 2024 (* 1.1 lo) (* 1.1 hi)))
        (draw-legend vp (plot-stacked-area vp years surveys :names names :labels t
                                                            :baseline :wiggle :order :inside-out)
                     :position :outside-right :size 9)
        (draw-box vp :title ":baseline :wiggle, :order :inside-out" :x-label "year"
                     :ticks '(:bottom) :numbers '(:bottom))))))
