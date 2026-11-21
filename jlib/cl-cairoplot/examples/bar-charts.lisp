;;;; bar-charts.lisp — example bar-charts
;;;;
;;;; Bar charts with BAR-CHART: grouped columns with error bars, value
;;;; labels and a legend; stacked columns with negative parts; a single
;;;; horizontal series sorted by value, labeled at the bar ends; and
;;;; slanted category names.  Categories sit at 0, 1, 2, ... and are
;;;; named on the axis by CATEGORY-TICKS.
;;;;
;;;; Uses: bar-chart (:mode :grouped/:stacked, :orientation, :errors,
;;;;       :value-labels, :names, :category-angle), category-ticks,
;;;;       make-panel-grid

(in-package :cl-cairoplot-examples)

(define-example bar-charts (d :size 900 :aspect 0.75)
  "Bar charts: grouped with error bars and a legend, stacked with
   negative parts and totals, horizontal sorted bars with value labels,
   and slanted category names."
  (page-title d "Bar charts")
  (let ((panels (make-panel-grid d 2 2 :left 0.1 :right 0.97 :bottom 0.1 :top 0.9
                                       :hspace 0.16 :wspace 0.12)))
    ;; grouped, with error bars
    (bar-chart (aref panels 0 0) '("g" "r" "i" "z" "y")
               '((412 655 598 431 220) (380 702 640 470 251) (455 610 577 402 198))
               :names '("night 1" "night 2" "night 3")
               :errors (list 20 '(25 25 25 25 25) '((15 30) (20 40) (20 40) (15 30) (10 20)))
               :category-label "filter" :value-label "detections"
               :title "grouped, with error bars" :legend :upper-right)
    ;; stacked, with a negative component and totals
    (bar-chart (aref panels 0 1) '("Q1" "Q2" "Q3" "Q4")
               '((40 55 30 62) (25 20 35 28) (-12 -18 -8 -20))
               :mode :stacked :value-labels t :value-range '(-30 200)
               :names '("granted" "director's" "lost to weather")
               :value-label "hours" :title "stacked, totals labeled" :legend :upper-right)
    ;; horizontal, one series, sorted, labeled
    (let* ((pairs (sort (copy-list '(("Ceres" 939.4) ("Vesta" 525.4) ("Pallas" 511)
                                     ("Hygiea" 434) ("Interamnia" 332) ("Europa" 319)
                                     ("Davida" 289)))
                        #'> :key #'second)))
      (bar-chart (aref panels 1 0) (mapcar #'first pairs) (mapcar #'second pairs)
                 :orientation :horizontal :value-labels t
                 :value-format (lambda (v) (format nil "~D km" (round v)))
                 :value-label "mean diameter (km)" :title "horizontal, sorted, labeled"))
    ;; long names, slanted
    (bar-chart (aref panels 1 1)
               '("photometry" "astrometry" "spectroscopy" "polarimetry" "imaging" "calibration")
               '(31 12 24 4 18 11)
               :category-angle 35 :value-label "nights" :title "slanted category names")))
