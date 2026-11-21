;;;; pie-charts.lisp — example pie-charts
;;;;
;;;; Pie and donut charts with PIE-CHART: a pie with names outside and
;;;; percentages inside; a donut with a total in its hole; and a donut
;;;; whose many small wedges are named in a legend instead (their
;;;; percentages left out where they don't fit).
;;;;
;;;; Uses: pie-chart (:inner, :center-text, :labels nil), draw-legend

(in-package :cl-cairoplot-examples)

(define-example pie-charts (d :size 900 :aspect 0.36)
  "A pie with names and percentages, a donut with a total in the
   middle, and a donut with a legend for its small wedges."
  (page-title d "Pie and donut charts")
  (pie-chart (make-viewport d 0.01 0.34 0.04 0.86) '(41 23 17 11 8)
             :names '("photometry" "astrometry" "spectroscopy" "imaging" "other")
             :title "telescope time by program")
  (pie-chart (make-viewport d 0.35 0.66 0.04 0.86) '(1406 612 288 194)
             :names '("main belt" "NEOs" "Trojans" "comets") :inner 0.55
             :center-text "2500 orbits" :title ":inner 0.55, :center-text")
  (let ((vp (make-viewport d 0.68 0.86 0.14 0.78)))
    (draw-legend vp (pie-chart vp '(52 21 9 7 5 3 2 1)
                               :names '("C" "S" "X" "D" "V" "K" "L" "other")
                               :inner 0.5 :labels nil :title "asteroid types, :labels nil")
                 :position :outside-right :size 9)))
