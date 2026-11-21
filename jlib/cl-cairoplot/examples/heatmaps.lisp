;;;; heatmaps.lisp — example heatmaps
;;;;
;;;; Annotated heatmaps with HEATMAP: a correlation matrix through a
;;;; diverging colormap centered on zero, and a table of clear nights per
;;;; month at several telescopes, with a missing month (NIL) in gray.
;;;; Values are written in black or white, whichever shows on the cell.
;;;;
;;;; Uses: heatmap (:center, :value-format, :colorbar, :cols-on,
;;;;       :col-angle, NIL cells)

(in-package :cl-cairoplot-examples)

(define-example heatmaps (d :size 900 :aspect 0.5)
  "A correlation matrix centered on zero with a diverging colormap, and
   a table of clear nights per month with a missing value."
  (page-title d "Annotated heatmaps")
  (let ((names '("g - r" "r - i" "mag" "period" "amplitude" "distance")))
    (heatmap (make-viewport d 0.1 0.42 0.1 0.78)
             '((1 0.82 -0.31 0.12 -0.05 0.44)
               (0.82 1 -0.27 0.08 -0.11 0.39)
               (-0.31 -0.27 1 -0.52 0.36 -0.71)
               (0.12 0.08 -0.52 1 -0.64 0.29)
               (-0.05 -0.11 0.36 -0.64 1 -0.18)
               (0.44 0.39 -0.71 0.29 -0.18 1))
             :row-names names :col-names names :cols-on :top :col-angle 40
             :colormap :coolwarm :center 0 :colorbar t :colorbar-label "r"
             :value-format (lambda (v) (format nil "~,2F" v))))
  (heatmap (make-viewport d 0.57 0.92 0.18 0.84)
           '((18 15 20 22 25 27 29 28 24 19 14 12)
             (12 14 17 21 23 26 nil 27 22 17 13 11)
             (24 22 25 26 28 29 30 30 27 25 23 22)
             (9 11 14 18 22 24 25 23 19 14 10 8))
           :row-names '("La Palma" "Maunakea" "Paranal" "Palomar")
           :col-names '("J" "F" "M" "A" "M" "J" "J" "A" "S" "O" "N" "D")
           :colormap :viridis :colorbar t :colorbar-label "clear nights"
           :title "clear nights per month (Maunakea July missing)"))
