;;;; erasing.lisp — example erasing
;;;;
;;;; Erasing, as live plots in a window use it: everything is drawn,
;;;; then the page is cleared with ERASE-DEVICE (the text "erased" and the
;;;; first curves are gone, the viewports keep their windows) and drawn
;;;; again; then only the right panel is cleared with ERASE-VIEWPORT and
;;;; redrawn with a new curve and its box.
;;;;
;;;; Uses: erase-device, erase-viewport, make-panel-grid, plot-polyline,
;;;;       draw-box

(in-package :cl-cairoplot-examples)

(define-example erasing (d :size 800 :aspect 0.4)
  "Erasing: the whole page with ERASE-DEVICE and drawn again, then one
   panel with ERASE-VIEWPORT and redrawn."
  (let ((grid (make-panel-grid d 1 2 :left 0.07 :right 0.98 :bottom 0.15 :top 0.85
                                     :wspace 0.08 :window '(0 10 -1.2 1.2)))
        (xs (loop for i to 100 collect (/ i 10.0))))
    ;; a first drawing, all erased
    (page-title d "erased")
    (dotimes (c 2)
      (plot-polyline (aref grid 0 c) '(0 10) '(-1 1))
      (draw-box (aref grid 0 c)))
    (erase-device d)
    ;; drawn again
    (page-title d "Erasing: the page, then one panel")
    (dotimes (c 2)
      (with-style ((aref grid 0 c) :color "#2a78d6" :line-width 1.5)
        (plot-polyline (aref grid 0 c) xs (mapcar #'sin xs)))
      (draw-box (aref grid 0 c) :title (if (= c 0) "after erase-device" "")))
    ;; only the right panel, cleared and redrawn
    (let ((vp (aref grid 0 1)))
      (erase-viewport vp)
      (with-style (vp :color "#eb6834" :line-width 1.5)
        (plot-polyline vp xs (mapcar #'cos xs)))
      (draw-box vp :title "after erase-viewport, redrawn"))))
