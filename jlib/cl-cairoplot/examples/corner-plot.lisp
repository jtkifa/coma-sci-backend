;;;; corner-plot.lisp — example corner-plot
;;;;
;;;; A corner plot of synthetic MCMC samples of an asteroid's fitted
;;;; parameters: four correlated parameters, one pair curved (a
;;;; "banana"), with each parameter's histogram, median and 16th-84th
;;;; percentile spread on the diagonal, 1 and 2 sigma contours below,
;;;; outlying samples as dots, and the true values marked.
;;;;
;;;; Uses: corner-plot (:names, :truths)

(in-package :cl-cairoplot-examples)

(defun mock-posterior (n)
  "N samples of four parameters: correlated normals, one pair curved."
  (let ((a '()) (e '()) (i '()) (h '()))
    (dotimes (k n)
      (let* ((z1 (gaussian-noise (+ 10000 k))) (z2 (gaussian-noise (+ 20000 k)))
             (z3 (gaussian-noise (+ 30000 k))) (z4 (gaussian-noise (+ 40000 k))))
        (push (+ 2.75 (* 0.02 z1)) a)
        (push (+ 0.18 (* 0.015 (+ (* 0.6 z1) (* 0.8 z2)))) e)
        (push (+ 9.3 (* 0.12 z3) (* 0.3 z2 z2)) i)
        (push (+ 15.2 (* 0.1 (+ (* -0.5 z1) (* 0.87 z4)))) h)))
    (list a e i h)))

(define-example corner-plot (d :size 800 :aspect 1)
  "A corner plot of four parameters' samples: histograms with medians
   and spreads, 1 and 2 sigma contours, outliers and true values."
  (corner-plot d (mock-posterior 6000)
               :names '("a (AU)" "e" "i (°)" "H (mag)")
               :truths '(2.75 0.18 9.6 15.2)
               :left 0.12 :bottom 0.11 :top 0.95))
