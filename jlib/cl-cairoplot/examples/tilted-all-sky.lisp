;;;; tilted-all-sky.lisp — example tilted-all-sky
;;;;
;;;; All-sky projections centered off the equator: the sky is rotated so
;;;; the center is in the middle of the ellipse, with its frame's north
;;;; pole straight up (as FITS's default LONPOLE), and both poles fall
;;;; inside.  The same equatorial all-sky map is reprojected onto each.
;;;; Meridians run pole to pole and are labeled below the equator;
;;;; parallels are labeled at their left ends, or on their lowest points
;;;; where they close around a pole.
;;;;
;;;; Uses: make-projection (DEC0 not 0), set-window-projection,
;;;;       plot-image :wcs, draw-sky-grid (:coordinates overlay),
;;;;       plot-points (:coords :sky)

(in-package :cl-cairoplot-examples)

(define-example tilted-all-sky (d :size 1000 :aspect 0.62)
  "Tilted all-sky projections: Hammer-Aitoff, Mollweide and plate
   carree centered at Dec -29, +22, +60 and +45, with grids and a map
   reprojected onto them."
  (page-title d "All-sky projections centered off the equator")
  (multiple-value-bind (map wcs) (mock-milky-way 360 180)
    (flet ((panel (box projection text &optional overlay)
             (let ((vp (apply #'make-viewport d box)))
               (set-window-projection vp projection)
               (plot-image vp map :wcs wcs :colormap :inferno :scale :sqrt :vmin 0 :vmax 110
                                  :interpolation :bilinear)
               ;; the celestial poles
               (with-color (vp :cyan)
                 (plot-points vp '(0 0) '(90 -90) :coords :sky :marker :plus :size 8))
               (draw-sky-grid vp :size 9 :grid-color '(1 1 1 0.4))
               (when overlay
                 (with-style (vp :line-width 0.6)
                   (draw-sky-grid vp :coordinates overlay :labels nil :frame nil
                                     :grid-color '(0.4 1 0.4 0.6))))
               (destructuring-bind (x1 x2 y1 y2) box
                 (declare (ignore y1))
                 (plot-text d (/ (+ x1 x2) 2) (+ y2 0.03) text :coords :device
                                                                :halign :center :size 11)))))
      (panel '(0.05 0.47 0.53 0.88) (make-projection :ait 266.4 -28.9)
             "AIT about the Galactic center (17h46m, -29°), galactic grid in green" :galactic)
      (panel '(0.55 0.97 0.53 0.88) (make-projection :mol 83.6 22)
             "MOL about Orion (5h34m, +22°)")
      (panel '(0.05 0.47 0.07 0.42) (make-projection :ait 120 60)
             "AIT about (8h, +60°)")
      (panel '(0.55 0.97 0.07 0.42) (make-projection :car 10 45)
             "CAR about (0h40m, +45°), ecliptic grid in green" :ecliptic))))
