;;;; surface-details.lisp — example surface-details
;;;;
;;;; 3D axes with ticks, numbers, labels and back-pane grids
;;;; (DRAW-AXES3D); smooth shading (mesh gradients) and flat shading;
;;;; contours drawn on the surface and hidden correctly behind nearer
;;;; parts of it.
;;;;
;;;; Uses: make-viewport, make-view3d, with-viewport, draw-axes3d,
;;;;       plot-surface, plot-floor-contours, with-style, plot-text

(in-package :cl-cairoplot-examples)

(define-example surface-details (d :size 900 :aspect 0.8)
  "3D axes with grids; smooth and flat shading; contours on the
   surface, hidden behind nearer parts of it."
  (page-title d "3D axes, smooth shading, contours on surfaces")
  (let ((z (bumps 41)))
    (flet ((panel (x1 y1 caption &rest view-keys)
             (let* ((vp (make-viewport d x1 (+ x1 0.46) y1 (+ y1 0.4)))
                    (view (apply #'make-view3d vp :x-range '(-3 3) :y-range '(-3 3)
                                                  :z-range '(-0.8 1.2) :margin 0.12
                                                  view-keys)))
               (plot-text d (+ x1 0.23) (+ y1 0.415) caption :coords :device
                                                             :halign :center :size 12)
               (values vp view))))
      ;; smooth shading with full axes
      (multiple-value-bind (vp view) (panel 0.03 0.5 "smooth shading, axes and grid")
        (with-viewport (vp :clip nil)
          (with-style (d :color :gray :line-width 0.6)
            (draw-axes3d view :part :back :grid t))
          (plot-surface view z :colormap :viridis :smooth t)
          (with-style (d :color :gray :line-width 0.6)
            (draw-axes3d view :part :front :x-label "x" :y-label "y" :z-label "z"))))
      ;; black contours on a smooth surface
      (multiple-value-bind (vp view) (panel 0.52 0.5 "contours on the surface")
        (with-viewport (vp :clip nil)
          (with-style (d :color :gray :line-width 0.6) (draw-axes3d view :part :back))
          (plot-surface view z :colormap :magma :smooth t
                               :contours '(-0.4 -0.2 0.2 0.4 0.6 0.8) :contour-color :white)
          (with-style (d :color :gray :line-width 0.6) (draw-axes3d view :part :front))))
      ;; colored contours, hidden behind the hills, from another side
      (multiple-value-bind (vp view) (panel 0.03 0.04 "colored contours, azimuth 150"
                                            :azimuth 150 :elevation 35)
        (with-viewport (vp :clip nil)
          (with-style (d :color :gray :line-width 0.6) (draw-axes3d view :part :back :grid t))
          (plot-floor-contours view z :levels '(-0.4 -0.2 0.2 0.4 0.6 0.8)
                                      :colormap :plasma)
          (plot-surface view z :colormap :gray :shade 0.7 :vmin -2 :vmax 1.5
                               :contours '(-0.4 -0.2 0.2 0.4 0.6 0.8)
                               :contour-colormap :plasma :contour-width 1.2)
          (with-style (d :color :gray :line-width 0.6)
            (draw-axes3d view :part :front :x-label "x" :y-label "y" :z-label "z"))))
      ;; flat shading, for comparison with the first panel
      (multiple-value-bind (vp view) (panel 0.52 0.04 "flat shading (compare the first)")
        (with-viewport (vp :clip nil)
          (with-style (d :color :gray :line-width 0.6)
            (draw-axes3d view :part :back :grid t))
          (plot-surface view z :colormap :viridis)
          (with-style (d :color :gray :line-width 0.6)
            (draw-axes3d view :part :front :x-label "x" :y-label "y" :z-label "z")))))))
