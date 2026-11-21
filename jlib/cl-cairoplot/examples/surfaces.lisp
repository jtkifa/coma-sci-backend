;;;; surfaces.lisp — example surfaces
;;;;
;;;; 3D surfaces z = f(x,y) in a VIEW3D: colormapped and shaded, with
;;;; floor contours and the box drawn in back and front halves; a
;;;; wireframe with hidden lines removed; a surface colored by a second
;;;; array; and a 3D curve with markers.
;;;;
;;;; Uses: make-viewport, make-view3d, with-viewport, draw-box3d,
;;;;       plot-floor-contours, plot-surface, plot3d-polyline,
;;;;       plot3d-points, plot3d-text, with-style, with-color

(in-package :cl-cairoplot-examples)

(defun bumps (n)
  "Two hills and a pit on an N x N grid over -3..3."
  (let ((a (make-array (list n n) :element-type 'double-float)))
    (dotimes (j n a)
      (dotimes (i n)
        (let ((x (- (* 6d0 (/ i (1- n))) 3)) (y (- (* 6d0 (/ j (1- n))) 3)))
          (setf (aref a j i)
                (+ (* 1.0 (exp (- (+ (expt (- x 1) 2) (expt (- y 0.5) 2)))))
                   (* 0.7 (exp (- (/ (+ (expt (+ x 1.2) 2) (expt (- y 1.3) 2)) 0.6))))
                   (* -0.6 (exp (- (/ (+ (expt (+ x 0.5) 2) (expt (+ y 1.4) 2)) 0.8)))))))))))

(define-example surfaces (d :size 900 :aspect 0.75)
  "3D surfaces: colormapped and shaded with floor contours, a hidden-line
   wireframe, a surface colored by a second array, and a 3D curve."
  (page-title d "3D surfaces (view3d)")
  (let ((z (bumps 41)))
    ;; colormapped, shaded, floor contours, box in two halves
    (let* ((vp (make-viewport d 0.02 0.5 0.48 0.91))
           (view (make-view3d vp :x-range '(-3 3) :y-range '(-3 3) :z-range '(-0.8 1.2))))
      (with-viewport (vp :clip nil)
        (with-style (d :color :gray :line-width 0.6) (draw-box3d view :part :back))
        (plot-floor-contours view z :levels '(-0.4 -0.2 0.2 0.4 0.6 0.8) :colormap :viridis
                                    :negative-dash '(3 2))
        (plot-surface view z :colormap :viridis)
        (with-style (d :color :gray :line-width 0.6) (draw-box3d view :part :front))
        ;; x and y on the front edges (camera at +x, -y for azimuth -60)
        (plot3d-text view 0 -3.6 -0.8 "x" :size 12 :halign :center :valign :top)
        (plot3d-text view 3.6 0 -0.8 "y" :size 12 :valign :top)
        ;; z on the leftmost vertical edge
        (plot3d-text view -3.2 -3.2 0.2 "z" :size 12 :halign :right))
      (plot-text d 0.26 0.92 "colormap, shading, floor contours" :coords :device
                 :halign :center :size 12))
    ;; hidden-line wireframe from another angle
    (let* ((vp (make-viewport d 0.52 0.98 0.48 0.91))
           (view (make-view3d vp :x-range '(-3 3) :y-range '(-3 3) :z-range '(-0.8 1.2)
                                 :azimuth -120 :elevation 40)))
      (with-viewport (vp :clip nil)
        (with-style (d :color :gray :line-width 0.6) (draw-box3d view :part :back))
        (plot-surface view z :fill nil :edge-width 0.5)
        (with-style (d :color :gray :line-width 0.6) (draw-box3d view :part :front)))
      (plot-text d 0.75 0.92 "hidden-line wireframe, azimuth -120" :coords :device
                 :halign :center :size 12))
    ;; colored by a second array (the slope), edges, perspective
    (let* ((slope (let ((s (make-array '(41 41) :element-type 'double-float)))
                    (dotimes (j 41 s)
                      (dotimes (i 41)
                        (setf (aref s j i)
                              (float (sqrt (+ (expt (- (aref z j (min 40 (1+ i)))
                                                       (aref z j (max 0 (1- i)))) 2)
                                              (expt (- (aref z (min 40 (1+ j)) i)
                                                       (aref z (max 0 (1- j)) i)) 2)))
                                     1d0))))))
           (vp (make-viewport d 0.02 0.5 0.02 0.46))
           (view (make-view3d vp :x-range '(-3 3) :y-range '(-3 3) :z-range '(-0.8 1.2)
                                 :azimuth -45 :elevation 25 :distance 2.5)))
      (with-viewport (vp :clip nil)
        (plot-surface view z :color-array slope :colormap :inferno :edges t
                             :edge-color '(0 0 0 0.3) :shade 0.3))
      (plot-text d 0.26 0.47 "colored by slope, cell edges, perspective" :coords :device
                 :halign :center :size 12))
    ;; a 3D curve with markers
    (let* ((vp (make-viewport d 0.52 0.98 0.02 0.46))
           (view (make-view3d vp :x-range '(-1 1) :y-range '(-1 1) :z-range '(0 1)
                                 :azimuth -50 :elevation 20))
           (ts (loop for k from 0 to 300 collect (/ k 300d0))))
      (with-viewport (vp :clip nil)
        (with-style (d :color :gray :line-width 0.6) (draw-box3d view))
        (with-style (d :color :blue :line-width 1.5)
          (plot3d-polyline view (mapcar (lambda (u) (* (- 1 (* 0.6 u)) (cos (* 25 u)))) ts)
                           (mapcar (lambda (u) (* (- 1 (* 0.6 u)) (sin (* 25 u)))) ts) ts))
        (with-color (d :red)
          (plot3d-points view '(1 0.4) '(0 0) '(0 1) :marker :filled-circle :size 4)))
      (plot-text d 0.75 0.47 "3D curve and points" :coords :device :halign :center :size 12))))
