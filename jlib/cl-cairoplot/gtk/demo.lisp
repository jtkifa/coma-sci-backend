;;;; demo.lisp — a live plot in a window
;;;;
;;;;   (asdf:load-system "cl-cairoplot-gtk")
;;;;   (cl-cairoplot-gtk:demo)
;;;;
;;;; Opens a :GTK device and draws four panels one at a time, a curve
;;;; growing point by point in the first, so the window shows drawing as
;;;; it happens; the window stays up afterwards.
;;;;
;;;; Uses: open-device :gtk, make-panel-grid, plot-polyline, plot-points,
;;;;       plot-histogram, plot-image, draw-box, close-device

(in-package #:cl-cairoplot-gtk)

(defun demo (&key (size 800) (pause 0.4))
  "Draw a few panels into a new window, PAUSE seconds apart, and return
   the (closed) device; its window stays up until closed."
  (let* ((d (cp:open-device :gtk "cl-cairoplot demo" :size size :aspect 0.75))
         (grid (cp:make-panel-grid d 2 2 :left 0.08 :right 0.97 :bottom 0.08 :top 0.93
                                         :hspace 0.12 :wspace 0.1)))
    (unwind-protect
         (progn
           ;; a damped oscillation, growing a few points at a time
           (let ((vp (aref grid 0 0)) (xs '()) (ys '()))
             (cp:set-window vp 0 10 -1.1 1.1)
             (cp:draw-box vp :title "drawn as it goes" :x-label "t")
             (loop for i from 0 to 200
                   do (push (/ i 20) xs)
                      (push (* (exp (/ i -80)) (sin (* i 0.12))) ys)
                      (when (zerop (mod i 10))
                        (cp:with-style (vp :color "#2a78d6" :line-width 1.5)
                          (cp:plot-polyline vp (reverse xs) (reverse ys)))
                        (sleep 0.03))))
           (sleep pause)
           ;; points
           (let ((vp (aref grid 0 1)))
             (cp:set-window vp -3 3 -3 3)
             (cp:with-color (vp "#eb6834")
               (cp:plot-points vp (loop for k below 300 collect (* 3 (- (random 2d0) 1)))
                               (loop for k below 300 collect (* 3 (- (random 2d0) 1)))
                               :marker :filled-circle :size 2))
             (cp:draw-box vp :title "points"))
           (sleep pause)
           ;; a histogram of a normal sample
           (let ((vp (aref grid 1 0))
                 (sample (loop repeat 2000
                               collect (loop repeat 12 sum (random 1d0) into s
                                             finally (return (- s 6))))))
             (cp:set-window vp -4 4 0 0.45)
             (cp:with-color (vp '(0.16 0.47 0.84 0.5))
               (cp:plot-histogram vp sample :range '(-4 4) :style :filled :normalize :density))
             (cp:draw-box vp :title "histogram" :x-label "x"))
           (sleep pause)
           ;; an image
           (let ((vp (aref grid 1 1))
                 (image (make-array '(60 80) :element-type 'double-float)))
             (dotimes (j 60)
               (dotimes (i 80)
                 (setf (aref image j i) (float (+ (sin (/ i 6)) (cos (/ j 5))) 1d0))))
             (cp:set-window-equal-scales vp -0.5 79.5 -0.5 59.5)
             (cp:plot-image vp image :colormap :viridis)
             (cp:draw-box vp :title "image")))
      (cp:close-device d))
    d))

;;; Reading the cursor
;;;
;;;   (cl-cairoplot-gtk:cursor-demo)
;;;
;;; Opens a window where each click adds a point, joined to the last by
;;; a line, with a rubber band following the pointer from the last point;
;;; q or Escape stops.  Returns the points.
;;;
;;; Uses: read-cursor (:mode :line, :anchor), plot-point, plot-line

(defun cursor-demo (&key (size 640))
  "Mark points by clicking in a new window, a rubber band joining the
   last one to the pointer, until q or Escape is pressed (or the window
   is closed).  Returns the points as a list of (X Y)."
  (let ((d (cp:open-device :gtk "click to add points, q to stop" :size size :aspect 0.75))
        (points '()))
    (cp:set-window d 0 10 0 10)
    (cp:draw-box d :title "click to add points; q or Escape to stop")
    (loop
      (multiple-value-bind (x y key)
          (cp:read-cursor d :mode (if points :line :none) :anchor (first points))
        (when (or (null key) (member key '(#\q :escape)))
          (return))
        (when points
          (cp:with-style (d :color "#2a78d6" :line-width 1.5)
            (cp:plot-line d (first (first points)) (second (first points)) x y)))
        (cp:with-color (d "#eb6834")
          (cp:plot-point d x y :marker :filled-circle :size 4))
        (push (list x y) points)))
    (cp:close-device d)
    (reverse points)))
