;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; this is a file of demo plots for pgplot.lisp
;; it is not exhaustive, but should show how to make
;; a few kinds of basic plots
;;
;; to run all demos, type (run-all-demos)
;;
;; individual demos can be run as
;;    (demo-plot-1)
;;    (demo-plot-2) ..etc
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage pgplot-examples
  (:use #:cl)
  (:export #:run-all-demos
	   #:run-all-demos-to-gif))





(in-package pgplot-examples)



#|

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; first, you should read this simple interactive session (enclosed in
;;  comments to prevent it running when file is loaded)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; open X11 device
(defparameter *p* (pgplot:open-device :x11))  

;; alternatively,
;; (defparameter *p* (pgplot:open-device :ps :filename "my-plot.ps"))
;; would have opened a postscript device.  Note that a poscript device
;; must be closed  to flush the postscript file to disk


;; set window size (default is 0,0 to 1,1)
(pgplot:set-window  *p* -1 1 -1 1) ;; ie, x1 x2 y1 y2

;; draw an axis box - many options exist to control the look
;; of the box, but we use the defalt
(pgplot:box *p*)

;; connect a line, with several optional qualities
(pgplot:connect *p*
		#(-0.95 0.8) ;; vector of x values
		#(-0.7 0.5)  ;; vector of y vales
		:color :blue            ;; optional
		:line-style :dashed     ;; optional
		:line-width 3)          ;; optional

;; draw an x label, and nudge label down a little
(pgplot:xlabel *p* "X axis"
	       :y-offset -0.03  ;; 3% of window
	       :character-height 1.4)

;; y label, nudging to left - note pgplot escaping,
;; (eg, u to jump to exponent), and note doubling of
;; backslashes, because lisp itself eats up one backslash
(pgplot:ylabel *p* "Y axis (snarks\\u(3/2)\\d)" ;; note pgplot escaping
	       :x-offset -0.02  ;; 2% of window
	       :character-height 1.4)

;; erase the device
(pgplot:erase *p*)

;; and close the window
(pgplot:close-device *p*)

;; we could have also done (pgplot:close-all-devices)

|#
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun run-all-demos ()
  (demo-plot-1)
  (demo-plot-2)
  (demo-plot-3)
  (demo-plot-3 :draw-legend t)
  (demo-plot-4)
  (demo-plot-5)
  (demo-plot-5  :log t)
  (demo-plot-6)
  (demo-plot-6 :do-labels t)
  (demo-plot-7)
  (demo-plot-8)
  (demo-plot-9)
  )

(defun run-all-demos-to-gif ()
  (ensure-directories-exist "./DEMO-PLOTS/#IGNORE#")
  (demo-plot-1 :device :gif :filename "./DEMO-PLOTS/pgplot-demo-1.gif")
  (demo-plot-2 :device :gif :filename "./DEMO-PLOTS/pgplot-demo-2.gif")
  (demo-plot-3 :device :gif :filename "./DEMO-PLOTS/pgplot-demo-3.gif")
  (demo-plot-3 :draw-legend t :device :gif :filename "./DEMO-PLOTS/pgplot-demo-3a.gif")
  (demo-plot-4 :device :gif :filename "./DEMO-PLOTS/pgplot-demo-4.gif")
  (demo-plot-5 :device :gif :filename "./DEMO-PLOTS/pgplot-demo-5.gif")
  (demo-plot-5  :log t :device :gif :filename "./DEMO-PLOTS/pgplot-demo-5a.gif")
  (demo-plot-6 :device :gif :filename "./DEMO-PLOTS/pgplot-demo-6.gif")
  (demo-plot-6 :do-labels t :device :gif :filename "./DEMO-PLOTS/pgplot-demo-6a.gif")
  (demo-plot-7 :device :gif :filename "./DEMO-PLOTS/pgplot-demo-7.gif")
  (demo-plot-8 :device :gif :filename "./DEMO-PLOTS/pgplot-demo-8.gif")
  (demo-plot-9 :device :gif :filename "./DEMO-PLOTS/pgplot-demo-9.gif")
  )

;; helper function to wait for user input
(defun key-wait () 
  (format t "Hit return to continue...~%")
  (read-char))

;; plot sin and cos - 
;; if you use  :ps for DEVICE, and a string for the FILENAME,
;; this will create a postscript plot
(defun demo-plot-1 (&key (device :x11) (filename nil))
  (princ "demo-plot-1 - 2d line plot with sin and cos")(terpri)
  (let* ((n 100)
	 (x (make-array n :element-type 'double-float))
	 (y1 (make-array n :element-type 'double-float))
	 (y2 (make-array n :element-type 'double-float))
	 ;; open the pgplot device
	 (p (pgplot:open-device device :filename filename)))
    ;; fill in arrays with x, sin(x) and cos(x)
    (loop for i from 0 to (1- n)
	  do (progn
	       (setf (aref x i) (* i (/ (* 1.0d0 n)) 2.0d0 pi))
	       (setf (aref y1 i) (cos (aref x i)))
	       (setf (aref y2 i) (sin (aref x i)))))
    (pgplot:set-window p 0 6.4 -1.1 1.1)
    (pgplot:box p)
    (pgplot:xlabel p "x")     (pgplot:ylabel p "y")
    (pgplot:connect p x y1 :color :blue)
    (pgplot:connect p x y2 :color :red)
    (when (not filename) (key-wait))
    (pgplot:close-device p)))

(defun demo-plot-2 (&key (device :x11) (filename nil))
  (princ "demo-plot-2 - 2d log plot of Gaussian")(terpri)
  (let* ((n 100)
	 (x (make-array n :element-type 'double-float))
	 (y (make-array n :element-type 'double-float))
	 ;; open the pgplot device
	 (p (pgplot:open-device device :filename filename)))
    ;; fill in arrays with x, sin(x) and cos(x)
    (loop for i from 0 to (1- n)
	  do (progn
	       (setf (aref x i) (* (- i (/ n 2)) (/ (* 1.0d0 n)) 6.0d0))
	       (setf (aref y i)
		     (log
		      (exp (* -0.5d0 (expt (aref x i) 2)))
		      10.0d0))))
    ;;
    (pgplot:set-window p -3 3 -3 1)
    (pgplot:box p :y-log t)
    (pgplot:xlabel p "x" :y-offset -0.02)
    (pgplot:ylabel p "y" :x-offset -0.02)
    (pgplot:toplabel p "Logarithm of Gaussian")
    (pgplot:connect p x y :color :default)
    (when (not filename) (key-wait))
    (pgplot:close-device p)))

;; now use points instead of lines
(defun demo-plot-3 (&key (device :x11) (filename nil) (draw-legend nil))
  (princ "demo-plot-3 - 2d points plot with sin and cos")(terpri)
  (when draw-legend
    (princ "  Using pgplot:draw-legend to add a legend")
    (terpri))
  (let* ((n 20) 
	 (x (make-array n :element-type 'double-float))
	 (y1 (make-array n :element-type 'double-float))
	 (y2 (make-array n :element-type 'double-float))
	 ;; open the pgplot device
	 (p (pgplot:open-device device :filename filename)))
    ;; fill in arrays with x, sin(x) and cos(x)
    (loop for i from 0 to (1- n)
	  do (progn
	       (setf (aref x i) (* i (/ (* 1.0d0 n)) 2.0d0 pi))
	       (setf (aref y1 i) (cos (aref x i)))
	       (setf (aref y2 i) (sin (aref x i)))))
    (pgplot:set-window p 0  6.4 -1.1 1.1)
    (pgplot:box p)
    (pgplot:xlabel p "x")     (pgplot:ylabel p "y")
    ;; the fill list of symbols is in pgplot::*point-symbol-alist*
    (pgplot:points p x y1 :asterisk  ;; symbol type represented by keyword
		   :color :blue)
    (pgplot:points p x y2 :circle-2
		   :color :red)
					;
    (when draw-legend
      (pgplot:draw-plot-legend 
       p 0.5 0.9
       '(("cos(x)" :point :asterisk :color :blue)
	 ("sin(x)" :point :circle-2 :color :red))
       :draw-box t))
    
    (when (not filename) (key-wait))
    (pgplot:close-device p)))



;; split up plot into top and bottom panes
(defun demo-plot-4 (&key (device :x11) (filename nil))
  (princ "demo-plot-4 - 2d 2-panel line plot with sin and cos")(terpri)
  (let* ((n 100) 
	 (x (make-array n :element-type 'double-float))
	 (y1 (make-array n :element-type 'double-float))
	 (y2 (make-array n :element-type 'double-float))
	 ;; open the pgplot device
	 (p (pgplot:open-device device :filename filename)))
    ;; fill in arrays with x, sin(x) and cos(x)
    (loop for i from 0 to (1- n)
	  do (progn
	       (setf (aref x i) (* i (/ (* 1.0d0 n)) 2.0d0 pi))
	       (setf (aref y1 i) (cos (aref x i)))
	       (setf (aref y2 i) (sin (aref x i)))))
    ;;
    (pgplot:set-window p 0 6.4 -1.1 1.1)
    ;;
    ;; do first pane (bottom)
    (pgplot:set-current-pane p
			     1 1  ;; pane 1 of 1 x panes
			     1 2  ;; pane 1 of 2 y panes
			     :y-separation 0.1  ;; y space between panes
			     :left-pad 0.20     ;; y space on left
			     :bottom-pad 0.08)  ;; extra space on bottom
    
    (pgplot:box p)
    (pgplot:xlabel p "\\ga")
    (pgplot:ylabel p "cos(\\ga)" :x-offset -0.03)
    (pgplot:connect p x y1)
    (pgplot:write-text p "Cos" 4.0 0.5 :character-height 1.3)
    ;;
    ;; do second pane (top)
    (pgplot:set-current-pane p
			     1 1  ;; pane 1 of 1 x panes
			     2 2  ;; pane 2 of 2 y panes
			     :y-separation 0.1  ;; y space between panes
			     :left-pad 0.20     ;; y space on left
			     :bottom-pad 0.08)  ;; extra space on bottom
    (pgplot:box p)
    (pgplot:ylabel p "sin(\\ga)" :x-offset -0.03)
    (pgplot:connect p x y2)
    (pgplot:write-text p "Sin" 4.0 0.5 :character-height 1.3)
    ;;    
    (when (not filename) (key-wait))
    (pgplot:close-device p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helper function: create gaussian deviate
(let ((GasdevIset 0.0)
      (GasdevGset 0.0))
  ;;
  (defun uniform-random ()
    (* 1.0d-6 (random 1000000)))
  ;;
  (defun %gaussian-random ()
    (cond ((zerop GasdevIset) 
           (let ((v1 0.0) (v2 0.0) (r 0.0) (fac 0.0))
             (do ()
                 ((< 0.0 r 1.0))
               (setf v1 (- (* 2.0 (uniform-random)) 1.0))
               (setf v2 (- (* 2.0 (uniform-random)) 1.0))
               (setf r (+ (expt v1 2) (expt v2 2))))
             (setf fac (sqrt (/ (* -2.0 (log r)) r)))
             (setf GasdevGset (* v1 fac))
             (setf GasdevIset 1)
             (* v2 fac)))
          (t (setf GasdevIset 0)
             GasdevGset)))
  ;;
  (defun gaussian-random (&optional (mean 0.0d0) (sigma 1.0d0))
    (+ mean (* (%gaussian-random) sigma))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun demo-plot-5 (&key (device :x11) (filename nil) (log nil))
  (if log
      (princ "demo-plot-5(b) - LOGARITHMIC histograms of gaussians in various styles")
  (princ "demo-plot-5(a) - histograms of gaussians in various styles"))
  (terpri)
  (let* ((n 500) 
	 (x1 (make-array n :element-type 'double-float))
	 (x2 (make-array n :element-type 'double-float))
	 (x3 (make-array n :element-type 'double-float))
	 ;; open the pgplot device
	 (p (pgplot:open-device device :filename filename)))
    ;; fill in arrays with x, sin(x) and cos(x)
    (loop for i from 0 to (1- n)
	  do (progn
	       (setf (aref x1 i) (gaussian-random -0.4 0.2))
	       (setf (aref x2 i) (gaussian-random  0.0 0.3))
	       (setf (aref x3 i) (gaussian-random  0.4 0.4))))
    ;;
    (if log
	(pgplot:set-window p -1.2 1.2 0 2)
      (pgplot:set-window p -1.2 1.2 0 90))
    ;;
    (pgplot:box p)
    ;; fill styles are in pgplot::*fill-area-style-alist*
    (pgplot:hist p x1 :color :blue
		 :fill-color :blue
		 :fill :hatched
		 :log log
		 :fill-line-width 5 ;; weight of line for hatching
		 :set-window nil ;; don't set window based on histogram height
		 :nbins 20)
    (pgplot:hist p x2 :color :green
		 :fill-color nil
		 :fill nil
		 :log log
		 :line-width 7
		 :set-window nil 
		 :nbins 20)
    (pgplot:hist p x3 :color :default
		 :fill-color nil
		 :fill :cross-hatched
		 :log log
		 :set-window nil 
		 :nbins 20)
    (pgplot:xlabel p "x")
    (pgplot:ylabel p (if log "Log(N)" "N"))
    (when (not filename) (key-wait))
    (pgplot:close-device p)))




(defun demo-plot-6 (&key (device :x11) (filename nil) (do-labels nil))
  (when (not do-labels)
    (princ "demo-plot-6 - Contour plot of 2d paraboloid")(terpri))
  (when do-labels
    (princ "demo-plot-6 - Contour plot of 2d paraboloid + labels")(terpri)
    (princ "   ** YES, SOME OF THE LABELS ARE BROKEN. BUG IN PGPLOT?")(terpri))
  (let* ((n 30)
	 (ncont 10)
	 (a (make-array (list n n) :element-type 'single-float))
	 (contour-levels (make-array 15))
	 (contour-labels (make-array 15))
	 ;; open the pgplot device
	 (p (pgplot:open-device device :filename filename)))
    ;; fill in array with a 2d paraboloid
    (dotimes (ix n)
      (dotimes (iy n)
	;; note that x varies across columns, and y down rows
	(setf (aref a iy ix)
	      (coerce
	       (+ (expt (- ix 10) 2)
		  (expt (- iy 15) 2))
	       'single-float))))
    ;; make levels and labels
    (loop for i below ncont
	  for x = (float (* (/ i (1- ncont)) (expt 15 2)) 1s0)
	  do
	  (setf (aref contour-levels i) x)
	  (setf (aref contour-labels i) (format nil "~,1F" x)))
    ;;
    ;; set the window - this should correspond to the true x,y bounds 
    ;; of the 2d array 'a', though these values are arbitrary
    (pgplot:set-window p -3 2 -2 4)
    (pgplot:box p)
    (pgplot:xlabel p "x" :y-offset -0.02)
    (pgplot:ylabel p "y" :x-offset -0.03)
    ;; contour has lots more arguments, but you'll have to read
    ;; the source.  In particular, you can define a coordinate transform
    ;; matrix from the array 'a' to the x,y system
    (pgplot:set-character-height p 0.8)
    (pgplot:contour p a :contours contour-levels
		    :contour-labels (and do-labels contour-labels))
    ;;
    (when (not filename) (key-wait))
    (pgplot:close-device p)))

(defun demo-plot-7 (&key (device :x11) (filename nil)
			 (type nil) (transfer-function :linear))
  (princ "demo-plot-7 - Image plot of 2d paraboloid")(terpri)
  (let* ((n 30) 
	 (a (make-array (list n n) :element-type 'single-float))
	 ;; open the pgplot device
	 (p (pgplot:open-device device :filename filename)))
    ;; fill in array with a 2d paraboloid
    (dotimes (ix n)
      (dotimes (iy n)
	;; note that x varies across columns, and y down rows
	(setf (aref a iy ix)
	      (coerce
	       (+ (expt (- ix 10) 2)
		  (expt (- iy 15) 2))
	       'single-float))))
    ;;
    ;; set the window - this should correspond to the true x,y bounds 
    ;; of the 2d array 'a', though these values are arbitrary
    (pgplot:set-window p -3 2 -2 4)
    (pgplot:set-viewport p 0.2 0.8 0.2 0.9)
    (pgplot:box p)
    (pgplot:xlabel p "x" :y-offset -0.02)
    (pgplot:ylabel p "y" :x-offset -0.03)
    ;; image has lots more arguments, described in doc string.
    ;; In particular, you can define a coordinate transform
    ;; matrix from the array 'a' to the x,y system
    (pgplot:image p a :type type :transfer-function transfer-function
		  :color-wedge-side :right )
    ;;
    (when (not filename) (key-wait))
    (pgplot:close-device p)))



(defun demo-plot-8 (&key (device :x11) (filename nil))
  (princ "demo-plot-8 - Some shapes") (terpri)
  (let*  ((p (pgplot:open-device device :filename filename)))
    (pgplot:circle  p
		   0.3 0.75 0.2 ;; x0 y0 radius
		   :color :blue
		   :fill-area-style :solid)
    (pgplot:ellipse p
		    0.7 0.6  ;; x0 y0
		    0.1 0.2   ;; major, minor axes
		    45        ;; theta (degrees)
		   :color :green)
    (pgplot:rectangle p
		      0.05 0.25 0.05 0.35 ;; x1 x2 y1 y2
		      :color :red
		      :line-style :solid
		      :line-width 2
		      :fill-area-style :cross-hatched)
    
    (pgplot:polygon p
		    #(0.55 0.65 0.85 0.88 0.95 0.80)
		    #(0.05 0.30 0.25 0.15 0.05 0.01)
		    :color :yellow
		    :fill-area-style nil)

    (pgplot:write-text p "Shapes!"
		       0.35 0.35
		       :character-height 2.0
		       :angle 45)
		    
    (when (not filename) (key-wait))
    (pgplot:close-device p)
    ))
    


(defun demo-plot-9 (&key (device :x11) (filename nil))
  (princ "demo-plot-9 - Vector Field") (terpri)
  (let*  ((p (pgplot:open-device device :filename filename))
	  (a (make-array (list 21 21) :element-type 'single-float))
	  (b (make-array (list 21 21) :element-type 'single-float)))
    (loop 
     for ix below 21
     for x = (/ (- ix 10.0) 10)
     do
     (loop 
      for iy below 21
      for y = (/ (- iy 10.0) 10)
      for r = (+ 0.01 (sqrt (+ (expt x 2) (expt y 2))))
      do
      (setf (aref a iy ix) (*    (/ y r)))
      (setf (aref b iy ix) (* -1 (/ x r)))
      ))
    ;;
    (pgplot:vector-field p a b)
    (pgplot:box p :y-num-labels-left nil :x-num-labels-bottom nil)
    (when (not filename) (key-wait))
    (pgplot:close-device p)
    ))
    









