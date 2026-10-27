#|

Cairo substitutions for PGPLOT

|#


(in-package fits-stamp-cairo)

;;;; ============================================================
;;;; Color Definitions
;;;; ============================================================

(defparameter *colors*
  '((:black      0.0   0.0   0.0)
    (:white      1.0   1.0   1.0)
    (:red        1.0   0.0   0.0)
    (:green      0.0   1.0   0.0)
    (:blue       0.0   0.0   1.0)
    (:cyan       0.0   1.0   1.0)
    (:magenta    1.0   0.0   1.0)
    (:yellow     1.0   1.0   0.0)
    (:orange     1.0   0.5   0.0)
    (:dark-gray  0.3   0.3   0.3)
    (:light-gray 0.7   0.7   0.7)
    (:very-light-gray 0.9   0.9   0.9)
    (:background 0.0   0.0   0.0)    ; black background for astro images
    (:default    1.0   0.0   0.0)    ; default to red like PGPLOT
    (:blue+cyan  0.0   0.8   1.0)))

(defun color-rgb (color-spec)
  "Return (r g b) values for a color keyword or list"
  (cond
    ((keywordp color-spec)
     (let ((entry (assoc color-spec *colors*)))
       (if entry
           (values (second entry) (third entry) (fourth entry))
           (values 1.0 0.0 0.0))))  ; default red
    ((and (listp color-spec) (= 3 (length color-spec)))
     (values (first color-spec) (second color-spec) (third color-spec)))
    (t (values 1.0 0.0 0.0))))

;;;; ============================================================
;;;; Stamp Context - the drawing state
;;;; ============================================================

(defclass stamp-context ()
  (;; Cairo objects
   (surface :initform nil :accessor ctx-surface)
   (cr      :initform nil :accessor ctx-cr)  ; cairo context
   ;; Output info
   (output-type :initarg :output-type :accessor ctx-output-type)
   (output-file :initarg :output-file :accessor ctx-output-file)
   ;; Device dimensions (pixels)
   (dev-width  :initform 800 :initarg :dev-width  :accessor ctx-dev-width)
   (dev-height :initform 800 :initarg :dev-height :accessor ctx-dev-height)
   ;; Viewport in device coords (0-1 normalized, then scaled to device)
   (vp-x1 :initform 0.1 :accessor ctx-vp-x1)
   (vp-x2 :initform 0.95 :accessor ctx-vp-x2)
   (vp-y1 :initform 0.1 :accessor ctx-vp-y1)
   (vp-y2 :initform 0.95 :accessor ctx-vp-y2)
   ;; World coordinate window
   (win-x1 :initform 0.0 :accessor ctx-win-x1)
   (win-x2 :initform 1.0 :accessor ctx-win-x2)
   (win-y1 :initform 0.0 :accessor ctx-win-y1)
   (win-y2 :initform 1.0 :accessor ctx-win-y2)
   ;; Current drawing state
   (line-width :initform 1.0 :accessor ctx-line-width)
   (font-size  :initform 12.0 :accessor ctx-font-size)
   ;; Scale factor for resolution independence (1.0 at 800px baseline)
   (scale-factor :initform 1.0 :accessor ctx-scale-factor)))

(defun ctx-scale (ctx value)
  "Scale a device-coordinate value by the context's scale factor"
  (* value (ctx-scale-factor ctx)))

(defun open-context (output-type &key filename (width 800) (height 800))
  "Create and open a new stamp context for rendering"
  (let ((ctx (make-instance 'stamp-context
                            :output-type output-type
                            :output-file filename
                            :dev-width width
                            :dev-height height)))
    (setf (ctx-surface ctx)
          (ecase output-type
            (:pdf  (cl-cairo2:create-pdf-surface filename
                                                  (coerce width 'double-float)
                                                  (coerce height 'double-float)))
            (:png  (cl-cairo2:create-image-surface :argb32 width height))
            (:svg  (cl-cairo2:create-svg-surface filename
                                                  (coerce width 'double-float)
                                                  (coerce height 'double-float)))))
    (setf (ctx-cr ctx) (cl-cairo2:create-context (ctx-surface ctx)))
    ;; Set scale factor for resolution independence (baseline 800px)
    (setf (ctx-scale-factor ctx) (/ (coerce width 'double-float) 800.0d0))
    ;; Set default state
    (cl-cairo2:select-font-face "DejaVu Sans" :normal :normal (ctx-cr ctx))
    ctx))

(defun close-context (ctx)
  "Close the context and finalize output"
  (when (ctx-cr ctx)
    (when (eq (ctx-output-type ctx) :png)
      (cl-cairo2:surface-write-to-png (ctx-surface ctx) (ctx-output-file ctx)))
    (cl-cairo2:destroy (ctx-cr ctx))
    (cl-cairo2:destroy (ctx-surface ctx))
    (setf (ctx-cr ctx) nil)
    (setf (ctx-surface ctx) nil)))

(defmacro with-stamp-context ((ctx output-type &key filename (width 800) (height 800)) &body body)
  "Execute body with a stamp context, ensuring cleanup"
  `(let ((,ctx (open-context ,output-type :filename ,filename :width ,width :height ,height)))
     (unwind-protect
          (progn ,@body)
       (close-context ,ctx))))

;;;; ============================================================
;;;; Coordinate Transformations
;;;; ============================================================

(defun set-viewport (ctx x1 x2 y1 y2)
  "Set viewport in normalized device coordinates (0-1)"
  (setf (ctx-vp-x1 ctx) x1
        (ctx-vp-x2 ctx) x2
        (ctx-vp-y1 ctx) y1
        (ctx-vp-y2 ctx) y2))

(defun set-window (ctx x1 x2 y1 y2)
  "Set world coordinate window"
  (setf (ctx-win-x1 ctx) (coerce x1 'double-float)
        (ctx-win-x2 ctx) (coerce x2 'double-float)
        (ctx-win-y1 ctx) (coerce y1 'double-float)
        (ctx-win-y2 ctx) (coerce y2 'double-float)))

(defun world-to-device (ctx wx wy)
  "Convert world coordinates to device (pixel) coordinates"
  (let* ((win-dx (- (ctx-win-x2 ctx) (ctx-win-x1 ctx)))
         (win-dy (- (ctx-win-y2 ctx) (ctx-win-y1 ctx)))
         ;; Normalized position in world window
         (nx (/ (- wx (ctx-win-x1 ctx)) win-dx))
         (ny (/ (- wy (ctx-win-y1 ctx)) win-dy))
         ;; Viewport bounds in device pixels
         (vp-px1 (* (ctx-vp-x1 ctx) (ctx-dev-width ctx)))
         (vp-px2 (* (ctx-vp-x2 ctx) (ctx-dev-width ctx)))
         (vp-py1 (* (ctx-vp-y1 ctx) (ctx-dev-height ctx)))
         (vp-py2 (* (ctx-vp-y2 ctx) (ctx-dev-height ctx)))
         ;; Device coordinates (Y flipped for screen coords)
         (dx (+ vp-px1 (* nx (- vp-px2 vp-px1))))
         (dy (- vp-py2 (* ny (- vp-py2 vp-py1)))))  ; flip Y
    (values dx dy)))

(defun world-to-device-distance (ctx wdx wdy)
  "Convert world coordinate distances to device distances"
  (let* ((win-dx (- (ctx-win-x2 ctx) (ctx-win-x1 ctx)))
         (win-dy (- (ctx-win-y2 ctx) (ctx-win-y1 ctx)))
         (vp-px1 (* (ctx-vp-x1 ctx) (ctx-dev-width ctx)))
         (vp-px2 (* (ctx-vp-x2 ctx) (ctx-dev-width ctx)))
         (vp-py1 (* (ctx-vp-y1 ctx) (ctx-dev-height ctx)))
         (vp-py2 (* (ctx-vp-y2 ctx) (ctx-dev-height ctx))))
    (values (* (/ wdx win-dx) (- vp-px2 vp-px1))
            (* (/ wdy win-dy) (- vp-py2 vp-py1)))))

(defmacro with-window ((ctx x1 x2 y1 y2) &body body)
  "Temporarily change the world coordinate window"
  (let ((old-x1 (gensym)) (old-x2 (gensym))
        (old-y1 (gensym)) (old-y2 (gensym)))
    `(let ((,old-x1 (ctx-win-x1 ,ctx))
           (,old-x2 (ctx-win-x2 ,ctx))
           (,old-y1 (ctx-win-y1 ,ctx))
           (,old-y2 (ctx-win-y2 ,ctx)))
       (unwind-protect
            (progn
              (set-window ,ctx ,x1 ,x2 ,y1 ,y2)
              ,@body)
         (set-window ,ctx ,old-x1 ,old-x2 ,old-y1 ,old-y2)))))

;;;; ============================================================
;;;; Colormap and Image Rendering
;;;; ============================================================

(defun make-grayscale-colormap (&key (imin 0) (imax 255) (reverse nil))
  "Create a grayscale colormap function.
   Maps normalized value [0,1] to RGB values."
  (lambda (v)
    (let* ((clamped (max 0.0 (min 1.0 v)))
           (gray (if reverse (- 1.0 clamped) clamped))
           (scaled (+ imin (* gray (- imax imin))))
           (byte-val (/ scaled 255.0)))
      (values byte-val byte-val byte-val))))

(defun render-image (ctx array &key (val1 0.0) (val2 1.0) (reverse t) (colormap nil))
  "Render a 2D array as an image within the current window.
   VAL1/VAL2 are the data values mapping to colormap min/max.
   REVERSE t means high values are dark (astronomical convention)."
  (let* ((cr (ctx-cr ctx))
         (h (array-dimension array 0))
         (w (array-dimension array 1))
         (cmap (or colormap (make-grayscale-colormap :reverse reverse)))
         (val-range (- val2 val1))
         ;; Create image surface for the data
         (img-surface (cl-cairo2:create-image-surface :argb32 w h))
         (img-cr (cl-cairo2:create-context img-surface))
         ;; Get viewport bounds in device coords
         (vp-px1 (* (ctx-vp-x1 ctx) (ctx-dev-width ctx)))
         (vp-px2 (* (ctx-vp-x2 ctx) (ctx-dev-width ctx)))
         (vp-py1 (* (ctx-vp-y1 ctx) (ctx-dev-height ctx)))
         (vp-py2 (* (ctx-vp-y2 ctx) (ctx-dev-height ctx)))
         (vp-width (- vp-px2 vp-px1))
         (vp-height (- vp-py2 vp-py1)))

    ;; Fill the image surface with colormap-mapped values
    (loop for j from 0 below h do
      (loop for i from 0 below w do
        (let* ((raw-val (aref array j i))
               (norm-val (/ (- raw-val val1) val-range)))
          (multiple-value-bind (r g b) (funcall cmap norm-val)
            (cl-cairo2:set-source-rgb r g b img-cr)
            (cl-cairo2:rectangle (coerce i 'double-float)
                                 (coerce (- h j 1) 'double-float)  ; flip Y
                                 1.0d0 1.0d0
                                 img-cr)
            (cl-cairo2:fill-path img-cr)))))

    ;; Draw the image surface scaled to the viewport
    (cl-cairo2:save cr)
    (cl-cairo2:translate vp-px1 vp-py1 cr)
    (cl-cairo2:scale (/ vp-width w) (/ vp-height h) cr)
    (cl-cairo2:set-source-surface img-surface 0.0d0 0.0d0 cr)
    (cl-cairo2:paint cr)
    (cl-cairo2:restore cr)

    (cl-cairo2:destroy img-cr)
    (cl-cairo2:destroy img-surface)))

;;;; ============================================================
;;;; Drawing Primitives
;;;; ============================================================

(defun set-color (ctx color)
  "Set the current drawing color"
  (multiple-value-bind (r g b) (color-rgb color)
    (cl-cairo2:set-source-rgb (coerce r 'double-float)
                               (coerce g 'double-float)
                               (coerce b 'double-float)
                               (ctx-cr ctx))))

(defun set-line-width (ctx width)
  "Set line width in device units (scaled for resolution independence)"
  (let ((scaled-width (ctx-scale ctx width)))
    (setf (ctx-line-width ctx) scaled-width)
    (cl-cairo2:set-line-width (coerce scaled-width 'double-float) (ctx-cr ctx))))

(defmacro with-settings ((ctx &key line-width color) &body body)
  "Execute body with temporary drawing settings"
  (let ((old-lw (gensym)) (cr (gensym)))
    `(let ((,old-lw (ctx-line-width ,ctx))
           (,cr (ctx-cr ,ctx)))
       (cl-cairo2:save ,cr)
       (unwind-protect
            (progn
              ,@(when line-width `((set-line-width ,ctx ,line-width)))
              ,@(when color `((set-color ,ctx ,color)))
              ,@body)
         (cl-cairo2:restore ,cr)
         (setf (ctx-line-width ,ctx) ,old-lw)))))

(defun draw-line (ctx x1 y1 x2 y2 &key (color :default) (line-width nil))
  "Draw a line in world coordinates"
  (let ((cr (ctx-cr ctx)))
    (when line-width (set-line-width ctx line-width))
    (set-color ctx color)
    (multiple-value-bind (dx1 dy1) (world-to-device ctx x1 y1)
      (multiple-value-bind (dx2 dy2) (world-to-device ctx x2 y2)
        (cl-cairo2:new-path cr)
        (cl-cairo2:move-to dx1 dy1 cr)
        (cl-cairo2:line-to dx2 dy2 cr)
        (cl-cairo2:stroke cr)))))

(defun connect (ctx xvec yvec &key (color :default) (line-width nil))
  "Draw connected line segments through points"
  (when (and (> (length xvec) 0) (> (length yvec) 0))
    (let ((cr (ctx-cr ctx)))
      (when line-width (set-line-width ctx line-width))
      (set-color ctx color)
      (cl-cairo2:new-path cr)
      (multiple-value-bind (dx dy) (world-to-device ctx (aref xvec 0) (aref yvec 0))
        (cl-cairo2:move-to dx dy cr))
      (loop for i from 1 below (min (length xvec) (length yvec)) do
        (multiple-value-bind (dx dy) (world-to-device ctx (aref xvec i) (aref yvec i))
          (cl-cairo2:line-to dx dy cr)))
      (cl-cairo2:stroke cr))))

(defun draw-circle (ctx cx cy radius &key (color :default) (line-width nil) (fill nil))
  "Draw a circle in world coordinates"
  (let ((cr (ctx-cr ctx)))
    (when line-width (set-line-width ctx line-width))
    (set-color ctx color)
    (multiple-value-bind (dcx dcy) (world-to-device ctx cx cy)
      ;; Approximate radius in device coords (use X scale)
      (multiple-value-bind (drx dry) (world-to-device-distance ctx radius radius)
        (declare (ignore dry))
        (cl-cairo2:new-path cr)  ; Start fresh path to avoid spurious lines
        (cl-cairo2:arc dcx dcy (abs drx) 0.0d0 (* 2.0d0 pi) cr)
        (if fill
            (cl-cairo2:fill-path cr)
            (cl-cairo2:stroke cr))))))

(defun draw-sun-symbol (ctx x y &key (size 1.0) (line-width-scale 1.0) (color :default)
			    (blank-backd nil))
  "Draw a sun symbol (circle with center dot) at world coordinates"
  (let* ((cr (ctx-cr ctx))
         (radius (ctx-scale ctx (* 6.0 size))))
    (set-color ctx color)
    (multiple-value-bind (dx dy) (world-to-device ctx x y)
      ;; blank backd circle
      (when blank-backd
	(set-color ctx :very-light-gray)
	(cl-cairo2:new-path cr)
	(cl-cairo2:set-line-width (* line-width-scale (ctx-scale ctx 1.5)) cr)
	(cl-cairo2:arc dx dy (* 1.1 radius) 0.0d0 (* 2.0d0 pi) cr)
	(cl-cairo2:fill-path cr))
      ;;
      (set-color ctx color)
      ;; Outer circle
      (cl-cairo2:new-path cr)
      (cl-cairo2:set-line-width (* line-width-scale (ctx-scale ctx 1.5)) cr)
      (cl-cairo2:arc dx dy radius 0.0d0 (* 2.0d0 pi) cr)
      (cl-cairo2:stroke cr)
      ;; Center dot
      (cl-cairo2:new-path cr)
      (cl-cairo2:arc dx dy (ctx-scale ctx (* 1.5 size)) 0.0d0 (* 2.0d0 pi) cr)
      (cl-cairo2:fill-path cr))))

(defun draw-arrow (ctx x1 y1 x2 y2 &key (color :default) (size 1.0) (line-width nil))
  "Draw an arrow from (x1,y1) to (x2,y2) in world coordinates"
  (let* ((cr (ctx-cr ctx))
         (head-length (ctx-scale ctx (* 10.0 size)))  ; in device units, scaled
         (head-angle (/ pi 6)))       ; 30 degrees
    (when line-width (set-line-width ctx line-width))
    (set-color ctx color)
    (multiple-value-bind (dx1 dy1) (world-to-device ctx x1 y1)
      (multiple-value-bind (dx2 dy2) (world-to-device ctx x2 y2)
        ;; Draw the line
        (cl-cairo2:new-path cr)
        (cl-cairo2:move-to dx1 dy1 cr)
        (cl-cairo2:line-to dx2 dy2 cr)
        (cl-cairo2:stroke cr)
        ;; Compute arrow head
        (let* ((angle (atan (- dy2 dy1) (- dx2 dx1)))
               (a1 (+ angle pi (- head-angle)))
               (a2 (+ angle pi head-angle))
               (hx1 (+ dx2 (* head-length (cos a1))))
               (hy1 (+ dy2 (* head-length (sin a1))))
               (hx2 (+ dx2 (* head-length (cos a2))))
               (hy2 (+ dy2 (* head-length (sin a2)))))
          ;; Draw filled arrow head
          (cl-cairo2:new-path cr)
          (cl-cairo2:move-to dx2 dy2 cr)
          (cl-cairo2:line-to hx1 hy1 cr)
          (cl-cairo2:line-to hx2 hy2 cr)
          (cl-cairo2:close-path cr)
          (cl-cairo2:fill-path cr))))))

(defun draw-point (ctx x y ptype &key (color :default) (size 2.0))
  "Draw a marker point at (x,y)"
  (let ((cr (ctx-cr ctx))
        (dev-size (ctx-scale ctx (* size 3.0))))  ; scale factor for visibility
    (set-color ctx color)
    (cl-cairo2:new-path cr)  ; Start fresh to avoid spurious lines
    (multiple-value-bind (dx dy) (world-to-device ctx x y)
      (ecase ptype
        ((:point :dot)
         (cl-cairo2:arc dx dy dev-size 0.0d0 (* 2.0d0 pi) cr)
         (cl-cairo2:fill-path cr))
        ((:circle :circle-2)
         (cl-cairo2:arc dx dy dev-size 0.0d0 (* 2.0d0 pi) cr)
         (cl-cairo2:stroke cr))
        ((:cross :plus)
         (cl-cairo2:move-to (- dx dev-size) dy cr)
         (cl-cairo2:line-to (+ dx dev-size) dy cr)
         (cl-cairo2:move-to dx (- dy dev-size) cr)
         (cl-cairo2:line-to dx (+ dy dev-size) cr)
         (cl-cairo2:stroke cr))
        ((:x :times)
         (cl-cairo2:move-to (- dx dev-size) (- dy dev-size) cr)
         (cl-cairo2:line-to (+ dx dev-size) (+ dy dev-size) cr)
         (cl-cairo2:move-to (- dx dev-size) (+ dy dev-size) cr)
         (cl-cairo2:line-to (+ dx dev-size) (- dy dev-size) cr)
         (cl-cairo2:stroke cr))
        ((:square)
         (cl-cairo2:rectangle (- dx dev-size) (- dy dev-size)
                              (* 2 dev-size) (* 2 dev-size) cr)
         (cl-cairo2:stroke cr))
        ((:filled-square)
         (cl-cairo2:rectangle (- dx dev-size) (- dy dev-size)
                              (* 2 dev-size) (* 2 dev-size) cr)
         (cl-cairo2:fill-path cr))))))

(defun draw-points (ctx xvec yvec ptype &key (color :default) (size 2.0))
  "Draw multiple marker points"
  (loop for i from 0 below (min (length xvec) (length yvec))
        for pt = (if (vectorp ptype) (aref ptype i) ptype)
        for sz = (if (vectorp size) (aref size i) size)
        do (draw-point ctx (aref xvec i) (aref yvec i) pt :color color :size sz)))

;;;; ============================================================
;;;; Text Rendering
;;;; ============================================================

(defun write-text (ctx text x y &key (color :default) (character-height 1.0)
                                     (center nil) (angle 0.0) (font :default))
  "Write text at world coordinates (x,y)"
  (declare (ignore font))  ; TODO: implement font selection
  (let* ((cr (ctx-cr ctx))
         (font-size (ctx-scale ctx (* character-height 12.0))))  ; base font size, scaled
    (set-color ctx color)
    (cl-cairo2:set-font-size font-size cr)
    (multiple-value-bind (dx dy) (world-to-device ctx x y)
      (cl-cairo2:save cr)
      (cl-cairo2:translate dx dy cr)
      (when (not (zerop angle))
        (cl-cairo2:rotate (- (* angle (/ pi 180.0))) cr))  ; negative for screen coords
      (if center
          ;; text-extents returns: x-bearing y-bearing width height x-advance y-advance
          (multiple-value-bind (x-bearing y-bearing text-width text-height x-adv y-adv)
              (cl-cairo2:text-extents text cr)
            (declare (ignore x-bearing y-bearing x-adv y-adv))
            (cl-cairo2:move-to (- (/ text-width 2)) (/ text-height 2) cr)
            (cl-cairo2:show-text text cr))
          (progn
            (cl-cairo2:move-to 0.0d0 0.0d0 cr)
            (cl-cairo2:show-text text cr)))
      (cl-cairo2:restore cr))))


(defun write-text-with-outline (ctx text x y &key (color :default) (outline-color :white)
                                                     (character-height 1.0) (outline-width 3.0)
                                                     (center nil) (angle 0.0))
    "Write text with an outline/aura effect"
    (let* ((cr (ctx-cr ctx))
           (font-size (ctx-scale ctx (* character-height 12.0))))
      (cl-cairo2:set-font-size font-size cr)
      (multiple-value-bind (dx dy) (world-to-device ctx x y)
        (cl-cairo2:save cr)
        (cl-cairo2:translate dx dy cr)
        (when (not (zerop angle))
          (cl-cairo2:rotate (- (* angle (/ pi 180.0))) cr))
        (let ((tx 0.0d0) (ty 0.0d0))
          (when center
            (multiple-value-bind (xb yb tw th xa ya) (cl-cairo2:text-extents text cr)
              (declare (ignore xb yb xa ya))
              (setf tx (- (/ tw 2)) ty (/ th 2))))
          (cl-cairo2:move-to tx ty cr)
          ;; Create text path
          (cl-cairo2:text-path text cr)
          ;; Stroke outline first (behind)
          (set-color ctx outline-color)
          (cl-cairo2:set-line-width (ctx-scale ctx outline-width) cr)
          (cl-cairo2:set-line-join :round cr)  ; smooth corners
          (cl-cairo2:stroke-preserve cr)
          ;; Fill text on top
          (set-color ctx color)
          (cl-cairo2:fill-path cr))
        (cl-cairo2:restore cr))))


(defun write-blanked-text (ctx text x y &key (color :default) (character-height 1.0)
                                             (center nil) (angle 0.0) (font :default)
                                             (blanking-color :background))
  "Write text with a filled background rectangle"
  (declare (ignore font))
  (let* ((cr (ctx-cr ctx))
         (font-size (ctx-scale ctx (* character-height 12.0)))
         (padding (ctx-scale ctx 2.0)))
    (cl-cairo2:set-font-size font-size cr)
    (multiple-value-bind (dx dy) (world-to-device ctx x y)
      (cl-cairo2:save cr)
      (cl-cairo2:translate dx dy cr)
      (when (not (zerop angle))
        (cl-cairo2:rotate (- (* angle (/ pi 180.0))) cr))
      ;; Measure text - returns: x-bearing y-bearing width height x-advance y-advance
      (multiple-value-bind (x-bearing y-bearing text-width text-height x-adv y-adv)
          (cl-cairo2:text-extents text cr)
        (declare (ignore x-adv y-adv))
        (let* ((tx (if center (- (/ text-width 2)) 0.0))
               (ty (if center (/ text-height 2) 0.0)))
          ;; Draw background rectangle
          (set-color ctx blanking-color)
          (cl-cairo2:rectangle (+ tx x-bearing (- padding))
                               (+ ty y-bearing (- padding))
                               (+ text-width (* 2 padding))
                               (+ text-height (* 2 padding))
                               cr)
          (cl-cairo2:fill-path cr)
          ;; Draw text
          (set-color ctx color)
          (cl-cairo2:move-to tx ty cr)
          (cl-cairo2:show-text text cr)))
      (cl-cairo2:restore cr))))

;;;; ============================================================
;;;; Axis Box with Tick Marks
;;;; ============================================================

(defun compute-nice-ticks (vmin vmax &key (target-count 5))
  "Compute nice tick mark positions for an axis"
  (let* ((range (- vmax vmin))
         (rough-step (/ range target-count))
         ;; Find a nice step size (1, 2, 5 times power of 10)
         (magnitude (expt 10.0 (floor (log rough-step 10))))
         (normalized (/ rough-step magnitude))
         (nice-step (* magnitude
                       (cond ((< normalized 1.5) 1.0)
                             ((< normalized 3.0) 2.0)
                             ((< normalized 7.0) 5.0)
                             (t 10.0))))
         ;; Compute first tick
         (first-tick (* nice-step (ceiling vmin nice-step))))
    ;; Generate tick list
    (loop for tick from first-tick to vmax by nice-step
          collect tick)))

(defun format-tick-label (value)
  "Format a tick value as a string"
  (let ((absval (abs value)))
    (cond
      ((zerop value) "0")
      ((and (>= absval 0.01) (< absval 10000))
       (format nil "~,2F" value))
      (t (format nil "~,2E" value)))))

(defun draw-box (ctx &key (size 1.0) (x-num-labels t) (y-num-labels t))
  "Draw axes box with tick marks and optional labels"
  (let* ((cr (ctx-cr ctx))
         (x1 (ctx-win-x1 ctx)) (x2 (ctx-win-x2 ctx))
         (y1 (ctx-win-y1 ctx)) (y2 (ctx-win-y2 ctx))
         (major-tick-length (ctx-scale ctx 8.0d0))
         (minor-tick-length (ctx-scale ctx 4.0d0))
         (font-size (ctx-scale ctx (* 10.0d0 size)))
         (label-offset (ctx-scale ctx 20.0d0)))

    ;; Reset source explicitly to solid color (black for visibility)
    (cl-cairo2:set-source-rgb 0.0d0 0.0d0 0.0d0 cr)
    (cl-cairo2:set-line-width (ctx-scale ctx 1.5d0) cr)
    (cl-cairo2:select-font-face "DejaVu Sans" :normal :normal cr)
    (cl-cairo2:set-font-size font-size cr)

    ;; Draw box outline
    (multiple-value-bind (dx1 dy1) (world-to-device ctx x1 y1)
      (multiple-value-bind (dx2 dy2) (world-to-device ctx x2 y2)
        (cl-cairo2:new-path cr)
        (cl-cairo2:move-to dx1 dy1 cr)
        (cl-cairo2:line-to dx2 dy1 cr)
        (cl-cairo2:line-to dx2 dy2 cr)
        (cl-cairo2:line-to dx1 dy2 cr)
        (cl-cairo2:close-path cr)
        (cl-cairo2:stroke cr)))

    ;; Helper to draw a single tick (inward pointing)
    (flet ((draw-tick (world-x world-y edge tick-len)
             ;; edge: :bottom, :top, :left, :right
             (multiple-value-bind (dx dy) (world-to-device ctx world-x world-y)
               (cl-cairo2:new-path cr)
               (cl-cairo2:move-to dx dy cr)
               (ecase edge
                 (:bottom (cl-cairo2:line-to dx (- dy tick-len) cr))  ; up into image
                 (:top    (cl-cairo2:line-to dx (+ dy tick-len) cr))  ; down into image
                 (:left   (cl-cairo2:line-to (+ dx tick-len) dy cr))  ; right into image
                 (:right  (cl-cairo2:line-to (- dx tick-len) dy cr))) ; left into image
               (cl-cairo2:stroke cr))))

      ;; X-axis ticks (major and minor)
      (let* ((x-ticks (compute-nice-ticks x1 x2))
             (sorted-ticks (sort (copy-list x-ticks) #'<)))
        (when (>= (length sorted-ticks) 2)
          (let ((tick-spacing (- (second sorted-ticks) (first sorted-ticks))))
            ;; Major ticks
            (dolist (tick sorted-ticks)
              (draw-tick tick y1 :bottom major-tick-length)
              (draw-tick tick y2 :top major-tick-length)
              (when x-num-labels
                (multiple-value-bind (tx ty) (world-to-device ctx tick y1)
                  (let ((label (format-tick-label tick)))
                    (multiple-value-bind (xb yb tw th xa ya) (cl-cairo2:text-extents label cr)
                      (declare (ignore xb yb th xa ya))
                      (cl-cairo2:move-to (- tx (/ tw 2.0d0)) (+ ty label-offset) cr)
                      (cl-cairo2:show-text label cr))))))
            ;; Minor ticks (4 between each major tick, plus edges)
            (let ((minor-step (/ tick-spacing 5.0d0)))
              ;; Between edge and first major tick
              (loop for minor-tick from (- (first sorted-ticks) minor-step) downto x1 by minor-step
                    when (> minor-tick x1)
                    do (draw-tick minor-tick y1 :bottom minor-tick-length)
                       (draw-tick minor-tick y2 :top minor-tick-length))
              ;; Between major ticks
              (loop for i from 0 below (1- (length sorted-ticks))
                    for t1 = (nth i sorted-ticks)
                    do (loop for j from 1 to 4
                             for minor-tick = (+ t1 (* j minor-step))
                             do (draw-tick minor-tick y1 :bottom minor-tick-length)
                                (draw-tick minor-tick y2 :top minor-tick-length)))
              ;; Between last major tick and edge
              (loop for minor-tick from (+ (car (last sorted-ticks)) minor-step) upto x2 by minor-step
                    when (< minor-tick x2)
                    do (draw-tick minor-tick y1 :bottom minor-tick-length)
                       (draw-tick minor-tick y2 :top minor-tick-length))))))

      ;; Y-axis ticks (major and minor)
      (let* ((y-ticks (compute-nice-ticks y1 y2))
             (sorted-ticks (sort (copy-list y-ticks) #'<)))
        (when (>= (length sorted-ticks) 2)
          (let ((tick-spacing (- (second sorted-ticks) (first sorted-ticks))))
            ;; Major ticks
            (dolist (tick sorted-ticks)
              (draw-tick x1 tick :left major-tick-length)
              (draw-tick x2 tick :right major-tick-length)
              (when y-num-labels
                (multiple-value-bind (tx ty) (world-to-device ctx x1 tick)
                  (let ((label (format-tick-label tick)))
                    (multiple-value-bind (xb yb tw th xa ya) (cl-cairo2:text-extents label cr)
                      (declare (ignore xb yb xa ya))
                      (cl-cairo2:move-to (- tx tw label-offset) (+ ty (/ th 2.0d0)) cr)
                      (cl-cairo2:show-text label cr))))))
            ;; Minor ticks (4 between each major tick, plus edges)
            (let ((minor-step (/ tick-spacing 5.0d0)))
              ;; Between edge and first major tick
              (loop for minor-tick from (- (first sorted-ticks) minor-step) downto y1 by minor-step
                    when (> minor-tick y1)
                    do (draw-tick x1 minor-tick :left minor-tick-length)
                       (draw-tick x2 minor-tick :right minor-tick-length))
              ;; Between major ticks
              (loop for i from 0 below (1- (length sorted-ticks))
                    for t1 = (nth i sorted-ticks)
                    do (loop for j from 1 to 4
                             for minor-tick = (+ t1 (* j minor-step))
                             do (draw-tick x1 minor-tick :left minor-tick-length)
                                (draw-tick x2 minor-tick :right minor-tick-length)))
              ;; Between last major tick and edge
              (loop for minor-tick from (+ (car (last sorted-ticks)) minor-step) upto y2 by minor-step
                    when (< minor-tick y2)
                    do (draw-tick x1 minor-tick :left minor-tick-length)
                       (draw-tick x2 minor-tick :right minor-tick-length)))))))))

(defun draw-toplabel (ctx text &key (character-height 1.4))
  "Draw a title at the top of the plot, in device coordinates above viewport"
  (let* ((cr (ctx-cr ctx))
         ;; Center of viewport in device coords
         (dev-x-mid (* 0.5 (+ (* (ctx-vp-x1 ctx) (ctx-dev-width ctx))
                              (* (ctx-vp-x2 ctx) (ctx-dev-width ctx)))))
         ;; vp-y1 * height is the top of the plot in device coords
         ;; We want to draw halfway between y=0 and vp-top
         (vp-top-device (* (ctx-vp-y1 ctx) (ctx-dev-height ctx)))
         (dev-y (* 0.5 vp-top-device))  ; center in the space above plot
         (font-size (ctx-scale ctx (* 19.6d0 character-height))))  ; 14 * 1.4 = 19.6 (40% larger)
    (cl-cairo2:set-source-rgb 0.0d0 0.0d0 0.0d0 cr)  ; black for visibility
    (cl-cairo2:select-font-face "DejaVu Sans" :normal :normal cr)
    (cl-cairo2:set-font-size font-size cr)
    ;; Center the text horizontally, place at baseline
    (multiple-value-bind (xb yb tw th xa ya) (cl-cairo2:text-extents text cr)
      (declare (ignore xb yb xa ya))
      (cl-cairo2:move-to (- dev-x-mid (/ tw 2.0d0)) (+ dev-y (/ th 3.0d0)) cr)
      (cl-cairo2:show-text text cr))))

