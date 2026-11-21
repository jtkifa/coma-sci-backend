;;;; colormaps.lisp — colormaps for images (backend-neutral)
;;;;
;;;; A COLORMAP maps a fraction 0..1 to a color.  A colormap spec is
;;;;   - a COLORMAP;
;;;;   - a keyword naming one in *NAMED-COLORMAPS*: :GRAY, :VIRIDIS,
;;;;     :PLASMA, :MAGMA, :INFERNO (matplotlib's perceptually uniform
;;;;     maps), :HEAT (PGPLOT's black-red-yellow-white), :COOLWARM (a
;;;;     Moreland diverging blue-white-red), :RAINBOW;
;;;;   - a list of color specs, a gradient through them at equal steps;
;;;;   - a function of a fraction returning values R G B (0..1).
;;;; REVERSE-COLORMAP flips one; DIVERGING-COLORMAP makes a Moreland
;;;; diverging map between any two colors.

(in-package :cl-cairoplot)

(defstruct (colormap (:constructor make-colormap (name function)))
  "NAME, and FUNCTION mapping a fraction 0..1 to values R G B in 0..1."
  name function)

(setf (documentation 'make-colormap 'function)
      "A colormap called NAME whose FUNCTION maps a fraction 0..1 to values R G B.")

(defmethod print-object ((cm colormap) stream)
  (print-unreadable-object (cm stream :type t)
    (format stream "~S" (colormap-name cm))))

(defun colormap-rgb (colormap fraction)
  "Values R G B of COLORMAP at FRACTION, clamped to 0..1."
  (funcall (colormap-function colormap) (max 0d0 (min 1d0 (float fraction 1d0)))))

(defvar *named-colormaps* (make-hash-table :test 'eq)
  "The colormaps by name (keywords), as DEFINE-COLORMAP defines them.")

(defun define-colormap (name function)
  "Name a colormap made from FUNCTION (fraction -> R G B)."
  (setf (gethash name *named-colormaps*) (make-colormap name function)))

(defun colormap-names ()
  "The names of the defined colormaps, sorted."
  (sort (loop for k being the hash-keys of *named-colormaps* collect k)
        #'string< :key #'symbol-name))

(defun parse-colormap (spec)
  "Return the COLORMAP designated by SPEC (see the top of this file)."
  (etypecase spec
    (colormap spec)
    (keyword (or (gethash spec *named-colormaps*)
                 (error "Unknown colormap ~S; see COLORMAP-NAMES." spec)))
    (list (gradient-colormap spec))
    (function (make-colormap :custom spec))))

(defun reverse-colormap (spec)
  "The colormap SPEC (a name or colormap) running the other way, light
   and dark exchanged."
  (let ((cm (parse-colormap spec)))
    (make-colormap (list :reversed (colormap-name cm))
                   (lambda (x) (funcall (colormap-function cm) (- 1 x))))))

;;; ============================================================
;;; Kinds of colormap
;;; ============================================================

(defun table-colormap (name table)
  "A colormap interpolating linearly in the N x 3 array TABLE."
  (let ((last (1- (array-dimension table 0))))
    (make-colormap
     name
     (lambda (x)
       (multiple-value-bind (k f) (floor (* x last))
         (when (= k last) (setf k (1- last) f 1))
         (flet ((at (c) (+ (* (- 1 f) (aref table k c)) (* f (aref table (1+ k) c)))))
           (values (at 0) (at 1) (at 2))))))))

(defun gradient-colormap (colors &key (name :gradient))
  "A colormap through the color specs COLORS at equal steps, linear in RGB."
  (let ((table (make-array (list (length colors) 3))))
    (loop for spec in colors
          for i from 0
          for c = (parse-color spec)
          do (setf (aref table i 0) (color-red c)
                   (aref table i 1) (color-green c)
                   (aref table i 2) (color-blue c)))
    (table-colormap name table)))

;;; Moreland's diverging colormaps ("Diverging Color Maps for Scientific
;;; Visualization", K. Moreland, 2009): interpolate in his Msh polar form
;;; of CIELAB, through white when the end colors are saturated and far
;;; apart in hue.  Colors are sRGB, linearized, with the D65 white point.

(defun srgb-to-linear (c)
  (if (<= c 0.04045) (/ c 12.92) (expt (/ (+ c 0.055) 1.055) 2.4)))

(defun linear-to-srgb (c)
  (let ((c (max 0d0 (min 1d0 c))))
    (if (<= c 0.0031308) (* 12.92 c) (- (* 1.055 (expt c (/ 2.4))) 0.055))))

(defparameter +d65+ '(0.95047d0 1d0 1.08883d0))

(defun rgb-to-msh (r g b)
  (let* ((r (srgb-to-linear r)) (g (srgb-to-linear g)) (b (srgb-to-linear b))
         (x (+ (* 0.4124 r) (* 0.3576 g) (* 0.1805 b)))
         (y (+ (* 0.2126 r) (* 0.7152 g) (* 0.0722 b)))
         (z (+ (* 0.0193 r) (* 0.1192 g) (* 0.9505 b))))
    (flet ((f (v) (if (> v 0.008856) (expt v 1/3) (+ (* 7.787 v) 16/116))))
      (destructuring-bind (xn yn zn) +d65+
        (let* ((fx (f (/ x xn))) (fy (f (/ y yn))) (fz (f (/ z zn)))
               (l (- (* 116 fy) 16)) (a (* 500 (- fx fy))) (bb (* 200 (- fy fz)))
               (m (sqrt (+ (* l l) (* a a) (* bb bb)))))
          (values m (if (zerop m) 0d0 (acos (/ l m))) (atan bb a)))))))

(defun msh-to-rgb (m s h)
  (let* ((l (* m (cos s))) (a (* m (sin s) (cos h))) (bb (* m (sin s) (sin h)))
         (fy (/ (+ l 16) 116)) (fx (+ fy (/ a 500))) (fz (- fy (/ bb 200))))
    (flet ((finv (v) (if (> v 0.206893) (* v v v) (/ (- v 16/116) 7.787))))
      (destructuring-bind (xn yn zn) +d65+
        (let ((x (* xn (finv fx))) (y (* yn (finv fy))) (z (* zn (finv fz))))
          (values (linear-to-srgb (+ (* 3.2406 x) (* -1.5372 y) (* -0.4986 z)))
                  (linear-to-srgb (+ (* -0.9689 x) (* 1.8758 y) (* 0.0415 z)))
                  (linear-to-srgb (+ (* 0.0557 x) (* -0.2040 y) (* 1.0570 z)))))))))

(defun adjust-hue (m s h m-unsaturated)
  "Moreland's hue spin for blending a saturated color toward an
   unsaturated one of magnitude M-UNSATURATED."
  (if (>= m m-unsaturated)
      h
      (let ((spin (/ (* s (sqrt (- (* m-unsaturated m-unsaturated) (* m m))))
                     (* m (sin s)))))
        (if (> h (/ pi -3)) (+ h spin) (- h spin)))))

(defun diverging-colormap (color1 color2 &key (name :diverging))
  "Moreland diverging colormap from color spec COLOR1 to COLOR2."
  (let ((c1 (parse-color color1)) (c2 (parse-color color2)))
    (multiple-value-bind (m1 s1 h1) (rgb-to-msh (color-red c1) (color-green c1) (color-blue c1))
      (multiple-value-bind (m2 s2 h2) (rgb-to-msh (color-red c2) (color-green c2) (color-blue c2))
        (make-colormap
         name
         (lambda (x)
           (let ((m1 m1) (s1 s1) (h1 h1) (m2 m2) (s2 s2) (h2 h2) (x x))
             ;; saturated and distinct hues: go through white in the middle
             (when (and (> s1 0.05) (> s2 0.05)
                        (> (min (abs (- h1 h2)) (- (* 2 pi) (abs (- h1 h2)))) (/ pi 3)))
               (let ((mid (max m1 m2 88)))
                 (if (< x 0.5)
                     (setf m2 mid s2 0 h2 0 x (* 2 x))
                     (setf m1 mid s1 0 h1 0 x (- (* 2 x) 1)))))
             (cond ((and (< s1 0.05) (> s2 0.05)) (setf h1 (adjust-hue m2 s2 h2 m1)))
                   ((and (< s2 0.05) (> s1 0.05)) (setf h2 (adjust-hue m1 s1 h1 m2))))
             (msh-to-rgb (+ (* (- 1 x) m1) (* x m2))
                         (+ (* (- 1 x) s1) (* x s2))
                         (+ (* (- 1 x) h1) (* x h2))))))))))

;;; ============================================================
;;; Named colormaps
;;; ============================================================

(define-colormap :gray (lambda (x) (values x x x)))
(setf (gethash :viridis *named-colormaps*) (table-colormap :viridis *viridis-data*)
      (gethash :plasma *named-colormaps*) (table-colormap :plasma *plasma-data*)
      (gethash :magma *named-colormaps*) (table-colormap :magma *magma-data*)
      (gethash :inferno *named-colormaps*) (table-colormap :inferno *inferno-data*))
;; PGPLOT's heat: successive red, green and blue ramps
(define-colormap :heat (lambda (x)
                         (values (min 1d0 (* 3 x))
                                 (max 0d0 (min 1d0 (* 3 (- x 0.3))))
                                 (max 0d0 (min 1d0 (* 3 (- x 0.6)))))))
;; Moreland's standard cool-warm endpoints
(setf (gethash :coolwarm *named-colormaps*)
      (diverging-colormap '(0.230 0.299 0.754) '(0.706 0.016 0.150) :name :coolwarm))
(define-colormap :rainbow (lambda (x)
                            (flet ((g (x0 s) (exp (* -0.5 (expt (/ (- x x0) s) 2)))))
                              (values (g 0d0 0.4) (g 0.5d0 0.33) (g 1d0 0.4)))))

;;; ============================================================
;;; Lookup tables
;;; ============================================================

(defun colormap-lut (spec &optional (n 1024))
  "N opaque colors sampling colormap SPEC evenly, as 32-bit ARGB words
   (alpha in the top byte, as cairo's image format)."
  (let ((cm (parse-colormap spec))
        (lut (make-array n :element-type '(unsigned-byte 32))))
    (dotimes (i n lut)
      (multiple-value-bind (r g b) (colormap-rgb cm (/ i (1- n)))
        (setf (aref lut i) (argb-word r g b 1))))))

(defun argb-word (r g b a)
  "A 32-bit premultiplied ARGB word from components in 0..1."
  (flet ((byte8 (v) (max 0 (min 255 (round (* 255 v))))))
    (logior (ash (byte8 a) 24) (ash (byte8 (* a r)) 16)
            (ash (byte8 (* a g)) 8) (byte8 (* a b)))))
