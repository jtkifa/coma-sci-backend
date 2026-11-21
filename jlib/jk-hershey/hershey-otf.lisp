;;;; hershey-otf.lisp — Load and use AVHershey OpenType fonts via FreeType
;;;;
;;;; The OTF fonts in data/OTF/ are the Hershey strokes buffered into
;;;; filled outlines (Light/Medium/Heavy = thin/medium/thick pen).
;;;; Glyphs are read in unscaled font units (1000/em, same coordinate
;;;; system as the SFDBIN fonts), flattened to closed polylines, and
;;;; cached per font so FreeType is only touched once per glyph.
;;;;
;;;; Callbacks receive outline contours, not stroke centerlines: each
;;;; contour is a MOVE followed by LINEs that return to the start point.

(defpackage :hershey-otf
  (:use :cl)
  (:export #:load-otf-font
           #:get-otf-font
           #:list-otf-fonts
           #:load-all-otf-fonts
           #:*otf-directory*
           #:*curve-steps*
           #:otf-font-name
           #:otf-font-family
           #:otf-font-style
           #:otf-font-units-per-em
           #:otf-font-ascent
           #:otf-font-descent
           #:otf-font-glyph-count
           #:otf-glyph-width
           #:otf-glyph-bbox
           #:map-otf-glyph
           #:map-otf-string))

(in-package :hershey-otf)

;;; ============================================================
;;; Configuration
;;; ============================================================

(defvar *otf-directory* nil
  "Directory containing .otf font files.")

(defvar *curve-steps* 8
  "Number of line segments used to flatten each conic/cubic curve segment.
   Takes effect for glyphs not yet cached.")

(defun otf-directory ()
  "Return the OTF directory, defaulting to system data directory."
  (or *otf-directory*
      (let ((system (asdf:find-system :hershey-fonts nil)))
        (when system
          (merge-pathnames #P"data/OTF/" (asdf:system-source-directory system))))))

;;; ============================================================
;;; Font registry
;;; ============================================================

(defvar *otf-registry* (make-hash-table :test 'equal)
  "Hash table mapping font names (lowercase, no AVHershey prefix) to OTF fonts.")

;;; ============================================================
;;; Structures
;;; ============================================================

(defstruct (otf-glyph (:conc-name glyph-))
  (width 0 :type real)
  (minx 0 :type real)
  (miny 0 :type real)
  (maxx 0 :type real)
  (maxy 0 :type real)
  ;; List of closed contours, each a vector #(x0 y0 x1 y1 ...)
  (contours nil :type list))

(defstruct (otf-font (:conc-name otf-font-))
  (name "" :type string)
  (family "" :type string)
  (style "" :type string)
  (units-per-em 1000 :type fixnum)
  (ascent 0 :type fixnum)
  (descent 0 :type fixnum)
  (glyph-count 0 :type fixnum)
  face
  ;; unicode -> OTF-GLYPH, or :MISSING
  (glyphs (make-hash-table) :type hash-table))

(defmethod print-object ((font otf-font) stream)
  (print-unreadable-object (font stream :type t :identity t)
    (format stream "~S" (otf-font-name font))))

;;; ============================================================
;;; Font loading
;;; ============================================================

(defun load-otf-font (pathname)
  "Open an OpenType font file with FreeType and return an OTF-FONT."
  (let* ((face (ft2:new-face pathname))
         (psname (ft2:get-postscript-name face)))
    (make-otf-font
     :name (if (and psname (plusp (length psname)))
               psname
               (pathname-name pathname))
     :family (or (ft2::ft-face-family-name face) "")
     :style (or (ft2::ft-face-style-name face) "")
     :units-per-em (ft2::ft-face-units-per-em face)
     :ascent (ft2::ft-face-ascender face)
     ;; FreeType descender is negative; store positive like hershey-fonts
     :descent (- (ft2::ft-face-descender face))
     :glyph-count (ft2::ft-face-num-glyphs face)
     :face face)))

(defun get-otf-font (name)
  "Get an OTF font by name from the registry (case-insensitive, AVHershey prefix optional)."
  (gethash (hershey-fonts::lookup-key name) *otf-registry*))

(defun list-otf-fonts ()
  "Return a list of all loaded OTF font names (mixed case, no AVHershey prefix)."
  (loop for font being the hash-values of *otf-registry*
        collect (hershey-fonts::strip-avhershey (otf-font-name font))))

(defun load-all-otf-fonts (&optional directory)
  "Load all .otf files from directory into the registry.
   Returns the number of fonts loaded."
  (let ((dir (or directory (otf-directory)))
        (count 0))
    (when dir
      (dolist (path (directory (merge-pathnames "*.otf" dir)))
        (let* ((font (load-otf-font path))
               (key (hershey-fonts::lookup-key (otf-font-name font))))
          (setf (gethash key *otf-registry*) font)
          (incf count))))
    count))

;;; ============================================================
;;; Glyph extraction — FreeType outline to cached polylines
;;; ============================================================

(defun flatten-quadratic (emit x0 y0 cx cy x1 y1 steps)
  (loop for i from 1 to steps
        for u = (/ i steps)
        for v = (- 1 u)
        do (funcall emit
                    (+ (* v v x0) (* 2 u v cx) (* u u x1))
                    (+ (* v v y0) (* 2 u v cy) (* u u y1)))))

(defun flatten-cubic (emit x0 y0 c1x c1y c2x c2y x1 y1 steps)
  (loop for i from 1 to steps
        for u = (/ i steps)
        for v = (- 1 u)
        do (funcall emit
                    (+ (* v v v x0) (* 3 u v v c1x) (* 3 u u v c2x) (* u u u x1))
                    (+ (* v v v y0) (* 3 u v v c1y) (* 3 u u v c2y) (* u u u y1)))))

(defun extract-glyph (font unicode)
  "Load UNICODE from FONT's face and return an OTF-GLYPH, or NIL if absent."
  (let ((face (otf-font-face font))
        (steps *curve-steps*))
    (when (zerop (ft2:get-char-index face unicode))
      (return-from extract-glyph nil))
    (ft2:load-char face unicode '(:no-scale :no-bitmap))
    (let ((width (ft2::ft-vector-x
                  (ft2::ft-glyphslot-advance (ft2::ft-face-glyph face))))
          (contours '())
          (points '())
          (start-x 0) (start-y 0)
          (cur-x 0) (cur-y 0))
      (labels ((emit (x y)
                 (push x points) (push y points)
                 (setf cur-x x cur-y y))
               (close-contour ()
                 (when points
                   ;; FreeType contours are implicitly closed
                   (unless (and (= cur-x start-x) (= cur-y start-y))
                     (emit start-x start-y))
                   (push (coerce (nreverse points) 'simple-vector) contours)
                   (setf points '()))))
        (when (eq :outline (ft2::ft-glyphslot-format (ft2::ft-face-glyph face)))
          (ft2:do-outline-decompose
              (ft2::ft-outlineglyph-outline (ft2:get-glyph face))
              (op p &optional p2 p3)
            ;; Points are stack-allocated by FreeType: read them now
            (let ((x (ft2::ft-vector-x p))
                  (y (ft2::ft-vector-y p)))
              (ecase op
                (:moveto
                 (close-contour)
                 (setf start-x x start-y y)
                 (emit x y))
                (:lineto
                 (emit x y))
                (:conicto
                 (flatten-quadratic #'emit cur-x cur-y
                                    (ft2::ft-vector-x p2) (ft2::ft-vector-y p2)
                                    x y steps))
                (:cubicto
                 (flatten-cubic #'emit cur-x cur-y
                                (ft2::ft-vector-x p2) (ft2::ft-vector-y p2)
                                (ft2::ft-vector-x p3) (ft2::ft-vector-y p3)
                                x y steps)))))
          (close-contour)))
      (setf contours (nreverse contours))
      (let ((minx 0) (miny 0) (maxx 0) (maxy 0) (first t))
        (dolist (c contours)
          (loop for i from 0 below (length c) by 2
                for x = (svref c i)
                for y = (svref c (1+ i))
                do (if first
                       (setf minx x maxx x miny y maxy y first nil)
                       (setf minx (min minx x) maxx (max maxx x)
                             miny (min miny y) maxy (max maxy y)))))
        (make-otf-glyph :width width :minx minx :miny miny
                        :maxx maxx :maxy maxy :contours contours)))))

(defun find-glyph (font unicode)
  "Return cached OTF-GLYPH for UNICODE, extracting it on first use, or NIL."
  (let* ((cache (otf-font-glyphs font))
         (glyph (gethash unicode cache)))
    (cond ((eq glyph :missing) nil)
          (glyph glyph)
          (t (let ((new (extract-glyph font unicode)))
               (setf (gethash unicode cache) (or new :missing))
               new)))))

;;; ============================================================
;;; Glyph access
;;; ============================================================

(defun otf-glyph-width (font unicode)
  "Return glyph advance width in font units, or NIL if glyph not present."
  (let ((g (find-glyph font unicode)))
    (when g (glyph-width g))))

(defun otf-glyph-bbox (font unicode)
  "Return (values width minx miny maxx maxy) or NIL."
  (let ((g (find-glyph font unicode)))
    (when g
      (values (glyph-width g)
              (glyph-minx g) (glyph-miny g)
              (glyph-maxx g) (glyph-maxy g)))))

;;; ============================================================
;;; Glyph drawing
;;; ============================================================

(defun map-otf-glyph (font unicode move-fn line-fn)
  "Walk glyph outline contours, calling MOVE-FN and LINE-FN with (x y)
   in font units. Each contour is closed. Returns width or NIL."
  (declare (type function move-fn line-fn))
  (let ((g (find-glyph font unicode)))
    (when g
      (dolist (c (glyph-contours g))
        (declare (type simple-vector c))
        (funcall move-fn (svref c 0) (svref c 1))
        (loop for i from 2 below (length c) by 2
              do (funcall line-fn (svref c i) (svref c (1+ i)))))
      (glyph-width g))))

(defun map-otf-string (font string move-fn line-fn
                       &key (x 0) (y 0) (scale 1.0) (kerning t))
  "Draw a string, advancing X by each glyph's width (plus kerning from
   the font's kern table when KERNING is true). Returns final X."
  (declare (type string string)
           (type real x y scale))
  (let ((cx (float x))
        (sy (float scale))
        (face (otf-font-face font))
        (prev nil))
    (flet ((offset-move (gx gy)
             (funcall move-fn
                      (+ cx (* gx sy))
                      (+ y (* gy sy))))
           (offset-line (gx gy)
             (funcall line-fn
                      (+ cx (* gx sy))
                      (+ y (* gy sy)))))
      (loop for ch across string
            do (when (and kerning prev)
                 (incf cx (* sy (ft2:get-kerning face prev ch :unscaled))))
               (let ((width (map-otf-glyph font (char-code ch)
                                           #'offset-move #'offset-line)))
                 (when width (incf cx (* width sy))))
               (setf prev ch)))
    cx))
