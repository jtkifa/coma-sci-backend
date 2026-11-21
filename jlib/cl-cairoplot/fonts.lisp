;;;; fonts.lisp — fonts and positioned text
;;;;
;;;; A FONT is an immutable description of a typeface and size, in two
;;;; kinds drawn the same way:
;;;;   SYSTEM-FONT   a font the OS knows by family name ("Helvetica")
;;;;   HERSHEY-FONT  a Hershey font from HERSHEY-FONTS ("rowmans"), which
;;;;                 gives plots PGPLOT's look
;;;; Change one with COPY-FONT.  A font spec is a FONT, a keyword naming
;;;; an entry in *NAMED-FONTS*, or a string naming a system font family;
;;;; PARSE-FONT turns any of these into a FONT.
;;;;
;;;; A new canvas starts with *DEFAULT-FONT* (defined in cairo-graphics.lisp).
;;;; The current font is part of the canvas drawing state, so WITH-FONT
;;;; scopes it like WITH-COLOR.  PUT-TEXT draws text aligned and rotated
;;;; about a point, as PGPLOT's PGPTXT does.  Text may be a list of runs
;;;; with their own scale and rise (superscripts), and characters a
;;;; font lacks fall back to *FALLBACK-FONTS*.

(in-package :cl-cairoplot)

;;; ============================================================
;;; Font classes
;;; ============================================================

(defclass font ()
  ((size :initarg :size :initform 12 :reader font-size
         :documentation "Em size in canvas units."))
  (:documentation "Abstract base class of fonts."))

(defclass system-font (font)
  ((family :initarg :family :reader system-font-family)
   (slant  :initarg :slant  :initform :normal :reader system-font-slant)
   (weight :initarg :weight :initform :normal :reader system-font-weight))
  (:documentation "A font of the operating system, drawn by Cairo (see
   MAKE-SYSTEM-FONT)."))

(defvar *hershey-space-width* 0.32
  "Default word space of Hershey fonts, as a fraction of the em.  The
   fonts' own space is 0.43 em, which makes text look gappy (Helvetica's
   is 0.28 em); NIL keeps the font's own.")

(defclass hershey-font (font)
  ((name      :initarg :name :reader hershey-font-name)
   (pen-width :initarg :pen-width :initform nil :reader hershey-font-pen-width
              :documentation "Stroke width as a fraction of the em, or NIL
                              for the cl-cairo-hershey-fonts default.")
   (space-width :initarg :space-width :initform nil
                :reader hershey-font-space-width
                :documentation "Word space as a fraction of the em, or NIL
                                for *HERSHEY-SPACE-WIDTH*."))
  (:documentation "A Hershey stroke font, drawn as lines (see MAKE-HERSHEY-FONT)."))

(defun make-system-font (family &key (size 12) (slant :normal) (weight :normal))
  "SLANT is :NORMAL, :ITALIC or :OBLIQUE; WEIGHT is :NORMAL or :BOLD."
  (make-instance 'system-font :family family :size size
                              :slant slant :weight weight))

(defun make-hershey-font (name &key (size 12) pen-width space-width)
  "A Hershey (stroke) font NAME, such as \"futural\", \"timesr\" or
   \"greek\", at SIZE; PEN-WIDTH is the stroke width as a fraction of the
   size, SPACE-WIDTH the width of a space."
  (make-instance 'hershey-font :name name :size size :pen-width pen-width
                               :space-width space-width))

(defmethod print-object ((font system-font) stream)
  (print-unreadable-object (font stream :type t)
    (format stream "~S ~A~@[ ~(~A~)~]~@[ ~(~A~)~]"
            (system-font-family font) (font-size font)
            (unless (eq (system-font-slant font) :normal) (system-font-slant font))
            (unless (eq (system-font-weight font) :normal) (system-font-weight font)))))

(defmethod print-object ((font hershey-font) stream)
  (print-unreadable-object (font stream :type t)
    (format stream "~S ~A~@[ pen ~A~]" (hershey-font-name font) (font-size font)
            (hershey-font-pen-width font))))

(defgeneric copy-font (font &key &allow-other-keys)
  (:documentation "A copy of FONT with the given slots changed, e.g.
   (COPY-FONT FONT :SIZE 20)."))

(defmethod copy-font ((font system-font)
                      &key (size (font-size font))
                           (family (system-font-family font))
                           (slant (system-font-slant font))
                           (weight (system-font-weight font)))
  (make-system-font family :size size :slant slant :weight weight))

(defmethod copy-font ((font hershey-font)
                      &key (size (font-size font))
                           (name (hershey-font-name font))
                           (pen-width (hershey-font-pen-width font))
                           (space-width (hershey-font-space-width font)))
  (make-hershey-font name :size size :pen-width pen-width :space-width space-width))

;;; ============================================================
;;; Named fonts and font specs
;;; ============================================================

(defvar *named-fonts* (make-hash-table :test 'eq)
  "Keyword -> FONT.")

(defun define-named-font (name font)
  "Name a font: the keyword NAME then designates FONT, e.g. in WITH-FONT."
  (setf (gethash name *named-fonts*) font))

;; PGPLOT's four fonts, plus system defaults
(define-named-font :normal (make-hershey-font "rowmans"))
(define-named-font :roman  (make-hershey-font "timesr"))
(define-named-font :italic (make-hershey-font "timesi"))
(define-named-font :script (make-hershey-font "scriptc"))
;; planet, zodiac and node symbols (see markers.lisp)
(define-named-font :astrology (make-hershey-font "astrology"))
(define-named-font :sans   (make-system-font "Helvetica"))
(define-named-font :serif  (make-system-font "Times"))
(define-named-font :mono   (make-system-font "Courier"))

(defun parse-font (spec)
  "Return the FONT designated by SPEC."
  (etypecase spec
    (font spec)
    (keyword (or (gethash spec *named-fonts*)
                 (error "Unknown font name ~S." spec)))
    (string (make-system-font spec))))

;;; ============================================================
;;; Using fonts on a canvas
;;; ============================================================

(defgeneric apply-font (canvas font)
  (:documentation "Make FONT the current font of CANVAS."))

(defmethod apply-font (canvas (font system-font))
  (set-system-font canvas (system-font-family font)
                   :slant (system-font-slant font)
                   :weight (system-font-weight font))
  (set-font-size canvas (font-size font)))

(defmethod apply-font (canvas (font hershey-font))
  (if (hershey-font-pen-width font)
      (set-hershey-font canvas (hershey-font-name font)
                        :pen-width (hershey-font-pen-width font))
      (set-hershey-font canvas (hershey-font-name font)))
  (set-font-size canvas (font-size font)))

(defmethod apply-font :after (canvas (font font))
  (setf (canvas-font canvas) font))

(defun current-font (canvas)
  "The font CANVAS draws text in."
  (or (canvas-font canvas) (parse-font *default-font*)))

(defun set-font (canvas spec &key size)
  "Make the font designated by SPEC current, at SIZE if given."
  (let ((font (parse-font spec)))
    (apply-font canvas (if size (copy-font font :size size) font))))

(defmacro with-font ((canvas spec &key size) &body body)
  "Run BODY with the font designated by SPEC (at SIZE if given),
   restoring the previous font afterward."
  (let ((c (gensym "CANVAS")))
    `(let ((,c (as-canvas ,canvas)))
       (with-saved-state (,c)
         (set-font ,c ,spec :size ,size)
         ,@body))))

;;; ============================================================
;;; Halos
;;; ============================================================
;;;
;;; A HALO is a contrasting outline around text, markers or lines, so
;;; they stand out over an image.  It has
;;;   COLOR    a color spec;
;;;   WIDTH    how far it reaches beyond the ink, in canvas units, or NIL
;;;            for a default proportional to the text, marker or line;
;;;   OPACITY  how strongly it covers what is underneath next to the ink;
;;;   LAYERS   its profile: a list of (WIDTH-FRACTION OPACITY-FRACTION),
;;;            outermost first.  A soft halo is several layers fading out
;;;            from the ink like a glow; a crisp one is a single layer.
;;; Make one with MAKE-HALO, vary it with COPY-HALO, or name it with
;;; DEFINE-NAMED-HALO.  A halo spec is a HALO, NIL (none), T (the
;;; :DEFAULT halo), a keyword naming a halo in *NAMED-HALOS* (else a
;;; color), a color spec, or a plist of MAKE-HALO arguments; PARSE-HALO
;;; turns any of these into a HALO.  :HALO keywords default to *HALO*,
;;; which WITH-HALO binds.
;;;
;;; How it is drawn: the ink's outline (a stroked path, or the outline
;;; of the glyphs for text) is stroked BASE + 2 WIDTH wide in the halo
;;; color, so WIDTH shows beyond the ink, and the ink is drawn on top.
;;; Each layer's halos are drawn opaque into a group that is blended in
;;; at the layer's opacity, so overlapping halos are not darker; all
;;; halos come before the ink, so they never cover neighboring ink.

(defstruct (halo (:constructor %make-halo (color width opacity layers))
                 (:copier nil))
  "An outline around text, markers and lines (see MAKE-HALO and
   WITH-HALO)."
  color width opacity layers)

(setf (documentation 'halo-p 'function) "True if OBJECT is a HALO.")

(defmethod print-object ((halo halo) stream)
  (print-unreadable-object (halo stream :type t)
    (format stream "~S~@[ width ~A~] opacity ~A, ~D layer~:P"
            (halo-color halo) (halo-width halo) (halo-opacity halo)
            (length (halo-layers halo)))))

(defparameter +soft-halo-layers+ '((1 0.45) (0.7 0.6) (0.38 1))
  "The profile of a soft halo: wide and faint outside, stronger inside.")

(defun make-halo (&key (color :white) width (opacity 0.4) (soft t) layers)
  "A halo of COLOR reaching WIDTH beyond the ink (NIL: by size), OPACITY
   next to it.  SOFT gives the three-layer fading profile, NIL a single
   crisp layer; LAYERS gives any profile instead (see the top of this
   section)."
  (%make-halo color width opacity
              (or layers (if soft +soft-halo-layers+ '((1 1))))))

(defun copy-halo (halo &key (color (halo-color halo)) (width (halo-width halo))
                            (opacity (halo-opacity halo)) (soft nil soft-p)
                            (layers (halo-layers halo)))
  "A copy of HALO with the given parts changed."
  (%make-halo color width opacity
              (if soft-p (if soft +soft-halo-layers+ '((1 1))) layers)))

(defvar *named-halos* (make-hash-table :test 'eq)
  "Keyword -> HALO.")

(defun define-named-halo (name halo)
  "Name a halo: the keyword NAME then designates HALO, e.g. in WITH-HALO."
  (setf (gethash name *named-halos*) halo))

(define-named-halo :default (make-halo))
(define-named-halo :soft (make-halo))
(define-named-halo :crisp (make-halo :soft nil :opacity 0.8))
(define-named-halo :dark (make-halo :color :black))
(define-named-halo :dark-crisp (make-halo :color :black :soft nil :opacity 0.8))
;; wider and smoother: five layers reaching 1.6 times the default width
(define-named-halo :glow (make-halo :opacity 0.5
                                    :layers '((1.6 0.12) (1.25 0.2) (0.9 0.3)
                                              (0.6 0.45) (0.3 1))))

(defvar *halo* nil
  "The default halo spec for text, markers and contours (NIL: none).")

(defun parse-halo (spec)
  "The HALO designated by SPEC, or NIL (see the top of this section)."
  (cond ((null spec) nil)
        ((halo-p spec) spec)
        ((eq spec t) (gethash :default *named-halos*))
        ((keywordp spec) (or (gethash spec *named-halos*) (make-halo :color spec)))
        ((and (consp spec) (member (first spec) '(:color :width :opacity :soft :layers)))
         (apply #'make-halo spec))
        (t (make-halo :color spec))))

(defmacro with-halo ((&rest spec) &body body)
  "Run BODY with halos around text, markers and contours.  SPEC is
   empty (the :DEFAULT halo), MAKE-HALO keywords, as
   (WITH-HALO (:COLOR :BLACK :OPACITY 0.6) ...), or one halo spec
   form, as (WITH-HALO (:DARK) ...) or (WITH-HALO (*MY-HALO*) ...)."
  `(let ((*halo* ,(cond ((null spec) '(parse-halo t))
                        ((and (keywordp (first spec)) (evenp (length spec)))
                         `(make-halo ,@spec))
                        (t `(parse-halo ,(first spec))))))
     ,@body))

(defun halo-layer-list (halo default-width)
  "HALO's layers as a list of (WIDTH OPACITY) in canvas units, using
   DEFAULT-WIDTH if the halo has none."
  (let ((width (or (halo-width halo) default-width)))
    (loop for (wf of) in (halo-layers halo)
          collect (list (* wf width) (* of (halo-opacity halo))))))

(defmacro do-halo-layers ((canvas spec default-width color width) &body body)
  "If halo SPEC is not NIL, run BODY once per layer with COLOR and WIDTH
   bound, drawing opaque halos that are blended in at the layer's
   opacity.  BODY should stroke with STROKE-HALO."
  (let ((c (gensym "CANVAS")) (h (gensym "HALO")) (layer-o (gensym "LAYER-OPACITY")))
    `(let ((,c ,canvas) (,h (parse-halo ,spec)))
       (when ,h
         (let ((,color (halo-color ,h)))
           (loop for (,width ,layer-o) in (halo-layer-list ,h ,default-width)
                 do (with-opacity-group (,c ,layer-o) ,@body)))))))

(defun stroke-halo (canvas color width &optional (base-width 0))
  "Stroke (and consume) the current path in COLOR, BASE-WIDTH + 2 WIDTH
   wide with round joins and caps: a halo WIDTH beyond a path of
   BASE-WIDTH, or beyond the edge of a filled area if BASE-WIDTH is 0."
  (with-saved-state (canvas)
    (set-color canvas color)
    (set-line-width canvas (+ base-width (* 2 width)))
    (set-line-join canvas :round)
    (set-line-cap canvas :round)
    (stroke canvas)))

;;; ============================================================
;;; Rich text and font fallback
;;; ============================================================
;;;
;;; Text is a string, or a list of runs, each a string or
;;;   (STRING &key SCALE RISE FONT)
;;; SCALE multiplies the current font size; RISE raises the run by that
;;; many em of the current font (negative lowers it); FONT is a font
;;; spec for the run, drawn at the scaled current size.  For example
;;;   ("10" ("-3" :scale 0.6 :rise 0.5))
;;; is ten to the minus three.
;;;
;;; A character the run's font lacks (Hershey fonts each cover only some
;;; of Unicode) is drawn in the first of *FALLBACK-FONTS* that has it.

(defvar *fallback-fonts* (list (make-hershey-font "greek")
                               (make-hershey-font "astrology")
                               :sans)
  "Font specs tried, in order, for characters missing from a font.
   Hershey \"greek\" has Greek letters and symbols such as ×;
   \"astrology\" has the sun, moon, planet, node and zodiac symbols;
   the system :SANS font covers the rest, e.g. Å and accented letters.")

(defun glyph-present-p (canvas char)
  "True if the current font has a glyph for CHAR (whitespace always
   counts)."
  (or (member char '(#\Space #\Tab #\No-break_space))
      (/= 0 (char-glyph-index canvas char))))

(defun split-by-fallback (canvas string font)
  "Split STRING into a list of (SUBSTRING . FONT), using FONT where it has
   the characters and fallback fonts (at FONT's size) elsewhere."
  (apply-font canvas font)
  (let ((fonts (map 'vector (lambda (ch) (if (glyph-present-p canvas ch) font nil))
                    string)))
    (when (position nil fonts)
      (dolist (spec *fallback-fonts*)
        (let ((fallback (copy-font (parse-font spec) :size (font-size font))))
          (apply-font canvas fallback)
          (loop for i below (length string)
                when (and (null (aref fonts i))
                          (glyph-present-p canvas (char string i)))
                  do (setf (aref fonts i) fallback))))
      (setf fonts (substitute font nil fonts)))
    (loop with start = 0
          for i from 1 to (length string)
          when (or (= i (length string))
                   (not (eq (aref fonts i) (aref fonts start))))
            collect (cons (subseq string start i) (aref fonts start))
            and do (setf start i))))

(defun layout-text (canvas text)
  "Lay out TEXT in the current font.  Return a list of segments
   (X STRING FONT RISE), in canvas units from the start of the
   baseline, and the total width."
  (let* ((base (current-font canvas))
         (x 0)
         (segments '()))
    (with-saved-state (canvas)
      (dolist (run (if (stringp text) (list text) text))
        (destructuring-bind (string &key (scale 1) (rise 0) font)
            (if (stringp run) (list run) run)
          (let ((run-font (copy-font (if font (parse-font font) base)
                                     :size (* scale (font-size base)))))
            (loop for (piece . piece-font) in (split-by-fallback canvas string run-font)
                  for space = (and (typep piece-font 'hershey-font)
                                   (or (hershey-font-space-width piece-font)
                                       *hershey-space-width*))
                  do (apply-font canvas piece-font)
                     (if space
                         ;; Hershey: words as segments, spaces only as advance
                         (loop with start = 0
                               for end = (or (position #\Space piece :start start)
                                             (length piece))
                               do (when (< start end)
                                    (let ((word (subseq piece start end)))
                                      (push (list x word piece-font (* rise (font-size base)))
                                            segments)
                                      (incf x (nth-value 4 (text-extents canvas word)))))
                                  (when (= end (length piece)) (return))
                                  (incf x (* space (font-size piece-font)))
                                  (setf start (1+ end)))
                         (progn
                           (push (list x piece piece-font (* rise (font-size base)))
                                 segments)
                           (incf x (nth-value 4 (text-extents canvas piece))))))))))
    (values (nreverse segments) x)))

;;; ============================================================
;;; Measuring and placing text
;;; ============================================================

(defun text-size (canvas text)
  "Return values WIDTH ASCENT DESCENT of TEXT (a string or runs) in the
   current font.  WIDTH is the advance; ASCENT and DESCENT are the
   font's, so they do not vary with the characters in TEXT."
  (let ((width (nth-value 1 (layout-text canvas text))))
    (multiple-value-bind (ascent descent) (font-extents canvas)
      (values width ascent descent))))

(defun halign-fraction (halign)
  (case halign
    (:left 0)
    (:center 1/2)
    (:right 1)
    (t (if (realp halign)
           halign
           (error "HALIGN must be :LEFT, :CENTER, :RIGHT or a number, not ~S."
                  halign)))))

(defun put-text (canvas x y text &key (halign :left) (valign :baseline)
                                      (angle 0) font size (halo *halo*))
  "Draw TEXT (a string or runs) anchored at (X,Y), rotated by ANGLE
   radians about it.  HALIGN is :LEFT, :CENTER, :RIGHT, or the fraction
   of the width left of the anchor.  VALIGN is :BASELINE, :BOTTOM
   (descent), :CENTER or :TOP (ascent).  FONT and SIZE, if given, apply
   to this text only.  HALO outlines the text (see WITH-HALO; default
   width 0.08 em).  Returns the text's width."
  (with-saved-state (canvas)
    (cond ((or font size)
           (set-font canvas (or font (current-font canvas)) :size size))
          ((null (canvas-font canvas))
           (set-font canvas *default-font*)))
    (multiple-value-bind (segments width) (layout-text canvas text)
      (multiple-value-bind (ascent descent) (font-extents canvas)
        (translate canvas x y)
        (unless (zerop angle)
          (rotate canvas angle))
        ;; y increases downward, so moving the baseline down is positive
        (let ((x0 (- (* (halign-fraction halign) width)))
              (y0 (ecase valign
                    (:baseline 0)
                    (:bottom (- descent))
                    (:center (/ (- ascent descent) 2))
                    (:top ascent))))
          ;; halos of all the glyphs first, then the glyphs over them
          (do-halo-layers (canvas halo (* 0.08 (font-size (current-font canvas)))
                                  halo-color halo-width)
            (loop for (sx string font rise) in segments
                  do (apply-font canvas font)
                     (new-path canvas)
                     (move-to canvas (+ x0 sx) (- y0 rise))
                     (text-path canvas string)
                     (stroke-halo canvas halo-color halo-width)))
          (loop for (sx string font rise) in segments
                do (apply-font canvas font)
                   (draw-text canvas (+ x0 sx) (- y0 rise) string))
          width)))))
