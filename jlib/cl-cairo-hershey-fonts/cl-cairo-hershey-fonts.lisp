;;;; cl-cairo-hershey-fonts.lisp — Hershey fonts as cl-cairo2 user fonts
;;;;
;;;; Wraps any font in the HERSHEY-FONTS registry as a Cairo user font,
;;;; so ordinary cl-cairo2 text calls draw with it:
;;;;
;;;;   (cl-cairo-hershey-fonts:use-font "rowmans" :size 40)
;;;;   (cl-cairo2:move-to 10 50)
;;;;   (cl-cairo2:show-text "Hello")
;;;;
;;;; Stroke fonts (the JHF fonts) are drawn by stroking their centerlines
;;;; with a round pen whose width is a fraction of the em, so it scales
;;;; with the text; outline fonts (AVHershey) are filled.  Cairo caches
;;;; each rendered glyph per size/transform, so Lisp only runs on a miss.
;;;;
;;;; Thread safety: cl-cairo2's USER-FONT-FACE passes callback arguments
;;;; through global objects, which races when two threads render glyphs
;;;; at once.  This file installs its own callbacks instead: each uses
;;;; only the cairo_t and extents pointers Cairo hands it, and finds its
;;;; font through a simple-vector that is replaced, never mutated, so
;;;; reads need no lock.  Threads drawing on separate contexts and
;;;; surfaces do not interfere.

(defpackage :cl-cairo-hershey-fonts
  (:use :cl)
  (:nicknames :cairo-hershey)
  (:export #:font-face
           #:use-font
           #:*default-pen-width*))

(in-package :cl-cairo-hershey-fonts)

;;; ============================================================
;;; Configuration and state
;;; ============================================================

(defvar *default-pen-width* 0.056
  "Pen width for stroke fonts, as a fraction of the em (0.056 matches the
   AVHershey Medium weight).")

(defstruct (face-record (:conc-name record-))
  (font nil)                            ; HERSHEY-FONTS font
  (stroke-p nil)
  (pen 0d0 :type double-float)          ; pen width in font units
  (face nil))                           ; cl-cairo2 FONT-FACE

(defvar *records* (vector)
  "Face records indexed by id.  Replaced wholesale under *LOCK*; callbacks
   read it without locking.")

(defvar *faces* (make-hash-table :test 'equal)
  "(font-name . pen) -> face record.  Accessed under *LOCK*.")

(defvar *lock* (bt:make-lock "cl-cairo-hershey-fonts"))

(defvar *user-data-key* (cffi:foreign-alloc :char)
  "Address used as Cairo's user-data key for a face's record id.")

(declaim (inline f2d))
(defun f2d (x) (coerce x 'double-float))

;;; ============================================================
;;; Callbacks
;;; ============================================================

(defun scaled-font-record (scaled-font)
  (let ((id (cffi:pointer-address
             (cl-cairo2::cairo_font_face_get_user_data
              (cl-cairo2::cairo_scaled_font_get_font_face scaled-font)
              *user-data-key*))))
    (svref *records* id)))

(defmacro with-callback-status (&body body)
  "Run BODY, returning a cairo status; never unwind through C."
  `(handler-case (progn ,@body :cairo_status_success)
     (error () :cairo_status_user_font_error)))

(cffi:defcallback hershey-init cl-cairo2::cairo_status_t
    ((scaled-font :pointer) (cr :pointer) (extents :pointer))
  (declare (ignore cr))
  (with-callback-status
    (let* ((font (record-font (scaled-font-record scaled-font)))
           (em (f2d hershey-fonts:+units-per-em+))
           (ascent (/ (hershey-fonts:font-ascent font) em))
           (descent (/ (hershey-fonts:font-descent font) em)))
      (cffi:with-foreign-slots ((cl-cairo2::ascent cl-cairo2::descent cl-cairo2::height
                                 cl-cairo2::max_x_advance cl-cairo2::max_y_advance)
                                extents (:struct cl-cairo2::cairo_font_extents_t))
        (setf cl-cairo2::ascent ascent
              cl-cairo2::descent descent
              cl-cairo2::height (+ ascent descent)
              cl-cairo2::max_x_advance 1d0
              cl-cairo2::max_y_advance 0d0)))))

(cffi:defcallback hershey-unicode-to-glyph cl-cairo2::cairo_status_t
    ((scaled-font :pointer) (unicode :unsigned-long) (glyph-index :pointer))
  (with-callback-status
    (let ((font (record-font (scaled-font-record scaled-font))))
      ;; Glyph indices are code points; 0 (absent) renders as nothing
      (setf (cffi:mem-ref glyph-index :unsigned-long)
            (if (and (< unicode (ash 1 28))
                     (hershey-fonts:glyph-width font unicode))
                unicode
                0)))))

(cffi:defcallback hershey-render-glyph cl-cairo2::cairo_status_t
    ((scaled-font :pointer) (glyph :unsigned-long) (cr :pointer) (extents :pointer))
  (with-callback-status
    (let* ((record (scaled-font-record scaled-font))
           (font (record-font record))
           (em (f2d hershey-fonts:+units-per-em+))
           (width (and (plusp glyph) (< glyph (ash 1 28))
                       (hershey-fonts:glyph-width font glyph))))
      (when width
        ;; Glyph space is 1 unit = 1 em with Y down; fonts are Y up
        (cl-cairo2::cairo_scale cr (/ em) (- (/ em)))
        (cl-cairo2::cairo_new_path cr)
        (hershey-fonts:map-glyph
         font glyph
         (lambda (x y) (cl-cairo2::cairo_move_to cr (f2d x) (f2d y)))
         (lambda (x y) (cl-cairo2::cairo_line_to cr (f2d x) (f2d y))))
        (cond ((record-stroke-p record)
               (cl-cairo2::cairo_set_line_width cr (record-pen record))
               (cl-cairo2::cairo_set_line_cap cr :cairo_line_cap_round)
               (cl-cairo2::cairo_set_line_join cr :cairo_line_join_round)
               (cl-cairo2::cairo_stroke cr))
              (t (cl-cairo2::cairo_fill cr))))
      ;; Only the advance is read; Cairo computes ink extents itself
      (cffi:with-foreign-slots ((cl-cairo2::x_advance cl-cairo2::y_advance)
                                extents (:struct cl-cairo2::cairo_text_extents_t))
        (setf cl-cairo2::x_advance (if width (/ width em) 0d0)
              cl-cairo2::y_advance 0d0)))))

;;; ============================================================
;;; Faces
;;; ============================================================

(defun make-cairo-face (id)
  (let ((ptr (cl-cairo2::cairo_user_font_face_create)))
    (cl-cairo2::cairo_user_font_face_set_init_func
     ptr (cffi:callback hershey-init))
    (cl-cairo2::cairo_user_font_face_set_unicode_to_glyph_func
     ptr (cffi:callback hershey-unicode-to-glyph))
    (cl-cairo2::cairo_user_font_face_set_render_glyph_func
     ptr (cffi:callback hershey-render-glyph))
    (cl-cairo2::cairo_font_face_set_user_data
     ptr *user-data-key* (cffi:make-pointer id) (cffi:null-pointer))
    (make-instance 'cl-cairo2::font-face :pointer ptr)))

(defun font-face (name &key (pen-width *default-pen-width*))
  "Cached cl-cairo2 font face for the HERSHEY-FONTS font NAME (e.g.
   \"rowmans\", \"SimplexMedium\").  PEN-WIDTH, a fraction of the em,
   applies to stroke fonts; each distinct width is a separate face.
   Loads the font registry on first use."
  (bt:with-lock-held (*lock*)
    (when (null (hershey-fonts:list-fonts))
      (hershey-fonts:load-all-fonts))
    (let* ((font (or (hershey-fonts:get-font name)
                     (error "No Hershey font named ~S." name)))
           (stroke-p (eq (hershey-fonts:font-kind font) :stroke))
           (pen (if stroke-p (f2d (* pen-width hershey-fonts:+units-per-em+)) 0d0))
           (key (cons (hershey-fonts:font-name font) pen)))
      (record-face
       (or (gethash key *faces*)
           (let* ((id (length *records*))
                  (record (make-face-record :font font :stroke-p stroke-p :pen pen)))
             ;; Publish the record before Cairo can call back with ID
             (setf *records* (concatenate 'simple-vector *records* (list record)))
             (setf (record-face record) (make-cairo-face id))
             (setf (gethash key *faces*) record)))))))

(defun use-font (name &key size (pen-width *default-pen-width*)
                           (context cl-cairo2:*context*))
  "Make NAME the current font of CONTEXT, optionally setting SIZE."
  (cl-cairo2:set-font-face (font-face name :pen-width pen-width) context)
  (when size
    (cl-cairo2:set-font-size size context))
  context)
