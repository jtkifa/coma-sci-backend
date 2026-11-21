;;;; hershey-fonts.lisp — Load and use Hershey vector fonts
;;;;
;;;; Binary format (all integers little-endian):
;;;;
;;;;   HEADER (128 bytes):
;;;;     0-3:    Magic "HSHY"
;;;;     4-5:    Version (uint16) = 3
;;;;     6-7:    Flags (uint16): bit 0 = stroke font (centerlines, to be
;;;;             stroked); clear = outline font (closed contours, to be filled)
;;;;     8-11:   Glyph count (uint32)
;;;;     12-15:  Reserved
;;;;     16-17:  Ascent (int16)
;;;;     18-19:  Descent (int16)
;;;;     20-63:  Reserved
;;;;     64-127: Font name (null-terminated ASCII)
;;;;
;;;;   INDEX TABLE: glyph-count × 8 bytes (sorted by unicode)
;;;;     Unicode: uint32
;;;;     Offset:  uint32
;;;;
;;;;   DATA SECTION: glyph records
;;;;     Per glyph: width, bbox, commands

(defpackage :hershey-fonts
  (:use :cl)
  (:export #:load-font
           #:get-font
           #:list-fonts
           #:load-all-fonts
           #:font-name
           #:font-ascent
           #:font-descent
           #:font-glyph-count
           #:font-codes
           #:font-kind
           #:+units-per-em+
           #:glyph-width
           #:glyph-bbox
           #:map-glyph
           #:map-string))

(in-package :hershey-fonts)

;;; ============================================================
;;; Constants
;;; ============================================================

(defconstant +magic+ #x59485348)
(defconstant +version+ 3)
(defconstant +header-size+ 128)
(defconstant +glyph-header-size+ 12)
(defconstant +units-per-em+ 1000
  "Font units per em for all fonts in this package.")
(defconstant +flag-stroke+ 1)
(defconstant +move+ 1)
(defconstant +line+ 2)
(defconstant +curve+ 3)
(defconstant +end+ 0)

;;; ============================================================
;;; Font registry
;;; ============================================================

(defvar *font-registry* (make-hash-table :test 'equal)
  "Hash table mapping font names (lowercase strings) to loaded fonts.")

(defun strip-avhershey (name)
  "Remove AVHershey prefix if present, preserving case."
  (if (and (>= (length name) 9)
           (string-equal "AVHershey" name :end2 9))
      (subseq name 9)
      name))

(defun lookup-key (name)
  "Return lowercase lookup key for font name."
  (string-downcase (strip-avhershey name)))

(defvar *font-directory* nil
  "Directory containing binary font files.")

(defun font-directory ()
  "Return the font directory, defaulting to system data directory."
  (or *font-directory*
      (let ((system (asdf:find-system :hershey-fonts nil)))
        (when system
          (merge-pathnames #P"data/SFDBIN/" (asdf:system-source-directory system))))))

;;; ============================================================
;;; Font structure
;;; ============================================================

(defstruct (font (:conc-name font-))
  (name "" :type string)
  (ascent 0 :type (signed-byte 16))
  (descent 0 :type (signed-byte 16))
  (glyph-count 0 :type (unsigned-byte 28))
  (flags 0 :type (unsigned-byte 16))
  (data #() :type (simple-array (unsigned-byte 8) (*)))
  (index-offset 0 :type (unsigned-byte 28))
  (data-offset 0 :type (unsigned-byte 28)))

;;; ============================================================
;;; Binary reader helpers
;;; ============================================================

(declaim (inline read-u16-le read-s16-le read-u32-le))

(defun read-u16-le (data offset)
  (+ (aref data offset)
     (ash (aref data (1+ offset)) 8)))

(defun read-s16-le (data offset)
  (let ((val (read-u16-le data offset)))
    (if (>= val 32768) (- val 65536) val)))

(defun read-u32-le (data offset)
  (+ (aref data offset)
     (ash (aref data (+ offset 1)) 8)
     (ash (aref data (+ offset 2)) 16)
     (ash (aref data (+ offset 3)) 24)))

(defun read-cstring (data offset max-len)
  (let ((end offset))
    (loop while (and (< end (+ offset max-len))
                     (> (aref data end) 0))
          do (incf end))
    (let ((str (make-string (- end offset))))
      (loop for i from offset below end
            for j from 0
            do (setf (char str j) (code-char (aref data i))))
      str)))

;;; ============================================================
;;; Font loading
;;; ============================================================

(defun load-font (pathname)
  "Load a binary font file."
  (with-open-file (in pathname :direction :input
                      :element-type '(unsigned-byte 8))
    (let* ((size (file-length in))
           (data (make-array size :element-type '(unsigned-byte 8))))
      (read-sequence data in)
      (unless (= (read-u32-le data 0) +magic+)
        (error "Invalid font file: bad magic"))
      (let ((ver (read-u16-le data 4)))
        (unless (= ver +version+)
          (error "Unsupported font version ~D (expected ~D)" ver +version+)))
      (let* ((glyph-count (read-u32-le data 8))
             (index-offset +header-size+)
             (data-offset (+ index-offset (* 8 glyph-count))))
        (make-font
         :name (read-cstring data 64 64)
         :ascent (read-s16-le data 16)
         :descent (read-s16-le data 18)
         :glyph-count glyph-count
         :flags (read-u16-le data 6)
         :data data
         :index-offset index-offset
         :data-offset data-offset)))))

(defun get-font (name)
  "Get a font by name from the registry (case-insensitive, AVHershey prefix optional)."
  (gethash (lookup-key name) *font-registry*))

(defun list-fonts ()
  "Return a list of all loaded font names (mixed case, no AVHershey prefix)."
  (loop for font being the hash-values of *font-registry*
        collect (strip-avhershey (font-name font))))

(defun load-all-fonts (&optional directory)
  "Load all .sfdbin files from directory into the registry.
   Returns the number of fonts loaded."
  (let ((dir (or directory (font-directory)))
        (count 0))
    (when dir
      (dolist (path (directory (merge-pathnames "*.sfdbin" dir)))
        (let* ((font (load-font path))
               (key (lookup-key (font-name font))))
          (setf (gethash key *font-registry*) font)
          (incf count))))
    count))

(defun font-kind (font)
  "Return :STROKE for single-stroke fonts (draw paths with a pen) or
   :OUTLINE for outline fonts (fill closed contours)."
  (if (logtest +flag-stroke+ (font-flags font)) :stroke :outline))

(defun font-codes (font)
  "Return a list of the Unicode code points present in FONT, ascending."
  (let ((data (font-data font))
        (base (font-index-offset font)))
    (loop for i below (font-glyph-count font)
          collect (read-u32-le data (+ base (* 8 i))))))

;;; ============================================================
;;; Glyph access — binary search, no consing
;;; ============================================================

(declaim (inline glyph-data-offset))
(defun glyph-data-offset (font unicode)
  "Binary search for unicode in index, return absolute data offset or NIL."
  (declare (type (unsigned-byte 28) unicode)
           (optimize (speed 3) (safety 1)))
  (let* ((data (font-data font))
         (count (font-glyph-count font))
         (index-base (font-index-offset font))
         (data-base (font-data-offset font))
         (lo 0)
         (hi (1- count)))
    ;; HI goes to -1 when UNICODE is below the first entry
    (declare (type (unsigned-byte 28) count index-base data-base)
             (type fixnum lo hi))
    (loop while (<= lo hi) do
      (let* ((mid (ash (+ lo hi) -1))
             (entry-pos (+ index-base (* 8 mid)))
             (entry-unicode (read-u32-le data entry-pos)))
        (declare (type (unsigned-byte 28) mid entry-pos entry-unicode))
        (cond ((= entry-unicode unicode)
               (return-from glyph-data-offset
                 (+ data-base (read-u32-le data (+ entry-pos 4)))))
              ((< entry-unicode unicode)
               (setf lo (1+ mid)))
              (t
               (setf hi (1- mid))))))
    nil))

(declaim (inline glyph-width glyph-bbox))

(defun glyph-width (font unicode)
  "Return glyph width, or NIL if glyph not present."
  (declare (type (unsigned-byte 28) unicode))
  (let ((off (glyph-data-offset font unicode)))
    (when off
      (read-s16-le (font-data font) off))))

(defun glyph-bbox (font unicode)
  "Return (values width minx miny maxx maxy) or NIL."
  (declare (type (unsigned-byte 28) unicode))
  (let ((off (glyph-data-offset font unicode)))
    (when off
      (let ((data (font-data font)))
        (values (read-s16-le data off)
                (read-s16-le data (+ off 2))
                (read-s16-le data (+ off 4))
                (read-s16-le data (+ off 6))
                (read-s16-le data (+ off 8)))))))

;;; ============================================================
;;; Glyph drawing
;;; ============================================================

(declaim (inline map-glyph))
(defun map-glyph (font unicode move-fn line-fn)
  "Walk glyph commands in-place, calling MOVE-FN and LINE-FN with (x y).
   Any curves are linearized to their endpoint. Returns width or NIL."
  (declare (type function move-fn line-fn)
           (optimize (speed 3) (safety 1)))
  (let ((off (glyph-data-offset font unicode)))
    (when off
      (let* ((data (font-data font))
             (width (read-s16-le data off))
             (pos (+ off +glyph-header-size+)))
        (declare (type (unsigned-byte 28) pos))
        (loop
          (let ((op (aref data pos)))
            (declare (type (unsigned-byte 8) op))
            (incf pos)
            (case op
              (#.+end+ (return width))
              (#.+move+
               (funcall move-fn
                        (read-s16-le data pos)
                        (read-s16-le data (+ pos 2)))
               (incf pos 4))
              (#.+line+
               (funcall line-fn
                        (read-s16-le data pos)
                        (read-s16-le data (+ pos 2)))
               (incf pos 4))
              (#.+curve+
               ;; Linearize: draw to endpoint
               (funcall line-fn
                        (read-s16-le data (+ pos 8))
                        (read-s16-le data (+ pos 10)))
               (incf pos 12)))))))))

(defun map-string (font string move-fn line-fn &key (x 0) (y 0) (scale 1.0))
  "Draw a string, advancing X by each glyph's width. Returns final X."
  (declare (type string string)
           (type real x y scale))
  (let ((cx (float x))
        (sy (float scale)))
    (flet ((offset-move (gx gy)
             (funcall move-fn
                      (+ cx (* gx sy))
                      (+ y (* gy sy))))
           (offset-line (gx gy)
             (funcall line-fn
                      (+ cx (* gx sy))
                      (+ y (* gy sy)))))
      (loop for ch across string
            for code = (char-code ch)
            for width = (map-glyph font code #'offset-move #'offset-line)
            when width do (incf cx (* width sy))))
    cx))
