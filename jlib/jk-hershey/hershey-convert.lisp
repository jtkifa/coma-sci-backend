;;;; hershey-convert.lisp — Convert SFD and JHF fonts to binary format
;;;;
;;;; This module handles parsing FontForge SFD files (AVHershey outline
;;;; fonts) and original Hershey .jhf files (single-stroke fonts), and
;;;; writing the compact binary format. Used once to build font files.

(defpackage :hershey-convert
  (:use :cl)
  (:export #:parse-sfd-file
           #:parse-jhf-file
           #:build-all-jhf-fonts
           #:*jhf-directory*
           #:write-binary-font
           #:convert-sfd-to-binary
           #:build-all-fonts
           #:*sfd-directory*
           #:*binary-directory*))

(in-package :hershey-convert)

;;; ============================================================
;;; Configuration
;;; ============================================================

(defvar *sfd-directory* nil
  "Directory containing SFD source files.")

(defvar *binary-directory* nil
  "Directory for output binary font files.")

(defvar *jhf-directory* nil
  "Directory containing Hershey .jhf source files.")

(defun system-data-directory ()
  "Return the data directory for this ASDF system."
  (let ((system (asdf:find-system :hershey-fonts nil)))
    (when system
      (merge-pathnames #P"data/" (asdf:system-source-directory system)))))

(defun ensure-directories ()
  "Set default directories if not already set."
  (let ((data-dir (system-data-directory)))
    (unless *sfd-directory*
      (setf *sfd-directory* (merge-pathnames #P"SFD/" data-dir)))
    (unless *jhf-directory*
      (setf *jhf-directory* (merge-pathnames #P"JHF/" data-dir)))
    (unless *binary-directory*
      (setf *binary-directory* (merge-pathnames #P"SFDBIN/" data-dir)))))

;;; ============================================================
;;; Constants (must match hershey-fonts)
;;; ============================================================

(defconstant +magic+ #x59485348)  ; "HSHY" little-endian
(defconstant +version+ 3)
(defconstant +header-size+ 128)
(defconstant +move+ 1)
(defconstant +line+ 2)
(defconstant +curve+ 3)
(defconstant +end+ 0)

;;; ============================================================
;;; Parsed structures (intermediate)
;;; ============================================================

(defstruct parsed-glyph
  (name "" :type string)
  (encoding -1 :type fixnum)
  (unicode -1 :type fixnum)
  (width 0 :type fixnum)
  (minx 0 :type fixnum)
  (miny 0 :type fixnum)
  (maxx 0 :type fixnum)
  (maxy 0 :type fixnum)
  (commands nil :type list))

(defstruct parsed-font
  (name "" :type string)
  (ascent 800 :type fixnum)
  (descent 200 :type fixnum)
  (kind :outline :type (member :outline :stroke))
  (glyphs nil :type list))

;;; ============================================================
;;; SFD Parser
;;; ============================================================

(defun parse-sfd-file (pathname)
  "Parse a FontForge SFD file, return a PARSED-FONT structure."
  (with-open-file (in pathname :direction :input)
    (let ((font (make-parsed-font))
          (glyphs nil))
      (loop for line = (read-line in nil nil)
            while line do
              (cond
                ((prefix-match "FontName: " line)
                 (setf (parsed-font-name font) (subseq line 10)))
                ((prefix-match "Ascent: " line)
                 (setf (parsed-font-ascent font)
                       (parse-integer (subseq line 8) :junk-allowed t)))
                ((prefix-match "Descent: " line)
                 (setf (parsed-font-descent font)
                       (parse-integer (subseq line 9) :junk-allowed t)))
                ((prefix-match "StartChar: " line)
                 (let ((glyph (parse-sfd-glyph in (subseq line 11))))
                   (when (and glyph (>= (parsed-glyph-unicode glyph) 0))
                     (push glyph glyphs))))))
      (setf (parsed-font-glyphs font) (nreverse glyphs))
      font)))

(defun prefix-match (prefix string)
  (and (>= (length string) (length prefix))
       (string= prefix string :end2 (length prefix))))

(defun parse-sfd-glyph (stream name)
  "Parse a single glyph from SFD stream."
  (let ((glyph (make-parsed-glyph :name name))
        (commands nil)
        (in-splineset nil)
        (minx most-positive-fixnum)
        (miny most-positive-fixnum)
        (maxx most-negative-fixnum)
        (maxy most-negative-fixnum)
        (has-coords nil))
    (flet ((update-bounds (x y)
             (when (and x y)
               (setf has-coords t)
               (setf minx (min minx x) maxx (max maxx x))
               (setf miny (min miny y) maxy (max maxy y)))))
      (loop for line = (read-line stream nil nil)
            while line do
              (cond
                ((string= line "EndChar")
                 (setf (parsed-glyph-commands glyph) (nreverse commands))
                 (if has-coords
                     (setf (parsed-glyph-minx glyph) minx
                           (parsed-glyph-miny glyph) miny
                           (parsed-glyph-maxx glyph) maxx
                           (parsed-glyph-maxy glyph) maxy)
                     (setf (parsed-glyph-minx glyph) 0
                           (parsed-glyph-miny glyph) 0
                           (parsed-glyph-maxx glyph) 0
                           (parsed-glyph-maxy glyph) 0))
                 (return-from parse-sfd-glyph glyph))
                ((prefix-match "Encoding: " line)
                 (let* ((rest (subseq line 10))
                        (tokens (split-tokens rest)))
                   (setf (parsed-glyph-encoding glyph)
                         (or (and (>= (length tokens) 1)
                                  (parse-integer (first tokens) :junk-allowed t))
                             -1))
                   (setf (parsed-glyph-unicode glyph)
                         (or (and (>= (length tokens) 2)
                                  (parse-integer (second tokens) :junk-allowed t))
                             -1))))
                ((prefix-match "Width: " line)
                 (setf (parsed-glyph-width glyph)
                       (or (parse-integer (subseq line 7) :junk-allowed t) 0)))
                ((string= line "SplineSet")
                 (setf in-splineset t))
                ((string= line "EndSplineSet")
                 (setf in-splineset nil))
                (in-splineset
                 (let ((cmd (parse-spline-line line)))
                   (when cmd
                     (push cmd commands)
                     (case (car cmd)
                       ((:move :line)
                        (update-bounds (second cmd) (third cmd)))
                       (:curve
                        (update-bounds (second cmd) (third cmd))
                        (update-bounds (fourth cmd) (fifth cmd))
                        (update-bounds (sixth cmd) (seventh cmd)))))))))
      nil)))

(defun parse-spline-line (line)
  "Parse spline command: 'x y m 1' or 'x1 y1 x2 y2 x3 y3 c 0'"
  (let ((tokens (split-tokens (string-trim '(#\Space #\Tab) line))))
    (when (>= (length tokens) 3)
      (let ((cmd-str (car (last tokens 2))))
        (when (= (length cmd-str) 1)
          (case (char cmd-str 0)
            (#\m (list :move
                       (parse-integer (first tokens) :junk-allowed t)
                       (parse-integer (second tokens) :junk-allowed t)))
            (#\l (list :line
                       (parse-integer (first tokens) :junk-allowed t)
                       (parse-integer (second tokens) :junk-allowed t)))
            (#\c (when (>= (length tokens) 8)
                   (list :curve
                         (parse-integer (nth 0 tokens) :junk-allowed t)
                         (parse-integer (nth 1 tokens) :junk-allowed t)
                         (parse-integer (nth 2 tokens) :junk-allowed t)
                         (parse-integer (nth 3 tokens) :junk-allowed t)
                         (parse-integer (nth 4 tokens) :junk-allowed t)
                         (parse-integer (nth 5 tokens) :junk-allowed t))))))))))

(defun split-tokens (string)
  "Split string on whitespace."
  (let ((tokens nil)
        (start 0)
        (len (length string)))
    (loop while (< start len) do
      (loop while (and (< start len)
                       (member (char string start) '(#\Space #\Tab)))
            do (incf start))
      (when (< start len)
        (let ((end start))
          (loop while (and (< end len)
                           (not (member (char string end) '(#\Space #\Tab))))
                do (incf end))
          (push (subseq string start end) tokens)
          (setf start end))))
    (nreverse tokens)))

;;; ============================================================
;;; JHF Parser (original Hershey single-stroke data)
;;; ============================================================
;;;
;;; One glyph per line: columns 0-4 glyph number, 5-7 vertex count
;;; (including the left/right pair), then character pairs.  Each
;;; coordinate is char-code minus (char-code #\R); " R" is pen up.
;;; Hershey Y points down with the baseline at +9; we flip to Y-up,
;;; put the baseline at 0 and the left bearing at x=0, and scale to
;;; the same 1000/em units as the AVHershey SFD fonts.

(defparameter *jhf-scale* 80/3
  "Font units per Hershey unit.  Cap height 21 -> 560, matching AVHershey.")

(defparameter *jhf-baseline* 9
  "Hershey Y coordinate of the baseline.")

(defun jhf-coord (ch)
  (- (char-code ch) (char-code #\R)))

(defun read-jhf-record (in)
  "Read one glyph record, joining continuation lines if a glyph wraps.
   Returns (values number vertex-count pair-string) or NIL at EOF."
  (let ((line (read-line in nil nil)))
    (loop while (and line (< (length (string-right-trim '(#\Space #\Return) line)) 8))
          do (setf line (read-line in nil nil)))
    (when line
      (setf line (string-right-trim '(#\Return) line))
      (let* ((number (parse-integer line :start 0 :end 5 :junk-allowed t))
             (count (parse-integer line :start 5 :end 8 :junk-allowed t))
             (pairs (subseq line 8)))
        (loop while (< (length pairs) (* 2 count))
              for more = (read-line in nil nil)
              while more
              do (setf pairs (concatenate 'string pairs
                                          (string-right-trim '(#\Return) more))))
        (values number count (subseq pairs 0 (min (length pairs) (* 2 count))))))))

(defun parse-jhf-glyph (number pairs unicode)
  "Build a PARSED-GLYPH from a JHF pair string."
  (let* ((left (jhf-coord (char pairs 0)))
         (right (jhf-coord (char pairs 1)))
         (s *jhf-scale*)
         (commands nil)
         (pen-up t)
         (minx most-positive-fixnum) (miny most-positive-fixnum)
         (maxx most-negative-fixnum) (maxy most-negative-fixnum))
    (loop for i from 2 below (1- (length pairs)) by 2
          for cx = (char pairs i)
          for cy = (char pairs (1+ i))
          do (if (and (char= cx #\Space) (char= cy #\R))
                 (setf pen-up t)
                 (let ((x (round (* s (- (jhf-coord cx) left))))
                       (y (round (* s (- *jhf-baseline* (jhf-coord cy))))))
                   (push (list (if pen-up :move :line) x y) commands)
                   (setf pen-up nil
                         minx (min minx x) maxx (max maxx x)
                         miny (min miny y) maxy (max maxy y)))))
    (unless commands
      (setf minx 0 miny 0 maxx 0 maxy 0))
    (make-parsed-glyph :name (format nil "H~D" number)
                       :encoding unicode
                       :unicode unicode
                       :width (round (* s (- right left)))
                       :minx minx :miny miny :maxx maxx :maxy maxy
                       :commands (nreverse commands))))

(defun parse-jhf-file (pathname &key name (mapping :ascii))
  "Parse a Hershey .jhf file, return a PARSED-FONT.
   The Nth record is assigned code (+ 32 N), i.e. the usual ASCII-ordered
   layout.  MAPPING is a function of (code) returning a Unicode code point
   (or NIL to drop the glyph), or :ASCII to keep codes as-is."
  (with-open-file (in pathname :direction :input)
    (let ((glyphs nil))
      (loop for n from 0
            do (multiple-value-bind (number count pairs) (read-jhf-record in)
                 (unless number (return))
                 (let ((unicode (if (eq mapping :ascii)
                                    (+ 32 n)
                                    (funcall mapping (+ 32 n)))))
                   (when (and unicode (>= count 1) (>= (length pairs) 2))
                     (push (parse-jhf-glyph number pairs unicode) glyphs)))))
      ;; Japanese uses slot 32 for a glyph; give such fonts a Roman-width space
      (unless (find 32 glyphs :key #'parsed-glyph-unicode)
        (push (parse-jhf-glyph 0 "JZ" 32) glyphs))
      (make-parsed-font :name (or name (pathname-name pathname))
                        :ascent 800
                        :descent 200
                        :kind :stroke
                        :glyphs (nreverse glyphs)))))

;;; ============================================================
;;; JHF slot layouts -> Unicode
;;; ============================================================
;;;
;;; Each .jhf file is a 96-slot (or longer) ASCII-ordered table, but the
;;; non-Latin files put Greek, Cyrillic, kana or symbols in those slots.
;;; A layout maps slot codes to real code points.  Slots not overridden
;;; keep their ASCII code; slot 127 is a filler in the text fonts and is
;;; dropped.  Layouts were checked against rendered glyph charts.

(defun slot-overrides (start string)
  "Alist mapping consecutive codes from START to the chars of STRING.
   A space in STRING drops that slot."
  (loop for ch across string
        for code from start
        collect (cons code (if (char= ch #\Space) nil (char-code ch)))))

(defun jhf-layout (overrides &key keep-del)
  "Mapping function for PARSE-JHF-FILE from an override alist."
  (lambda (code)
    (let ((entry (assoc code overrides)))
      (cond (entry (cdr entry))
            ((= code 127) (when keep-del code))
            ((> code 127) nil)
            (t code)))))

(defparameter *greek-alphabetic*
  (append (slot-overrides 65 "ΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩ  ")
          (slot-overrides 97 "αβγδεζηθικλμνξοπρστυφχψω  ")
          (slot-overrides 94 "↑"))
  "greekc, greeks: letters in Greek alphabetical order, Y/Z empty.")

(defparameter *greek-symbol*
  (append (slot-overrides 65 "ΑΒΧΔΕΦΓΗΙ·ΚΛΜΝΟΠΘΡΣΤΥ°ΩΞΨΖ")
          (slot-overrides 97 "αβχδεφγηι×κλμνοπθρστυ÷ωξψζ"))
  "greek, timesg: Adobe Symbol-font layout.")

(defparameter *cyrillic-phonetic*
  (append (slot-overrides 65 "АБЭДЙФГЖИЧКЛМНОПШРСТЮВЩХУЗ")
          (slot-overrides 97 "абэдйфгжичклмнопшрстювщхуз")
          (slot-overrides 36 "Ыцы")
          (slot-overrides 91 "Е")
          (slot-overrides 93 "ЪЯЬЦ")
          (slot-overrides 123 "еъяь~"))
  "cyrillic: phonetic layout, extra letters in punctuation slots.
   Note the data's capital Й has no breve.")

(defparameter *cyrillic-alphabetic*
  (append (slot-overrides 65 "АБВГДЕЖЗИЙКЛМНОПРСТУФХЦЧШЩ")
          (slot-overrides 97 "абвгдежзийклмнопрстуфхцчшщ")
          (slot-overrides 94 "↑"))
  "cyrilc_1: first 26 letters of the alphabet, А through Щ.")

(defparameter *japanese*
  (slot-overrides
   32 (concatenate 'string
                   "。、一日円年大国人東中本京出時上者十 二三四五六七八九      "
                   "あいうえおかきくけこさしすせそたちつてとなにぬねのはひふへほまみむめもや ゆ よらりるれろわゐ ゑをん    "
                   "がぎぐげござじずぜぞだぢづでどばびぶべぼぱぴぷぺぽ"
                   "アイウエオカキクケコサシスセソタチツテトナニヌネノハヒフヘホマミムメモヤ ユ ヨラリルレロワヰ ヱヲン    "
                   "ガギグゲゴザジズゼゾダヂヅデドバビブベボパピプペポ"))
  "japanese: a few kanji, then hiragana and katakana gojuon grids
   (repeated placeholder kana and filler dots dropped).")

(defparameter *astrology*
  (append (slot-overrides 33 "♓")
          (slot-overrides 35 "☉☿♀⊕♂")
          (slot-overrides 42 "♃♄")
          (slot-overrides 45 "♅")
          (slot-overrides 47 "♆")
          (slot-overrides 58 "♇☾☄✳☊☋♈")
          (slot-overrides 91 "♉")
          (slot-overrides 93 "♊♋♌♍")
          (slot-overrides 123 "♏♐♑♒~"))
  "astrology: planets, nodes and zodiac in punctuation slots; Latin letters
   and digits are a complex italic.")

(defparameter *jhf-layouts*
  `(("greekc" . ,*greek-alphabetic*)
    ("greeks" . ,*greek-alphabetic*)
    ("greek" . ,*greek-symbol*)
    ("timesg" . ,*greek-symbol*)
    ("cyrillic" . ,*cyrillic-phonetic*)
    ("cyrilc_1" . ,*cyrillic-alphabetic*)
    ("japanese" . ,*japanese*)
    ("astrology" . ,*astrology*))
  "File stem -> slot override alist.  Unlisted fonts are plain ASCII.")

(defparameter *jhf-symbol-fonts*
  '("markers" "mathlow" "mathupp" "meteorology" "music" "symbolic")
  "Symbol fonts with no Unicode mapping: glyphs stay in their ASCII
   slots (including 127), like a classic Symbol font.")

(defun jhf-mapping (stem)
  "Return the PARSE-JHF-FILE mapping for a .jhf file stem."
  (let ((overrides (cdr (assoc stem *jhf-layouts* :test #'string=))))
    (jhf-layout overrides
                :keep-del (member stem *jhf-symbol-fonts* :test #'string=))))

;;; ============================================================
;;; Binary Writer (little-endian)
;;; ============================================================

(defun write-binary-font (parsed-font pathname)
  "Write parsed font to binary file."
  (let* ((glyphs (parsed-font-glyphs parsed-font))
         (sorted-glyphs (sort (copy-list glyphs) #'<
                              :key #'parsed-glyph-unicode))
         (glyph-count (length sorted-glyphs))
         (index-entries nil)
         (current-offset 0))
    (dolist (g sorted-glyphs)
      (let ((bytes (encode-glyph g)))
        (push (list (parsed-glyph-unicode g) current-offset bytes) index-entries)
        (incf current-offset (length bytes))))
    (setf index-entries (nreverse index-entries))
    (with-open-file (out pathname
                         :direction :output
                         :if-exists :supersede
                         :element-type '(unsigned-byte 8))
      ;; Header
      (write-u32-le out +magic+)
      (write-u16-le out +version+)
      (write-u16-le out (if (eq (parsed-font-kind parsed-font) :stroke) 1 0))
      (write-u32-le out glyph-count)
      (write-u32-le out 0)
      (write-s16-le out (parsed-font-ascent parsed-font))
      (write-s16-le out (parsed-font-descent parsed-font))
      (loop repeat (- 64 20) do (write-byte 0 out))
      ;; Font name
      (let ((name (parsed-font-name parsed-font)))
        (loop for i below 64 do
          (write-byte (if (< i (length name))
                          (char-code (char name i))
                          0)
                      out)))
      ;; Index table
      (dolist (entry index-entries)
        (write-u32-le out (first entry))
        (write-u32-le out (second entry)))
      ;; Glyph data
      (dolist (entry index-entries)
        (write-sequence (third entry) out)))
    pathname))

(defun encode-glyph (glyph)
  "Encode glyph to byte vector."
  (let ((out (make-array 64 :element-type '(unsigned-byte 8)
                         :adjustable t :fill-pointer 0)))
    (push-s16-le out (parsed-glyph-width glyph))
    (push-s16-le out (parsed-glyph-minx glyph))
    (push-s16-le out (parsed-glyph-miny glyph))
    (push-s16-le out (parsed-glyph-maxx glyph))
    (push-s16-le out (parsed-glyph-maxy glyph))
    (push-u16-le out (length (parsed-glyph-commands glyph)))
    (dolist (cmd (parsed-glyph-commands glyph))
      (ecase (car cmd)
        (:move
         (vector-push-extend +move+ out)
         (push-s16-le out (second cmd))
         (push-s16-le out (third cmd)))
        (:line
         (vector-push-extend +line+ out)
         (push-s16-le out (second cmd))
         (push-s16-le out (third cmd)))
        (:curve
         (vector-push-extend +curve+ out)
         (push-s16-le out (second cmd))
         (push-s16-le out (third cmd))
         (push-s16-le out (fourth cmd))
         (push-s16-le out (fifth cmd))
         (push-s16-le out (sixth cmd))
         (push-s16-le out (seventh cmd)))))
    (vector-push-extend +end+ out)
    out))

(defun push-u16-le (vec val)
  (vector-push-extend (ldb (byte 8 0) val) vec)
  (vector-push-extend (ldb (byte 8 8) val) vec))

(defun push-s16-le (vec val)
  (push-u16-le vec (logand #xFFFF (if (< val 0) (+ val 65536) val))))

(defun write-u16-le (stream val)
  (write-byte (ldb (byte 8 0) val) stream)
  (write-byte (ldb (byte 8 8) val) stream))

(defun write-s16-le (stream val)
  (write-u16-le stream (logand #xFFFF (if (< val 0) (+ val 65536) val))))

(defun write-u32-le (stream val)
  (write-byte (ldb (byte 8 0) val) stream)
  (write-byte (ldb (byte 8 8) val) stream)
  (write-byte (ldb (byte 8 16) val) stream)
  (write-byte (ldb (byte 8 24) val) stream))

;;; ============================================================
;;; Convenience functions
;;; ============================================================

(defun convert-sfd-to-binary (sfd-path binary-path)
  "Parse SFD file and write binary font."
  (let ((font (parse-sfd-file sfd-path)))
    (format t "~&Parsed ~A: ~D glyphs~%"
            (parsed-font-name font)
            (length (parsed-font-glyphs font)))
    (write-binary-font font binary-path)
    (format t "  -> ~A~%" (enough-namestring binary-path))
    binary-path))

(defun build-all-fonts (&key (force nil))
  "Convert all SFD files in *sfd-directory* to binary format.
   Skips files that are already up-to-date unless FORCE is true."
  (ensure-directories)
  (uiop:ensure-all-directories-exist (list *binary-directory*))
  (let ((sfd-files (directory (merge-pathnames "*.sfd" *sfd-directory*)))
        (count 0))
    (dolist (sfd-path sfd-files)
      (let* ((name (pathname-name sfd-path))
             (binary-path (merge-pathnames
                           (make-pathname :name name :type "sfdbin")
                           *binary-directory*)))
        (when (or force
                  (not (probe-file binary-path))
                  (> (file-write-date sfd-path)
                     (file-write-date binary-path)))
          (convert-sfd-to-binary sfd-path binary-path)
          (incf count))))
    (format t "~&Built ~D font~:P.~%" count)
    count))

(defun convert-jhf-to-binary (jhf-path binary-path)
  "Parse JHF file using its layout from *JHF-LAYOUTS* and write binary font."
  (let* ((stem (pathname-name jhf-path))
         (font (parse-jhf-file jhf-path :mapping (jhf-mapping stem))))
    (format t "~&Parsed ~A: ~D glyphs~%"
            (parsed-font-name font)
            (length (parsed-font-glyphs font)))
    (write-binary-font font binary-path)
    (format t "  -> ~A~%" (enough-namestring binary-path))
    binary-path))

(defun build-all-jhf-fonts (&key (force nil))
  "Convert all .jhf files in *jhf-directory* to binary format in
   *binary-directory*, named by file stem (e.g. rowmans.sfdbin).
   Skips files that are already up-to-date unless FORCE is true."
  (ensure-directories)
  (uiop:ensure-all-directories-exist (list *binary-directory*))
  (let ((jhf-files (directory (merge-pathnames "*.jhf" *jhf-directory*)))
        (count 0))
    (dolist (jhf-path jhf-files)
      (let* ((name (pathname-name jhf-path))
             (binary-path (merge-pathnames
                           (make-pathname :name name :type "sfdbin")
                           *binary-directory*)))
        (when (or force
                  (not (probe-file binary-path))
                  (> (file-write-date jhf-path)
                     (file-write-date binary-path)))
          (convert-jhf-to-binary jhf-path binary-path)
          (incf count))))
    (format t "~&Built ~D font~:P.~%" count)
    count))
