;;;; hershey-ttf.lisp — Write Hershey stroke fonts as TrueType files
;;;;
;;;; Two modes:
;;;;
;;;;   :STROKED     Each stroke segment becomes a closed capsule (a bar
;;;;                with round ends, built from quadratic arcs) of width
;;;;                *PEN-WIDTH*.  Capsules overlap at joints; TrueType's
;;;;                nonzero fill unions them, and the OVERLAP_SIMPLE flag
;;;;                is set on every glyph.  For screen and print.
;;;;
;;;;   :SINGLE-LINE Each stroke polyline becomes a zero-area contour that
;;;;                runs out and back along itself.  Invisible (or a
;;;;                hairline) on screen, but plotter/laser/CAM software
;;;;                that follows paths draws each stroke once.
;;;;
;;;; Input is any font from HERSHEY-FONTS; intended for the JHF stroke
;;;; fonts (the AVHershey fonts are outlines already and have OTFs).
;;;;
;;;; Tables written: OS/2 cmap glyf head hhea hmtx loca maxp name post.
;;;; All integers are big-endian.

(defpackage :hershey-ttf
  (:use :cl)
  (:export #:write-ttf
           #:build-all-ttf-fonts
           #:*pen-width*
           #:*ttf-directory*
           #:*jhf-family-names*))

(in-package :hershey-ttf)

;;; ============================================================
;;; Configuration
;;; ============================================================

(defvar *pen-width* 56
  "Stroke width in font units for :STROKED mode.  56 matches the
   AVHershey Medium weight.")

(defvar *ttf-directory* nil
  "Directory for output .ttf files.")

(defparameter *units-per-em* 1000)

(defun ttf-directory ()
  "Return the TTF output directory, defaulting to system data directory."
  (or *ttf-directory*
      (let ((system (asdf:find-system :hershey-fonts nil)))
        (when system
          (merge-pathnames #P"data/TTF/" (asdf:system-source-directory system))))))

(defparameter *jhf-family-names*
  '(("astrology" . "Astrology")
    ("cursive" . "Cursive")
    ("cyrilc_1" . "Cyrillic Complex Alt")
    ("cyrillic" . "Cyrillic Complex")
    ("futural" . "Sans Simplex")
    ("futuram" . "Sans Duplex")
    ("gothgbt" . "Gothic English Triplex")
    ("gothgrt" . "Gothic German Triplex")
    ("gothiceng" . "Gothic English")
    ("gothicger" . "Gothic German")
    ("gothicita" . "Gothic Italian")
    ("gothitt" . "Gothic Italian Triplex")
    ("greek" . "Greek Simplex Alt")
    ("greekc" . "Greek Complex")
    ("greeks" . "Greek Simplex")
    ("japanese" . "Japanese")
    ("markers" . "Markers")
    ("mathlow" . "Math Lower")
    ("mathupp" . "Math Upper")
    ("meteorology" . "Meteorology")
    ("music" . "Music")
    ("rowmand" . "Roman Duplex")
    ("rowmans" . "Roman Simplex")
    ("rowmant" . "Roman Triplex")
    ("scriptc" . "Script Complex")
    ("scripts" . "Script Simplex")
    ("symbolic" . "Symbolic")
    ("timesg" . "Greek Complex Alt")
    ("timesi" . "Italic Complex")
    ("timesib" . "Italic Triplex")
    ("timesr" . "Roman Complex")
    ("timesrb" . "Roman Complex Bold"))
  "JHF file stem -> font family name (prefixed with \"Hershey \").")

(defparameter *copyright*
  "The Hershey Fonts were originally created by Dr. A. V. Hershey while working at the U. S. National Bureau of Standards.")

(defparameter *description*
  "The format of the Font data in this distribution was originally created by James Hurt, Cognition, Inc., 900 Technology Park Drive, Billerica, MA 01821. Converted to TrueType by hershey-ttf.")

;;; ============================================================
;;; Byte buffers (big-endian)
;;; ============================================================

(defun make-buf (&optional (size 256))
  (make-array size :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))

(defun put-u8 (buf v) (vector-push-extend (ldb (byte 8 0) v) buf))

(defun put-u16 (buf v)
  (put-u8 buf (ldb (byte 8 8) v))
  (put-u8 buf (ldb (byte 8 0) v)))

(defun put-i16 (buf v) (put-u16 buf (ldb (byte 16 0) v)))

(defun put-u32 (buf v)
  (put-u16 buf (ldb (byte 16 16) v))
  (put-u16 buf (ldb (byte 16 0) v)))

(defun put-u64 (buf v)
  (put-u32 buf (ldb (byte 32 32) v))
  (put-u32 buf (ldb (byte 32 0) v)))

(defun put-tag (buf tag)
  (loop for ch across tag do (put-u8 buf (char-code ch))))

(defun put-bytes (buf bytes)
  (loop for b across bytes do (put-u8 buf b)))

(defun pad4 (buf)
  (loop until (zerop (mod (fill-pointer buf) 4)) do (put-u8 buf 0)))

(defun set-u32 (buf pos v)
  (loop for i from 0 below 4
        do (setf (aref buf (+ pos i)) (ldb (byte 8 (- 24 (* 8 i))) v))))

(defun table-checksum (bytes)
  (let ((sum 0)
        (len (length bytes)))
    (loop for i from 0 below len by 4
          do (setf sum (ldb (byte 32 0)
                            (+ sum
                               (loop for j from 0 below 4
                                     for k = (+ i j)
                                     sum (ash (if (< k len) (aref bytes k) 0)
                                              (- 24 (* 8 j))))))))
    sum))

;;; ============================================================
;;; Geometry — strokes to TrueType contours
;;; ============================================================
;;;
;;; A contour is a list of (x y on-curve-p).  Outer contours run
;;; clockwise (Y up), the TrueType convention.

(defun glyph-strokes (font code)
  "List of polylines for CODE, each a list of (x . y) with repeated
   points removed."
  (let ((strokes '())
        (cur '()))
    (flet ((finish ()
             (when cur (push (nreverse cur) strokes))))
      (hershey-fonts:map-glyph
       font code
       (lambda (x y) (finish) (setf cur (list (cons x y))))
       (lambda (x y)
         (unless (and cur (= x (car (first cur))) (= y (cdr (first cur))))
           (push (cons x y) cur))))
      (finish))
    (nreverse strokes)))

(defun arc (cx cy r start-angle steps)
  "Clockwise arc around (CX,CY) in 45-degree quadratic steps from
   START-ANGLE; returns the points after the start point."
  (let ((step (/ pi 4))
        (rc (/ r (cos (/ pi 8)))))
    (loop for k from 1 to steps
          for mid = (- start-angle (* (- k 1/2) step))
          for end = (- start-angle (* k step))
          collect (list (round (+ cx (* rc (cos mid))))
                        (round (+ cy (* rc (sin mid)))) nil)
          collect (list (round (+ cx (* r (cos end))))
                        (round (+ cy (* r (sin end)))) t))))

(defun dot-contour (x y r)
  "Closed circle of radius R around (X,Y)."
  (let ((top (list (round x) (round (+ y r)) t)))
    (cons top (butlast (arc x y r (/ pi 2) 8)))))

(defun capsule-contour (x0 y0 x1 y1 r)
  "Bar of half-width R from (X0,Y0) to (X1,Y1) with round ends."
  (let* ((theta (atan (- y1 y0) (- x1 x0)))
         (nx (* r (cos (+ theta (/ pi 2)))))
         (ny (* r (sin (+ theta (/ pi 2))))))
    (append (list (list (round (+ x0 nx)) (round (+ y0 ny)) t)
                  (list (round (+ x1 nx)) (round (+ y1 ny)) t))
            (arc x1 y1 r (+ theta (/ pi 2)) 4)
            (list (list (round (- x0 nx)) (round (- y0 ny)) t))
            (butlast (arc x0 y0 r (- theta (/ pi 2)) 4)))))

(defun stroked-contours (strokes half-width)
  (loop for stroke in strokes
        if (null (rest stroke))
          collect (dot-contour (car (first stroke)) (cdr (first stroke)) half-width)
        else
          append (loop for (p0 p1) on stroke
                       while p1
                       collect (capsule-contour (car p0) (cdr p0)
                                                (car p1) (cdr p1) half-width))))

(defun single-line-contours (strokes)
  (loop for stroke in strokes
        for pts = (mapcar (lambda (p) (list (car p) (cdr p) t)) stroke)
        collect (append pts (rest (butlast (reverse pts))))))

;;; ============================================================
;;; glyf encoding
;;; ============================================================

(defstruct tglyph
  (code 0)
  (advance 0)
  (contours nil)
  (xmin 0) (ymin 0) (xmax 0) (ymax 0)
  (points 0))

(defun make-glyph-from-contours (code advance contours)
  (let ((g (make-tglyph :code code :advance advance :contours contours)))
    (when contours
      (let ((pts (apply #'append contours)))
        (setf (tglyph-xmin g) (reduce #'min pts :key #'first)
              (tglyph-xmax g) (reduce #'max pts :key #'first)
              (tglyph-ymin g) (reduce #'min pts :key #'second)
              (tglyph-ymax g) (reduce #'max pts :key #'second)
              (tglyph-points g) (length pts))))
    g))

(defun encode-glyf (g)
  "Simple-glyph bytes for G, or an empty vector for a blank glyph."
  (let ((buf (make-buf))
        (contours (tglyph-contours g)))
    (when contours
      (put-i16 buf (length contours))
      (put-i16 buf (tglyph-xmin g))
      (put-i16 buf (tglyph-ymin g))
      (put-i16 buf (tglyph-xmax g))
      (put-i16 buf (tglyph-ymax g))
      (let ((end -1))
        (dolist (c contours)
          (incf end (length c))
          (put-u16 buf end)))
      (put-u16 buf 0)                   ; no instructions
      (let ((flags (make-buf)) (xs (make-buf)) (ys (make-buf))
            (px 0) (py 0) (first t))
        (dolist (c contours)
          (dolist (p c)
            (destructuring-bind (x y on) p
              (let ((dx (- x px)) (dy (- y py))
                    (flag (if on 1 0)))
                (when first
                  (setf flag (logior flag #x40) first nil)) ; OVERLAP_SIMPLE
                (cond ((zerop dx) (setf flag (logior flag #x10)))
                      ((< (abs dx) 256)
                       (setf flag (logior flag #x02 (if (plusp dx) #x10 0)))
                       (put-u8 xs (abs dx)))
                      (t (put-i16 xs dx)))
                (cond ((zerop dy) (setf flag (logior flag #x20)))
                      ((< (abs dy) 256)
                       (setf flag (logior flag #x04 (if (plusp dy) #x20 0)))
                       (put-u8 ys (abs dy)))
                      (t (put-i16 ys dy)))
                (put-u8 flags flag)
                (setf px x py y)))))
        (put-bytes buf flags)
        (put-bytes buf xs)
        (put-bytes buf ys)))
    buf))

;;; ============================================================
;;; Tables
;;; ============================================================

(defun font-glyphs (font mode)
  "TGLYPH list: .notdef first, then every BMP code point in FONT."
  (cons (make-tglyph :code nil :advance 500)
        (loop for code in (hershey-fonts:font-codes font)
              when (<= code #xFFFF)
                collect (let ((strokes (glyph-strokes font code)))
                          (make-glyph-from-contours
                           code
                           (hershey-fonts:glyph-width font code)
                           (ecase mode
                             (:stroked (stroked-contours strokes (/ *pen-width* 2)))
                             (:single-line (single-line-contours strokes))))))))

(defun build-glyf-loca (glyphs)
  (let ((glyf (make-buf 65536))
        (loca (make-buf)))
    (dolist (g glyphs)
      (put-u32 loca (fill-pointer glyf))
      (put-bytes glyf (encode-glyf g))
      (pad4 glyf))
    (put-u32 loca (fill-pointer glyf))
    (values glyf loca)))

(defun build-cmap (glyphs)
  "Format 4 subtable, referenced from (0,3) and (3,1)."
  (let* ((pairs (loop for g in (rest glyphs) for gid from 1
                      collect (cons (tglyph-code g) gid)))
         (segments '()))
    ;; Runs of consecutive codes have consecutive glyph ids
    (loop with start = nil and prev = nil and start-gid = nil
          for (code . gid) in pairs
          do (cond ((and prev (= code (1+ prev)))
                    (setf prev code))
                   (t (when start (push (list start prev start-gid) segments))
                      (setf start code prev code start-gid gid)))
          finally (when start (push (list start prev start-gid) segments)))
    (setf segments (nreverse segments))
    (setf segments (append segments (list (list #xFFFF #xFFFF nil))))
    (let* ((segx2 (* 2 (length segments)))
           (search (* 2 (expt 2 (1- (integer-length (length segments))))))
           (sub (make-buf)))
      (put-u16 sub 4)
      (put-u16 sub (+ 16 (* 4 segx2)))  ; length
      (put-u16 sub 0)                   ; language
      (put-u16 sub segx2)
      (put-u16 sub search)
      (put-u16 sub (1- (integer-length (/ search 2))))
      (put-u16 sub (- segx2 search))
      (dolist (s segments) (put-u16 sub (second s)))
      (put-u16 sub 0)                   ; reserved pad
      (dolist (s segments) (put-u16 sub (first s)))
      (dolist (s segments)
        (put-i16 sub (if (third s) (- (third s) (first s)) 1)))
      (dolist (s segments) (declare (ignore s)) (put-u16 sub 0))
      (let ((cmap (make-buf)))
        (put-u16 cmap 0)
        (put-u16 cmap 2)
        (put-u16 cmap 0) (put-u16 cmap 3) (put-u32 cmap 20)
        (put-u16 cmap 3) (put-u16 cmap 1) (put-u32 cmap 20)
        (put-bytes cmap sub)
        cmap))))

(defun utf16be (string)
  (let ((buf (make-buf)))
    (loop for ch across string do (put-u16 buf (char-code ch)))
    buf))

(defun build-name (records)
  "RECORDS is a list of (name-id . string), written for platform 3/1/0x409."
  (let* ((records (sort (copy-list records) #'< :key #'car))
         (strings (mapcar (lambda (r) (utf16be (cdr r))) records))
         (buf (make-buf))
         (offset 0))
    (put-u16 buf 0)
    (put-u16 buf (length records))
    (put-u16 buf (+ 6 (* 12 (length records))))
    (loop for (id . nil) in records
          for s in strings
          do (put-u16 buf 3) (put-u16 buf 1) (put-u16 buf #x409)
             (put-u16 buf id) (put-u16 buf (length s)) (put-u16 buf offset)
             (incf offset (length s)))
    (dolist (s strings) (put-bytes buf s))
    buf))

(defun ttf-timestamp ()
  "Seconds since 1904-01-01 (CL universal time counts from 1900)."
  (- (get-universal-time) (* 1460 86400)))

(defun glyph-top (glyphs ch)
  (let ((g (find (char-code ch) glyphs :key #'tglyph-code)))
    (if (and g (tglyph-contours g)) (tglyph-ymax g) 0)))

(defun build-tables (glyphs &key family subfamily typo-family typo-subfamily
                                 postscript-name weight)
  "Return an alist of (tag . bytes) for all tables."
  (multiple-value-bind (glyf loca) (build-glyf-loca glyphs)
    (let* ((inked (remove nil glyphs :key #'tglyph-contours))
           (xmin (reduce #'min inked :key #'tglyph-xmin :initial-value 0))
           (ymin (reduce #'min inked :key #'tglyph-ymin :initial-value 0))
           (xmax (reduce #'max inked :key #'tglyph-xmax :initial-value 0))
           (ymax (reduce #'max inked :key #'tglyph-ymax :initial-value 0))
           (ascent (max 800 ymax))
           (descent (min -200 ymin))
           (num-glyphs (length glyphs))
           (codes (remove nil (mapcar #'tglyph-code glyphs)))
           (advances (mapcar #'tglyph-advance glyphs))
           (now (ttf-timestamp))
           (head (make-buf)) (hhea (make-buf)) (hmtx (make-buf))
           (maxp (make-buf)) (post (make-buf)) (os2 (make-buf)))
      ;; head
      (put-u32 head #x00010000)
      (put-u32 head #x00010000)         ; fontRevision 1.0
      (put-u32 head 0)                  ; checkSumAdjustment, set later
      (put-u32 head #x5F0F3CF5)
      (put-u16 head #b1001)             ; baseline y=0, integer ppem
      (put-u16 head *units-per-em*)
      (put-u64 head now)
      (put-u64 head now)
      (put-i16 head xmin) (put-i16 head ymin)
      (put-i16 head xmax) (put-i16 head ymax)
      (put-u16 head 0)                  ; macStyle
      (put-u16 head 8)                  ; lowestRecPPEM
      (put-i16 head 2)                  ; fontDirectionHint
      (put-i16 head 1)                  ; long loca
      (put-i16 head 0)
      ;; hhea
      (put-u32 hhea #x00010000)
      (put-i16 hhea ascent)
      (put-i16 hhea descent)
      (put-i16 hhea 0)                  ; lineGap
      (put-u16 hhea (reduce #'max advances))
      (put-i16 hhea (reduce #'min inked :key #'tglyph-xmin :initial-value 0))
      (put-i16 hhea (reduce #'min inked :initial-value 0
                            :key (lambda (g) (- (tglyph-advance g) (tglyph-xmax g)))))
      (put-i16 hhea xmax)               ; xMaxExtent (lsb = xmin)
      (put-i16 hhea 1) (put-i16 hhea 0) (put-i16 hhea 0)
      (loop repeat 5 do (put-i16 hhea 0))
      (put-u16 hhea num-glyphs)
      ;; hmtx
      (dolist (g glyphs)
        (put-u16 hmtx (tglyph-advance g))
        (put-i16 hmtx (tglyph-xmin g)))
      ;; maxp
      (put-u32 maxp #x00010000)
      (put-u16 maxp num-glyphs)
      (put-u16 maxp (reduce #'max glyphs :key #'tglyph-points))
      (put-u16 maxp (reduce #'max glyphs :key (lambda (g) (length (tglyph-contours g)))))
      (put-u16 maxp 0) (put-u16 maxp 0)
      (put-u16 maxp 2)
      (loop repeat 8 do (put-u16 maxp 0))
      ;; post (version 3: no glyph names)
      (put-u32 post #x00030000)
      (put-u32 post 0)                  ; italicAngle
      (put-i16 post -100)
      (put-i16 post 50)
      (loop repeat 5 do (put-u32 post 0))
      ;; OS/2 version 4
      (put-u16 os2 4)
      (put-i16 os2 (round (reduce #'+ (rest advances)) (max 1 (length (rest advances)))))
      (put-u16 os2 weight)
      (put-u16 os2 5)                   ; medium width
      (put-u16 os2 0)                   ; installable embedding
      (dolist (v '(650 600 0 75 650 600 0 350)) (put-i16 os2 v))
      (put-i16 os2 50) (put-i16 os2 260)
      (put-i16 os2 0)
      (loop repeat 10 do (put-u8 os2 0)) ; panose
      (loop repeat 4 do (put-u32 os2 0)) ; unicode ranges
      (put-tag os2 "HRSY")
      (put-u16 os2 #x40)                ; REGULAR
      (put-u16 os2 (reduce #'min codes))
      (put-u16 os2 (reduce #'max codes))
      (put-i16 os2 800) (put-i16 os2 -200) (put-i16 os2 0)
      (put-u16 os2 ascent)
      (put-u16 os2 (- descent))
      (put-u32 os2 1) (put-u32 os2 0)   ; code pages: Latin 1
      (put-i16 os2 (glyph-top glyphs #\x))
      (put-i16 os2 (glyph-top glyphs #\H))
      (put-u16 os2 0) (put-u16 os2 32) (put-u16 os2 0)
      (list (cons "OS/2" os2)
            (cons "cmap" (build-cmap glyphs))
            (cons "glyf" glyf)
            (cons "head" head)
            (cons "hhea" hhea)
            (cons "hmtx" hmtx)
            (cons "loca" loca)
            (cons "maxp" maxp)
            (cons "name" (build-name
                          `((0 . ,*copyright*)
                            (1 . ,family)
                            (2 . ,subfamily)
                            (3 . ,(format nil "1.000;HRSY;~A" postscript-name))
                            (4 . ,(format nil "~A ~A" typo-family typo-subfamily))
                            (5 . "Version 1.000")
                            (6 . ,postscript-name)
                            (10 . ,*description*)
                            (16 . ,typo-family)
                            (17 . ,typo-subfamily))))
            (cons "post" post)))))

(defun write-sfnt (tables pathname)
  "Assemble TABLES (sorted alist of tag . bytes) into an sfnt file."
  (let* ((n (length tables))
         (search (* 16 (expt 2 (1- (integer-length n)))))
         (out (make-buf 65536))
         (offset (+ 12 (* 16 n)))
         (head-offset nil))
    (put-u32 out #x00010000)
    (put-u16 out n)
    (put-u16 out search)
    (put-u16 out (1- (integer-length (/ search 16))))
    (put-u16 out (- (* 16 n) search))
    (loop for (tag . bytes) in tables
          do (put-tag out tag)
             (put-u32 out (table-checksum bytes))
             (put-u32 out offset)
             (put-u32 out (length bytes))
             (when (string= tag "head") (setf head-offset offset))
             (incf offset (* 4 (ceiling (length bytes) 4))))
    (loop for (nil . bytes) in tables
          do (put-bytes out bytes)
             (pad4 out))
    (set-u32 out (+ head-offset 8)
             (ldb (byte 32 0) (- #xB1B0AFBA (table-checksum out))))
    (with-open-file (s pathname :direction :output :if-exists :supersede
                                :element-type '(unsigned-byte 8))
      (write-sequence out s))
    pathname))

;;; ============================================================
;;; Public interface
;;; ============================================================

(defun write-ttf (font pathname &key (mode :stroked) family)
  "Write FONT (a HERSHEY-FONTS font) to PATHNAME as TrueType.
   MODE is :STROKED (pen width *PEN-WIDTH*) or :SINGLE-LINE.
   FAMILY defaults to \"Hershey <name>\" from *JHF-FAMILY-NAMES*."
  (let* ((stem (hershey-fonts:font-name font))
         (base (or family
                   (format nil "Hershey ~A"
                           (or (cdr (assoc stem *jhf-family-names* :test #'string-equal))
                               stem))))
         (single (eq mode :single-line))
         (typo-family (if single (format nil "~A Single Line" base) base))
         (typo-subfamily (if single "Regular" "Medium"))
         (ps-name (format nil "~A-~A" (remove #\Space typo-family) typo-subfamily)))
    (write-sfnt (build-tables (font-glyphs font mode)
                              :family (if single typo-family
                                          (format nil "~A ~A" base typo-subfamily))
                              :subfamily "Regular"
                              :typo-family typo-family
                              :typo-subfamily typo-subfamily
                              :postscript-name ps-name
                              :weight (if single 400 500))
                pathname)))

(defun build-all-ttf-fonts (&key (modes '(:stroked :single-line)) directory)
  "Write TTFs for every JHF font (those named in *JHF-FAMILY-NAMES*)
   found in the HERSHEY-FONTS registry, loading fonts if needed.
   Files are named by PostScript name, e.g. HersheyRomanSimplex-Medium.ttf.
   Returns the number of files written."
  (when (zerop (length (hershey-fonts:list-fonts)))
    (hershey-fonts:load-all-fonts))
  (let ((dir (or directory (ttf-directory)))
        (count 0))
    (uiop:ensure-all-directories-exist (list dir))
    (loop for (stem . name) in *jhf-family-names*
          for font = (hershey-fonts:get-font stem)
          when font
            do (dolist (mode modes)
                 (let* ((single (eq mode :single-line))
                        (file (format nil "Hershey~A~:[-Medium~;SingleLine-Regular~]"
                                      (remove #\Space name) single))
                        (path (merge-pathnames (make-pathname :name file :type "ttf") dir)))
                   (write-ttf font path :mode mode)
                   (format t "~&  -> ~A~%" (enough-namestring path))
                   (incf count))))
    (format t "~&Wrote ~D TTF file~:P.~%" count)
    count))
