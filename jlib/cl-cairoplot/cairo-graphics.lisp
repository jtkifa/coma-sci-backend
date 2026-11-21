;;;; cairo-graphics.lisp — thin layer over cl-cairo2
;;;;
;;;; This is the only file in cl-cairoplot that refers to cl-cairo2; the
;;;; rest of the package draws through the functions and macros here, so
;;;; another backend could replace this file.
;;;;
;;;; A CANVAS is an open output surface (PDF, PS, SVG, PNG, or an
;;;; in-memory PIXEL-BUFFER) plus its drawing context.  Every function
;;;; takes the canvas as its first argument, so separate canvases can be
;;;; drawn on from separate threads.
;;;;
;;;; THREAD SAFETY: Cairo contexts are not thread-safe — a single canvas
;;;; must not be used from multiple threads concurrently.  Each canvas
;;;; has a recursive lock, and every function that touches the Cairo
;;;; context acquires it.  For performance-critical code, use the /tus
;;;; (thread-unsafe) variants inside WITH-CANVAS-LOCK:
;;;;
;;;;   ;; Naive/safe: each call acquires the lock (~60ns overhead each)
;;;;   (draw-polyline canvas xs ys)
;;;;
;;;;   ;; Fast: one lock acquisition for the whole block
;;;;   (with-canvas-lock (canvas)
;;;;     (new-path/tus canvas)
;;;;     (loop for x across xs for y across ys ...)
;;;;     (stroke/tus canvas))
;;;;
;;;; Compound operations like DRAW-POLYLINE already use /tus internally,
;;;; so they have minimal overhead.
;;;;
;;;; Coordinates here are the backend's native ones: origin at the top
;;;; left, y increasing downward, in points (1/72 inch) for vector output
;;;; and pixels for PNG and buffers.  The plot device layer maps its own
;;;; coordinates onto these.
;;;;
;;;; Drawing state (color, line width, dash, font, transformation, clip)
;;;; lives in the backend and is scoped with WITH-SAVED-STATE and the
;;;; WITH-* macros built on it.  The current path is not part of that
;;;; state.
;;;;
;;;; Cairo and the system font libraries do float arithmetic that can
;;;; produce NaN or infinity internally, which SBCL traps as an error.
;;;; Calls that render, finish output or touch fonts are therefore made
;;;; inside WITH-BACKEND-FLOATS.

(in-package :cl-cairoplot)

(defmacro with-backend-floats (&body body)
  "Run BODY with float traps masked, for calls into cairo."
  `(float-utils:with-float-traps-masked (:all t) ,@body))

;;; ============================================================
;;; Thread Safety Infrastructure
;;; ============================================================

(defmacro with-canvas-lock ((canvas) &body body)
  "Execute BODY with CANVAS's lock held.  Use this when calling /tus
   (thread-unsafe) variants directly for maximum performance.  Safe to
   nest because the lock is recursive."
  (let ((c (gensym "CANVAS")))
    `(let ((,c (as-canvas ,canvas)))
       (bt2:with-recursive-lock-held ((canvas-lock ,c))
         ,@body))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-tus-name (name)
    "Return NAME/TUS interned in NAME's package, case-preserving."
    (let* ((name-string (symbol-name name))
           (suffix (if (or (zerop (length name-string))
                           (upper-case-p (char name-string 0)))
                       "/TUS" "/tus")))
      (intern (concatenate 'string name-string suffix)
              (symbol-package name))))

  (defparameter *batchable-ops*
    '(move-to line-to rel-move-to rel-line-to curve-to
      arc arc-negative rectangle close-path new-path new-sub-path
      stroke stroke-preserve fill-path fill-preserve paint
      set-color set-line-width set-line-cap set-line-join set-dash set-fill-rule
      translate rotate scale transform reset-transform
      clip clip-preserve reset-clip
      draw-line polyline-path draw-polyline draw-polygon draw-rectangle draw-circle
      finish-shape
      set-system-font set-hershey-font set-font-size
      show-text draw-text text-path
      draw-argb-image paint-mesh)
    "Functions whose /TUS variants can be called inside WITH-BATCHED-GRAPHICS."))

(defmacro with-batched-graphics ((canvas) &body body)
  "Execute BODY with graphics commands batched for efficiency.  In
   practice, this means acquiring the canvas lock once and compiling
   primitive calls directly to their fast internal '/tus' variants.

   Use this instead of multiple individual drawing calls when many
   operations will be performed in sequence (e.g. building paths with
   many points, drawing many shapes).  Each call listed in
   *BATCHABLE-OPS* is shadowed by a macrolet that expands to the
   lock-free variant, so the transformation happens at compile time
   with no runtime overhead.

   Example:
     (with-batched-graphics (canvas)
       (new-path canvas)
       (move-to canvas x0 y0)
       (dolist (pt points)
         (line-to canvas (car pt) (cdr pt)))
       (stroke canvas))"
  (let ((c (gensym "CANVAS"))
	;; macros that bind function op to op/tus,
	;; creating a substitution in BODY of op/tus for op
        (macrolets (loop for op in *batchable-ops*
                         for tus = (make-tus-name op)
                         collect `(,op (&rest args) (list* ',tus args)))))
    `(let ((,c (as-canvas ,canvas)))
       (bt2:with-recursive-lock-held ((canvas-lock ,c))
         (macrolet ,macrolets
           ,@body)))))

(defmacro defun/tus (name (canvas &rest args) &body body)
  "Define NAME/TUS (thread-unsafe) with BODY, and NAME as a locking wrapper.
   The first argument must be the canvas.  Docstrings and declarations are
   handled correctly; &key and &optional in ARGS are supported."
  (let ((tus-name (make-tus-name name))
        (doc nil)
        (decls nil)
        (real-body body)
        (call-args nil)
        (in-key nil))
    ;; Build call-args, handling &key specially
    (dolist (a args)
      (cond ((eq a '&key) (setf in-key t))
            ((member a lambda-list-keywords) nil)  ; ignore other keywords
            (in-key
             ;; For &key args, emit :keyword value
             (let ((var (if (consp a) (car a) a)))
               (push (intern (symbol-name var) :keyword) call-args)
               (push var call-args)))
            (t
             ;; For required and &optional args, just the variable name
             (push (if (consp a) (car a) a) call-args))))
    (setf call-args (nreverse call-args))
    ;; Extract docstring
    (when (and (stringp (car real-body)) (cdr real-body))
      (setf doc (pop real-body)))
    ;; Extract declarations
    (loop while (and (consp (car real-body))
                     (eq (caar real-body) 'declare))
          do (push (pop real-body) decls))
    (setf decls (nreverse decls))
    `(progn
       (defun ,tus-name (,canvas ,@args)
         ,@(when doc (list (concatenate 'string doc " (thread-unsafe; caller holds lock)")))
         ,@decls
         ,@real-body)
       (defun ,name (,canvas ,@args)
         ,@(when doc (list doc))
         ,@decls
         (with-canvas-lock (,canvas)
           (,tus-name ,canvas ,@call-args))))))

(defmacro define-path-op/tus (name c-name (&rest args) &optional doc)
  "Define NAME/TUS as a direct CFFI path call, and NAME as a locking wrapper.
   ARGS are converted to doubles.  Both are declared inline."
  (let ((tus-name (make-tus-name name)))
    `(progn
       (declaim (inline ,tus-name ,name))
       (defun ,tus-name (canvas ,@args)
         ,@(when doc (list (concatenate 'string doc " (thread-unsafe)")))
         (cffi:foreign-funcall ,c-name :pointer (%pointer canvas)
                               ,@(loop for a in args
                                       append `(:double (float ,a 1d0)))
                               :void))
       (defun ,name (canvas ,@args)
         ,@(when doc (list doc))
         (with-canvas-lock (canvas)
           (,tus-name canvas ,@args))))))

;;; ============================================================
;;; Pixel buffers
;;; ============================================================
;;;
;;; A PIXEL-BUFFER is raster memory a canvas can draw into, in cairo's
;;; layout: HEIGHT rows of STRIDE bytes, top row first.  FORMAT is
;;;   :ARGB32  one 32-bit native-endian word per pixel, alpha in the top
;;;            byte, colors premultiplied by alpha (bytes B G R A on a
;;;            little-endian machine)
;;;   :RGB24   same word, top byte unused
;;;   :A8      one alpha byte per pixel
;;; STRIDE is the row length in bytes, padded to a multiple of 4.
;;;
;;; OCTET-PIXEL-BUFFER holds the pixels in a Lisp (UNSIGNED-BYTE 8)
;;; vector, copied to and from the canvas on open, FLUSH-CANVAS and
;;; close.  FOREIGN-PIXEL-BUFFER is foreign memory that the canvas draws
;;; into directly; it must stay allocated while the canvas is open.

(defun format-bytes-per-pixel (format)
  (ecase format ((:argb32 :rgb24) 4) (:a8 1)))

(defun format-stride (format width)
  "Smallest row length in bytes for WIDTH pixels of FORMAT."
  (* 4 (ceiling (* width (format-bytes-per-pixel format)) 4)))

(defclass pixel-buffer ()
  ((width  :initarg :width  :reader pixel-buffer-width)
   (height :initarg :height :reader pixel-buffer-height)
   (format :initarg :format :initform :argb32 :reader pixel-buffer-format)
   (stride :initarg :stride :initform nil :reader pixel-buffer-stride))
  (:documentation "Pixels in memory that a :BUFFER canvas draws into."))

(defmethod initialize-instance :after ((buffer pixel-buffer) &key)
  (with-slots (width format stride) buffer
    (let ((min-stride (format-stride format width)))
      (cond ((null stride) (setf stride min-stride))
            ((or (< stride min-stride) (/= 0 (mod stride 4)))
             (error "Stride ~A is invalid for ~A pixels of ~S."
                    stride width format))))))

(defun pixel-buffer-size (buffer)
  "Size of the pixel data in bytes."
  (* (pixel-buffer-stride buffer) (pixel-buffer-height buffer)))

(defmethod print-object ((buffer pixel-buffer) stream)
  (print-unreadable-object (buffer stream :type t :identity t)
    (format stream "~S ~Ax~A" (pixel-buffer-format buffer)
            (pixel-buffer-width buffer) (pixel-buffer-height buffer))))

(defclass octet-pixel-buffer (pixel-buffer)
  ((data :initarg :data :reader pixel-buffer-data))
  (:documentation "A pixel buffer in a Lisp array of octets (see
   MAKE-OCTET-PIXEL-BUFFER)."))

(defun make-octet-pixel-buffer (width height &key (format :argb32) data)
  "Make a pixel buffer held in a Lisp vector.  DATA, if given, must be a
   (SIMPLE-ARRAY (UNSIGNED-BYTE 8) (*)) that CFFI can share with foreign
   code (see CFFI:MAKE-SHAREABLE-BYTE-VECTOR); its contents are kept."
  (let* ((buffer (make-instance 'octet-pixel-buffer
                                :width width :height height :format format))
         (size (pixel-buffer-size buffer)))
    (cond ((null data)
           (setf data (cffi:make-shareable-byte-vector size))
           (fill data 0))
          ((not (typep data '(simple-array (unsigned-byte 8) (*))))
           (error "DATA must be a (SIMPLE-ARRAY (UNSIGNED-BYTE 8) (*))."))
          ((< (length data) size)
           (error "DATA has ~A bytes; ~A are needed." (length data) size)))
    (setf (slot-value buffer 'data) data)
    buffer))

(defclass foreign-pixel-buffer (pixel-buffer)
  ((pointer :initarg :pointer :reader pixel-buffer-pointer)
   (owned-p :initarg :owned-p :initform nil :reader pixel-buffer-owned-p))
  (:documentation "A pixel buffer in foreign memory (see MAKE-FOREIGN-PIXEL-BUFFER)."))

(defun make-foreign-pixel-buffer (width height &key (format :argb32) stride
                                                    pointer)
  "Make a pixel buffer in foreign memory.  With POINTER, use that memory
   (STRIDE defaults to the minimum); otherwise allocate zeroed memory,
   to be released with FREE-PIXEL-BUFFER."
  (let ((buffer (make-instance 'foreign-pixel-buffer
                               :width width :height height :format format
                               :stride stride)))
    (setf (slot-value buffer 'pointer)
          (or pointer
              (cffi:foreign-alloc :uint8 :count (pixel-buffer-size buffer)
                                         :initial-element 0))
          (slot-value buffer 'owned-p) (null pointer))
    buffer))

(defun free-pixel-buffer (buffer)
  "Release foreign memory allocated by MAKE-FOREIGN-PIXEL-BUFFER."
  (when (and (typep buffer 'foreign-pixel-buffer)
             (pixel-buffer-owned-p buffer)
             (pixel-buffer-pointer buffer))
    (cffi:foreign-free (pixel-buffer-pointer buffer))
    (setf (slot-value buffer 'pointer) nil))
  buffer)

(defun copy-bytes (to from count)
  (cffi:foreign-funcall "memcpy" :pointer to :pointer from :size count
                                 :pointer))

(defgeneric make-buffer-surface (buffer)
  (:documentation "Return an image surface drawing into BUFFER."))

(defgeneric sync-buffer (buffer surface)
  (:documentation "Make BUFFER hold everything drawn so far on SURFACE."))

(defmethod make-buffer-surface ((buffer foreign-pixel-buffer))
  (cl-cairo2:create-image-surface-for-data
   (or (pixel-buffer-pointer buffer) (error "~A has been freed." buffer))
   (pixel-buffer-format buffer) (pixel-buffer-width buffer)
   (pixel-buffer-height buffer) (pixel-buffer-stride buffer)))

(defmethod sync-buffer ((buffer foreign-pixel-buffer) surface)
  (cl-cairo2:surface-flush surface))

(defmethod make-buffer-surface ((buffer octet-pixel-buffer))
  (let ((surface (cl-cairo2:create-image-surface
                  (pixel-buffer-format buffer) (pixel-buffer-width buffer)
                  (pixel-buffer-height buffer))))
    (assert (= (cl-cairo2:image-surface-get-stride surface)
               (pixel-buffer-stride buffer)))
    (cl-cairo2:surface-flush surface)
    (cffi:with-pointer-to-vector-data (data (pixel-buffer-data buffer))
      (copy-bytes (cl-cairo2:image-surface-get-data surface :pointer-only t)
                  data (pixel-buffer-size buffer)))
    (cl-cairo2:surface-mark-dirty surface)
    surface))

(defmethod sync-buffer ((buffer octet-pixel-buffer) surface)
  (cl-cairo2:surface-flush surface)
  (cffi:with-pointer-to-vector-data (data (pixel-buffer-data buffer))
    (copy-bytes data (cl-cairo2:image-surface-get-data surface :pointer-only t)
                (pixel-buffer-size buffer))))

(defgeneric pixel-word (buffer offset)
  (:documentation "The native-endian 32-bit word at byte OFFSET of BUFFER."))

(defmethod pixel-word ((buffer foreign-pixel-buffer) offset)
  (cffi:mem-ref (pixel-buffer-pointer buffer) :uint32 offset))

(defvar *little-endian-p*
  (cffi:with-foreign-object (p :uint32)
    (setf (cffi:mem-ref p :uint32) 1)
    (= 1 (cffi:mem-ref p :uint8))))

(defmethod pixel-word ((buffer octet-pixel-buffer) offset)
  (let ((data (pixel-buffer-data buffer)))
    (loop for i below 4
          for byte = (aref data (+ offset (if *little-endian-p* i (- 3 i))))
          sum (ash byte (* 8 i)))))

(defun pixel-ref (buffer x y)
  "Return values R G B A (0-255, as stored, i.e. premultiplied) of the
   pixel at column X, row Y.  Reflects drawing up to the last
   FLUSH-CANVAS or CLOSE-CANVAS."
  (let ((row (* y (pixel-buffer-stride buffer))))
    (if (eq (pixel-buffer-format buffer) :a8)
        (values 0 0 0 (if (typep buffer 'octet-pixel-buffer)
                          (aref (pixel-buffer-data buffer) (+ row x))
                          (cffi:mem-ref (pixel-buffer-pointer buffer) :uint8
                                        (+ row x))))
        (let ((word (pixel-word buffer (+ row (* 4 x)))))
          (values (ldb (byte 8 16) word) (ldb (byte 8 8) word)
                  (ldb (byte 8 0) word)
                  (if (eq (pixel-buffer-format buffer) :rgb24)
                      255
                      (ldb (byte 8 24) word)))))))

;;; ============================================================
;;; Canvas
;;; ============================================================

(defclass canvas ()
  ((output-type :initarg :output-type :reader canvas-output-type)
   (destination :initarg :destination :reader canvas-destination)
   (width       :initarg :width       :reader canvas-width)
   (height      :initarg :height      :reader canvas-height)
   (background  :initarg :background  :reader canvas-background)
   (resolution  :initarg :resolution  :initform 1 :reader canvas-resolution
                :documentation "Pixels per canvas unit of a :PNG canvas.")
   (surface     :initform nil :accessor canvas-surface)
   (context     :initform nil :accessor canvas-context)
   (pointer     :initform nil :accessor canvas-pointer
                :documentation "The context's raw cairo_t pointer, for the
                                direct path calls below (see Paths).")
   (font        :initform nil :accessor canvas-font
                :documentation "The FONT last made current by APPLY-FONT,
                                saved and restored with the drawing state.")
   (lock        :initform (bt2:make-recursive-lock :name "canvas")
                :reader canvas-lock
                :documentation "Recursive lock for thread-safe canvas access."))
  (:documentation "Something to draw on with Cairo: a PDF, PostScript, SVG or PNG
   file, or a pixel buffer (see OPEN-CANVAS)."))

(defmethod print-object ((canvas canvas) stream)
  (print-unreadable-object (canvas stream :type t :identity t)
    (format stream "~A ~Ax~A ~S~:[ closed~;~]"
            (canvas-output-type canvas) (canvas-width canvas)
            (canvas-height canvas) (canvas-destination canvas)
            (canvas-context canvas))))

(defun canvas-open-p (canvas)
  "True until CANVAS is closed."
  (not (null (canvas-context canvas))))

(defun %context (canvas)
  (or (canvas-context canvas)
      (error "~A is closed." canvas)))

(declaim (inline %pointer))
(defun %pointer (canvas)
  ;; NIL once closed, so a closed canvas errors here instead of handing
  ;; cairo a freed pointer.
  (or (canvas-pointer canvas)
      (error "~A is closed." canvas)))

(defgeneric as-canvas (designator)
  (:documentation "The canvas DESIGNATOR stands for: a canvas itself, or
   something drawn on one (a viewport stands for its device).  The
   WITH-STYLE family of macros accepts any designator.")
  (:method ((canvas canvas)) canvas))

(defmacro with-saved-state ((canvas) &body body)
  "Run BODY, then restore color, line style, font, transformation and clip.
   Thread-safe: holds the canvas lock for the duration."
  (let ((c (gensym "CANVAS")) (font (gensym "FONT")))
    `(let* ((,c (as-canvas ,canvas))
            (,font (canvas-font ,c)))
       (with-canvas-lock (,c)
         (cl-cairo2:save (%context ,c))
         (unwind-protect (progn ,@body)
           (cl-cairo2:restore (%context ,c))
           (setf (canvas-font ,c) ,font))))))

(defun paint-background (canvas)
  (let ((background (canvas-background canvas)))
    (when background
      (with-saved-state (canvas)
        (set-color canvas background)
        (paint canvas)))))

(defvar *default-font* :normal
  "Font spec (see PARSE-FONT in fonts.lisp) a new canvas starts with.")

(defun open-canvas (output-type destination &key (width 600) (height width)
                                                 (background :white)
                                                 (font *default-font*)
                                                 (resolution 1)
                                                 (class 'canvas) initargs)
  "Open a canvas of OUTPUT-TYPE drawing to DESTINATION:
     :PDF, :PS, :SVG or :PNG   DESTINATION is a filename
     :BUFFER                   DESTINATION is a PIXEL-BUFFER
   WIDTH and HEIGHT are in points, or pixels for :PNG; a buffer's own
   size is used instead.  A :PNG or :BUFFER canvas has RESOLUTION
   pixels per unit (e.g. 2 for a sharp picture on a high-resolution
   screen): a PNG image is RESOLUTION times WIDTH x HEIGHT pixels, a
   buffer's pixels are RESOLUTION times the canvas's units, and
   everything drawn, line widths and text included, is scaled up with
   it.  BACKGROUND is a color spec painted on each page, or NIL to
   leave it transparent (or a buffer's contents intact).
   FONT is the initial font spec.  CLASS, a subclass of CANVAS, is
   instantiated with the extra INITARGS (a plist)."
  (unless (member output-type '(:png :buffer))
    (setf resolution 1))
  (when (eq output-type :buffer)
    (check-type destination pixel-buffer)
    (setf width (/ (pixel-buffer-width destination) resolution)
          height (/ (pixel-buffer-height destination) resolution)))
  (let* ((w (coerce width 'double-float))
         (h (coerce height 'double-float))
         (canvas (apply #'make-instance class :output-type output-type
                                              :destination destination
                                              :width width :height height
                                              :background background
                                              :resolution resolution
                                              initargs))
         (path (unless (eq output-type :buffer)
                 (namestring (merge-pathnames destination)))))
    (setf (canvas-surface canvas)
          (ecase output-type
            (:pdf (cl-cairo2:create-pdf-surface path w h))
            (:ps  (cl-cairo2:create-ps-surface path w h))
            (:svg (cl-cairo2:create-svg-surface path w h))
            (:png (cl-cairo2:create-image-surface :argb32 (round (* resolution width))
                                                         (round (* resolution height))))
            (:buffer (make-buffer-surface destination))))
    (setf (canvas-context canvas)
          (cl-cairo2:create-context (canvas-surface canvas))
          ;; GET-POINTER is cl-cairo2's (unexported) reader for the cairo_t
          (canvas-pointer canvas)
          (cl-cairo2::get-pointer (canvas-context canvas)))
    (reset-transform canvas)                ; the RESOLUTION
    (paint-background canvas)
    (set-font canvas font)
    canvas))

(defun close-canvas (canvas)
  "Finish the output and release the canvas.  Safe to call twice."
  (when (canvas-context canvas)
    (with-canvas-lock (canvas)
      (with-backend-floats
       (unwind-protect
            (case (canvas-output-type canvas)
              (:png (cl-cairo2:surface-write-to-png
                     (canvas-surface canvas)
                     (namestring (merge-pathnames (canvas-destination canvas)))))
              (:buffer (sync-buffer (canvas-destination canvas)
                                    (canvas-surface canvas))))
         ;; drop the raw pointer first: it dangles once the context is destroyed
         (setf (canvas-pointer canvas) nil)
         (cl-cairo2:destroy (canvas-context canvas))
         (cl-cairo2:destroy (canvas-surface canvas))  ; finishes vector output
         (setf (canvas-context canvas) nil
               (canvas-surface canvas) nil)))))
  canvas)

(defmacro with-opacity-group ((canvas opacity) &body body)
  "Draw BODY into a separate layer, then blend the layer onto CANVAS at
   OPACITY (0..1), so overlapping drawing in BODY is uniformly
   translucent instead of darker where it overlaps.  At OPACITY 1, BODY
   is simply drawn."
  (let ((c (gensym "CANVAS")) (a (gensym "OPACITY")))
    `(let ((,c (as-canvas ,canvas)) (,a ,opacity))
       (if (>= ,a 1)
           (progn ,@body)
           ;; popping the group sets the source, so keep it inside a
           ;; saved state
           (with-saved-state (,c)
             (cl-cairo2:push-group (%context ,c))
             (unwind-protect (progn ,@body)
               (cl-cairo2:pop-group-to-source (%context ,c))
               (with-backend-floats
                 (cl-cairo2:paint-with-alpha ,a (%context ,c)))))))))

(defun set-document-metadata (canvas &key title author subject keywords creator
                                           create-date)
  "Set PDF document metadata (ignored for other output types).  Each is
   a string.  CREATE-DATE, ISO 8601 like \"2026-10-31T12:00:00Z\",
   defaults to the time of writing; \"\" omits it, which makes the
   output byte-for-byte reproducible."
  (when (eq (canvas-output-type canvas) :pdf)
    (with-canvas-lock (canvas)
      ;; cl-cairo2 has no binding; values are cairo_pdf_metadata_t
      (loop for (value key) in `((,title 0) (,author 1) (,subject 2) (,keywords 3)
                                 (,creator 4) (,create-date 5))
            when value
              do (cffi:foreign-funcall "cairo_pdf_surface_set_metadata"
                                       :pointer (cl-cairo2::get-pointer
                                                 (canvas-surface canvas))
                                       :int key :string value :void))))
  canvas)

(defmacro with-canvas ((var output-type destination &rest keys) &body body)
  "Bind VAR to a canvas opened with OPEN-CANVAS, closing it afterward."
  `(let ((,var (open-canvas ,output-type ,destination ,@keys)))
     (unwind-protect (progn ,@body)
       (close-canvas ,var))))

(defun flush-canvas (canvas)
  "Complete pending drawing; for a :BUFFER canvas, update the buffer."
  (with-canvas-lock (canvas)
    (with-backend-floats
      (if (eq (canvas-output-type canvas) :buffer)
          (sync-buffer (canvas-destination canvas) (canvas-surface canvas))
          (cl-cairo2:surface-flush (canvas-surface canvas)))))
  canvas)

(defun erase-canvas (canvas &optional (color (canvas-background canvas)))
  "Paint the whole of CANVAS with COLOR (default its background),
   replacing what was drawn; NIL makes it transparent."
  ;; with-saved-state acquires the lock; use /tus variants inside
  (with-saved-state (canvas)
    (reset-transform/tus canvas)
    (reset-clip/tus canvas)
    (with-backend-floats
      (if color
          (progn (set-color/tus canvas color)
                 (cl-cairo2:set-operator :source (%context canvas)))
          (cl-cairo2:set-operator :clear (%context canvas))))
    (paint/tus canvas)))

(defun next-page (canvas)
  "Emit the current page and start a new one (PDF and PS only)."
  (with-canvas-lock (canvas)
    (with-backend-floats (cl-cairo2:show-page (%context canvas)))
    (paint-background canvas)))

;;; ============================================================
;;; Drawing state
;;; ============================================================

(defun/tus set-color (canvas color)
  "Set the drawing color from a color spec (see PARSE-COLOR)."
  (let ((c (parse-color color)))
    (cl-cairo2:set-source-rgba (color-red c) (color-green c) (color-blue c)
                               (color-alpha c) (%context canvas))))

(defun/tus set-line-width (canvas width)
  "Set the line width, in canvas units (points for vector output, pixels
   for images)."
  (cl-cairo2:set-line-width width (%context canvas)))

(defun line-width (canvas)
  "The current line width."
  (with-canvas-lock (canvas)
    (cl-cairo2:get-line-width (%context canvas))))

(defun/tus set-line-cap (canvas cap)
  "CAP is :BUTT, :ROUND or :SQUARE."
  (cl-cairo2:set-line-cap cap (%context canvas)))

(defun/tus set-line-join (canvas join)
  "JOIN is :MITER, :ROUND or :BEVEL."
  (cl-cairo2:set-line-join join (%context canvas)))

(defun/tus set-dash (canvas dashes &optional (offset 0))
  "DASHES is a sequence of alternating on/off lengths; NIL or empty is solid."
  (cl-cairo2:set-dash offset (map 'vector (lambda (d) (coerce d 'double-float))
                                  dashes)
                      (%context canvas)))

(defun fill-rule (canvas)
  "The current fill rule, :WINDING or :EVEN-ODD."
  (with-canvas-lock (canvas)
    (ecase (cl-cairo2:get-fill-rule (%context canvas))
      (:winding :winding)
      (:odd :even-odd))))

(defun/tus set-fill-rule (canvas rule)
  "RULE is :WINDING or :EVEN-ODD; it governs filling and clipping."
  (cl-cairo2:set-fill-rule (ecase rule (:winding :winding) (:even-odd :odd))
                           (%context canvas)))

(defun/tus translate (canvas dx dy)
  "Move the origin of canvas coordinates by DX, DY."
  (cl-cairo2:translate dx dy (%context canvas)))

(defun/tus rotate (canvas radians)
  "Rotate canvas coordinates by RADIANS (clockwise on the page, as canvas
   y runs down)."
  (cl-cairo2:rotate radians (%context canvas)))

(defun/tus scale (canvas sx &optional (sy sx))
  "Scale canvas coordinates by SX and SY (default SX); line widths scale
   too."
  (cl-cairo2:scale sx sy (%context canvas)))

(defun/tus reset-transform (canvas)
  "Return to the canvas's native coordinates (WIDTH x HEIGHT units,
   whatever its resolution)."
  (cl-cairo2:reset-trans-matrix (%context canvas))
  (let ((r (canvas-resolution canvas)))
    (unless (= r 1)
      (with-backend-floats (cl-cairo2:scale r r (%context canvas))))))

(defun/tus transform (canvas xx yx xy yy x0 y0)
  "Apply the affine transform x' = XX*x + XY*y + X0, y' = YX*x + YY*y + Y0
   to subsequent drawing (cairo's argument order)."
  (flet ((d (v) (float v 1d0)))
    (cl-cairo2:transform (cl-cairo2:make-trans-matrix
                          :xx (d xx) :yx (d yx) :xy (d xy) :yy (d yy)
                          :x0 (d x0) :y0 (d y0))
                         (%context canvas))))

(defmacro with-style ((canvas &key color line-width dash line-cap line-join)
                      &body body)
  "Run BODY with the given style settings, restoring them afterward.
   Omitted settings are left unchanged.  CANVAS may be a viewport (see
   AS-CANVAS)."
  (let ((c (gensym "CANVAS")))
    `(let ((,c (as-canvas ,canvas)))
       (with-saved-state (,c)
         ,@(when color      `((set-color ,c ,color)))
         ,@(when line-width `((set-line-width ,c ,line-width)))
         ,@(when dash       `((set-dash ,c ,dash)))
         ,@(when line-cap   `((set-line-cap ,c ,line-cap)))
         ,@(when line-join  `((set-line-join ,c ,line-join)))
         ,@body))))

(defmacro with-color ((canvas color) &body body)
  "Run BODY with the current color set to COLOR (see PARSE-COLOR),
   restoring it afterward."
  `(with-style (,canvas :color ,color) ,@body))

(defmacro with-line-width ((canvas width) &body body)
  "Run BODY with the line WIDTH (canvas units), restoring it afterward."
  `(with-style (,canvas :line-width ,width) ,@body))

(defmacro with-dash ((canvas dashes) &body body)
  "Run BODY with the dash pattern DASHES (see SET-DASH), restoring it
   afterward."
  `(with-style (,canvas :dash ,dashes) ,@body))

(defmacro with-translation ((canvas dx dy) &body body)
  "Run BODY with canvas coordinates moved by DX, DY, restoring them
   afterward."
  (let ((c (gensym "CANVAS")))
    `(let ((,c ,canvas))
       (with-saved-state (,c)
         (translate ,c ,dx ,dy)
         ,@body))))

(defmacro with-rotation ((canvas radians &key (x 0) (y 0)) &body body)
  "Run BODY rotated by RADIANS about the point (X,Y)."
  (let ((c (gensym "CANVAS")) (xv (gensym "X")) (yv (gensym "Y")))
    `(let ((,c ,canvas) (,xv ,x) (,yv ,y))
       (with-saved-state (,c)
         (translate ,c ,xv ,yv)
         (rotate ,c ,radians)
         (translate ,c (- ,xv) (- ,yv))
         ,@body))))

(defmacro with-scaling ((canvas sx &optional (sy sx)) &body body)
  "Run BODY scaled by SX, SY.  Note that this scales line widths too."
  (let ((c (gensym "CANVAS")) (sxv (gensym "SX")))
    `(let* ((,c ,canvas) (,sxv ,sx))
       (with-saved-state (,c)
         (scale ,c ,sxv ,(if (eq sy sx) sxv sy))
         ,@body))))

;;; ============================================================
;;; Paths
;;; ============================================================
;;;
;;; Path construction calls cairo directly through CFFI rather than
;;; through cl-cairo2, because it runs once per data point and
;;; cl-cairo2's wrapper is ~10x the cost of the call itself (measured
;;; on SBCL: ~70 ns vs ~7 ns per LINE-TO).  The wrapper's per-call work
;;; is a WITH-SLOTS lookup of the pointer, plus a second foreign call
;;; to cairo_status and an enum-table lookup to warn on errors.
;;;
;;; Skipping that status check loses nothing: a cairo context's error
;;; status is sticky (once in error it stays in error and ignores
;;; further drawing), and STROKE, FILL-PATH, CLIP etc. still go through
;;; cl-cairo2, which checks it, so an error while building a path is
;;; still reported when the path is used.

(define-path-op/tus new-path "cairo_new_path" ()
  "Start a new, empty path, discarding the current one.")
(define-path-op/tus new-sub-path "cairo_new_sub_path" ()
  "Begin a new subpath without a current point, so the next ARC does not
   join the previous one.")
(define-path-op/tus close-path "cairo_close_path" ()
  "Close the current subpath with a line back to its start.")
(define-path-op/tus move-to "cairo_move_to" (x y)
  "Begin a new subpath at the canvas point (X,Y).")
(define-path-op/tus line-to "cairo_line_to" (x y)
  "Add a line from the current point to the canvas point (X,Y).")
(define-path-op/tus rel-move-to "cairo_rel_move_to" (dx dy)
  "Begin a new subpath DX, DY from the current point.")
(define-path-op/tus rel-line-to "cairo_rel_line_to" (dx dy)
  "Add a line from the current point to the point DX, DY from it.")

(define-path-op/tus curve-to "cairo_curve_to" (x1 y1 x2 y2 x3 y3)
  "Cubic Bezier from the current point to (X3,Y3) with control points 1, 2.")

(define-path-op/tus arc "cairo_arc" (xc yc radius angle1 angle2)
  "Arc of increasing angle (radians) from ANGLE1 to ANGLE2.  With y down,
   increasing angle runs clockwise on the page.")

(define-path-op/tus arc-negative "cairo_arc_negative" (xc yc radius angle1 angle2)
  "Add a circular arc as ARC does, but from ANGLE1 to ANGLE2 in the
   direction of decreasing angle.")

(define-path-op/tus rectangle "cairo_rectangle" (x y width height)
  "Add a closed rectangle subpath with corner (X,Y) and the given WIDTH
   and HEIGHT, in canvas units.")

(defun current-point (canvas)
  "Return the current point as two values, or NIL if there is none."
  (with-canvas-lock (canvas)
    (let ((ctx (%context canvas)))
      (when (cl-cairo2:has-current-point ctx)
        (cl-cairo2:get-current-point ctx)))))

(defun/tus stroke (canvas)
  "Draw the current path's outline in the current color and line style,
   then clear the path."
  (with-backend-floats (cl-cairo2:stroke (%context canvas))))

(defun/tus stroke-preserve (canvas)
  "Draw the current path's outline as STROKE does, keeping the path."
  (with-backend-floats (cl-cairo2:stroke-preserve (%context canvas))))

(defun/tus fill-path (canvas)
  "Fill the current path with the current color (by the fill rule), then
   clear the path."
  (with-backend-floats (cl-cairo2:fill-path (%context canvas))))

(defun/tus fill-preserve (canvas)
  "Fill the current path as FILL-PATH does, keeping the path."
  (with-backend-floats (cl-cairo2:fill-preserve (%context canvas))))

(defun/tus paint (canvas)
  "Fill the whole clip region with the current color."
  (with-backend-floats (cl-cairo2:paint (%context canvas))))

(defun/tus finish-shape (canvas fill)
  "Fill (if FILL is true) or stroke the current path."
  (if fill (fill-path/tus canvas) (stroke/tus canvas)))

(defun/tus draw-line (canvas x1 y1 x2 y2)
  "Stroke a line between the canvas points (X1,Y1) and (X2,Y2)."
  (new-path/tus canvas)
  (move-to/tus canvas x1 y1)
  (line-to/tus canvas x2 y2)
  (stroke/tus canvas))

(defun/tus polyline-path (canvas xs ys)
  "Start a new path through the points of sequences XS and YS."
  (new-path/tus canvas)
  (let ((first t))
    (map nil (lambda (x y)
               (if first
                   (progn (move-to/tus canvas x y) (setf first nil))
                   (line-to/tus canvas x y)))
         xs ys)))

(defun/tus draw-polyline (canvas xs ys)
  "Stroke line segments joining the points of sequences XS and YS."
  (polyline-path/tus canvas xs ys)
  (stroke/tus canvas))

(defun/tus draw-polygon (canvas xs ys &key fill)
  "Stroke, or fill if FILL, the closed polygon with vertices XS, YS."
  (polyline-path/tus canvas xs ys)
  (close-path/tus canvas)
  (finish-shape/tus canvas fill))

(defun/tus draw-rectangle (canvas x y width height &key fill)
  "Stroke, or fill if FILL, the rectangle with corner (X,Y), WIDTH and
   HEIGHT in canvas units."
  (new-path/tus canvas)
  (rectangle/tus canvas x y width height)
  (finish-shape/tus canvas fill))

(defun/tus draw-circle (canvas xc yc radius &key fill)
  "Stroke, or fill if FILL, the circle of RADIUS about the canvas point
   (XC,YC)."
  (new-path/tus canvas)
  (arc/tus canvas xc yc radius 0 (* 2 pi))
  (close-path/tus canvas)
  (finish-shape/tus canvas fill))

;;; ============================================================
;;; Clipping
;;; ============================================================

(defun/tus clip (canvas)
  "Intersect the clip region with the current path, consuming the path."
  (with-backend-floats (cl-cairo2:clip (%context canvas))))

(defun/tus clip-preserve (canvas)
  "Intersect the clip with the current path as CLIP does, keeping the
   path."
  (with-backend-floats (cl-cairo2:clip-preserve (%context canvas))))

(defun/tus reset-clip (canvas)
  "Remove all clipping (prefer scoping clips with WITH-CLIP)."
  (cl-cairo2:reset-clip (%context canvas)))

(defun clip-extents (canvas)
  "Bounding box of the clip region as values X1 Y1 X2 Y2."
  (with-canvas-lock (canvas)
    (cl-cairo2:clip-extents (%context canvas))))

(defun in-clip-p (canvas x y)
  "True if the canvas point (X,Y) is inside the current clip."
  (with-canvas-lock (canvas)
    ;; defined but not exported by cl-cairo2
    (cl-cairo2::in-clip x y (%context canvas))))

(defmacro with-clip ((canvas &key (fill-rule :winding)) (&body path-forms)
                     &body body)
  "Run BODY clipped to the path built by PATH-FORMS.  With FILL-RULE
   :EVEN-ODD, overlapping subpaths cut holes, so e.g. a canvas-sized
   rectangle plus a circle clips to the outside of the circle."
  (let ((c (gensym "CANVAS")) (rule (gensym "RULE")))
    `(let ((,c ,canvas))
       (with-saved-state (,c)
         (new-path ,c)
         ,@path-forms
         (let ((,rule (cl-cairo2:get-fill-rule (%context ,c))))
           (set-fill-rule ,c ,fill-rule)
           (clip ,c)
           (cl-cairo2:set-fill-rule ,rule (%context ,c)))
         ,@body))))

;;; ============================================================
;;; Images
;;; ============================================================

(defun/tus draw-argb-image (canvas pixels &key (filter :nearest))
  "Paint PIXELS, a (SIMPLE-ARRAY (UNSIGNED-BYTE 32) (H W)) of premultiplied
   ARGB words (see ARGB-WORD), so that pixel (row r, column c) covers
   the unit square [c,c+1] x [r,r+1] of the current user space.  Set the
   transform first to place it.  FILTER :NEAREST shows pixels as
   squares; :BILINEAR smooths between their centers."
  (declare (type (simple-array (unsigned-byte 32) (* *)) pixels))
  (destructuring-bind (h w) (array-dimensions pixels)
    (let ((surface (cl-cairo2:create-image-surface :argb32 w h)))
      (unwind-protect
           (progn
             (cl-cairo2:surface-flush surface)
             (let ((data (cl-cairo2:image-surface-get-data surface :pointer-only t))
                   (words-per-row (/ (cl-cairo2:image-surface-get-stride surface) 4)))
               (dotimes (r h)
                 (let ((base (* r words-per-row)))
                   (dotimes (c w)
                     (setf (cffi:mem-aref data :uint32 (+ base c)) (aref pixels r c))))))
             (cl-cairo2:surface-mark-dirty surface)
             (cl-cairo2:save (%context canvas))
             (unwind-protect
                  (progn
                    (cl-cairo2:set-source-surface surface 0 0 (%context canvas))
                    ;; cl-cairo2 has no pattern filter/extend setters; values are
                    ;; cairo_filter_t NEAREST=3, BILINEAR=4 and cairo_extend_t PAD=3
                    ;; (PAD keeps bilinear edges from fading to transparent)
                    (let ((pattern (cffi:foreign-funcall "cairo_get_source"
                                                         :pointer (%pointer canvas) :pointer)))
                      (cffi:foreign-funcall "cairo_pattern_set_filter" :pointer pattern
                                            :int (ecase filter (:nearest 3) (:bilinear 4)) :void)
                      (cffi:foreign-funcall "cairo_pattern_set_extend" :pointer pattern
                                            :int 3 :void))
                    (new-path/tus canvas)
                    (rectangle/tus canvas 0 0 w h)
                    (fill-path/tus canvas))
               (cl-cairo2:restore (%context canvas))))
        (cl-cairo2:destroy surface))))
  canvas)

(defun/tus paint-mesh (canvas patches)
  "Paint PATCHES as one cairo mesh gradient: each is (XS YS COLORS), four
   corners in user space and their colors as (R G B A) lists, with
   colors blended smoothly across the patch.  Later patches paint over
   earlier ones, and adjacent patches meet without antialiasing seams."
  ;; cl-cairo2 has no mesh pattern binding, so this calls cairo directly
  (flet ((d (v) (float v 1d0)))
    (let ((pattern (cffi:foreign-funcall "cairo_pattern_create_mesh" :pointer)))
      (unwind-protect
           (progn
             (dolist (patch patches)
               (destructuring-bind (xs ys colors) patch
                 (cffi:foreign-funcall "cairo_mesh_pattern_begin_patch" :pointer pattern :void)
                 (loop for x in xs for y in ys for k from 0
                       do (if (zerop k)
                              (cffi:foreign-funcall "cairo_mesh_pattern_move_to" :pointer pattern
                                                    :double (d x) :double (d y) :void)
                              (cffi:foreign-funcall "cairo_mesh_pattern_line_to" :pointer pattern
                                                    :double (d x) :double (d y) :void)))
                 (loop for (r g b a) in colors for k from 0
                       do (cffi:foreign-funcall "cairo_mesh_pattern_set_corner_color_rgba"
                                                :pointer pattern :unsigned-int k
                                                :double (d r) :double (d g) :double (d b)
                                                :double (d (or a 1)) :void))
                 (cffi:foreign-funcall "cairo_mesh_pattern_end_patch" :pointer pattern :void)))
             (cl-cairo2:save (%context canvas))
             (unwind-protect
                  (progn
                    (cffi:foreign-funcall "cairo_set_source" :pointer (%pointer canvas)
                                          :pointer pattern :void)
                    (with-backend-floats
                      (cffi:foreign-funcall "cairo_paint" :pointer (%pointer canvas) :void)))
               (cl-cairo2:restore (%context canvas))))
        (cffi:foreign-funcall "cairo_pattern_destroy" :pointer pattern :void))))
  canvas)

;;; ============================================================
;;; Text
;;; ============================================================

(defun/tus set-system-font (canvas family &key (slant :normal) (weight :normal))
  "Use the system font FAMILY (e.g. \"DejaVu Sans\").  SLANT is :NORMAL,
   :ITALIC or :OBLIQUE; WEIGHT is :NORMAL or :BOLD."
  (with-backend-floats
    (cl-cairo2:select-font-face family slant weight (%context canvas))))

(defun/tus set-hershey-font (canvas name
                             &key (pen-width cl-cairo-hershey-fonts:*default-pen-width*))
  "Use the Hershey font NAME (e.g. \"rowmans\").  PEN-WIDTH, a fraction
   of the em, applies to stroke fonts."
  (with-backend-floats
    (cl-cairo-hershey-fonts:use-font name :pen-width pen-width
                                          :context (%context canvas))))

(defun/tus set-font-size (canvas size)
  "SIZE is the em size in canvas units."
  (cl-cairo2:set-font-size size (%context canvas)))

(defun/tus show-text (canvas string)
  "Draw STRING with its baseline origin at the current point, advancing it."
  (with-backend-floats (cl-cairo2:show-text string (%context canvas))))

(defun/tus draw-text (canvas x y string)
  "Draw STRING with its baseline origin at (X,Y)."
  (new-path/tus canvas)
  (move-to/tus canvas x y)
  (show-text/tus canvas string)
  (new-path/tus canvas))

(defun char-glyph-index (canvas char)
  "The current font's glyph index for CHAR; 0 means it has no glyph.
   (A missing glyph cannot be detected from its extents: system fonts
   draw it as a box with a normal advance.)"
  (with-canvas-lock (canvas)
    (let ((scaled-font (cffi:foreign-funcall "cairo_get_scaled_font"
                                             :pointer (%pointer canvas) :pointer)))
      (cffi:with-foreign-objects ((glyphs :pointer) (count :int))
        (setf (cffi:mem-ref glyphs :pointer) (cffi:null-pointer)
              (cffi:mem-ref count :int) 0)
        (cffi:with-foreign-string ((utf8 bytes) (string char)
                                   :encoding :utf-8 :null-terminated-p nil)
          (let ((status (with-backend-floats
                          (cffi:foreign-funcall "cairo_scaled_font_text_to_glyphs"
                                                :pointer scaled-font
                                                :double 0d0 :double 0d0
                                                :pointer utf8 :int bytes
                                                :pointer glyphs :pointer count
                                                :pointer (cffi:null-pointer)
                                                :pointer (cffi:null-pointer)
                                                :pointer (cffi:null-pointer)
                                                :int))))
            (if (and (zerop status) (plusp (cffi:mem-ref count :int)))
                ;; cairo_glyph_t starts with its unsigned long index
                (prog1 (cffi:mem-ref (cffi:mem-ref glyphs :pointer) :unsigned-long)
                  (cffi:foreign-funcall "cairo_glyph_free"
                                        :pointer (cffi:mem-ref glyphs :pointer) :void))
                0)))))))

(defun/tus text-path (canvas string)
  "Add the outlines of STRING's glyphs, in the current font, to the path
   at the current point (stroke fonts give the outlines of their strokes)."
  (with-backend-floats (cl-cairo2:text-path string (%context canvas))))

(defun text-extents (canvas string)
  "Return values X-BEARING Y-BEARING WIDTH HEIGHT X-ADVANCE Y-ADVANCE
   of STRING in the current font."
  (with-canvas-lock (canvas)
    (with-backend-floats (cl-cairo2:text-extents string (%context canvas)))))

(defun font-extents (canvas)
  "Return values ASCENT DESCENT HEIGHT of the current font."
  (with-canvas-lock (canvas)
    (let ((fe (with-backend-floats
                (cl-cairo2:get-font-extents (%context canvas)))))
      (values (cl-cairo2:font-ascent fe)
              (cl-cairo2:font-descent fe)
              (cl-cairo2:font-height fe)))))
