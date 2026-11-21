;;;; Write the reference chapter of the manual from the docstrings.
;;;;
;;;;   sbcl --non-interactive --load docs/tools/make-reference.lisp
;;;;
;;;; docs/src/10-reference.md: every exported function, macro, variable and class of
;;;; cl-cairoplot and cl-cairoplot-gtk, with its docstring, grouped by source file.  Slot
;;;; accessors without a docstring of their own show their slot's documentation.  Names
;;;; that have no documentation at all are listed on the terminal.  Development tool:
;;;; needs SBCL (sb-introspect); loads cl-cairoplot-gtk, so cl-ez-gtk-server.

(require :asdf)
(require :sb-introspect)
(defvar *docs* (merge-pathnames "../" (make-pathname :name nil :type nil :defaults *load-truename*)))
(asdf:load-system "cl-cairoplot-gtk" :verbose nil)

(defpackage #:make-reference
  (:use #:cl))
(in-package #:make-reference)

(defvar *docs* cl-user::*docs*)

(defparameter *files*
  '(("plot-device" "Devices and coordinates")
    ("viewport" "Viewports, windows and targets")
    ("layout" "Panels and insets")
    ("drawing" "Drawing in any coordinates")
    ("markers" "Markers")
    ("axes" "Axes")
    ("time-axes" "Date and time axes")
    ("legend" "Legends")
    ("charts" "Error bars, areas, histograms and arrows")
    ("categorical" "Bar charts, box plots and violin plots")
    ("areas" "Step plots and stacked areas")
    ("pie" "Pie and donut charts")
    ("heatmap" "Heatmaps")
    ("image" "Images and colorbars")
    ("contours" "Contours")
    ("vectors" "Vector fields")
    ("polar" "Polar plots")
    ("corner" "Corner plots")
    ("plot3d" "3D surfaces")
    ("sky" "Sky projections and WCS")
    ("clipping" "Clipping regions")
    ("colors" "Colors")
    ("colormaps" "Colormaps")
    ("fonts" "Fonts, text and halos")
    ("cairo-graphics" "Canvases, paths and drawing state")
    ("cl-cairoplot-gtk" "Windows (system cl-cairoplot-gtk)")
    ("demo" "Windows (system cl-cairoplot-gtk)"))
  "Source files in the order of the chapter, with their section titles.")

(defparameter *packages* '(:cl-cairoplot :cl-cairoplot-gtk))

(defvar *undocumented* '())

(defun source-file (symbol kind)
  (let ((source (first (sb-introspect:find-definition-sources-by-name symbol kind))))
    (and source (sb-introspect:definition-source-pathname source)
         (pathname-name (sb-introspect:definition-source-pathname source)))))

(defun reader-info (symbol)
  "For a slot accessor (of a class or a structure): its class and the slot's
documentation and name, else NIL."
  (let ((f (fdefinition symbol)))
    (cond ((typep f 'generic-function)
           (let ((m (find-if (lambda (m) (typep m 'sb-mop:standard-reader-method))
                             (sb-mop:generic-function-methods f))))
             (when m
               (let ((slot (sb-mop:accessor-method-slot-definition m)))
                 (values (class-name (first (sb-mop:method-specializers m)))
                         (documentation slot t)
                         (sb-mop:slot-definition-name slot))))))
          (t
           ;; a structure accessor: STRUCT-SLOT
           (loop for class in (list-all-structures)
                 for prefix = (format nil "~a-" (symbol-name class))
                 when (and (eql 0 (search prefix (symbol-name symbol)))
                           (find (subseq (symbol-name symbol) (length prefix))
                                 (sb-mop:class-slots (find-class class))
                                 :key (lambda (s) (symbol-name (sb-mop:slot-definition-name s)))
                                 :test #'string=))
                   return (values class nil (intern (subseq (symbol-name symbol) (length prefix)))))))))

(defun list-all-structures ()
  (let ((out '()))
    (dolist (p *packages* out)
      (do-symbols (s p)
        (let ((c (find-class s nil)))
          (when (and c (typep c 'structure-class) (eq (symbol-package s) (find-package p)))
            (pushnew s out)))))))

(defun function-file (symbol kind)
  (or (source-file symbol kind)
      (source-file symbol :generic-function)
      (source-file symbol :structure-accessor)
      (let ((class (reader-info symbol)))
        (and class (or (source-file class :class) (source-file class :structure))))))

(defun documented-symbols ()
  "(file kind symbol) for the exported functions, macros, variables and classes."
  (let ((entries '()))
    (dolist (p *packages* entries)
      (do-external-symbols (s p)
        (when (eq (symbol-package s) (find-package p))
          (cond ((fboundp s)
                 (let ((kind (if (macro-function s) :macro :function)))
                   (push (list (or (function-file s kind) "other") kind s) entries)))
                ((and (boundp s) (not (constantp s)))
                 (push (list (or (source-file s :variable) "other") :variable s) entries)))
          (let ((class (find-class s nil)))
            (when (and class (not (typep class 'built-in-class)))
              (push (list (or (source-file s :class) (source-file s :structure) "other") :class s)
                    entries))))))))

(defun lambda-list-text (symbol)
  "SYMBOL's lambda list for the reader: without default values, but with the
destructuring lists of macros."
  (let ((*print-case* :downcase) (*package* (find-package :cl-cairoplot)))
    (labels ((text (list)
               (let ((after-keyword nil))
                 (format nil "~{~a~^ ~}"
                         (mapcar (lambda (x)
                                   (cond ((member x lambda-list-keywords)
                                          (setf after-keyword t) (format nil "~a" x))
                                         ((and (listp x) after-keyword) (format nil "~a" (first x)))
                                         ((listp x) (format nil "(~a)" (text x)))
                                         (t (format nil "~a" x))))
                                 list)))))
      (let ((list (sb-introspect:function-lambda-list symbol)))
        (if (and (reader-info symbol) (typep (fdefinition symbol) 'generic-function))
            (format nil "~a" (reader-info symbol))
            (text list))))))

(defun words (symbol)
  (substitute #\Space #\- (string-downcase (symbol-name symbol))))

(defun function-doc (symbol)
  (or (documentation symbol 'function)
      (multiple-value-bind (class doc slot) (reader-info symbol)
        (when class
          (format nil "~:[The ~a of a ~a.~*~;~*~*~a~]~:[~; `setf` changes it.~]"
                  doc (words slot) (string-upcase (symbol-name class)) doc
                  (fboundp `(setf ,symbol)))))
      (progn (push symbol *undocumented*) "")))

(defun write-entry (kind symbol out)
  (let ((*print-case* :downcase))
    (ecase kind
      ((:function :macro)
       (format out "~%#### `(~(~a~)~@[ ~a~])`~@[ *macro*~]~%~%" symbol
               (let ((l (lambda-list-text symbol))) (and (plusp (length l)) l))
               (eq kind :macro))
       (format out "~a~%" (tidy (function-doc symbol))))
      (:variable
       (format out "~%#### `~(~a~)` *variable*~%~%~a~%" symbol
               (tidy (or (documentation symbol 'variable)
                         (progn (push symbol *undocumented*) "")))))
      (:class
       (format out "~%#### `~(~a~)` *~:[class~;structure~]*~%~%~a~%" symbol
               (typep (find-class symbol) 'structure-class)
               (tidy (or (documentation symbol 'type) "")))))))

(defun tidy (doc)
  "DOC with the indentation of its continuation lines removed, so that Markdown sees
plain paragraphs."
  (with-output-to-string (out)
    (with-input-from-string (in doc)
      (loop for line = (read-line in nil)
            for first = t then nil
            while line
            do (unless first (write-char #\Newline out))
               (write-string (string-left-trim " " line) out)))))

(defun write-reference (file)
  (let ((entries (documented-symbols)))
    (with-open-file (out file :direction :output :if-exists :supersede :external-format :utf-8)
      (format out "# Function reference

This chapter is generated from the docstrings, grouped by the source file that defines
each function, macro, variable and class.  Names in capitals in the descriptions are
arguments, as is usual in Lisp.  All are in the package `cl-cairoplot` (nickname
`cairoplot`), except the windows section, which is `cl-cairoplot-gtk`.
")
      (loop for (name title) in (append *files* '(("other" "Other")))
            for here = (sort (remove-if-not (lambda (e) (equal (first e) name)) entries)
                             #'string< :key (lambda (e) (symbol-name (third e))))
            with done = '()
            when here
              do (unless (member title done :test #'string=)
                   (format out "~%## ~a~%" title)
                   (push title done))
                 (dolist (e here) (write-entry (second e) (third e) out))))
    (length entries)))

(let ((n (write-reference (merge-pathnames "src/10-reference.md" *docs*))))
  (format t "~&Wrote src/10-reference.md: ~d entries~%" n)
  (when *undocumented*
    (format t "~&Undocumented: ~{~(~a~)~^ ~}~%" (sort *undocumented* #'string<))))
(sb-ext:exit :code 0 :abort t)
