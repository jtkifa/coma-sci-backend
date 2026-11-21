;;;; Run the Lisp code blocks of the documentation, and make its pictures.
;;;;
;;;;   sbcl --non-interactive --load docs/tools/run-docs.lisp [--end-toplevel-options page.md ...]
;;;;
;;;; Each page runs in a fresh package that uses CL and CL-CAIROPLOT, its blocks in
;;;; order, as a reader would type them, in a scratch directory (so files the examples
;;;; write go there).  A block is run when it is marked as Lisp, ```lisp or ```{.lisp
;;;; ...}, and not .norun.  With image="name.png", every OPEN-DEVICE in the block makes
;;;; docs/images/name.png instead, at resolution 2 (the HTML and PDF show it at the size
;;;; asked for), whatever output type the block gives: the code on the page is the code
;;;; that made its picture.  Errors are reported with the page and the
;;;; block, and make the exit status 1.

(require :asdf)
(defvar *docs* (merge-pathnames "../" (make-pathname :name nil :type nil :defaults *load-truename*)))
(asdf:load-system "cl-cairoplot" :verbose nil)

(defpackage #:run-docs (:use #:cl))
(in-package #:run-docs)

(defvar *docs* cl-user::*docs* "The docs directory.")
(defvar *image* nil "The picture the block being run makes, a pathname, or NIL.")

;;; While *IMAGE* is set, OPEN-DEVICE makes it instead
(sb-int:encapsulate 'cl-cairoplot:open-device 'docs
                    (lambda (open type destination &rest keys)
                      (if *image*
                          (apply open :png (namestring *image*) :resolution 2 keys)
                          (apply open type destination keys))))

(defstruct snippet line attributes code)

(defun parse-attributes (info)
  "The attributes of a fence's info string, like lisp or {.lisp .norun image=\"x.png\"}, as
a plist: :CLASSES and the key=value pairs."
  (let ((info (string-trim " {}" info)) (classes '()) (pairs '()))
    (loop with start = 0
          for i = (position-if-not (lambda (c) (char= c #\Space)) info :start start)
          while i
          do (let* ((quote-start (position #\" info :start i))
                    (space (position #\Space info :start i))
                    (equals (position #\= info :start i)))
               (cond ((and equals (or (null space) (< equals space)) quote-start
                           (= quote-start (1+ equals)))
                      (let ((end (position #\" info :start (1+ quote-start))))
                        (push (cons (subseq info i equals) (subseq info (1+ quote-start) end)) pairs)
                        (setf start (1+ end))))
                     (t (let ((word (subseq info i (or space (length info)))))
                          (push (string-left-trim "." word) classes)
                          (setf start (or space (length info))))))))
    (list :classes (nreverse classes) :pairs pairs)))

(defun code-blocks (file)
  "The fenced code blocks of FILE."
  (with-open-file (s file :external-format :utf-8)
    (loop with blocks = '()
          for line = (read-line s nil)
          for n from 1
          while line
          do (when (and (>= (length line) 3) (string= (subseq line 0 3) "```"))
               (let ((attributes (parse-attributes (subseq line 3)))
                     (start n)
                     (code (with-output-to-string (out)
                             (loop for l = (read-line s nil)
                                   do (incf n)
                                   until (or (null l) (and (>= (length l) 3) (string= (subseq l 0 3) "```")))
                                   do (write-line l out)))))
                 (push (make-snippet :line start :attributes attributes :code code) blocks)))
          finally (return (nreverse blocks)))))

(defun runnable-p (snippet)
  (let ((classes (getf (snippet-attributes snippet) :classes)))
    (and (member "lisp" classes :test #'string-equal)
         (not (member "norun" classes :test #'string-equal)))))

(defun run-block (snippet package)
  "Evaluate SNIPPET's forms in PACKAGE; return the last value."
  (let ((*package* package) (value nil))
    (with-input-from-string (s (snippet-code snippet))
      (loop for form = (read s nil s)
            until (eq form s)
            do (setf value (eval form))))
    value))

(defvar *failures* 0)

(defun run-page (file scratch)
  (format t "~&~a~%" (file-namestring file))
  (let* ((name (format nil "DOCS-~:@(~a~)" (pathname-name file)))
         (package (progn (when (find-package name) (delete-package name))
                         (make-package name :use '(:cl :cl-cairoplot)))))
    (unwind-protect
         (dolist (snippet (code-blocks file))
           (when (runnable-p snippet)
             (let* ((image (cdr (assoc "image" (getf (snippet-attributes snippet) :pairs)
                                       :test #'string=)))
                    (*image* (and image (merge-pathnames (concatenate 'string "images/" image) *docs*)))
                    (*default-pathname-defaults* scratch))
               (handler-case
                   (handler-bind ((warning #'muffle-warning))
                     (when (and *image* (probe-file *image*))
                       (delete-file *image*))   ; so a stale one is not taken for it
                     (run-block snippet package)
                     (when image
                       (unless (probe-file *image*)
                         (error "The block opened no device, so made no ~a." image))
                       (format t "~&  line ~d: ~a~%" (snippet-line snippet) image)))
                 (error (e)
                   (incf *failures*)
                   (format t "~&  line ~d: ERROR ~a~%" (snippet-line snippet) e))))))
      (delete-package package))))

(let* ((pages (or (rest (member "--end-toplevel-options" sb-ext:*posix-argv* :test #'string=))
                  (mapcar #'namestring (directory (merge-pathnames "src/*.md" *docs*)))))
       (scratch (uiop:ensure-directory-pathname
                 (merge-pathnames "cl-cairoplot-docs/" (uiop:temporary-directory)))))
  (ensure-directories-exist scratch)
  (ensure-directories-exist (merge-pathnames "images/" *docs*))
  (uiop:with-current-directory (scratch)
    (dolist (page (sort (copy-list pages) #'string<))
      (run-page (pathname page) scratch)))
  (format t "~&~:[All code blocks ran.~;~:*~d code block~:p failed.~]~%"
          (and (plusp *failures*) *failures*))
  (sb-ext:exit :code (if (plusp *failures*) 1 0) :abort t))
