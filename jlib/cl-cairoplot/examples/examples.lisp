;;;; examples.lisp — the example framework and shared example data
;;;;
;;;;   (asdf:load-system "cl-cairoplot-examples")
;;;;   (cl-cairoplot-examples:list-examples)                  ; names and descriptions
;;;;   (cl-cairoplot-examples:make-examples)                  ; all, as PDFs
;;;;   (cl-cairoplot-examples:make-examples :type :png :only '(rotated-viewport))
;;;;
;;;; MAKE-EXAMPLES writes one file per example into example-plots/ next to
;;;; cl-cairoplot.asd.  Each example lives in its own file in this
;;;; directory, named after it, as a DEFINE-EXAMPLE form: a function of an
;;;; open plot device plus the page shape it wants.  To add one, write a
;;;; new file like the others and list it in cl-cairoplot-examples.asd
;;;; (the order there is the order of LIST-EXAMPLES).
;;;;
;;;; This file holds the framework and data shared by several examples:
;;;; sample curves, repeatable noise and synthetic images.

(defpackage :cl-cairoplot-examples
  (:use :cl :cl-cairoplot)
  (:export #:make-examples #:list-examples #:*example-directory*))

(in-package :cl-cairoplot-examples)

(defvar *examples* '()
  "List of (NAME DOCUMENTATION SIZE ASPECT FUNCTION), in definition order.")

(defparameter *example-directory*
  (asdf:system-relative-pathname "cl-cairoplot" "example-plots/"))

(defmacro define-example (name (device &key (size 720) (aspect 0.75)) doc &body body)
  "Define example NAME, drawn by BODY on DEVICE, a plot device SIZE
   points wide with height/width ASPECT."
  `(progn
     (setf *examples*
           (append (remove ',name *examples* :key #'first)
                   (list (list ',name ,doc ,size ,aspect
                               (lambda (,device) ,@body)))))
     ',name))

(defun list-examples ()
  "Print the examples and their descriptions."
  (loop for (name doc) in *examples*
        do (format t "~&~(~A~)~%    ~A~%" name doc)))

(defun make-examples (&key (directory *example-directory*) (type :pdf) only)
  "Write each example (or only those named in ONLY) to DIRECTORY as a
   file of TYPE (:PDF, :PNG, :SVG or :PS).  Returns the files written."
  (ensure-directories-exist directory)
  (loop for (name nil size aspect function) in *examples*
        when (or (null only) (member name only))
          collect (let ((file (merge-pathnames
                               (make-pathname :name (string-downcase name)
                                              :type (string-downcase type))
                               directory)))
                    (with-device (d type file :size size :aspect aspect)
                      ;; no creation date, so unchanged examples rebuild to
                      ;; identical files and don't churn in git
                      (set-document-metadata d :title (string-downcase name)
                                               :creator "cl-cairoplot examples"
                                               :create-date "")
                      (funcall function d))
                    (format t "~&wrote ~A~%" (namestring file))
                    file)))

;;; ============================================================
;;; Helpers
;;; ============================================================

(defun page-title (device string)
  (plot-text device 0.5 0.975 string :coords :device
                                     :halign :center :valign :top :size 16))

(defun samples (f x1 x2 &optional (n 400))
  "Values XS, YS of F at N+1 points from X1 to X2, as double-float vectors."
  (let ((xs (make-array (1+ n) :element-type 'double-float))
        (ys (make-array (1+ n) :element-type 'double-float)))
    (dotimes (i (1+ n) (values xs ys))
      (let ((x (+ x1 (* (- x2 x1) (/ i n)))))
        (setf (aref xs i) (float x 1d0)
              (aref ys i) (float (funcall f x) 1d0))))))

(defun noise (i)
  "Repeatable pseudo-random number in [-0.5, 0.5) for integer I."
  (- (mod (* 43758.5453d0 (sin (* 12.9898d0 i))) 1) 0.5))

(defun gaussian-noise (i)
  "Repeatable, roughly normal pseudo-random number (mean 0, sigma 1)
   for integer I: a sum of 12 uniform ones (Irwin-Hall)."
  (loop for k below 12 sum (noise (+ (* 12 i) k 7919))))

;;; ============================================================
;;; Examples
;;; ============================================================

;;; a synthetic sky: a bright star, two fainter ones, a faint ring
;;; and noise, on a 160 x 120 grid
(defun sky-image (&key (w 160) (h 120))
  (let ((a (make-array (list h w) :element-type 'double-float)))
    (dotimes (j h a)
      (dotimes (i w)
        (flet ((star (x0 y0 flux sigma)
                 (* flux (exp (/ (+ (expt (- i x0) 2) (expt (- j y0) 2))
                                 (* -2 sigma sigma))))))
          (setf (aref a j i)
                (float (+ 10
                   (star 60 70 1000 3) (star 120 40 150 2.5) (star 30 25 60 2)
                   (* 25 (exp (- (expt (/ (- (sqrt (+ (expt (- i 100) 2)
                                                      (expt (- j 80) 2)))
                                             22)
                                          4)
                                       2))))
                   (* 3 (noise (+ i (* 1000 j))))) 1d0)))))))

(defun wave-image (&key (w 120) (h 120))
  "A signed field for diverging colormaps."
  (let ((a (make-array (list h w) :element-type 'double-float)))
    (dotimes (j h a)
      (dotimes (i w)
        (setf (aref a j i)
              (float (* (sin (/ i 12)) (cos (/ j 17))
                        (exp (- (/ (+ (expt (- i 60) 2) (expt (- j 60) 2)) 5000))))
                     1d0))))))

(defun terrain-image (&key (n 100))
  "Smooth hills and a valley for labeled contours."
  (let ((a (make-array (list n n) :element-type 'double-float)))
    (dotimes (j n a)
      (dotimes (i n)
        (let ((x (/ i (float n 1d0))) (y (/ j (float n 1d0))))
          (setf (aref a j i)
                (+ (* 900 (exp (- (/ (+ (expt (- x 0.3) 2) (expt (- y 0.6) 2)) 0.03))))
                   (* 600 (exp (- (/ (+ (expt (- x 0.72) 2) (expt (- y 0.35) 2)) 0.05))))
                   (* 200 x) 100)))))))

(defun ring-image (&key (n 120))
  "Elliptical rings bending tightly at their ends, for curved labels."
  (let ((a (make-array (list n n) :element-type 'double-float)))
    (dotimes (j n a)
      (dotimes (i n)
        (let ((x (/ (- i 60) 60d0)) (y (/ (- j 60) 60d0)))
          (setf (aref a j i) (sqrt (+ (* x x) (* 4 y y)))))))))
