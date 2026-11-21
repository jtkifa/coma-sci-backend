;;;; colors.lisp — backend-neutral colors
;;;;
;;;; A color spec is a COLOR, a keyword naming an entry in
;;;; *NAMED-COLORS*, a list (R G B) or (R G B A) of numbers in [0,1], or
;;;; a hex string "#RRGGBB" or "#RRGGBBAA".
;;;; PARSE-COLOR turns any of these into a COLOR.

(in-package :cl-cairoplot)

(defstruct (color (:constructor make-color (red green blue &optional (alpha 1d0))))
  "A color: RED, GREEN, BLUE and ALPHA (opacity), each 0..1 (see
   PARSE-COLOR)."
  (red   0d0 :type real)
  (green 0d0 :type real)
  (blue  0d0 :type real)
  (alpha 1d0 :type real))

(setf (documentation 'make-color 'function)
      "A color of RED, GREEN, BLUE and ALPHA (opacity), each 0..1."
      (documentation 'color-p 'function)
      "True if OBJECT is a COLOR.")

(defvar *named-colors* (make-hash-table :test 'eq)
  "Keyword -> COLOR.")

(defun define-named-color (name red green blue &optional (alpha 1d0))
  "Name a color: the keyword NAME then designates RED, GREEN, BLUE and
   ALPHA (0..1), e.g. in WITH-COLOR."
  (setf (gethash name *named-colors*) (make-color red green blue alpha)))

;; PGPLOT's standard color indices 0-15, plus a few extras
(loop for (name r g b) in
      '((:black     0.0  0.0  0.0)  (:white       1.0  1.0  1.0)
        (:red       1.0  0.0  0.0)  (:green       0.0  1.0  0.0)
        (:blue      0.0  0.0  1.0)  (:cyan        0.0  1.0  1.0)
        (:magenta   1.0  0.0  1.0)  (:yellow      1.0  1.0  0.0)
        (:orange    1.0  0.5  0.0)  (:green-yellow 0.5 1.0  0.0)
        (:green-cyan 0.0 1.0  0.5)  (:blue-cyan   0.0  0.5  1.0)
        (:blue-magenta 0.5 0.0 1.0) (:red-magenta 1.0  0.0  0.5)
        (:dark-gray 0.33 0.33 0.33) (:light-gray  0.67 0.67 0.67)
        (:gray      0.5  0.5  0.5)  (:very-light-gray 0.9 0.9 0.9)
        ;; common extras
        (:purple    0.5  0.0  0.5)  (:brown       0.6  0.3  0.1)
        (:pink      1.0  0.6  0.7)  (:navy        0.0  0.0  0.5)
        (:teal      0.0  0.5  0.5)  (:olive       0.5  0.5  0.0)
        (:maroon    0.5  0.0  0.0))
      do (define-named-color name r g b))

(defun parse-color (spec)
  "Return the COLOR designated by SPEC."
  (etypecase spec
    (color spec)
    (keyword (or (gethash spec *named-colors*)
                 (error "Unknown color name ~S." spec)))
    (list (destructuring-bind (r g b &optional (a 1d0)) spec
            (make-color r g b a)))
    (string (let ((hex (string-left-trim "#" spec)))
              (unless (and (member (length hex) '(6 8))
                           (every (lambda (c) (digit-char-p c 16)) hex))
                (error "Bad hex color ~S (want \"#RRGGBB\" or \"#RRGGBBAA\")." spec))
              (flet ((channel (k) (/ (parse-integer hex :start (* 2 k) :end (* 2 (1+ k)) :radix 16)
                                     255d0)))
                (make-color (channel 0) (channel 1) (channel 2)
                            (if (= (length hex) 8) (channel 3) 1d0)))))))
