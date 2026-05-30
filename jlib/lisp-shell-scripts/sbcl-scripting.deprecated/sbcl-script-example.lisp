;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mandatory initialization lines
(require 'sb-posix) (require 'asdf)
(load (sb-posix:getenv "SBCLRC"))
(asdf:load-system "sbcl-scripting")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; USER MODIFIED SECTION BEGINS

;; load any systems we need
(defparameter *systems-to-load* '("cffi"))
(mapc 'asdf:load-system *systems-to-load*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun print-usage-and-quit (&optional err-string)
  (when err-string (format *error-output* "~A~%~%" err-string))
  (format
   *error-output*
   "Invoke as ~%  ~A --foo some_foo --bar/--no-bar thing1 thing2~%~%"
   sbcl-scripting:*script-name*)
  (sb-ext:exit :code (if err-string 1 0)))

(defun main ()
  (multiple-value-bind (named-args un-named-args)
      (sbcl-scripting:get-args '(:foo :bar) 'print-usage-and-quit)
    (format t "The named args are: ~%~10T~S~%~%" named-args)
    (format t "The un-named args are: ~%~10T~S~%~%" un-named-args)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; .... other stuff goes here

;; run the main function
(main)
