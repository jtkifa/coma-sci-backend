
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mandatory initialization lines
(require 'sb-posix) (require 'asdf)
(load (sb-posix:getenv "SBCLRC"))
(asdf:load-system "sbcl-scripting")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package coma-json-server dependencies for Docker deployment
;;
;; Usage: package-coma-json-server OUTPUT-DIR [-verbose]

;; Load coma-json-server first (packager needs it to compute dependencies)
(asdf:load-system "coma-json-server")
(asdf:load-system "coma-json-server-packager")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun print-usage-and-quit (&optional err-string (print-usage t))
  (when err-string (format *error-output* "~%~A~%~%" err-string))
  (when print-usage
    (format *error-output*
	    "Usage: ~A OUTPUT-DIR [-verbose]

Packages coma-json-server and its dependencies into OUTPUT-DIR
for Docker deployment.

  OUTPUT-DIR   Target directory for the packaged files (required)
  -verbose     Print detailed progress information

Example:
  ~A /tmp/coma-docker-package -verbose
"
	    (file-io:file-minus-dir sbcl-scripting:*script-name*)
	    (file-io:file-minus-dir sbcl-scripting:*script-name*)))
  (sb-ext:exit :code (if err-string 1 0)))

(setf sbcl-scripting:*print-usage-and-quit* 'print-usage-and-quit)

(defun main ()
  (multiple-value-bind (named-args un-named-args)
      (sbcl-scripting:get-args '(:verbose))

    (when (null un-named-args)
      (print-usage-and-quit "ERROR: OUTPUT-DIR is required"))

    (let ((output-dir (first un-named-args))
	  (verbose (sbcl-scripting:arg-is-present :verbose named-args)))

      (format t "Packaging coma-json-server to: ~A~%" output-dir)
      (when verbose
	(format t "Verbose mode enabled~%"))

      (coma-json-server-packager:export-coma-json-server
       output-dir
       :verbose verbose)

      (format t "~%Done. Package created in: ~A~%" output-dir))))

(main)
