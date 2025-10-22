

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mandatory initialization lines
(require 'sb-posix) (require 'asdf)
(load (sb-posix:getenv "SBCLRC"))
(asdf:load-system "sbcl-scripting")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; USER MODIFIED SECTION BEGINS

;; load any systems we need
(defparameter *systems-to-load* '("coma-json-server" "swank"))
(mapc 'asdf:load-system *systems-to-load*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun print-usage-and-quit (&optional err-string (print-usage t))
  (when err-string (format *error-output* "~%~A~%~%" err-string))
  (when print-usage
    (format
     *error-output*
     "Invoke as 

     ~A 
        [-INFILE  infile.json] 
        [-OUTFILE outfile.json] 
        [-WEB-SERVER]
        [-WEB-HOST \"127.0.0.1\"]
        [-WEB-PORT 9999]     
        [-LIST]
        [-BACKTRACE]
        [-ENABLE-CFITSIO-MUTEX]
        [-LISP-REPL]
        [-LISP-REPL-PORT 9944]

where [...] arguments are optional
 
   -INFILE is the input JSON file, stdin by default
   -OUTFILE is the output JSON file, stdout by default
   -WEB-SERVER, if present, activates a HTTP-based protocol for submitting
     JSON commands, on WEB-HOST and WEB-PORT, by default 127.0.0.1:9999.
     WEB-HOST can be '*' to publish on all network interfaces
   -LIST lists available JSON commands, and quits
   -BACKTRACE means to fail with a backtrace rather than returning error 
     objects, for debugging.
   -ENABLE-CFITSIO-MUTEX turns on a mutex preventing more than one call
    to cfitsio library at once.  It is always on for single-threaded cfitsio.
   -LISP-REPL opens a SLIME Emacs server on LISP-REPL-PORT for interactive
    debugging of a running instance of this server.  Port defaults to 9944.
"
     (file-io:file-minus-dir sbcl-scripting:*script-name*)))
    (sb-ext:exit :code 1))
 

(setf sbcl-scripting:*print-usage-and-quit* 'print-usage-and-quit)

	  


(defun main ()
  (multiple-value-bind (named-args un-named-args)
      (sbcl-scripting:get-args
       '(:infile :outfile :backtrace :web-server :web-host :web-port :list
	 :enable-cfitsio-mutex :lisp-repl :lisp-repl-port))

    ;; increase number of allowed server threads. Perhaps we really need
    ;; webserver to queue tasks differently, as lambda functions
    (setf hunchentoot::*default-max-thread-count* 1000)
    (setf hunchentoot::*default-max-accept-count* (+ hunchentoot::*default-max-thread-count* 20))

    
    (let ((infile nil)
	  (outfile nil)
	  (list nil)
	  (web-server nil)
	  (web-host "127.0.0.1")
	  (web-port 9999)
	  (backtrace nil)
	  (lisp-repl nil)
	  (lisp-repl-port 9944))
      
      (flet ((have-arg (arg)
	       (sbcl-scripting:arg-is-present arg named-args))
	     (zero-arg (arg)
	       (sbcl-scripting:get-zero-arg-or-error arg named-args))
	     (get-arg (arg)
	       (sbcl-scripting:get-single-arg-or-error arg named-args)))


	(when (zero-arg :list)
	  (format t "Command list:~%")
	  (loop for command in (coma-json-server:get-command-list)
		do (format t "   ~A~%" command))
	  (terpri)
	  (sb-ext:exit :code 0 :abort t))
	
	(when (have-arg :infile)
	  (setf infile (get-arg :infile)))
	(when (have-arg :outfile)
	  (setf outfile (get-arg :outfile)))


	;; launch the LISP REPL if desired
	(when (zero-arg :lisp-repl)
	  (setf lisp-repl t)
	  (when (have-arg :lisp-repl-port)
	    (setf lisp-repl-port (ignore-errors
				  (parse-integer (get-arg :lisp-repl-port)))))
	  (when (not (and (integerp lisp-repl-port)
			  (< 0 lisp-repl-port (expt 2 16))))
	    (print-usage-and-quit
	     (format nil "LISP-REPL-PORT=~A is not in [1,65535]" lisp-repl-port)))
	  (format t "Creating Lisp/Emacs SLIME connection on port ~A~%" lisp-repl-port)
	  (swank:create-server :port lisp-repl-port))
	
	;; launch the web server if desired
	(when (zero-arg :web-server)
	  (setf web-server t)
	  (when (have-arg :web-port)
	    (setf web-port (ignore-errors
			    (parse-integer (get-arg :web-port)))))
	  (when (have-arg :web-host)
	    (setf web-host (get-arg :web-host)))
	  (when (not (and (integerp web-port)
			  (< 0 web-port (expt 2 16))))
	    (print-usage-and-quit
	     (format nil "WEB-PORT=~A is not in [1,65535]" web-port)))
	  (multiple-value-bind (server err)
	      (ignore-errors
	       (coma-json-server:launch-coma-json-server-web-interface
		:port web-port :host web-host))
	    (when (not server)
	      (print-usage-and-quit
	       (format nil "ERROR starting web server: ~A" err)
	       nil))))

	
	(setf backtrace (zero-arg :backtrace))
	(if (zero-arg :enable-cfitsio-mutex)
	    (progn (setf cfitsio:*threadlock-cfitsio-calls* t)
		   (when web-server ;; don't print in pipe mode - it will confuse output
		     (format t "NOTE - threadlocking cfitsio calls.~%")))
	    (progn
	      (setf cfitsio:*threadlock-cfitsio-calls* nil)
	      (when (not (cfitsio:cfitsio-is-multithreaded))
		(when web-server
		  (format t "NOTE - cfitsio is compiled single threaded. Mutex will be enabled.~%")))))



	(if (not web-server)
	    (coma-json-server:run-coma-json-server
	     :infile infile
	     :outfile outfile
	     :backtrace backtrace)
	    ;; if running a web server, then just loop the main thread forever
	    (progn
	      (format t "Serving web requests on ~A:~A~%" web-host web-port)
	      (loop (sleep 1000))))))))

(main)
