 

(in-package run-program)


(defun run-program-to-string (program args
			      &key
				(environment (%get-default-environment))
				(timeout nil)
				(max-output-length #.(expt 2 24))
				(sleep-time 0.1)
				(shutdown-time 5.0))
  "Run PROGRAM with argument list ARGS, like RUN-PROGRAM.
The output is converted to a string. An optional TIMEOUT (sec) is
supported.  SLEEP-TIME is the maxilength of time to sleep between
attempts to get more output.  SHUTDOWN-TIME is the maximum time given
to the process to quit before killing it.

Returns (VALUES PROCESS-EXIT-CODE OUTPUT-STRING)
"
  (let ((proc nil))
    (unwind-protect
	 (progn
	   (setf proc (run-program program args
				   :environment environment
				   :wait nil))
	   (when (not proc)
	     (error "Process for ~A ~A failed to start" program args))
	   (let
	       ((output-string
		  (with-output-to-string (sout)
		    (loop
		      with in-stream = (process-output proc)
		      with start-time = (get-universal-time)
		      with nbytes of-type fixnum = 0
		      do
			 ;; keep getting chars until we can't
			 (loop ;until (not (listen in-stream))
			       ;; LISTEN caused a stack exhaustion, but READ-CHAR-NO-HANG
			       ;; works - perhaps an SBCL bug (on arm64?)
			       for c = (read-char-no-hang in-stream nil nil)
			       while c 
			       do (write-char c sout)
				  (incf nbytes 1))
			 ;; when done, return
			 (when (not (process-alive-p proc))
			   (return nil))
			 (when (> nbytes max-output-length)
			   (error "External process output too long."))
			 ;; take a nap to allow process to do something
			 (sleep sleep-time)
			 (when (and timeout
				    (> (- (get-universal-time) start-time)
				       timeout))
			   (error "External program timed out"))))))
	   (values (process-exit-code proc) output-string)))
      ;; unwind-protected form to kill the process
      (progn
	;; first close off streams
	(when (process-p proc) ;; PROC=NIL if invalid executable, eg
	  (process-close proc)) ;; this is needed whether or not alive - closes files
	;; then kill it if it alive, giving a little sleep to let it shut down itself
	(when (process-p proc)
	  (loop for time = 0.0 then (+ time 0.03) 
		while (and (<= time shutdown-time)
			   (process-alive-p proc))
		do (sleep 0.1)) ;; give it a bit more time to finish and try again
	  ;;
	  ;; now if it isn't dead, kill it
	  (when (process-alive-p proc)
	    (ignore-errors (process-kill proc))))))))
				
