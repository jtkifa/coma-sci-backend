
(in-package astorb)

(defparameter *astorb-url* "https://ftp.lowell.edu/pub/elgb/astorb.dat.gz")

(declaim (special *astorb-data-dir*))

(defun retrieve-newest-astorb-file (&key (verbose-stream nil))
  (let* ((mjd-day (floor (astro-time:ut-to-mjd (get-universal-time))))
	 (output-file (format nil "~A/astorb.dat.~D.gz"
			      (string-right-trim "/" *astorb-data-dir*)
			      mjd-day)))
    (when verbose-stream
      (format t "  Astorb download output file is ~A~%"
	      output-file))
    (with-open-file (sout output-file
			  :direction :output
			  :if-exists :supersede
			  :element-type '(unsigned-byte 8))
			  
      (let ((stream (drakma:http-request *astorb-url* :want-stream t)))
	(loop with nbytes of-type fixnum = 0 
	      for b = (read-byte stream nil nil) 
	      until  (not b)
	      do (write-byte b sout) 
		 (incf nbytes)
		 (when (and verbose-stream
			    (zerop (mod nbytes 10000000)))
		   (format verbose-stream
			   "   Downloaded ~A bytes~%" nbytes)
		   (force-output verbose-stream))
	      finally (return
			(values
			 output-file
			 nbytes)))))))


(defun retrieve-newest-astorb-file/iterate  (&key (verbose-stream nil) (ntries 3)
						  (sleep-time 10))
  (loop for i below ntries
	do (multiple-value-bind (file nbytes)
	       (ignore-errors (retrieve-newest-astorb-file :verbose-stream verbose-stream))
	     (when (and file (> nbytes 1e8)) ;; a short file would be an error
	       (return (values file nbytes))))
	   (sleep sleep-time)
	finally
	   (error "Failed to retrieve astorb file from ~A in ~A attempts."
		  *astorb-url* ntries)))
				 
      
  
