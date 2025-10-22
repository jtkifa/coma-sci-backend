
(in-package coma-json-server)

(def-json-command status (json-req)
  (with-json-command-setup (json-req)
    (let* ((all-threads (bt:all-threads))
	   (tdescs (mapcar (lambda (thread) (format nil "~A" thread))
			   all-threads))
	   (n-threads (length all-threads))
	   (webresults (list-waiting-web-results)) ;; (ID RESULT-OR-NIL UT-EXPIRATION COMMAND)
	   (webresults-printable
	     (mapcar (lambda (wr)
		       (alexandria:alist-hash-table 
			`(("ID" . ,(first wr))  ;; the ID of the rquest
			  ("COMMAND" . ,(fourth wr)) ;; the command name
			  ("STATUS" . ,(if (second wr) "DONE" "WORKING"))
			  ("EXPIRES-IN-SECONDS" . ,(- (third wr)
						      (get-universal-time))))))
		     webresults)))
	   

      (set-param "N-THREADS" n-threads)
      (set-param "THREADS"   tdescs)
      (set-param "WORK-IN-PROGRESS" webresults-printable)
      (set-param "CFITSIO-IS-COMPILED-MULTITHREADED"
		 (json-bool (cfitsio:cfitsio-is-multithreaded)))
      (set-param "CFITSIO-IS-RUNNING-MULTITHREADED"
		 (json-bool
		  (and (cfitsio:cfitsio-is-multithreaded)
		       (not cfitsio:*threadlock-cfitsio-calls*)))))))

