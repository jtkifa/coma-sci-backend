
(in-package coma-json-server)


(defparameter *backtrace* nil)

(defmacro bt-ignore-errors (&body body)
  (let ((body-func-sym (gensym "bt-ignore-body-func")))
    `(flet ((,body-func-sym ()
	      (progn ,@body)))
       (if *backtrace*
	   ;;(values (,body-func-sym) nil)
	   (,body-func-sym)  ;; 2025-01-7 - don't nuke other values
	   (ignore-errors
	    (,body-func-sym))))))
			   

(defun %error-and-quit (message)
  (format *error-output* "FATAL ERROR: ~A~%" message)
  (sb-ext:exit :code -1 :abort t))

(defun %open-file-with-errors
  (filename &key 
	      (direction :input)
	      (element-type 'character)
	      if-exists if-does-not-exist)
  (multiple-value-bind (stream error)
      (bt-ignore-errors
       (open filename :direction direction
		      :element-type element-type
		      :if-exists if-exists
		      :if-does-not-exist if-does-not-exist))
    (when (not stream)
      (%error-and-quit
       (format
	nil
	"ERROR opening file ~A in direction ~A: ~A"
	filename direction error)))
    stream))




;; build a json response with nothing but an error
(defun build-json-error-response (json-req error-name error-desc)
  (make-json-object
   :type "RESPONSE"
   :command (if json-req (json-object-command json-req) "UNKNOWN")
   :id (if json-req (json-object-id json-req) "UKNOWN")
   :compact-output (json-object-compact-output json-req)
   :parameters nil
   :runtime-start (get-internal-run-time)
   :realtime-start (get-internal-real-time)
   :error (make-error-object
	   :error error-name
	   :desc error-desc)))
	   

;; this will encode either a %WEB-RETURN-OBJECT, which is a JSON-OBJECT
;; or an ERROR-OBJECT - or a WEB-RESPONSE-OBJECT
(defun encode-json-object (jsobj &key (stream *standard-output*))
  (let ((yason:*parse-json-arrays-as-vectors* t)
	(yason::*default-indent* 10)
	(yason:*yason-float-type* 'double-float)
	(prett-print-json (or (if (typep jsobj 'json-object)
			(not (json-object-compact-output jsobj))
			t)))) ;; always pretty-print errors
    (flet ((do-encoding (stream)
	     (cond ((typep jsobj 'json-object)
		    (yason:encode-alist  
		     `(("TYPE" . ,(json-object-type jsobj))
		       ("COMMAND" . ,(json-object-command jsobj))
		       ("ID" . ,(json-object-id jsobj))
		       ,@(if
			  (json-object-error jsobj)
			  `(("ERROR" . ,(alexandria:alist-hash-table
					 `(("TYPE" . "ERROR")
					   ("ERROR" . ,(error-object-error (json-object-error jsobj)))
					   ("DESCRIPTION"  . ,(error-object-desc (json-object-error jsobj)))
					   ("BACKTRACE"
					    . ,(error-object-backtrace (json-object-error jsobj))))))))
		       
		       ,@(if (json-object-parameters jsobj)
			     `(("PARAMETERS" . ,(cond
						  ((hash-table-p (json-object-parameters jsobj))
						   (json-object-parameters jsobj))
						  (t
						   (alexandria:alist-hash-table
						    (json-object-parameters jsobj)))))))
		       ,@(if (json-object-runtime jsobj)
			     `(("CPUTIME-ALLPROC" . ,(json-object-runtime jsobj))))
		       ,@(if (json-object-realtime jsobj)
			     `(("CLOCKTIME" . ,(json-object-realtime jsobj))))

		       ) ;; end of yason:encode-alist
		     stream))
		   ;;
		   ((typep jsobj 'error-object)
		    (yason:encode-alist
		     `(("TYPE" . "ERROR")
		       ("ERROR" . ,(error-object-error jsobj))
		       ,@(if (error-object-desc jsobj)
			     `(("DESCRIPTION"  . ,(error-object-desc jsobj))))
		       ,@(if (error-object-backtrace jsobj)
			     `(("BACKTRACE"  . ,(error-object-backtrace jsobj)))))
		     stream))
		   ;;
		   ((typep jsobj 'web-request-response-object)
		    (yason:encode-alist
		     (remove nil ;; remove error clause if NIL
			     `(("TYPE" . ,"WEB-REQUEST-RESPONSE")
			       ("STATUS" . ,(web-request-response-object-status jsobj))
			       ,(if (web-request-response-object-error jsobj) ;; can be NIL
				    (cons "ERROR" (web-request-response-object-error jsobj)))
			       ("DESCRIPTION" . ,(web-request-response-object-desc jsobj))))
		     stream)))
	     (terpri stream)
	     (force-output stream)))
      (if prett-print-json
	  (let ((jstream (yason:make-json-output-stream stream :indent 2)))
	    (do-encoding jstream))
	  (do-encoding stream)))))


(defun encode-json-object-to-string (jsobj)
  (with-output-to-string (s)
    (encode-json-object jsobj :stream s)))

;; all strings upcased
(defun %parse-json-key-fn (string) (string-upcase string))

;; read one json object from stream; return (VALUES NIL ERROR) on an error
(defun read-json-request-object (stream)
  (multiple-value-bind (json-hash error)
      (ignore-errors ;; just ordinary ignore errors, for EOF
       (yason:parse stream 
		    :json-arrays-as-vectors t 
		    :object-key-fn '%parse-json-key-fn))
    (if (not json-hash)
	(values nil error)
	(block ret ;; parse it
	  (let ((type (gethash "TYPE" json-hash))
		(command (gethash "COMMAND" json-hash))
		(compact-output (gethash "COMPACT-OUTPUT" json-hash)) ;; otherwise pretty print
		(id      (gethash "ID"      json-hash))
		(callback-url (gethash "CALLBACK-URL"    json-hash))
		(parameters (gethash "PARAMETERS" json-hash)))
	    (when (or (not type) (not command) (not id))
	      (return-from ret
		(values nil (format nil "Command had missing TYPE=~A COMMAND=~A ID=~A"
				    type command id))))
	    (when (not (equalp type "REQUEST"))
	      (return-from ret
		(values nil (format nil "Command had missing TYPE=~A not TYPE=request." type ))))
	    (when (and parameters (not (hash-table-p parameters)))
	      (return-from ret
		(values nil (format nil "PARAMETERS=~A is not a JSON sub-object." parameters))))
	    ;; else its a good command
	    (make-json-object
	     :type type
	     :command command
	     :id id
	     :compact-output compact-output
	     :callback-url callback-url
	     :parameters parameters))))))  
	      
	    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; the global hash of available command names - KEY=command
;; and VALUE = (function json-command-object)
(defparameter *json-command-hash* (make-hash-table :test 'equalp))
;; this is called whenever a new command is defined
(defun insert-json-command (command function)
  (setf (gethash command *json-command-hash*) function))

(defun get-json-command (command)
  (gethash command *json-command-hash*))

(defun get-command-list ()
  (loop for key being the hash-key of *json-command-hash*
	collect key))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; define a json command, creating a function JSON-COMMAND-XXX and
;; adding "command" to *json-command-hash*.  The function has one argument,
;; named json-req-name.  See example in hello.lisp
(defmacro def-json-command (command-sym (json-req-name) &body body)
  (when (not body) (error "BODY not given in def-json-command macro"))
  (let ((function-name (intern (string-upcase (format nil "%%json-command-~A" command-sym))))
	(command-string (string-upcase (string command-sym))))
    `(progn
       (insert-json-command ,command-string ',function-name)
       (defun ,function-name (,json-req-name)
	 ,@body))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ensure the json result matches the json request
(defun combobulate-result-object (json-request json-result)
  (when (typep json-result 'json-object) ;; could be error-object too
    (setf (json-object-command json-result)
	  (json-object-command json-request))
    (setf (json-object-id json-result)
	  (json-object-id json-request))
    (setf (json-object-compact-output json-result)
	  (json-object-compact-output json-request))
    (setf (json-object-type json-result)
	  "RESPONSE")
    (when (json-object-parameters json-result)
      (setf (gethash "TYPE" (json-object-parameters json-result))
	    "PARAMETERS"))) 
  json-result)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; macro that starts JSON commands that will
;; 1. define variable JSON-RESP by running COMBOBULATE-RESULT-OBJECT
;;    on THE-JSON-REQ
;; 2. define local function (RETURN-WITH-ERROR ERROR-KEY ERROR-DESC)
;;    for quitting with an error
;; 3. define PARAMETERS for input parameters; if PARAMETERS is not
;;    present, then return an error
;; 4. define PARAMETERS-OUT
;; 5. define local function
;;     (GET-PARAM KEY &KEY DEFAULT MANDATORY SATISFIES SATISFIES-DESC)
;; 6. define (SET-PARAM KEY VALUE) to set an return parameter in
;;    JSON-RESP
;; 7. catching any uncaught errors and giving them to RETURN-WITH-ERROR
;; 8. computing the run times
;; 9. returning JSON-RESP at end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro with-json-command-setup ((the-json-req) &body json-body)
  `(let* ((JSON-RESP
	    (combobulate-result-object ;; set up ID, COMMAND, etc
	     ,the-json-req
	     (make-json-object :type "response"
			       :parameters (make-hash-table :test 'equal))))
	  (parameters (json-object-parameters ,the-json-req))
	  (parameters-out (json-object-parameters json-resp)))
     (block %json-retblock
       ;;
       (labels ((compute-runtimes ()
		  (setf (json-object-runtime-end json-resp) (get-internal-run-time))
		  (setf (json-object-realtime-end json-resp) (get-internal-real-time))
		  (setf (json-object-runtime json-resp)
			(/ (- (json-object-runtime-end json-resp) (json-object-runtime-start json-resp))
			   (* 1d0 internal-time-units-per-second)))
		  (setf (json-object-realtime json-resp)
			(/ (- (json-object-realtime-end json-resp) (json-object-realtime-start json-resp))
			   (* 1d0 internal-time-units-per-second))))
		(RETURN-WITH-ERROR (err-name err-desc)
		  (compute-runtimes)
		  (setf (json-object-error json-resp)
			(make-error-object
			 :error err-name
			 :desc err-desc))
		  (return-from %json-retblock json-resp))
		;;
		(GET-PARAM (%key &key default required satisfies satisfies-desc) 
		  (multiple-value-bind (%val %is-present)
		      (gethash %key parameters default)
		    ;(format t "KEY=~A  val=~A is-present=~A~%" %key %val %is-present)
		    (when (and required (not %is-present))
		      (RETURN-WITH-ERROR
		       (format nil "NO-~A" %key)
		       (format nil "Required argument ~A not present" %key)))
		    (when (and satisfies
			       %is-present ;; run satisifes only if we got a value
			       (not (funcall satisfies %val)))
		      (RETURN-WITH-ERROR
		       (format nil "INVALID-~A" %key)
		       (if satisfies-desc
			   (format nil "Parameter ~A invalid: ~A"
				   %key satisfies-desc)
			   (format nil "Parameter ~A is invalid" %key))))
		    %val))
		;;
		(SET-PARAM (%key %value)
		  (setf (gethash %key parameters-out)
			%value)))
	 ;;
	 (when (not parameters)
	   (RETURN-WITH-ERROR "NO-PARAMETERS" "No PARAMETERS provided."))
	 (set-param "TYPE" "PARAMETERS")
	 (multiple-value-bind (%ok-ret %err)
	     (ignore-errors
	      (progn ,@json-body t)) ;; return TRUE if ,@body doesn't fail
	   (when (not %ok-ret)
	     (return-with-error "INTERNAL-JSON-FUNCTION-ERROR"
				(format nil "Unhandled error <~A>" %err))))
	 (compute-runtimes)
	 JSON-RESP))))

;; macro to eval a failure expression and if it returns T, do
;; (RETURN-WITH-ERROR %ERROR-NAME %ERROR-DESC) if it fails
(defmacro jcom-test-expr (%failure-expr %error-name  %test-desc)
  `(when ,%failure-expr
     (return-with-error ,%error-name ,%test-desc)))
				     
			     
     

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
(defun execute-json-command (json-request)
  (declare (type json-object json-request))
  (let ((json-function  (gethash (json-object-command json-request) *json-command-hash*)))
    (cond ((not json-function)
	   (build-json-error-response
	    json-request
	    "UNKNOWN-COMMAND"
	    (format nil "COMMAND=~S is not a known coma-json-server function"
		    (json-object-command json-request))))
	  (t 
	   (multiple-value-bind (json-result err)
	       (bt-ignore-errors (funcall json-function json-request))
	     ;; something went REALLY wrong so we make an error object
	     (when (not (json-object-p json-result))
	       (setf json-result
		     (build-json-error-response
		      json-request
		      "INTERNAL-ERROR-NOT-JSON-RESPONSE"
		      (format nil "JSON object not returned by EXECUTE-JSON-COMMAND for command=~A  function=~A ERROR=<~A>"
			      (json-object-command json-request)
			      json-function
			      (error-object-to-string  err)))))
	     ;; make sure the ID and COMMAND are correct
	     (combobulate-result-object json-request json-result)
	     json-result)))))
	  
      

;; if backtrace is true then don't run tasks inside BT-IGNORE-ERRORS
(defun run-coma-json-server (&key infile outfile backtrace)
  (let ((instream  *standard-input*)
	(outstream *standard-output*)
	(*backtrace* backtrace)) ;; special var
		   

    (unwind-protect
	 (progn
	   (when infile
	     (setf instream
		   (%open-file-with-errors  infile :direction :input
					    :if-does-not-exist :error)))
	   (when outfile
	     (setf outstream
		   (%open-file-with-errors
		    outfile :direction :output
			    :if-exists :supersede
			    :if-does-not-exist :create)))

	   (loop
	     with non-recoverable-error = nil
	     until non-recoverable-error
	     do
		;; stream closed, DONE with input
		(when (not (open-stream-p instream))
		  (return nil)) 
		;;
		(multiple-value-bind (json-req-obj json-req-error)
		    (read-json-request-object instream)
		  (cond
		    ((typep json-req-error 'cl:end-of-file)
		     (return))
		    ;; 
		    (json-req-error
		     (progn
		       ;; this error is not recoverable, because there was a syntax glitch
		       (setf non-recoverable-error t)
		       (encode-json-object
			(make-error-object
			 :error "BAD_COMMAND"
			 :desc (format nil "NON-RECOVERABLE ERROR ~A" json-req-error))
			:stream outstream)))
		    ;;
		    (t
		     (encode-json-object  
		      (execute-json-command json-req-obj)
		      :stream outstream
		      )))))

	   ;; unwind protected form
	   (progn
	     (when infile (bt-ignore-errors (close instream)))
	     (when outfile (bt-ignore-errors (close outstream))))))))

	     

	

