#|

Put coma-json-server on a web interface

|#

(in-package coma-json-server)

(defvar *hunchentoot-server* nil)
(defvar *default-web-host* "127.0.0.1")
(defvar *default-web-port* 9999)


;; if there are more workers than this, then make the thread sleep.
;; this is to avoid hitting resource limits
(defparameter *max-running-workers-allowed* 100)

;; disable hunchentoot backtrace in logging because hitting dynamic
;; extent streams on stack might crash system ... maybe
(setf hunchentoot:*log-lisp-backtraces-p* nil)


;; use this to do application/json
;; https://stackoverflow.com/questions/12764686/using-hunchentoot-to-parse-post-request-sent-by-model-save-in-backbone-js
(hunchentoot:define-easy-handler (web-submit-json :uri "/submit-json")
    (request)  ;; request here is "request=..." in GET or POST
  ;;
  (setf (hunchentoot:content-type*) "application/json")
  (flet ((process-request (json-text) ;; REQ
	   (multiple-value-bind (json-text http-code)
	       (submit-json-web-request json-text)
	     ;; ##### daryl said the new http codes crashed server
	     ;; (setf http-code 200) ;; ##### FIXME ###### - temporary patch
	     (setf (hunchentoot:return-code*) http-code)
	     ;;
	    (with-output-to-string (s)
	      (yason:encode ;; encode a special hash
	       json-text
	       s)))))
    ;;
    (let* ((h-request  hunchentoot:*request*) ;; hunchentoot request
	   (request-method (hunchentoot:request-method h-request))
	   (content-type (hunchentoot:header-in :content-type h-request)))
    (cond (request ;; a GET/POST
	   (process-request request))
	  ((and (eq request-method :post)
		(or (equalp content-type "application/json")
		    (equalp content-type "text/json")))
	   (process-request (hunchentoot:raw-post-data :force-text t)))))))





;; newer version that catches bad json conversion
(hunchentoot:define-easy-handler  (web-retrieve-json :uri "/retrieve-json")
    (id)
  ;; id may not be upcased
  (setf (hunchentoot:content-type*) "application/json")
  (multiple-value-bind (retval http-code)
      (retrieve-json-web-request id)
    ;; bind the result to JSON-STRING, or catch an error
    (multiple-value-bind (json-string err)
	(ignore-errors
	 ;; throw an error if object of the wrong type
	 (when (not (%web-return-object-p retval))
	   (error "Final stage output object is not JSON-OBJECT or ERROR-OBJECT - is of type ~A"
		  (type-of retval)))
	 ;;
	 ;; else turn the result into a string
	 (with-output-to-string (s)  ;; a pending result, or error
	   (encode-json-object retval :stream s))) ;; a good result

      (cond
	;; good JSON output
	(json-string
	 ;; use the supplied HTTP code
	 (setf (hunchentoot:return-code*) http-code)
	 json-string)
	;; error in JSON output
	(t
	 (setf (hunchentoot:return-code*) hunchentoot:+http-internal-server-error+)
	 (with-output-to-string (serr)
	   (yason:encode-alist 
	    `(("TYPE" . "ERROR")
	      ("ERROR" . "JSON-OUTPUT-ERROR")
	      ("DESCRIPTION" . ,(format nil
					"Error converting JSON hash table to legal JSON: ~A"
					;; could there be some unprintable error?
					(or (ignore-errors (format nil "~A" err))
					    "COULD-NOT-PRINT-ERROR")
					)))
	    serr)))))))



(defun launch-coma-json-server-web-interface (&key (port *default-web-port*)
						(host *default-web-host*))
  (setf *hunchentoot-server*
	(hunchentoot:start (make-instance 'hunchentoot:easy-acceptor
					  :address host
					  ;; :document-root nil
					  :access-log-destination nil
					  :port port  )))

  ;;
  *hunchentoot-server*)

(defun stop-coma-json-server-web-interface ()
  (when *hunchentoot-server*
    (hunchentoot:stop *hunchentoot-server*)
    (setf *hunchentoot-server* nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; hash by ID - the hash value is (result ut command-name) where result can be NIL
;; for a pending result
;;
(defvar *web-service-result-hash* (make-hash-table :test 'equalp))
(defvar *web-hash-lock* (bordeaux-threads:make-recursive-lock))
;; how long a web result lasts until being purged
(defvar *web-result-lifetime* (* 24 3600))

;; get a web result but only if it is completed (not NIL).
;; if REMOVE is set, then remove it from hash upon retrieval
(defun get-web-result (id &key (remove t))
  (bordeaux-threads:with-recursive-lock-held (*web-hash-lock*)
    (let* ((hres (gethash id *web-service-result-hash*))
	   (res  (first hres)))
      ;; delete this result if we got a valid answer
      (when (and res ;; not just a placeholder NIL
		 remove)
	(remhash id *web-service-result-hash*))
      ;;
      res)))
;;
;; is a web result named by ID present?  If INSERT-PLACEHOLDER is
;; true, then in the same lock insert an empty result.  The COMMAND
;; tells which command this is.
(defun web-result-exists? (id &key (insert-placeholder nil)
			      (command "UNKNOWN-COMMAND")
			      (lifetime *web-result-lifetime*))
  (bordeaux-threads:with-recursive-lock-held (*web-hash-lock*)
    (let ((exists
	    (not (not (gethash id *web-service-result-hash*)))))
      (when (and (not exists) insert-placeholder)
	(setf (gethash id *web-service-result-hash*)
	      (list nil (+ lifetime (get-universal-time)) command)))
      exists)))
	
;; is the web result done?
(defun web-result-done? (id)
  (bordeaux-threads:with-recursive-lock-held (*web-hash-lock*)
    (not (not (first (gethash id *web-service-result-hash*))))))
;;

;; list results as (ID RESULT-OR-NIL UT-EXPIRATION COMMAND)
(defun list-waiting-web-results ()
  (bordeaux-threads:with-recursive-lock-held (*web-hash-lock*)
    (loop for id being the hash-key of *web-service-result-hash*
	  for stuff being the hash-value of *web-service-result-hash*
	  collect (cons id stuff))))






;; set RESULT to the answer of web result named by ID
(defun set-web-final-result (id final-result)
  (bordeaux-threads:with-recursive-lock-held (*web-hash-lock*)
    (let* ((hres (gethash id *web-service-result-hash*)))
      (when (and hres ;; placeholder must still be here
		 (not (first hres))) ;; and must not have a result
	(setf (first hres) final-result)))))
;;
;; purge any web results that are past their expiration ut
(defun purge-web-results ()
  (bordeaux-threads:with-recursive-lock-held (*web-hash-lock*)
    (loop with ut-now = (get-universal-time)
	  with h = *web-service-result-hash*
	  for id being the hash-key of h
	  for hres being the hash-value of h
	  for ut-expiration = (second hres)
	  when (>= ut-now ut-expiration)
	    do (remhash id *web-service-result-hash*))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; limit the number of running workers to avoid too many open filehandles
(defparameter *n-running-workers* 0)

(defun may-we-run-another-worker ()
  (bordeaux-threads:with-recursive-lock-held (*web-hash-lock*)
    (if (< *n-running-workers*  *max-running-workers-allowed*)
	(incf *n-running-workers*) ;; return true value
	nil))) ;; no more workers, for now
(defun running-worker-done ()
  (bordeaux-threads:with-recursive-lock-held (*web-hash-lock*)
    (decf *n-running-workers*)))

;; launch a thread to carry out a json request, and return
;; a json object of type WEB-REQUEST-RESPONSE
(defun submit-json-web-request (req-string)
  (flet ((return-with-error (error-name error-desc http-code)
	   (let ((jhash (make-hash-table :test 'equal)))
	     (setf (gethash "TYPE" jhash) "WEB-REQUEST-RESPONSE")
	     (setf (gethash "STATUS" jhash) "ERROR")
	     (setf (gethash "ERROR" jhash)
		   (build-error-hash-json :name error-name :desc error-desc))
	     (return-from submit-json-web-request
	       (values jhash http-code))))
	 (return-with-success (id)
	   (let ((jhash (make-hash-table :test 'equal)))
	     (setf (gethash "TYPE" jhash) "WEB-REQUEST-RESPONSE")
	     (setf (gethash "STATUS" jhash) "OK")
	     (setf (gethash "ID" jhash) id)
	     (return-from submit-json-web-request
	       (values jhash hunchentoot:+http-ok+)))))
    
    (when (not req-string)
      (return-with-error "NO-REQUEST"
			 "No 'REQUEST' in web command"
			 hunchentoot:+http-bad-request+))
    (multiple-value-bind (json-obj error-obj)
	(with-input-from-string (s req-string)
	  (read-json-request-object s))
      (when (not json-obj)
	(return-with-error "JSON-PARSE-ERROR"
			   (format nil "~A" error-obj)
			   hunchentoot:+http-bad-request+))
      ;;
      (when (web-result-exists? (json-object-id json-obj)
				:insert-placeholder t
				:command (json-object-command json-obj))
	(return-with-error
	 "SUBMISSION-ID-EXISTS"
	 (format nil "A submission with ID='~A' already exists - the ID must be a unique identifier." (json-object-id json-obj))
	 hunchentoot:+http-bad-request+))
      ;;
      (bordeaux-threads:make-thread
       (lambda ()
	 ;; keep number of running workers below *max-running-workers-allowed*
	 (loop while (not (may-we-run-another-worker))
	       do (sleep 1.0))

	 (unwind-protect ;; should not be needed, but protect decrement of running worker count
	      (let ((result-obj ;; can be an error object too
		      (execute-json-command json-obj))) ;; already catches errors
		(set-web-final-result
		 (json-object-id json-obj)
		 result-obj))
	   (running-worker-done))

	 ;; decrement current number of running workers
	 ;; do web callback if one was specified
	 (when (json-object-callback-url json-obj)
	   (ignore-errors
	    (drakma:http-request (json-object-callback-url json-obj)
				 :connection-timeout 10))))
       :name (format nil "COMA-JSON-SERVER-WEB-REQ-ID-~A" (json-object-id json-obj)))
      ;;
      (purge-web-results)
      (return-with-success (json-object-id json-obj)))))


;; to avoid race condition between "web-result-exists?" and "get-web-result"
;; in case  retrieve-json-web-request is called multiple times, at the same moment,
;; for the same id .... very unlikely, but possible in theory
(defvar *retrieve-json-lock* (bt:make-lock "retrive-json-lock"))

(defun retrieve-json-web-request (id)
  (flet ((return-with-error (error-name error-desc http-error-code)
	   (return-from retrieve-json-web-request
	     (values
	      (make-web-request-response-object
	       :status "ERROR"
	       :error error-name
	       :desc error-desc)
	      http-error-code)))
	 ;;
	 (return-with-pending ()
	   (return-from retrieve-json-web-request
	     (values
	      (make-web-request-response-object
	       :status "PENDING"
	       :error nil
	       :desc "Computation of result still pending")
	      ;; return code 202
	      hunchentoot:+http-accepted+))))
     ;;
     (when (not id)
       (return-with-error
	"NO-REQUEST-ID-GIVEN" "No ID given in GET or POST" hunchentoot:+http-bad-request+))
     ;;
     ;; in theory, there is a race condition here if two attempts to get result
     ;; are made at exactly the same time
     (bt:with-lock-held (*retrieve-json-lock*)
       (cond
	 ((not (web-result-exists? id))
	  (return-with-error
	   "REQUEST-DOES-NOT-EXIST"
	   (format nil "Request ID='~A' does not exist.  It may already have been retrieved, or ID is wrong, or it timed out from the output queue." id)
	   hunchentoot:+http-ok+)) ;; NO CONTENT is wrong because it implies no body
       ;;
	 ((not (web-result-done? id))
	  (return-with-pending))
	 ;;
	 (t
	  (values
	   (get-web-result id :remove t)
	   hunchentoot:+http-ok+))))))
     
     



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *hello-test*  ;; simple hello to the server
  "{\"TYPE\":\"REQUEST\", \"COMMAND\":\"HELLO\", \"PARAMETERS\":{}, \"ID\":\"abc123\"}")

(defparameter *hello-test/sleep*  ;; simple hello to the server with a 30s sleep
  "{\"TYPE\":\"REQUEST\", \"COMMAND\":\"HELLO\", \"PARAMETERS\":{\"DELAY\": 30}, \"ID\":\"abc123s\"}")

(defparameter *hello-test/callback*  ;; simple hello to the server with a 30s sleep
  "{\"TYPE\":\"REQUEST\", \"COMMAND\":\"HELLO\", \"PARAMETERS\":{\"DELAY\": 30}, \"ID\":\"abc123s\",\"CALLBACK-URL\":\"http://localhost:8000/ack\"}")

(defun make-test-json-web-request (json-text &key (port *default-web-port*)
					       (host *default-web-host*)
					       (content-type "application/x-www-form-urlencoded" ))
  (multiple-value-bind (bytes status headers)
      (drakma:http-request (format nil "http://~A:~A/submit-json" host port)
			   :method :post
			   :content-type content-type
			   :content (if (member content-type '("text/json" "application/json") :test 'equalp)
					json-text
					nil)
			   :parameters
			   (if (equalp content-type "application/x-www-form-urlencoded" )
			       `(("request" . ,json-text))
			       nil))
    (when (not (stringp bytes))
      (setf bytes (map 'string 'code-char bytes)))
    (values
     bytes status headers)))

(defun retrieve-test-json-web-request (id &key (port *default-web-port*)
					(host *default-web-host*))
   (multiple-value-bind (bytes status headers)
      (drakma:http-request (format nil "http://~A:~A/retrieve-json" host port)
			   :method :post
			   :parameters
			   `(("id" . ,id)))
       (when (not (stringp bytes))
	 (setf bytes (map 'string 'code-char bytes)))
      (values
       bytes status headers))) 
     
