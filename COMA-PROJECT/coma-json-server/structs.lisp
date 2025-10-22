(in-package coma-json-server)


(defstruct %web-return-object) 

;; a json call fails
(defstruct (error-object (:include %web-return-object))
  (error "UNSPECIFIED_ERROR")
  (desc )
  (backtrace ))
	  
;; a json call returns
(defstruct (json-object (:include %web-return-object))
  (type "RESPONSE") ;; "request" or "response" 
  (command "NO_COMMAND")
  (id  "NO_ID")
  (compact-output nil) ;; otherwise pretty-print with indentation
  parameters ;; an alist or a hash
  ;; these are valid only for a "response" type
  error
  callback-url  ;; for a request only, this is the URL to call back
  ;; for timing
  (realtime-start (get-internal-real-time))
  realtime-end
  (runtime-start (get-internal-run-time))
  runtime-end
  runtime   ;; decimal seconds
  realtime  ;; decimal seconds
  
  )
    

;; emitted when responding to a json-retrieve that is not complete
;; or pending
(defstruct (web-request-response-object (:include %web-return-object))
  (type "WEB-REQUEST-RESPONSE")
  (status nil) ;; "PENDING" or "ERROR"
  (error nil)  ;; the name of the error
  (desc nil) ;; the description of the error or status
  )
