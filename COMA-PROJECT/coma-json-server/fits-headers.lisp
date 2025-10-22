#|
  read and write fits headers


{
    "type":"request",
    "command":"read-fits-header",
    "id":"123abc",
    "parameters": {"FITS-FILE" :"./foo.fits",
                   "EXTENSION" : 1,    // 1 by default
		   "KEYWORD" : "EXPTIME"}
}


{
    "type":"request",
    "command":"write-fits-header",
    "id":"123abc",
    "parameters": {"FITS-FILE":"./foo.fits",
		   "KEYWORD" : "EXPTIME",
                   "EXTENSION" : 1,    // 1 by default
                   "VALUE" : 300,
                   "COMMENT" : "The exposure time"}
}





|#

(in-package coma-json-server)


(def-json-command read-fits-header (json-req)
  (with-json-command-setup (json-req)
    (let* ((fits-file (get-param "FITS-FILE" :required t))
	   (keyword (get-param "KEYWORD" :required t))
	   (extension   (get-param "EXTENSION" :default 1)))
      
      (jcom-test-expr (not (probe-file fits-file))
		      "FITS-NOT-FOUND"
		      (format nil "Fits file ~A not found" fits-file))    
      
      (multiple-value-bind (value comment/err keyname)
	  (bt-ignore-errors
	    (cf:read-fits-header fits-file keyword 
				 :extension extension))
	(cond
	  ((typep comment/err 'error)
	   (return-with-error "READ-FITS-HEADER-ERROR"  (format nil "ERROR: ~a" comment/err)))
	  ((not keyname)
	   (return-with-error "FITS-HEADER-NOT-FOUND"
			      (format nil "ERROR: Fits header ~A not found" keyname)))
	  (t ;; success
	   (progn
	     (set-param "KEYWORD" keyword)
	     (set-param "VALUE"   value)
	     (set-param "COMMENT" comment/err))))))))

(def-json-command write-fits-header (json-req)
  (with-json-command-setup (json-req)
    (let* ((fits-file (get-param "FITS-FILE" :required t))
	   (keyword (get-param "KEYWORD" :required t))
	   (value (get-param "VALUE" :required t))
	   (comment (get-param "COMMENT" :default nil))
	   (extension   (get-param "EXTENSION" :default 1)))
      
      (jcom-test-expr (not (probe-file fits-file))
		      "FITS-NOT-FOUND"
		      (format nil "Fits file ~A not found" fits-file))    
      
      (multiple-value-bind (ret err)
	  (bt-ignore-errors
	    (cf:write-fits-header fits-file keyword value
				  :extension extension
				  :comment (if comment (format nil "~A" comment))))
	(when (not ret)
	  (return-with-error "WRITE-FITS-HEADER-ERROR"
			      (format nil "ERROR: ~a" err)))
	(set-param "SUCCESS" 'yason:true)))))

