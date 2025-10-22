
#|

{
  "TYPE":"COMMAND",
  "REQUEST":"SHIFT-AND-ADD",
  "ID":"123abc",
  "PARAMETERS": {"FITS-FILES": ["file1.fits", "file2.fits", ...],
                 "FITS-DIRECTORY" : "/some/path", // directory relative to which fits-files exist (optional)
                 "OUTBASE":    "MyOutputFitsName",
                 "OUTPUT-DIRECTORY": "./",
                 "MASK"   :    false,
                 "STATIC-SKY-SUBTRACT" : false,
                 "OBJECT-NAME": "76P"   // otherwise use object in first image

                 // the usual ORBIT object, or strings describing where to obtain the orbit
                 "ORBIT": {ID: .. EPOCH-MJD: .. ...}, // or "JPL-ORBIT" or "MPC-ORBIT"

                 // images further than this (arcsec) from first image will
                 // be rejected, to prevent exessively large stacks
                 "MAX-SKY-SPREAD": 1800
           
              
               }
}


The output parameters are:

{
  "SUCCESS": true | false,
  "FAILURE-REASON": "...", // present if SUCCESS=false
  "OUTPUT-STACK": "/path/to/output_stack.fits",
  "ACCEPTED-IMAGES": ["good1.fits" , ...],
  "REJECTED-IMAGES":  ["bad1.fits" , ...],
  "REJECTION-REASONS": ["REASON-1", ...]
}



|#

(in-package coma-json-server)


(def-json-command shift-and-add (json-req)
  (with-json-command-setup (json-req)
    (let* ((fits-files-vec (get-param "FITS-FILES" :required t))
	   (outbase    (get-param  "OUTBASE" :default "stack"))
	   (output-directory (get-param "OUTPUT-DIRECTORY" :default "./"))
	   (mask  (get-param "MASK"))
	   (static-sky-subtract (get-param "STATIC-SKY-SUBTRACT"))
	   (object-name-given (get-param "OBJECT-NAME"))
	   (json-orbit (get-param "ORBIT"))
	   (directory (get-param "FITS-DIRECTORY")) ;; NIL is OK - means use literal fits path
	   ;;
	   (fits-file0 (if (and (vectorp fits-files-vec) 
				(plusp (length fits-files-vec)))
			   (elt fits-files-vec 0)))
	   (resampling "LANCZOS3")
	   (max-sky-spread 1800)
	   (object-name
	     (or object-name-given
		 (ignore-errors
		  (first
		   (small-body-name:parse-small-body-name 
		    (instrument-id:get-object-for-fits fits-file0))))))
	   (mjd (instrument-id:get-mjd-mid-for-fits fits-file0))
	   (orbit (if (and json-orbit mjd)
		      (get-orbit-using-method
		       json-orbit object-name :mjd mjd))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (when (not (and (vectorp fits-files-vec)
		      (plusp (length fits-files-vec))
		      (every 'stringp fits-files-vec)))
	(return-with-error
	 "INVALID-FITS-FILES"
	 "FITS-FILES should be a vector of non-zero length consisting of fits files (strings)"))
		 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	  ;;
	  ;;
      (when (not mjd)
	(return-with-error "MJD-NOT-FOUND"
			   (format nil "Could not get MJD of first first file.")))

      (when (and json-orbit (not orbit))
	(return-with-error
	 "FAILED-TO-PARSE-ORBIT"
	 (format
	  nil
	  "Failed to parse or obtain provided ORBIT object ~A"
	  (yason:encode orbit)))) ;; will encode hash or string


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (when (not (ignore-errors (ensure-directories-exist
				 (format nil "~A/#IGNORE#"
					 (string-right-trim "/" output-directory )))))
	(return-with-error "CANNOT-CREATE-OUTPUT-DIRECTORY"
			   (format nil "Cannot create output directory ~A" output-directory)))
      
      (let* ((logfile-name  (format nil "~A/shift-and-add.log" (string-right-trim "/" output-directory)))
	     (logger (or (ignore-errors (logger:make-file-logger logfile-name :if-exists :supersede))
			 (return-with-error "CANNOT-CREATE-LOG-FILE"
					    (format nil "Cannot create log file ~A" logfile-name))))
	     
	     (saaplan (make-instance
		       'shift-and-add:saaplan  
		       :object-name object-name
		       :imageout-base outbase
		       :output-directory output-directory
		       :observatory nil
		       :locator (shift-and-add:build-orbit-locator orbit)
		       :swarp-resampling-type resampling
		       :max-sky-spread max-sky-spread
		       :logger logger
		       :image-preproc
		       (when static-sky-subtract
			 (make-instance
			  'shift-and-add:static-sky-subtract-preproc
			  ;; FIXME - we don't use this option because it seems ineffective
			  :mask-object nil
			  ;;
			  ;; :pre-subtract-fits static-sky-presub-fits
			  ;; :pre-subtract-radius static-sky-presub-radius
			  ))
		       :image-weighter
		       (when mask
			 (make-instance 'shift-and-add:stacked-masker)))))
	
	
	(multiple-value-bind (success sresult)
	    (ignore-errors
	     (shift-and-add:shift-and-add-fits-list (coerce fits-files-vec 'list)
						    :saaplan saaplan
						    :directory directory))
	  
	  (when (and (not success)
		     (not (shift-and-add:stack-result-p sresult)))
	    (return-with-error
	     "INTERNAL-ERROR-SHIFT-AND-ADD"
	     (format nil "Internal Error running shift-and-add: ~A" sresult)))
	  ;;
	  (set-param "OUTPUT-STACK" 
		     (shift-and-add:stack-result-output-stack sresult))
	  (set-param "ACCEPTED-IMAGES" 
		     (shift-and-add:stack-result-accepted-fits-list sresult))
	  (set-param "REJECTED-IMAGES" 
		     (shift-and-add:stack-result-rejected-fits-list sresult))
	  (set-param "REJECTION-REASONS"   
		     (shift-and-add:stack-result-reject-reasons-list sresult))
	  ;;
	  (set-param "SUCCESS"  (not (not success)))
	  (when (not success)
	    (set-param "FAILURE-REASON" 
		       (or (shift-and-add:stack-result-error sresult)
			   "UNKNOWN"))
	    (return-with-error
	     "SHIFT-AND-ADD-FAILED"
	     (format nil "Failure reason: ~A"
		     (or (shift-and-add:stack-result-error sresult) "UNKNOWN")))) )))))

