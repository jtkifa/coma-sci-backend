
(in-package coma-json-server)

#|

Request:

{
    "TYPE":"REQUEST",
    "COMMAND":"REDUCE-IMAGES",
    "ID":"123ABC",
    "PARAMETERS": {
        "FITS-FILES":["image1.fits","image2.fits",..],
        "FITS-DIRECTORY" : "/some/path", // directory relative to which fits-files exist (OPTIONAL)
        "OUTPUT-DIRECTORY": "/directory/for/output"  // MANDATORY
        "MAKE-BIAS": true,       // true by default (must specify false to turn off)
        "MAKE-FLAT": true,       // true by default ...
        "REDUCE-OBJECTS": true,  // true by default ...
        "FRINGECORRECT":  true,  // true by default ...
    }
}


Response:

{
    "TYPE":"RESPONSE",
    "COMMAND":"REDUCE-IMAGES",
    "ID":"123ABC",
    "PARAMETERS":{
          "TYPE": "PARAMETERS",
          "ACCEPTED-FITS": ["image7.fits", ...],  // images that were initially accepted
          "REJECTED-FITS": ["image3.fits", ...],  // images that were rejected
          "REJECTION-REASONS": ["REASON..", ...], // why the images were rejected
          "BIAS-LIST": ["BIAS.fits"],       // final bias images (should be just one)
          "FLAT-LIST": ["FLAT-R.fits", ...],      // final flat images
          "FRINGE-FRAME-LIST": ["fringe-I.fits", ...], // final fringe images
          "REDUCED-LIST": ["image7_r.fits", ...], // reduced images (not fringe corrected)
          "FRINGECORRECTED-LIST": ["image7_fr.fits",...] // reduced images that were fringe corrected
    }
}




|#


(defun validate-reduction-fits-list (fits-list logger)
  (loop with id0 = (ignore-errors (instrument-id:identify-instrument (first fits-list)))
	with fits-list-out = nil
	with fits-list-failed = nil
	with why-failed = nil
	for fits in fits-list
	do
	   (multiple-value-bind (id bad-qualities)
	       (ignore-errors (instrument-id:validate-fits-file fits :output :list))
	     (cond ((not id)
		    (push fits fits-list-failed)
		    (push "NOT-IDENTIFIABLE-FITS" why-failed)
		    (logger:writelog
		     logger
		     (format nil "File ~A is not identifiable by INSTRUMENT-ID - dropping from list"
			     fits)
		     :log-type :error
		     :log-level 3))
		   (bad-qualities ;; list of image qualities we couldn't parse
		    (push fits fits-list-failed)
		    (push "UNREADABLE-QUALITIES" why-failed)
		    (logger:writelog
		     logger
		     (format nil "File ~A has un-readable qualities ~A - dropping from list"
			     fits bad-qualities)
		     :log-type :error
		     :log-level 3))
		   ((not (instrument-id:instruments-combinable-p id id0))
		    (push fits fits-list-failed)
		    (push "QUALITIES-INCOMPATIBLE-WITH-FIRST-FITS-FILE" why-failed)
		    (logger:writelog
		     logger
		     (format nil "File ~A is not combinable with first file ~A - dropping from list"
			     (first fits-list) fits)
		     :log-type :error
		     :log-level 3))
		   (t
		    (push fits fits-list-out))))
	finally
	   (return (values (reverse fits-list-out)
			   (reverse fits-list-failed)
			   (reverse why-failed)))))


(def-json-command reduce-images (json-req)
  (with-json-command-setup (json-req)
    (let* ((make-bias (get-param "MAKE-BIAS" :default T))
	   (make-flat (get-param "MAKE-FLAT" :default T))
	   (reduce-objects (get-param "REDUCE-OBJECTS" :default T))
	   (fringecorrect (get-param "FRINGECORRECT" :default T))
	   (output-directory
	     (get-param "OUTPUT-DIRECTORY" :required t
					   :satisfies 'stringp
					   :satisfies-desc "must be a string"))
	   (fits-list-raw (let ((fvec (get-param "FITS-FILES" :required t)))
			    (when (not (and (vectorp fvec)
					    (every 'stringp fvec)))
			      (return-with-error "FITS-FILES-NOT-A-VECTOR-OF-STRINGS"
						 "FITS-FILES is not a vector of strings"))
			    (coerce fvec 'list)))
	   ;; NIL or a directory without trailing slash
	   (fits-directory (let ((fd (get-param "FITS-DIRECTORY")))
			     (when (and fd (not (stringp fd)))
			       (return-with-error
				"FITS-DIRECTORY-IS-NOT-A-STRING"
				(format nil "ERROR: FITS-DIRECTORY=~A is not a string" fd)))
			     (when fd (string-right-trim "/" fd))))
	   (fits-list
	     (cond ((not fits-directory) fits-list-raw)
		   (t
		    (mapcar (lambda (fits) (format nil "~A/~A" fits-directory fits))
			    fits-list-raw))))
		       
	       
	   (log-file (let ((lf (format nil "~A/reduce-images.log" output-directory)))
		       (when (not (ignore-errors (ensure-directories-exist lf)))
			 (return-with-error "COULD-NOT-ENSURE-OUTPUT-DIRECTORY"
					    (format nil "Could not ensure existence of OUTPUT-DIRECTORY=~A"
						    output-directory)))
		       lf))
	   (logger (logger:make-file-logger log-file :if-exists :append)))

	  (multiple-value-bind (fits-list-accepted fits-list-rejected why-rejected)
	      (validate-reduction-fits-list fits-list logger)
	    
	    (set-param "ACCEPTED-FITS" (coerce fits-list-accepted 'vector))
	    (set-param "REJECTED-FITS" (coerce fits-list-rejected 'vector))
	    (set-param "REJECTION-REASONS" (coerce why-rejected 'vector))
	    
	    (let* ((reduction-plan (imred:make-reduction-plan-for-instrument
				    (instrument-id:identify-instrument (first fits-list-accepted))  
				    :make-zero   make-bias
				    :make-flats  make-flat
				    :ccdproc-objects reduce-objects
				    :fringecorrect fringecorrect
				    :logger logger
				    :target-dir  output-directory)))
	      
	      (multiple-value-bind (fileset err)
		  (ignore-errors
		   (imred:process-fits-list fits-list-accepted reduction-plan))
		(when (not fileset)
		  (return-with-error
		   "INTERNAL-IMRED-FAILURE"
		   (format nil "ERROR: Internal Failure ~A" err)))
		
		(flet ((vecify (file-list) ;; convert list to vector and strip out directory
			 (coerce (mapcar 'file-io:file-minus-dir file-list) 'vector)))
		  (set-param "BIAS-LIST"
			(vecify (imred:fileset-output-bias-list fileset)))
		  (set-param "FLAT-LIST"
			(vecify (imred:fileset-output-flat-list fileset)))
		  (set-param "FRINGE-FRAME-LIST"
			(vecify (imred:fileset-output-fringe-list fileset)))
		  (set-param "REDUCED-OBJECT-LIST"
			(vecify (imred:fileset-output-reduced-obj-list fileset)))
		  (set-param "FRINGECORRECTED-OBJECT-LIST"
			(vecify (imred:fileset-output-fringecorrected-obj-list fileset))))))))))
		
		
		
		
	    
				  

	  

	 

	  
	  
	  
	     

  
