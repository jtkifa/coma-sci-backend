
(in-package coma-json-server)

#|

Given an image set (for reduction), splits it up into compatible groups
by instrument, chip-id, and the sizes of all the extensions

Request:

{
    "TYPE":"REQUEST",
    "COMMAND":"SPLIT-IMAGES-INTO-COMPATIBLE-SETS",
    "ID":"123ABC",
    "PARAMETERS": {
        "FITS-FILES": ["image1.fits","image2.fits",..],
        "FITS-DIRECTORY" : "/some/path", // directory relative to which fits-files exist (OPTIONAL)
    }
}


Response:

{
    "TYPE":"RESPONSE",
    "COMMAND":"SPLIT-IMAGES-INTO-COMPATIBLE-SETS",
    "ID":"123ABC",
    "PARAMETERS":{
          "TYPE": "PARAMETERS",
          "REJECTED-FITS": ["image99.fits", "image100.fits", ...],
          "REJECTION-REASONS": ["reason for image99", "reason for image100"],
          "COMPATIBLE-IMAGE-SETS":
             [
               {
                  "TYPE": "COMPATIBLE-IMAGE-SET",
                  "INSTRUMENT": "some-instrument",  // using INSTRUMENT-ID package notation
                  "CHIP-ID": 1,  // the chip-id for a one-chip instrument
                  "EXTENSION-SIGNATURE": [[1024,1024]]  // vector of image sizes for each extension
                  "FITS-FILES": ["image1.fits","image2.fits",..]
               },
              ...
             ]
    }
}




|#





(def-json-command split-images-into-compatible-sets  (json-req)
  (let* ((parameters-out (make-hash-table :test 'equalp))
	 (parameters (json-object-parameters json-req))
	 (json-resp
	   (combobulate-result-object ;; set up ID, COMMAND, etc
	    json-req
	    (make-json-object
	     :type "RESPONSE"
	     :parameters parameters-out))))
    
    (block retblock
      (flet ((return-with-error (err-name err-desc)
	       (setf (json-object-error json-resp)
		     (make-error-object
		      :error err-name
		      :desc err-desc))
	       (return-from retblock json-resp)))
	
	(let* ((fits-list (let ((fvec (or (gethash "FITS-FILES" parameters)
					  (return-with-error "FITS-FILES-NOT-SPECIFIED"
							     "ERROR: FITS-FILES must be given."))))
			    (when (not (and (vectorp fvec)
					    (every 'stringp fvec)))
			      (return-with-error "FITS-FILES-NOT-A-VECTOR-OF-STRINGS"
						 "FITS-FILES is not a vector of strings"))
			    (coerce fvec 'list)))
	       ;; NIL or a directory without trailing slash
	       (fits-directory (let ((fd (gethash "FITS-DIRECTORY" parameters)))
				 (when (and fd (not (stringp fd)))
				   (return-with-error
				    "FITS-DIRECTORY-IS-NOT-A-STRING"
				    (format nil "ERROR: FITS-DIRECTORY=~A is not a string" fd)))
				 (when fd (string-right-trim "/" fd)))))


	  (multiple-value-bind (comset-list rejected-fits-list rejection-reasons)
	      (imred:divide-image-list-into-compatible-image-sets fits-list :directory fits-directory)
	    
	    (setf (gethash "REJECTED-FITS" parameters-out) (coerce rejected-fits-list 'vector))
	    (setf (gethash "REJECTION-REASONS" parameters-out) (coerce rejection-reasons 'vector))

	    (loop for comset in comset-list
		  for comset-json  = (let ((h (make-hash-table :test 'equalp)))
				       (setf (gethash "TYPE" h)  "COMPATIBLE-IMAGE-SET")
				       (setf (gethash "INSTRUMENT" h)
					     (string (imred:compatible-image-set-instrument comset)))
				       (setf (gethash "CHIP-ID" h)
					     (imred:compatible-image-set-chip-id comset))
				       (setf (gethash "EXTENSION-SIGNATURE" h)
					     (imred:compatible-image-set-extension-signature comset))
				       (setf (gethash "FITS-FILES" h)
					     (imred:compatible-image-set-file-list comset))
				       h)
		  collect comset-json into outlist
		  finally
		     (setf (gethash  "COMPATIBLE-IMAGE-SETS" parameters-out)
			   (coerce outlist 'vector)))

	    json-resp))))))
				       
					      
		
		
	    
				  

	  

	 

	  
	  
	  
	     

  
