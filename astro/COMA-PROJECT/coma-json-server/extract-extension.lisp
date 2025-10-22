
#|

Extract one more more extensins

{
  "TYPE":"REQUEST",
  "COMMAND":"EXTRACT-EXTENSION",
  "ID":"123abc",
  "PARAMETERS": {
                 "TYPE":"PARAMETERS", // optional
                 "FITS-FILE":"/dir/sample.fits",
                 "OUTPUT-DIRECTORY": "/place/to/put/data/",
                 
                 // only allowed if either (1) a single extension is requested; or (2) it's a 
                 // instrument-id:onechip class instrument
                 "OUTPUT-FITS": "output_name.fits",
                 
                 "DECOMPRESS": true, // defaults to TRUE - if fzipped, remove fzipping

                 "EXTRACT-EXTENSION": 11,   // a number,  "ALL" or a vector like [3,4,10,11,12]

                 // [optional] the extension the object is in
                 "OBJECT-EXTENSION":  11,    

                 // [optional] if given, create OBJECT and COMA.OBJECT headers to  
                 // override the object header for headers that are 
                 // NOT the object header
                 "RENAME-NON-OBJECT-EXTENSIONS": "none"
               }
}

A  response will look like:

{
    "TYPE":"RESPONSE”,
    "COMMAND": "EXTRACT-EXTENSION",
    "ID": "123abc",
    "PARAMETERS": {
                   "TYPE": "PARAMETERS",
                    "FITS-FILES": [
                                   { 
                                    "TYPE": "EXTRACTED-EXTENSION",
                                    "EXTENSION": 1,
                                    "FITS-FILE": "sample_1.fits",
                                    "OBJECT-NAME": "2P" // the final object name (eg "none")
                                    },
                                    .... // more extensions
                                  ]
                  }
}


|#

(in-package coma-json-server) 

#|

%EXTRACT-MAKE-FITSNAME  is a function to generate the extracted name
of a fits file.

Given a fits name like "/some/place/file.fits.fz"  transform it to 
"file.ext19.fits" (where, eg 19 is the extension number).  Any 'fz'
extension is removed and then replaced at end only if IS-COMPRESSED is
true and DECOMPRESS is false.

Any .fits / .fit / .flt suffix is stripped, and replaced by '.fits' at the end
but before .fz if present.

|#



(defun %extract-make-fitsname (fits-file iext &key decompress is-compressed)  
  (let* ((basename (file-io:file-minus-dir fits-file))
	 (dot-split (string-utils:split-string basename "."))
	 (dot-split-rev (reverse dot-split))
	 (ncomp (length dot-split)))
    ;; Is the last component 'fz'?  If so, remove it.
    ;; It will be added on if needed.
    (when (and (> ncomp 1) 
	       (equalp (car dot-split-rev) "fz"))
      (decf ncomp)
      (setf dot-split-rev (cdr dot-split-rev)))
    ;; is the now-last component ".fits" or similiar?  If so, remove
    ;; it.  ".fits" will be added if needed
    (when (and (> ncomp 1) 
	       (member (car dot-split-rev)
		       '("fits" "flt" "fit")
		       :test 'equalp))
      (decf ncomp)
      (setf dot-split-rev (cdr dot-split-rev)))
    ;; rebuild the string
    (with-output-to-string (s)
      (loop for i from 0
	    for comp in (reverse dot-split-rev)
	    do (write-string comp s)
	       (write-string "."s))
      ;; add extension
      (when iext
	(format s "ext~2,'0D." iext))
      (write-string "fits" s)
      ;; add .fz only if compressed and not decompressing
      (when (and is-compressed (not decompress))
	(write-string ".fz" s)))))

	   

(def-json-command extract-extension (json-req)
  (with-json-command-setup (json-req)
    (let* ((fits-file (get-param "FITS-FILE" :required t))
	   ;; if extension=NIL then use the default single-image
	   ;; extension, or the first finite image extension
	   (extract-extension
	     (get-param "EXTRACT-EXTENSION"
			:default "all"
			:satisfies (lambda (ext)
				     (or (equalp ext "ALL")
					 (and (integerp ext) (plusp ext))
					 (and (vectorp ext)
					      (every (lambda (val)
						       (and (integerp val) (plusp val)))
						     ext))))
			:satisfies-desc "Must be 'all', a positive integer, or a vector of positive integers"))
	   (object-extension
	     (get-param "OBJECT-EXTENSION"
			:satisfies (lambda (ext) (and (integerp ext) (plusp ext)))
			:satisfies-desc "Must be a positive integer"))
           (output-directory (get-param "OUTPUT-DIRECTORY" :satisfies 'stringp))
	   (output-fits      (get-param "OUTPUT-FITS" :satisfies 'stringp))
	   (decompress (get-param "DECOMPRESS" :default t))
	   (rename-non-object (get-param "RENAME-NON-OBJECT-EXTENSIONS"))
	   
	   ;; for inst, throw an error if FITS-FILE not found, then another one
	   ;; if fits file can't be ID'd
	   (inst  (progn
		     (when (not (bt-ignore-errors (probe-file fits-file)))
		       (return-with-error
			"FITS-FILE-NOT-FOUND"
			(format nil "Fits file ~A not found" fits-file)))
		     ;;
		    (or (bt-ignore-errors
			 (instrument-id:identify-instrument fits-file))
		      (return-with-error  "COULD-NOT-IDENTIFY-FITS-FILE"
					  "Could not identify type of fits file."))))
	   ;;
	   (onechip-p (typep inst 'instrument-id:onechip))
	   (extensions-out (make-array 0 :adjustable t :fill-pointer 0)))


      ;; this test isn't really needed 
      (when (typep inst 'instrument-id:onechip)
	(let ((image-ext (instrument-id:get-image-extension-for-onechip-instrument
			  inst fits-file)))
	  (when (not (or (equalp extract-extension "all")
			 (eql    extract-extension image-ext)
			 (and (vectorp extract-extension)
			      (every (lambda (ext) (eql ext image-ext)) extract-extension))))
	    (return-with-error "WRONG-ONECHIP-EXTENSION"
			       (format nil "Instrument ~A of type onechip has image extension ~A but extension ~A requested"
				       inst image-ext extract-extension)))))

      (when (not (stringp output-directory))
	(return-with-error "NO-OUTPUT-DIRECTORY-GIVEN"
			   "OUTPUT-DIRECTORY parameter not given."))

      (when (and output-fits
		 (not (or (integerp extract-extension)
			  onechip-p)))
	(return-with-error "OUTPUT-FITS-INVALID"
			   "OUTPUT-FITS cannot be specified unless only a single extension is being extracted."))
		     
      
      (setf output-directory (concatenate 'string (string-right-trim "/" output-directory) "/"))

      (when (not (bt-ignore-errors (ensure-directories-exist (concatenate 'string output-directory "IGNORE"))))
	(return-with-error
	 "CANNOT-MAKE-OUTPUT-DIRECTORY"
	 (format nil "Cannot use or make OUTPUT-DIRECTORY '~a'" output-directory)))
      
      ;; open fits file and march through extensions
      (cf:with-open-fits-file (fits-file ff)
	(loop
	  ;; list of all extensions to extract
	  with extensions = (cond (onechip-p
				    (list
				     (instrument-id:get-image-extension-for-onechip-instrument
				      inst ff)))
				  ((equalp extract-extension "all")
				   (loop for i from 1 to (cf:fits-file-num-hdus ff) collect i))
				  ((vectorp extract-extension)
				   (coerce extract-extension 'list))
				  (t ;; an integer
				   (list extract-extension)))
	  with original-object-name = (instrument-id:get-object-for-instrument inst ff)
	  for iext in extensions
	  ;; set COMA.OBJECT header if not the object object, and if renaming
	  for write-coma-object = nil 
	  for ext-objname = (cond
			      ;; if in object's extension, always use original name
			      ((eql object-extension iext)
			       original-object-name)
			      ;; else if object extension is given, and if we're renaming
			      ;; use the renamed version
			      ((and object-extension rename-non-object)
			       (setf write-coma-object t) ;; kludgy
			       rename-non-object)
			      ;; otherwise, keep original name
			      (t
			       original-object-name))
	  do
	     (cf:move-to-extension ff iext)
	     ;;
	     (when ;; is it an image, and is it being extracted?
		 (and
		  ;; is it an image?
		  (eq (cf:fits-file-current-hdu-type ff) :image)
		  (let ((imsize (cf:fits-file-current-image-size ff)))
		    (and (eql (length imsize) 2)
			 (> (aref imsize 0) 1)
			 (> (aref imsize 1) 1)))
		  ;; is this extension being extracted?
		  (or (equalp extract-extension "all")
		      (eql extract-extension iext)
		      (and (vectorp extract-extension)
			   (find iext extract-extension :test 'eql))))
	       (let* ((ext-is-compressed (cf:compressed-image-p ff))
		      (outfits-base (or output-fits ;; only for onechip or one extension
					(%extract-make-fitsname fits-file iext
								:decompress decompress
								:is-compressed ext-is-compressed)))
		      (outfits (concatenate 'string output-directory outfits-base))
		      (ext-obj
			(alexandria:alist-hash-table
			 `(("TYPE" . "EXTRACTED-EXTENSION")
			   ("EXTENSION" . ,iext)
			   ("FITS-FILE" . ,outfits-base)
			   ("OBJECT-NAME" . ,ext-objname))
			 :test 'equalp)))
		 (multiple-value-bind (result err)
		     (bt-ignore-errors
		       (progn
			 (instrument-id:extract-one-image-from-mosaic-fits 
			  ff iext outfits
			  :decompress decompress
			  :overwrite t)
			 (when write-coma-object
			   (cf:write-fits-header outfits "OBJECT" ext-objname :extension 1)
			   (cf:write-fits-header outfits "COMA.OBJECT" ext-objname :extension 1)) 
			 t))
		   (when (not result)
		     (setf (gethash "ERROR" ext-obj) (format nil "ERROR: ~A" err))))
		 (vector-push-extend  ext-obj extensions-out)))))

      (set-param "FITS-FILES" extensions-out))))

      
		   
		   


	  
	     
							      
					      
		     


      
